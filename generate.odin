package odin_win32_generator

import "core:os"
import "core:io"
import "core:encoding/json"
import "core:fmt"
import "core:path/filepath"
import "core:strings"
import "core:thread"
import sync "core:sync/sync2"


Api_Type_Name_To_Api_Ref_Map :: struct {
	top_level: map[string]map[Api_Ref]bool,
	nested:    map[string]map[Api_Ref]bool,
}

File :: struct {
	pkg: ^Pkg,
	full_path: string,
	base_path: string,
	api:       string,
	name: string,
	data: []byte,
	root: json.Value,

	type_map: Api_Type_Name_To_Api_Ref_Map,
}

Api_Ref :: struct {
	api:  string,
	name: string,
}


Pkg :: struct {
	name: string,
	files: [dynamic]^File,
	deps: map[^Pkg]bool,
}


Generator :: struct {
	files: [dynamic]^File,
	file_map: map[string]^File,
	pkgs: map[string]^Pkg, // Key: package name

	api_type_map: map[string]^Api_Type_Name_To_Api_Ref_Map,

	full_type_set:      map[Api_Ref]bool,
	api_cycle_type_set: map[Api_Ref][]Api_Ref,

	api_to_file_map: map[string]^File,
	api_to_pkg_map: map[string]^Pkg,
	cycle_types: map[Api_Ref]json.Value,

	type_in_pkg_types: map[string]bool,

	apis_not_needed_for_types: map[string]bool,

	type_names_to_remove: map[Api_Ref]bool,

	procedures_to_ignore: map[Api_Ref]bool,
}
trim_at_first_dot :: proc(name: string) -> string {
	n := strings.index_byte(name, '.');
	if n < 0 {
		n = len(name);
	}
	return name[:n];
}

get_api_name_from_file_name :: proc(name: string, allocator := context.allocator) -> string {
	n := strings.last_index_byte(name, '.');
	if n < 0 {
		n = len(name);
	}
	return name[:n];
}
get_package_name_from_file_name :: proc(name: string, allocator := context.allocator) -> string {
	n := strings.index_byte(name, '.');
	if n < 0 {
		n = len(name);
	}
	return strings.to_snake_case(name[:n], allocator);
}

is_anon_type :: proc(type_name: string) -> bool {
	return strings.has_suffix(type_name, "_e__Struct") || strings.has_suffix(type_name, "_e__Union");
}

get_api_ref_top_level_type :: proc(type_obj: json.Object) -> string {
	parents := type_obj["Parents"];
	if parents != nil {
		if array, ok := parents.(json.Array); ok && len(array) != 0 {
			return array[0].(string);
		}
	}
	return type_obj["Name"].(string);
}

get_json_api_refs :: proc(api_refs: ^map[Api_Ref]bool, json_obj: json.Value) {
	switch v in json_obj {
	case json.Object:
			if name, _ := v["Kind"].(string); name == "ApiRef" {
				key := Api_Ref{
					api = v["Api"].(string),
					name = get_api_ref_top_level_type(v),
				};
				api_refs[key] = true;
				return;
			}
			for _, entry in v {
				get_json_api_refs(api_refs, entry);
			}
	case json.Array:
		for entry in v {
			get_json_api_refs(api_refs, entry);
		}
	case json.Null, string, i64, f64, bool:
		// ignore
	}
}

get_nested_name :: proc(type_name: string, ref: Api_Ref) -> string {
	type_names := strings.split(type_name, ".", context.temp_allocator);
	ref_names  := strings.split(ref.name,  ".", context.temp_allocator);
	i := 0;
	for i < len(ref_names) && i < len(type_names) {
		if ref_names[i] != type_names[len(type_names)-i-1] {
			break;
		}
		i += 1;
	}
	b: [dynamic]byte;
	j := 0;
	for n in type_names {
		if j > 0 {
			append(&b, '.');
		}
		append(&b, n);
		j += 1;
	}

	for n in ref_names[i:] {
		if j > 0 {
			append(&b, '.');
		}
		append(&b, n);
		j += 1;
	}

	return string(b[:]);
}

apis_to_ignore := []string{
	"AI.",
	"Data.",
	"Web.",
	"System.Diagnostics.Debug",
};

do_ignore_api :: proc(api: string) -> bool {
	for i in apis_to_ignore {
		if strings.has_prefix(api, i) {
			return true;
		}
	}
	return false;
}

functions_to_ignore := []Api_Ref{
	{"System.TpmBaseServices", "GetDeviceID"},
};

calculate_type_graph :: proc(g: ^Generator) {
	Type_Node :: struct {
		preds: map[Api_Ref]bool,
		succs: map[Api_Ref]bool,
	};
	Type_Graph :: map[Api_Ref]^Type_Node;

	graph: Type_Graph;
	defer delete(graph);


	for file in g.files {
		table := &file.type_map;
		for type_name, refs in table.top_level {
			type_ref := Api_Ref{file.api, type_name};
			node := new(Type_Node);
			node.succs = refs;
			graph[type_ref] = node;
		}
		for type_name, refs in table.nested {
			type_ref := Api_Ref{file.api, type_name};
			node := new(Type_Node);
			node.succs = refs;
			graph[type_ref] = node;
		}
	}

	for file in g.files {
		table := &file.type_map;
		for type_name, refs in table.top_level {
			type_ref := Api_Ref{file.api, type_name};
			node := graph[type_ref];
			assert(node != nil);
			for succ in node.succs {
				succ_node := graph[succ];
				if succ_node != nil {
					fmt.assertf(succ_node != nil, "%v", succ);
					succ_node.preds[type_ref] = true;
				}
			}
		}
		for type_name, refs in table.nested {
			type_ref := Api_Ref{file.api, type_name};
			node := graph[type_ref];
			assert(node != nil);
			for succ in node.succs {
				succ_node := graph[succ];
				if succ_node != nil {
					fmt.assertf(succ_node != nil, "%v", succ);
					succ_node.preds[type_ref] = true;
				}
			}
		}
	}

	for type_ref, node in graph {
		if len(node.preds) == 0 {
			g.type_names_to_remove[type_ref] = true;
		}
	}

	fmt.println("TYPE GRAPH DONE");
}

main :: proc() {
	defer fmt.println("[Done]");

	g := &Generator{};

	filepaths, err := filepath.glob("win32json/api/*.json");
	assert(err == nil);

	total_file_data_size := 0;

	fmt.println("[Collect Files]");
	// Collect file
	for path in filepaths {
		filename := filepath.base(path);
		base_path := filename;
		filename = filename[:strings.last_index_byte(filename, '.')];
		if n := strings.index_byte(filename, '.'); n >= 0 {
			filename = filename[n+1:];
		}
		filename, _ = strings.replace_all(filename, ".", "_");
		filename = strings.to_snake_case(filename);

		if do_ignore_api(base_path) {
			continue;
		}

		data, ok := os.read_entire_file(path);
		assert(ok);

		total_file_data_size += len(data);

		append(&g.files, new_clone(File{
			full_path = path,
			base_path = base_path,
			name = filename,
			data = data,
		}));
	}

	// Sort files into packages
	for file in g.files {
		pkg_name := get_package_name_from_file_name(file.base_path, context.temp_allocator);

		pkg, pkg_exists := g.pkgs[pkg_name];
		if !pkg_exists {
			pkg = new(Pkg);
			pkg.name = pkg_name;
			g.pkgs[strings.clone(pkg_name)] = pkg;
		}
		file.pkg = pkg;
		append(&pkg.files, file);

		file.api = get_api_name_from_file_name(file.base_path);
		g.api_type_map[file.api] = &file.type_map;
		g.api_to_file_map[file.api] = file;
		g.api_to_pkg_map[file.api] = pkg;
	}

	fmt.println("[Parse Files]");

	@static wg: sync.Wait_Group;

	// Thread pool? Who needs that? ;)
	sync.wait_group_add(&wg, len(g.files));
	for file in g.files {
		thread.run_with_poly_data(file, proc(file: ^File) {
			root, err := json.parse(file.data, .JSON, /*parse integers*/true);
			if err != nil {
				fmt.eprintf("Error parsing file %s: %v\n", file.full_path, err);
				os.exit(1);
			}
			file.root = root;
			sync.wait_group_done(&wg);
		});
	}
	sync.wait_group_wait(&wg);

	fmt.println("[Loading Types]");

	for file in g.files {
		add_type_refs :: proc(type_map: ^Api_Type_Name_To_Api_Ref_Map, name_prefix: string, type_json: json.Value) {
			type_obj := type_json.(json.Object);
			kind := type_obj["Kind"].(string);
			name := type_obj["Name"].(string);
			full_name := strings.concatenate({name_prefix, name});

			api_refs: map[Api_Ref]bool;
			switch kind {
			case "Struct", "Union":
				get_json_api_refs(&api_refs, type_obj["Fields"]);
				nested_types, _ := type_obj["NestedTypes"].(json.Array);
				for nested_type in nested_types {
					add_type_refs(type_map, strings.concatenate({full_name, "."}), nested_type);
				}
			case "Enum", "ComClassID":
				get_json_api_refs(&api_refs, type_json);
				assert(len(api_refs) == 0);
			case "Com", "FunctionPointer", "NativeTypedef":
				get_json_api_refs(&api_refs, type_json);
			case:
				panic(kind);
			}

			table: ^map[string]map[Api_Ref]bool;
			table = &type_map.top_level;
			if len(name_prefix) != 0 {
				table = &type_map.nested;
			}

			if full_name in table^ {
				t := &table[full_name];
				for k, v in api_refs {
					t[k] = v;
				}
			} else {
				table[full_name] = api_refs;
			}
		}

		root := file.root.(json.Object);
		types := root["Types"].(json.Array);
		for type in types {
			add_type_refs(&file.type_map, "", type);
		}
	}

	fmt.println("[Check Type Refs Exist]");

	for file in g.files {
		i := 0;
		for type_name, refs in file.type_map.top_level {
			i += 1;
			if len(refs) == 0 {
				continue;
			}

			for ref in refs {
				if do_ignore_api(ref.api) {
					// fmt.println(file.api, type_name);
					g.type_names_to_remove[{file.api, type_name}] = true;
					continue;
				}
				if !is_anon_type(ref.name) {
					table := g.api_type_map[ref.api];
					fmt.assertf(table != nil, "%s needed by %s", ref.api, type_name);
					if ref.name not_in table.top_level {
						nested_name := get_nested_name(type_name, ref);
						defer delete(nested_name);
						assert(nested_name in table.nested, "nested_name not found");
					}
				}
			}
		}
	}

	fmt.println("[Calculate Entity Count]");
	total_const_count := 0;
	total_type_count := 0;
	total_proc_count := 0;
	for file in g.files {
		root := file.root.(json.Object);
		constants, _ := root["Constants"].(json.Array);
		types    , _ := root["Types"].(json.Array);
		functions, _ := root["Functions"].(json.Array);
		total_const_count += len(constants);
		total_type_count  += len(types);
		total_proc_count  += len(functions);

	}
	fmt.println("[Constant  Max Count]", total_const_count);
	fmt.println("[Type      Max Count]", total_type_count);
	fmt.println("[Procedure Max Count]", total_proc_count);

	fmt.println("[Calculate Type References]");
	api_recursive_type_refs_table_pkg: map[^Pkg]map[string][dynamic][]Api_Ref;
	api_recursive_type_refs_table_api: map[string]map[string][dynamic][]Api_Ref;

	for file in g.files {
		get_recursive_chains :: proc(
			g: ^Generator,
			table: ^Api_Type_Name_To_Api_Ref_Map,
			handled: ^map[Api_Ref]bool,
			refs: map[Api_Ref]bool,
			result: ^[dynamic][]Api_Ref,
			base_chain: []Api_Ref,
		) {
			for ref in refs {
				table := g.api_type_map[ref.api];
				if table == nil {
					continue;
				}
				if ref in g.type_names_to_remove {
					continue;
				}
				if !is_anon_type(ref.name) && ref.name in table.top_level {
					next_chain := make([]Api_Ref, len(base_chain)+1);
					copy(next_chain, base_chain);
					next_chain[len(base_chain)] = ref;

					ref_refs := table.top_level[ref.name];
					if len(ref_refs) == 0 || ref in handled^ {
						append(result, next_chain);
					} else {
						handled[ref] = true;
						get_recursive_chains(g, table, handled, ref_refs, result, next_chain);
					}
				}
			}
		}

		handled: map[Api_Ref]bool;
		defer delete(handled);

		recursive_type_refs_table: map[string][dynamic][]Api_Ref;

		table := &file.type_map;
		for type_name, refs in table.top_level {
			if (Api_Ref{file.api, type_name} in g.type_names_to_remove) {
				continue;
			}

			clear(&handled);
			recursive_chains: [dynamic][]Api_Ref;
			get_recursive_chains(g, table, &handled, refs, &recursive_chains, nil);

			recursive_type_refs_table[type_name] = recursive_chains;
		}

		if file.pkg not_in api_recursive_type_refs_table_pkg {
			api_recursive_type_refs_table_pkg[file.pkg] = make(map[string][dynamic][]Api_Ref);
		}
		prev, _ := &api_recursive_type_refs_table_pkg[file.pkg];
		for k, v in recursive_type_refs_table {
			prev[k] = v;
		}

		api_recursive_type_refs_table_api[file.api] = recursive_type_refs_table;
	}

	fmt.println("[Calculate Package Dependency Graph]");
	for pkg, m in api_recursive_type_refs_table_pkg {
		for type_name, refs_da in m do for refs in refs_da {
			for ref in refs {
				other_pkg := g.api_to_pkg_map[ref.api];
				if other_pkg != nil && other_pkg != pkg {
					pkg.deps[other_pkg] = true;
				}
			}
		}
	}

	fmt.println("[Calculate Procedure References]");
	for file in g.files {
		file_obj := file.root.(json.Object);
		functions, _ := file_obj["Functions"].(json.Array);
		func_loop: for func in functions {
			obj := func.(json.Object);
			name := obj["Name"].(string);

			proc_ref := Api_Ref{file.api, name};

			refs: map[Api_Ref]bool;
			defer delete(refs);
			get_json_api_refs(&refs, obj["ReturnType"]);
			get_json_api_refs(&refs, obj["Params"]);
			for ref in refs {
				if do_ignore_api(ref.api) || ref in g.type_names_to_remove {
					// fmt.printf("\tIgnoring Procedure: %s %s\n", proc_ref.api, proc_ref.name);
					g.procedures_to_ignore[proc_ref] = true;
					continue func_loop;
				}
			}
		}
	}

	for proc_ref in functions_to_ignore {
		g.procedures_to_ignore[proc_ref] = true;
	}

	// fmt.println("[Writing Package Files]");
	// write_to_odin(g);
}
