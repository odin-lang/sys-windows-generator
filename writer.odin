package odin_win32_generator

import "core:encoding/json"
import "core:fmt"
import "core:io"
import "core:os"
import "core:slice"
import "core:strings"
import "core:strconv"


BUILTIN_PKG :: "_builtin";
TYPES_PKG :: "_types";

Grouped_Procedures :: struct {
	dll_import: string,
	procedures: [dynamic]json.Value,
}

File_Generator :: struct {
	g: ^Generator,
	fd: os.Handle,
	b: ^strings.Builder,
	file: ^File,
	fullpath: string,

	imports: map[string]bool,
	foreign_imports: map[string]string, // Key: lib, Value: identifier
	procedures: map[string]^Grouped_Procedures, // Key: DLL Import
	ignore_imports: bool,
	is_types_pkg: bool,

	disallow_maybe: bool,

	all_decls: ^map[string]bool,

	depth: int,
	nested_types: json.Array,

	nested_types_to_print: [dynamic]json.Value,

	missing_clr_types: ^map[string]json.Object,
}

odin_keywords := map[string]string{
	"auto_cast"   = "auto_cast_",
	"bit_set"     = "bit_set_",
	"cast"        = "cast_",
	"context"     = "context_",
	"defer"       = "defer_",
	"distinct"    = "distinct_",
	"dynamic"     = "dynamic_",
	"fallthrough" = "fallthrough_",
	"foreign"     = "foreign_",
	"in"          = "in_",
	"map"         = "map_",
	"package"     = "package_",
	"proc"        = "proc_",
	"transmute"   = "transmute_",
	"when"        = "when_",
	"where"       = "where_",

	// other special things

	"Node"        = "node",
};


value_type_to_odin_type :: proc(value: string) -> string {
	switch value {
	case "Byte":        return "u8";
        case "UInt16":      return "u16";
        case "Int32":       return "i32";
        case "UInt32":      return "u32";
        case "Int64":       return "i64";
        case "UInt64":      return "u64";
        case "Single":      return "f32";
        case "Double":      return "f64";
        case "String":      return "string";
        case "PropertyKey":
        	panic("cannot call value_type_to_odin_type for \"PropertyKey\"");
	}
	fmt.panicf("unknown value %s", value);
}


write_constant :: proc(fg: ^File_Generator, w: io.Writer, constant: json.Value) {
	obj := constant.(json.Object);
	name := obj["Name"].(string);
	value_type, _ := obj["ValueType"].(string);

	if value_type == "PropertyKey" {
		// TODO(bill): GUID shit
		return;
	}

	if name == "INVALID_HANDLE_VALUE" {
		fmt.wprintf(w, "%s :: HANDLE(~uintptr(0));\n", name);
		return;
	}
	switch v in obj["Value"] {
	case i64, f64, bool:
		fmt.wprintf(w, "%s :: %v;\n", name, v);
	case string:
		fmt.wprintf(w, "%s :: %q;\n", name, v);

	case json.Array, json.Object, json.Null:
		fmt.panicf("%s: %v", fg.file.name, v);
	}
}

Record_Kind :: enum {None, Struct, Union};
get_anon_kind :: proc(s: string) -> Record_Kind {
	if strings.has_suffix(s, "_e__Struct") {
		return .Struct;
	} else if strings.has_suffix(s, "_e__Union") {
		return .Union;
	}
	return .None;
}

Field_Option :: enum {
	// Reason
	Direct_Type_Access, // If not set, assumed to be a variable/field declaration of some kind

	// Modifiers
	Optional,
	// Unused modifiers
	Const,
	Not_Null_Terminated,
	Is_Null_Terminated,
	Obsolete,
}

Field_Options :: distinct bit_set[Field_Option];

parse_field_options :: proc(field_attrs: json.Array) -> (field_options: Field_Options) {
	for attr_obj in field_attrs {
		#partial switch attr in attr_obj {
		case string:
			switch attr {
			case "Const":
				field_options += {.Const};
			case "NotNullTerminated":
				field_options += {.Not_Null_Terminated};
			case "NullNullTerminated":
				field_options += {.Is_Null_Terminated};
			case "Obselete": // This is a typo in the win32json generation
				field_options += {.Obsolete};
			case "Optional":
				field_options += {.Optional};
			}
		}
	}
	return;
}

Depth_Kind :: enum {
	Top_Level, Child, Array, Return,
}

Target_Kind :: enum {
	Default,
	Function_Pointer,
	Com,
}

add_import :: proc(fg: ^File_Generator, import_path: string) -> bool {
	if fg.ignore_imports {
		switch import_path {
		case "core:c", BUILTIN_PKG:
			// Okay
		case:
			return false;
		}
	}
	if fg.file.pkg.name != import_path {
		fg.imports[import_path] = true;
		return true;
	}
	return false;
}

write_type_ref :: proc(fg: ^File_Generator, w: io.Writer, obj: json.Object, field_options: Field_Options, depth_kind := Depth_Kind.Top_Level) {
	kind := obj["Kind"].(string);
	switch kind {
	case "Native":
		name := obj["Name"].(string);
		assert(name != "Void");
		switch name {
		case "Boolean":
			io.write_string(w, "bool");
		case "SByte":
			io.write_string(w, "i8");
		case "Byte":
			io.write_string(w, "u8");
		case "Int16":
			io.write_string(w, "i16");
		case "UInt16":
			io.write_string(w, "u16");
		case "Int32":
			io.write_string(w, "i32");
		case "UInt32":
			io.write_string(w, "u32");
		case "Int64":
			io.write_string(w, "i64");
		case "UInt64":
			io.write_string(w, "u64");
		case "Char":
			io.write_string(w, "byte");
		case "Single":
			io.write_string(w, "f32");
		case "Double":
			io.write_string(w, "f64");
		case "IntPtr":
			io.write_string(w, "int");
		case "UIntPtr":
			io.write_string(w, "uintptr");
		case "Guid":
			if add_import(fg, BUILTIN_PKG) {
				io.write_string(w, BUILTIN_PKG);
				io.write_byte(w, '.');
			}
			io.write_string(w, "GUID");
		case:
			io.write_string(w, name);
		}

	case "ApiRef":
		name := obj["Name"].(string);
		api := obj["Api"].(string);

		switch kind := get_anon_kind(name); kind {
		case .Struct, .Union:
			assert(len(fg.nested_types) != 0);
			for nested_type_value in fg.nested_types {
				nested_type := nested_type_value.(json.Object);
				if nested_type["Name"].(string) == name {
					write_struct_or_union(fg, w, nested_type, name, kind);
					return;
				}
			}
			panic(name);
		case .None:
		}

		target_kind: Target_Kind;
		switch obj["TargetKind"].(string) {
		case "Default":         target_kind = .Default;
		case "FunctionPointer": target_kind = .Function_Pointer;
		case "Com":             target_kind = .Com;
		}

		parents := obj["Parents"].(json.Array);

		is_maybe := false;
		if .Direct_Type_Access not_in field_options {
			if .Optional in field_options {
				switch name {
				case "LPARAM", "WPARAM":
					is_maybe = false;
				case:
					is_maybe = true;
				}
			}

			if is_maybe && !fg.disallow_maybe {
				io.write_string(w, "Maybe(");
			}

			if target_kind == .Com {
				io.write_string(w, "^");
			}
		}
		defer if is_maybe && !fg.disallow_maybe {
			io.write_string(w, ")");
		}

		switch name {
		case "PSTR":
			io.write_string(w, "cstring");
			return;
		case "PWSTR":
			io.write_string(w, "^u16");
			return;
		}

		api_ref := Api_Ref{api, name};
		api_ref_pkg := fg.g.api_to_pkg_map[api];
		fmt.assertf(api_ref_pkg != nil, "%v", api);

		if !fg.is_types_pkg && add_import(fg, TYPES_PKG) {
			fmt.wprintf(w, "%s.", TYPES_PKG);
		}
		io.write_string(w, name);


	case "PointerTo":
		child := obj["Child"].(json.Object);
		child_options := field_options - {.Optional};
		if .Optional in field_options && !fg.disallow_maybe {
			io.write_string(w, "Maybe(");
		}

		if name, _ := child["Name"].(string); name == "Void" {
			io.write_string(w, "rawptr");
		} else {
			io.write_string(w, "^");

			child_options -= {.Const};
			write_type_ref(fg, w, child, child_options, .Child);
		}

		if .Optional in field_options && !fg.disallow_maybe{
			io.write_string(w, ")");
		}

	case "Array":
		child := obj["Child"].(json.Object);
		array_len := i64(1);
		#partial switch ss in obj["Shape"] {
		case json.Object:
			array_len = ss["Size"].(i64);
		case json.Null:
			array_len = 1;
		case:
			panic("Invalid Array Size");
		}
		fmt.wprintf(w, "[%d]", array_len);
		write_type_ref(fg, w, child, field_options, .Child);

	case "LPArray":
		child := obj["Child"].(json.Object);
		if name, _ := child["Name"].(string); name == "Void" {
			io.write_string(w, "rawptr");
		} else {
			child_options := field_options - {.Optional, .Const};
			write_type_ref(fg, w, child, child_options, .Child);
		}

	case "MissingClrType":
		name := obj["Name"].(string);
		if fg.missing_clr_types != nil && name not_in fg.missing_clr_types^ {
			fg.missing_clr_types[name] = obj;
		}

		if add_import(fg, BUILTIN_PKG) {
			io.write_string(w, BUILTIN_PKG);
			io.write_byte(w, '.');
		}
		io.write_string(w, name);
		// fmt.panicf("%s -> %v", kind, obj["Name"]);
	case:
		panic(kind);
	}
}
write_indent :: proc(fg: ^File_Generator, w: io.Writer) {
	for i in 0..<fg.depth {
		io.write_byte(w, '\t');
	}
}

is_field_anon :: proc(field: json.Object) -> bool {
	field_type := field["Type"].(json.Object);
	name, _ := field_type["Name"].(string);
	if get_anon_kind(name) != .None {
		return strings.has_prefix(name, "Anonymous");
	}
	return false;
}

write_struct_or_union :: proc(fg: ^File_Generator, w: io.Writer, obj: json.Object, name: string, kind: Record_Kind) {
	assert(name != "");
	arches, _ := obj["Architectures"].(json.Object);

	struct_size, _ := obj["Size"].(i64);
	struct_packing_size, _ := obj["PackingSize"].(i64);
	struct_fields, _ := obj["Fields"].(json.Array);
	struct_nested_types, _ := obj["NestedTypes"].(json.Array);

	prev_nested_types := fg.nested_types;
	defer fg.nested_types = prev_nested_types;

	fg.nested_types = struct_nested_types;

	for nested_type in fg.nested_types {
		append(&fg.nested_types_to_print, nested_type);
	}


	if struct_size != 0 {
		assert(len(struct_fields) != 0, name);
	}

	is_packed := struct_packing_size == 1;

	io.write_string(w, "struct ");
	if kind == .Union {
		io.write_string(w, "#raw_union ");
	} else if struct_packing_size == 1 {
		io.write_string(w, "#packed ");
	}
	io.write_string(w, "{\n");
	defer {
		write_indent(fg, w);
		io.write_string(w, "}");
	}

	fg.depth += 1;
	defer fg.depth -= 1;

	max_field_len := 0;
	for field_obj in struct_fields {
		field := field_obj.(json.Object);
		field_name := field["Name"].(string);
		field_name = odin_keywords[field_name] or_else field_name;
		if !is_field_anon(field) {
			max_field_len = max(max_field_len, len(field_name));
		}
	}

	for field_obj in struct_fields {
		field := field_obj.(json.Object);
		field_name := field["Name"].(string);
		field_name = odin_keywords[field_name] or_else field_name;

		field_type := field["Type"].(json.Object);
		field_attrs, _ := field["Attrs"].(json.Array);
		write_indent(fg, w);

		is_anon := is_field_anon(field);
		if is_anon {
			io.write_string(w, "using _: ");
		} else {
			io.write_string(w, field_name);
			io.write_string(w, ": ");
			for i in 0 ..< max_field_len-len(field_name) {
				io.write_byte(w, ' ');
			}
		}

		field_options := parse_field_options(field_attrs);

		write_type_ref(fg, w, field_type, field_options);

		io.write_string(w, ",\n");
	}
}

is_return_type_void :: proc(return_type: json.Object) -> bool {
	name, _ := return_type["Name"].(string);
	return name == "Void";
}

write_com :: proc(fg: ^File_Generator, w: io.Writer, obj: json.Object, name: string) {
	assert(name != "");
	if name == "placeholder_ignore_me" {
		return;
	}

	arches, _ := obj["Architectures"].(json.Object);


	com_optional_guid, _ := obj["Guid"].(string);
	com_optional_iface, _ := obj["Interface"].(json.Object);
	com_methods := obj["Methods"].(json.Array);

	if com_optional_guid != "" {
		fmt.wprintf(w, "IID_%s_String :: %q;\n", name, com_optional_guid);

		g, _ := strings.remove_all(com_optional_guid, "-", context.temp_allocator);
		guid: [32]byte;
		copy(guid[:], g);
		str := string(guid[:]);
		Data1, _ := strconv.parse_u64(str[0:][:8],  16);
		Data2, _ := strconv.parse_u64(str[8:][:4],  16);
		Data3, _ := strconv.parse_u64(str[12:][:4], 16);
		Data4_slice := str[16:][:16];
		Data4 := (^[8][2]u8)(raw_data(Data4_slice))^;


		fmt.wprintf(w, "IID_%s_Value  :: ", name);
		if add_import(fg, BUILTIN_PKG) {
			io.write_string(w, BUILTIN_PKG);
			io.write_byte(w, '.');
		}
		io.write_string(w, "GUID{");
		fmt.wprintf(w, "0x%8x, 0x%4x, 0x%4x, ", Data1, Data2, Data3);
		io.write_string(w, "{");
		for _, i in Data4 {
			if i > 0 {
				io.write_string(w, ", ");
			}
			value, _ := strconv.parse_u64(string(Data4[i][:]), 16);
			fmt.wprintf(w, "0x%2x", value);
		}

		io.write_string(w, "}");
		io.write_string(w, "};\n");
	}

	fmt.wprintf(w, "%s :: struct ", name);
	io.write_string(w, "{\n");
	fmt.wprintf(w, "\tusing vtable: ^%s_Vtable,\n", name);
	io.write_string(w, "}\n");
	fmt.wprintf(w, "%s_Vtable :: struct ", name);
	io.write_string(w, "{\n");

	// if com_optional_iface != nil {
	// 	fmt.println(name, "->", com_optional_iface);
	// }

	method_conflicts: map[string]int;
	defer delete(method_conflicts);

	max_method_len := 0;
	for method_value in com_methods {
		method_obj := method_value.(json.Object);
		method_name := method_obj["Name"].(string);
		// TODO(bill): doesn't take into account conflicts but it'll be good enough for now
		max_method_len = max(max_method_len, len(method_name));
	}

	for method_value in com_methods {
		method_obj := method_value.(json.Object);
		method_name := method_obj["Name"].(string);
		return_type := method_obj["ReturnType"].(json.Object);
		params := method_obj["Params"].(json.Array);

		count := method_conflicts[method_name];
		method_conflicts[method_name] = count+1;

		padding_length := max_method_len-len(method_name);

		fmt.wprintf(w, "\t%s", method_name);
		if count != 0 {
			suffix := fmt.tprintf("_v%d", count);
			padding_length -= len(suffix);
			fmt.wprintf(w, suffix);
		}
		io.write_string(w, ": ");
		for i in 0..<padding_length {
			io.write_byte(w, ' ');
		}
		io.write_string(w, "proc \"stdcall\" (");
		fmt.wprintf(w, "self: ^%s", name);
		for param_value, i in params {
			param_obj := param_value.(json.Object);
			param_name := param_obj["Name"].(string);
			param_type := param_obj["Type"].(json.Object);
			param_attrs, _ := param_obj["Attrs"].(json.Array);
			assert(param_name != "");

			param_name = odin_keywords[param_name] or_else param_name;

			fmt.wprintf(w, ", %s: ", param_name);
			field_options := parse_field_options(param_attrs);
			write_type_ref(fg, w, param_type, field_options);
		}

		io.write_string(w, ")");

		if !is_return_type_void(return_type) {
			io.write_string(w, " -> ");
			write_type_ref(fg, w, return_type, nil);
		}

		io.write_string(w, ",\n");
	}

	io.write_string(w, "}\n");


}

write_type :: proc(fg: ^File_Generator, w: io.Writer, type: json.Value, originally_from_comment := "") {
	obj := type.(json.Object);
	name := obj["Name"].(string);
	kind := obj["Kind"].(string);
	arches, _ := obj["Architectures"].(json.Object);

	if kind == "ComClassID" {
		return;
	}

	if get_anon_kind(name) != .None {
		return;
	}

	if (Api_Ref{fg.file.api, name}) in fg.g.type_names_to_remove {
		return;
	}

	if name in fg.all_decls {
		return;
	}
	fg.all_decls[name] = true;

	if originally_from_comment != "" {
		io.write_string(w, "// Type originally from ");
		io.write_string(w, originally_from_comment);
		io.write_string(w, "\n");
	}

	if fg.is_types_pkg {
		fg.g.type_in_pkg_types[name] = true;
	}

	if name in types_that_conflict_with_consts {
		io.write_string(w, "// Warning: this type conflicts with a constant\n");
		fmt.wprintf(w, "%s__CONFLICT :: struct{{}};\n", name);
		return;
	}
	if name in types_to_skip {
		io.write_string(w, "// Warning: type has not been generated due to an error\n");
		if kind == "FunctionPointer" {
			fmt.wprintf(w, "%s :: distinct rawptr;\n", name);
		} else {
			fmt.wprintf(w, "%s :: struct{{}};\n", name);
		}
		return;
	}

	if !fg.is_types_pkg {
		assert(add_import(fg, TYPES_PKG));
		fmt.wprintf(w, "%s :: %s.%s;\n", name, TYPES_PKG, name);
		return;
	}


	defer {
		io.write_byte(w, '\n');
	}


	if (Api_Ref{fg.file.api, name} in fg.g.api_cycle_type_set) {
		if add_import(fg, BUILTIN_PKG) {
			fmt.wprintf(w, "%s :: %s.%s;\n", name, BUILTIN_PKG, name);
			return;
		}
	}


	switch kind {
	case "NativeTypedef":
		switch name {
		case "PSTR":
			io.write_string(w, "PSTR :: cstring;\n");
			return;
		case "PWSTR":
			io.write_string(w, "PWSTR :: ^u16;\n");
			return;
		case "BOOL":
			io.write_string(w, "BOOL :: distinct b32;\n");
			return;
		}


		def := obj["Def"].(json.Object);
		also_usable_for := obj["AlsoUsableFor"];

		#partial switch alias_name in also_usable_for {
		case json.Null:
			// Okay
		case string:
			if api, ok := also_usable_type_api_map[alias_name]; ok {
				api_pkg := fg.g.api_to_pkg_map[api];
				assert(api_pkg != nil);
				if api_pkg != fg.file.pkg {
					fg.imports[api_pkg.name] = true;
					fmt.wprintf(w, "%s :: %s.%s;\n", name, api_pkg.name, alias_name);
				} else {
					fmt.wprintf(w, "%s :: %s;\n", name, alias_name);
				}
				return;
			}

			fmt.panicf("Invalid also_usable_for type: %s :: %s;", name, alias_name);
		case:
			panic("Invalid also_usable_for type");
		}

		if name in handle_types {
			fmt.wprintf(w, "%s :: distinct rawptr;\n", name);
			return;
		}

		fmt.wprintf(w, "%s :: distinct ", name);
		write_type_ref(fg, w, def, nil);
		io.write_string(w, ";\n");


	case "Enum":
		values := obj["Values"].(json.Array);
		integer_base, _ := obj["IntegerBase"].(string);
		fmt.wprintf(w, "%s :: enum ", name);
		is_unsigned := false;
		// TODO(bill): integer base type
		switch integer_base {
		case "Int16":
			io.write_string(w, "i16 {\n");
		case "UInt16":
			io.write_string(w, "u16 {\n");
			is_unsigned = true;
		case "Int32":
			io.write_string(w, "i32 {\n");
		case "UInt32":
			io.write_string(w, "u32 {\n");
			is_unsigned = true;
		case "Int64":
			io.write_string(w, "i64 {\n");
		case "UInt64":
			io.write_string(w, "u64 {\n");
			is_unsigned = true;
		case "":
			fg.imports["core:c"] = true;
			io.write_string(w, "c.int {\n");
		case:
			panic(integer_base);
		}

		max_len := 0;
		for value in values {
			obj := value.(json.Object);
			k := obj["Name"].(string);
			max_len = max(len(k), max_len);
		}
		for value in values {
			obj := value.(json.Object);
			k := obj["Name"].(string);
			v := obj["Value"].(i64);
			fmt.wprintf(w, "\t%s ", k);
			for i in 0..<max_len - len(k) {
				io.write_byte(w, ' ');
			}
			if is_unsigned {
				fmt.wprintf(w, "= %v,\n", u64(v));
			} else {
				fmt.wprintf(w, "= %v,\n", v);
			}
		}

		io.write_string(w, "}\n");
	case "Struct":
		io.write_string(w, name);
		io.write_string(w, " :: ");
		write_struct_or_union(fg, w, obj, name, .Struct);
		io.write_string(w, "\n");
	case "Union":
		io.write_string(w, name);
		io.write_string(w, " :: ");
		write_struct_or_union(fg, w, obj, name, .Union);
		io.write_string(w, "\n");

	case "Com":
		write_com(fg, w, obj, name);

	case "FunctionPointer":
		return_type, _ := obj["ReturnType"].(json.Object);
		params, _ := obj["Params"].(json.Array);

		fmt.wprintf(w, "%s :: proc \"stdcall\" (", name);

		for param_value, i in params {
			param_obj := param_value.(json.Object);
			param_name := param_obj["Name"].(string);
			param_type := param_obj["Type"].(json.Object);
			param_attrs, _ := param_obj["Attrs"].(json.Array);
			assert(param_name != "");
			if i > 0 {
				io.write_string(w, ", ");
			}
			param_name = odin_keywords[param_name] or_else param_name;

			fmt.wprintf(w, "%s: ", param_name);
			field_options := parse_field_options(param_attrs);
			write_type_ref(fg, w, param_type, field_options);
		}

		io.write_string(w, ")");

		if !is_return_type_void(return_type) {
			io.write_string(w, " -> ");
			write_type_ref(fg, w, return_type, nil);
		}

		io.write_string(w, ";\n");
	case:
		panic(kind);
	}

	for nested_type in fg.nested_types_to_print {
		write_type(fg, w, nested_type);
	}
	clear(&fg.nested_types_to_print);
}

File_Kind :: enum {
	Constants,
	Types,
	Procedures,
}


write_proc :: proc(fg: ^File_Generator, w: io.Writer, procedure: json.Value) {
	obj := procedure.(json.Object);
	name := obj["Name"].(string);
	return_type, _ := obj["ReturnType"].(json.Object);
	params, _ := obj["Params"].(json.Array);

	fmt.wprintf(w, "\t%s :: proc(", name);

	for param_value, i in params {
		param_obj := param_value.(json.Object);
		param_name := param_obj["Name"].(string);
		param_type := param_obj["Type"].(json.Object);
		param_attrs, _ := param_obj["Attrs"].(json.Array);
		assert(param_name != "");
		if i > 0 {
			io.write_string(w, ", ");
		}
		param_name = odin_keywords[param_name] or_else param_name;

		fmt.wprintf(w, "%s: ", param_name);
		field_options := parse_field_options(param_attrs);
		write_type_ref(fg, w, param_type, field_options);
	}

	io.write_string(w, ")");

	if !is_return_type_void(return_type) {
		io.write_string(w, " -> ");
		write_type_ref(fg, w, return_type, nil);
	}

	io.write_string(w, " ---\n");
}

path_to_identifier :: proc(path: string) -> string {
	buf := make([]byte, len(path));
	copy(buf, path);
	for c, i in buf {
		switch c {
		case '-', '.':
			buf[i] = '_';
		}
	}
	return string(buf);
}

generate_file :: proc(using fg: ^File_Generator, file_kind: File_Kind) {
	fmt.printf("Generating: %s\n", fg.fullpath);
	strings.reset_builder(b);
	w := strings.to_writer(b);

	root := file.root.(json.Object);

	switch file_kind {
	case .Constants:
		constants, _ := root["Constants"].(json.Array);
		fmt.wprintln(w, "\n");
		for constant in constants {
			write_constant(fg, w, constant);
		}

	case .Types:
		types, _ := root["Types"].(json.Array);
		for type in types {
			write_type(fg, w, type);
		}

	case .Procedures:
		functions, _ := root["Functions"].(json.Array);

		for func in functions {
			obj := func.(json.Object);
			dll_import := obj["DllImport"].(string);
			name := obj["Name"].(string);

			proc_ref := Api_Ref{file.api, name};
			if proc_ref in g.procedures_to_ignore {
				continue;
			}

			if dll_import not_in foreign_imports {
				foreign_imports[dll_import] = path_to_identifier(dll_import);
				fg.procedures[dll_import] = new_clone(Grouped_Procedures{dll_import = dll_import});
			}
			append(&fg.procedures[dll_import].procedures, func);
		}

		for dll_import, gp in fg.procedures {
			fmt.wprintln(w, "\n");
			fmt.wprintln(w, "@(default_calling_convention=\"stdcall\")");
			fmt.wprintf(w, "foreign %s {{\n", foreign_imports[dll_import]);
			for procedure in gp.procedures {
				write_proc(fg, w, procedure);
			}
			fmt.wprintln(w, "}\n");
		}

	}

	fmt.fprintln(fd, "// This code was generated; DO NOT EDIT.\n");

	fmt.fprintln(fd, "//+build windows");
	fmt.fprintln(fd, "//+lazy");
	fmt.fprintf(fd, "package win32_%s\n", file.pkg.name);

	import_count := 0;
	for import_path in imports {
		if import_path == file.pkg.name {
			continue;
		}
		if import_count == 0 {
			fmt.fprintf(fd, "\n");
		}
		if strings.has_prefix(import_path, "core:") {
			fmt.fprintf(fd, "import \"%s\"\n", import_path);
		} else {
			fmt.fprintf(fd, "import %s \"../%s\"\n", import_path, import_path);
		}
		import_count += 1;
	}
	if import_count != 0 {
		fmt.fprintf(fg.fd, "\n");
	}
	import_count = 0;
	for lib, ident in foreign_imports {
		fmt.fprintf(fd, "foreign import %s \"system:%s.lib\"\n", ident, lib);
	}
	if import_count != 0 {
		fmt.fprintf(fg.fd, "\n");
	}


	os.write(fd, b.buf[:]);
}

generate_file_of_types :: proc(fg: ^File_Generator) {
	fmt.printf("Generating: %s\n", fg.fullpath);
	strings.reset_builder(fg.b);
	w := strings.to_writer(fg.b);

	assert(fg.file != nil);
	root := fg.file.root.(json.Object);
	constants, _ := root["Constants"].(json.Array);
	types,     _ := root["Types"].(json.Array);
	functions, _ := root["Functions"].(json.Array);

	for type in types {
		write_type(fg, w, type);
	}

	fmt.fprintln(fg.fd, "// This code was generated; DO NOT EDIT.\n");

	fmt.fprintln(fg.fd, "//+build windows");
	fmt.fprintln(fg.fd, "//+lazy");
	fmt.fprintln(fg.fd, "package win32__types\n");


	import_count := 0;
	for import_path in fg.imports {
		if import_path == fg.file.pkg.name {
			continue;
		}
		if import_count == 0 {
			fmt.fprintf(fg.fd, "\n");
		}
		if strings.has_prefix(import_path, "core:") {
			fmt.fprintf(fg.fd, "import \"%s\"\n", import_path);
		} else {
			fmt.fprintf(fg.fd, "import %s \"../%s\"\n", import_path, import_path);
		}
		import_count += 1;
	}

	if import_count != 0 {
		fmt.fprintf(fg.fd, "\n");
	}


	os.write(fg.fd, fg.b.buf[:]);
}

write_to_odin :: proc(g: ^Generator) {
	file_builder := strings.make_builder();

	base_dir := "win32odin-v2";
	os.make_directory(base_dir, 0);

	os.make_directory(fmt.tprintf("%s", base_dir), 0);

	missing_clr_types := make(map[string]json.Object);
	defer delete(missing_clr_types);

	{ // package types
		pkg_name := "_types";
		os.make_directory(fmt.tprintf("%s/%s", base_dir, pkg_name), 0);

		all_decls := make(map[string]bool);
		defer delete(all_decls);

		for _, pkg in g.pkgs {
			for file in pkg.files {
				if g.apis_not_needed_for_types[file.api] {
					continue;
				}

				file_name := file.name;
				// NOTE(bill): prevent the `*_linux.odin` problem
				if strings.has_suffix(file_name, "_linux") {
					file_name = strings.concatenate({file_name, "os"});
				}

				path := fmt.aprintf("%s/%s/%s_%s.odin", base_dir, pkg_name, pkg.name, file_name);
				defer delete(path);
				f, err := os.open(path, os.O_WRONLY|os.O_CREATE|os.O_TRUNC, 0);
				assert(err == 0);
				defer os.close(f);
				file_gen := &File_Generator{
					g    = g,
					fd   = f,
					b    = &file_builder,
					file = file,
					fullpath = path,
					ignore_imports = true,
					is_types_pkg = true,
					all_decls = &all_decls,
					missing_clr_types = &missing_clr_types,
				};
				defer delete(file_gen.nested_types_to_print);
				generate_file_of_types(file_gen);
			}
		}
	}


	if false do for _, pkg in g.pkgs {
		os.make_directory(fmt.tprintf("%s/%s", base_dir, pkg.name), 0);
		for file in pkg.files {
			root := file.root.(json.Object);

			for file_kind in File_Kind {
				@static suffixes := [File_Kind]string{
					.Constants  = "consts",
					.Types      = "types",
					.Procedures = "procs",
				};
				@static root_array := [File_Kind]string{
					.Constants  = "Constants",
					.Types      = "Types",
					.Procedures = "Functions",
				};

				suffix := suffixes[file_kind];
				if entities, _ := root[root_array[file_kind]].(json.Array); len(entities) == 0 {
					continue;
				}

				all_decls := make(map[string]bool);
				defer delete(all_decls);

				path := fmt.tprintf("%s/%s/%s_%s.odin", base_dir, pkg.name, file.name, suffix);
				f, err := os.open(path, os.O_WRONLY|os.O_CREATE|os.O_TRUNC, 0);
				assert(err == 0);
				defer os.close(f);
				file_gen := &File_Generator{
					g    = g,
					fd   = f,
					b    = &file_builder,
					fullpath = path,
					file = file,
					all_decls = &all_decls,
					disallow_maybe = true,
					missing_clr_types = &missing_clr_types,
				};
				generate_file(file_gen, file_kind);
				delete(file_gen.imports);
				for _, ident in file_gen.foreign_imports {
					delete(ident);
				}
				delete(file_gen.foreign_imports);
				for _, g in file_gen.procedures {
					delete(g.procedures);
					free(g);
				}
				delete(file_gen.procedures);
			}
		}
	}

	if false { // package builtin
		pkg_name := BUILTIN_PKG;
		os.make_directory(fmt.tprintf("%s/%s", base_dir, pkg_name), 0);

		path := fmt.aprintf("%s/%s/builtin_types.odin", base_dir, pkg_name);
		defer delete(path);
		f, err := os.open(path, os.O_WRONLY|os.O_CREATE|os.O_TRUNC, 0);
		assert(err == 0);
		defer os.close(f);

		strings.reset_builder(&file_builder);
		w := strings.to_writer(&file_builder);

		fmt.wprintln(w, "// This code was generated; DO NOT EDIT.\n");

		fmt.wprintln(w, "//+build windows");
		fmt.wprintln(w, "//+lazy");
		fmt.wprintln(w, "package win32__builtin\n");

		fmt.wprintln(w, "GUID :: struct {");
		fmt.wprintln(w, "\tData1: u32,");
		fmt.wprintln(w, "\tData2: u16,");
		fmt.wprintln(w, "\tData3: u16,");
		fmt.wprintln(w, "\tData4: [8]u8,");
		fmt.wprintln(w, "}\n");


		if len(missing_clr_types) != 0 {
			fmt.wprintln(w, "// Missing CLR Types\n");

			for name, missing_clr_type in missing_clr_types {
				fmt.wprintf(w, "%s :: struct {{rawptr}};\n", name);
			}
		}

		os.write(f, file_builder.buf[:]);
	}
}

types_to_skip := map[string]bool {
	"FILEGROUPDESCRIPTORA"                      = true,
	"EXCEPTION_REGISTRATION_RECORD"             = true,
	"NT_TIB"                                    = true,
	"NT_TIB32"                                  = true,
	"NT_TIB64"                                  = true,
	"PFN_WER_RUNTIME_EXCEPTION_DEBUGGER_LAUNCH" = true,
	"PFN_WER_RUNTIME_EXCEPTION_EVENT"           = true,
	"PFN_WER_RUNTIME_EXCEPTION_EVENT_SIGNATURE" = true,
	"VDMGLOBALFIRSTPROC"                        = true,
	"VDMGLOBALNEXTPROC"                         = true,
	"VDMMODULEFIRSTPROC"                        = true,
	"VDMMODULENEXTPROC"                         = true,
};


types_that_conflict_with_consts := map[string]bool {
	"AE_SRVSTATUS"    = true,
	"AE_SESSLOGON"    = true,
	"AE_SESSLOGOFF"   = true,
	"AE_SESSPWERR"    = true,
	"AE_CONNSTART"    = true,
	"AE_CONNSTOP"     = true,
	"AE_CONNREJ"      = true,
	"AE_RESACCESS"    = true,
	"AE_RESACCESSREJ" = true,
	"AE_CLOSEFILE"    = true,
	"AE_SERVICESTAT"  = true,
	"AE_ACLMOD"       = true,
	"AE_UASMOD"       = true,
	"AE_NETLOGON"     = true,
	"AE_NETLOGOFF"    = true,
	"AE_LOCKOUT"      = true,
};


also_usable_type_api_map := map[string]string {
	"HDC"        = "Graphics.Gdi",
	"HGDIOBJ"    = "Graphics.Gdi",
	"HICON"      = "UI.WindowsAndMessaging",
	"HANDLE"     = "System.SystemServices",
	"HeapHandle" = "System.SystemServices",
};

handle_types := map[string]bool{
	"HANDLE"                   = true,
	"SOCKET"                   = true,
	"HICON"                    = true,
	"HCURSOR"                  = true,
	"HBRUSH"                   = true,
	"HSYNTHETICPOINTERDEVICE"  = true,
	"HPROPSHEETPAGE"           = true,
	"HIMAGELIST"               = true,
	"HTREEITEM"                = true,
	"HCOLORSPACE"              = true,
	"HCRYPTASYNC"              = true,
	"HCERTCHAINENGINE"         = true,
	"HCOMDB"                   = true,
	"HKEY"                     = true,
	"HWINWATCH"                = true,
	"HWINSTA"                  = true,
	"HDESK"                    = true,
	"HSTRING"                  = true,
	"HSZ"                      = true,
	"HCONV"                    = true,
	"HCONVLIST"                = true,
	"HDDEDATA"                 = true,
	"HRASCONN"                 = true,
	"HAMSICONTEXT"             = true,
	"HAMSISESSION"             = true,
	"HCMNOTIFICATION"          = true,
	"HKL"                      = true,
	"HIFTIMESTAMPCHANGE"       = true,
	"HWSAEVENT"                = true,
	"HTASK"                    = true,
	"HWINEVENTHOOK"            = true,
	"HUIANODE"                 = true,
	"HUIAPATTERNOBJECT"        = true,
	"HUIATEXTRANGE"            = true,
	"HUIAEVENT"                = true,
	"HMIDI"                    = true,
	"HMIDIIN"                  = true,
	"HMIDIOUT"                 = true,
	"HMIDISTRM"                = true,
	"HMIXER"                   = true,
	"HMIXEROBJ"                = true,
	"HWAVE"                    = true,
	"HWAVEOUT"                 = true,
	"HWAVEIN"                  = true,
	"HMMIO"                    = true,
	"HDRVR"                    = true,
	"HACMDRIVERID"             = true,
	"HACMDRIVER"               = true,
	"HACMSTREAM"               = true,
	"HACMOBJ"                  = true,
	"HIC"                      = true,
	"HVIDEO"                   = true,
	"HSWDEVICE"                = true,
	"HINTERACTIONCONTEXT"      = true,
	"HRAWINPUT"                = true,
	"HRECOALT"                 = true,
	"HRECOCONTEXT"             = true,
	"HRECOGNIZER"              = true,
	"HRECOLATTICE"             = true,
	"HRECOWORDLIST"            = true,
	"HIMC"                     = true,
	"HIMCC"                    = true,
	"HSAVEDUILANGUAGES"        = true,
	"HRSRC"                    = true,
	"HSURF"                    = true,
	"HINSTANCE"                = true,
	"HPOWERNOTIFY"             = true,
	"HUMPD"                    = true,
	"HSTR"                     = true,
	"HSPRITE"                  = true,
	"HSEMAPHORE"               = true,
	"HLSURF"                   = true,
	"HFASTMUTEX"               = true,
	"HDRVOBJ"                  = true,
	"HDEV"                     = true,
	"HBM"                      = true,
	"HPCON"                    = true,
	"HMENU"                    = true,
	"HACCEL"                   = true,
	"HDROP"                    = true,
	"HPSXA"                    = true,
	"HDC"                      = true,
	"HGDIOBJ"                  = true,
	"HBITMAP"                  = true,
	"HRGN"                     = true,
	"HPEN"                     = true,
	"HBRUSH"                   = true,
	"HFONT"                    = true,
	"HMETAFILE"                = true,
	"HENHMETAFILE"             = true,
	"HMONITOR"                 = true,
	"HPALETTE"                 = true,
	"HWND"                     = true,
	"HHOOK"                    = true,
	"HGESTUREINFO"             = true,
	"HTOUCHINPUT"              = true,
	"HGLRC"                    = true,
	"HFILTER"                  = true,
	"HPTPROVIDER"              = true,
	"HPSS"                     = true,
	"HPSSWALK"                 = true,
	"HSTRING_BUFFER"           = true,
	"JET_SESID"                = true,
	"PSID"                     = true,
	"AUTHZ_AUDIT_EVENT_HANDLE" = true,
	"HeapHandle"               = true,
};
