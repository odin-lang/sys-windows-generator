# sys-windows-generator
package sys/windows Generator (WIP)

This is a Work In Progress project for generating the entire win32 API from a win32md file.

It currently needs to help from someone who can figure out how to sort the import graph so that only the 
minimally needed types are required in the base types package which is included by every win32 package.
Currently every type is added to this base package and it is huge which makes the parser the bottleneck.

Goal-wise importing a basic package should take a few dozen milliseconds MAXIMUM (ideally less) but this
is not currently possible.


HELP IS WANTED!
