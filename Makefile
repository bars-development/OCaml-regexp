# Makefile for OCaml project

# Define module names (without .ml extension)
MODULES = mylist regular_expressions engine

# Bytecode and Native Code Outputs
BYTECODE_OUTPUT = project.cmo
BYTECODE_LIBRARY = project.cma
NATIVE_OUTPUT = project.cmx
NATIVE_LIBRARY = project.cmxa

# Compiler and Flags
OCAMLC = ocamlc
OCAMLOPT = ocamlopt
CLEAN_FILES = *.cmo *.cmi *.cmx *.o *.cma *.cmxa *.a *.exe project

# Default target (bytecode compilation)
all: bytecode-lib

# Compile each module to .cmo/.cmi (bytecode)
%.cmo: %.ml
	$(OCAMLC) -c $<

# Compile each module to .cmx/.o (native code)
%.cmx: %.ml
	$(OCAMLOPT) -c $<

# Bytecode output (linked)
bytecode: $(MODULES:=.cmo)
	$(OCAMLC) -o $(BYTECODE_OUTPUT) $^

# Bytecode library
bytecode-lib: $(MODULES:=.cmo)
	$(OCAMLC) -a -o $(BYTECODE_LIBRARY) $^

# Native code output (linked)
native: $(MODULES:=.cmx)
	$(OCAMLOPT) -o $(NATIVE_OUTPUT) $^

# Native library
native-lib: $(MODULES:=.cmx)
	$(OCAMLOPT) -a -o $(NATIVE_LIBRARY) $^

# Clean up generated files
clean:
	rm -f $(CLEAN_FILES)

# Phony targets to prevent conflicts with files of the same name
.PHONY: all bytecode bytecode-lib native native-lib clean
