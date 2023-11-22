.PHONY: default
default: build-ocaml

OCaml_files := $(shell find *.ml)
OCaml_files_from_build := $(patsubst %.ml, build/%.ml, $(OCaml_files))



.PHONY: build-x86_64
build-ocaml:
	@echo "[OCaml files] :"
	@echo $(OCaml_files)


	@mkdir -p build

	@cp -t build/ $(OCaml_files)
	@ocamlopt -I $(ocamlfind query graphics) graphics.cma -o build/out.elf $(OCaml_files_from_build)
	@cp build/out.elf out.elf
	
CLEAN:
	@rm -f build/*.cmx
	@rm -f build/*.o
	@rm -f build/*.cmi
	@rm -f build/*.elf
	@rm -f build/*.ml
	@echo CLEAN
