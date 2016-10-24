OCB_FLAGS = -use-menhir -tag thread -use-ocamlfind
OCB = ocamlbuild

all: native

native:
	$(OCB) $(OCB_FLAGS) test.native

.PHONY: native all
