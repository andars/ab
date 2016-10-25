OCB_FLAGS = -use-menhir -tag thread -use-ocamlfind
OCB = ocamlbuild

all: native

parse:
	$(OCB) $(OCB_FLAGS) parse.native
	./parse.native main.b

compile:
	$(OCB) $(OCB_FLAGS) compile.native
	./compile.native main.b > out.s
	as out.s -o out.o
	ld out.s brt0.s -o a.out

clean:
	$(OCB) -clean

.PHONY: native all
