OCB_FLAGS = -use-menhir -tag thread -use-ocamlfind
OCB = ocamlbuild

all: compile

parse:
	$(OCB) $(OCB_FLAGS) parse.native
	./parse.native main.b

compile:
	$(OCB) $(OCB_FLAGS) compile.native
	mkdir -p build/
	./compile.native main.b > build/out.s
	as build/out.s -o build/out.o
	as brt0.s -o build/brt0.o
	ld build/out.o build/brt0.o -o a.out

clean:
	$(OCB) -clean

.PHONY: native all
