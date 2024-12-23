OCAMLC=ocamlc
OCAMLYACC=ocamlyacc -v
OCAMLLEX=ocamllex
OCAMLVERSION=`$(OCAMLC) -version`
OCAMLOPT=ocamlopt
OCAMLDEP=ocamldep
OCAMLLIB=/usr/lib/ocaml
OTHEROBJECTS=unix.cma 
OPTIONS=-g -dtypes -annot
OCAMLFLAGS=$(INCLUDES) $(OTHEROBJECTS) $(OPTIONS)
OCAMLOPTFLAGS=$(INCLUDES) $(OTHEROBJECTS)


OBJECTS= llbc.ml typing.ml parser.ml lexer.ml main.ml

PROGRAM=llbc

all:	$(PROGRAM)

$(PROGRAM): $(OBJECTS) parser.cmi
	$(OCAMLC) $(OCAMLFLAGS) $(OBJECTS) -o $(PROGRAM)

clean:	
	rm -f llbc.mli lexer.ml parser.ml parser.mli parser.output *.cmi *.cmo *.annot output.*

remove:
	rm -f $(PROGRAM)

test:	llbc
	./llbc -i examples/example_1 -c

debug:$(PROGRAM)
	ocamldebug $(PROGRAM) $(OCAMLFLAGS)


%.cmi:	%.mli
	ocamlc $(ocamlc_flags) -c $<

parser.ml parser.mli: llbc.cmi parser.mly 
	$(OCAMLYACC) parser.mly

lexer.ml: llbc.cmi lexer.mll
	$(OCAMLLEX) lexer.mll

llbc.mli: llbc.ml
	ocamlc $(ocamlc_flags) -c -i llbc.ml > llbc.mli

