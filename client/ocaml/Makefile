OCAMLMAKEFILE=../../OCamlMakefile

SOURCES=lifedb.ml client.ml
PACKS=netclient json-static
RESULT=lifedb

PP=../../camlp4find $(PACKS)
export PP

.PHONY: all
all: bcl ncl
	@ :

show-%:
	echo $($*)

include $(OCAMLMAKEFILE)
