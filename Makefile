OCAMLMAKEFILE = OCamlMakefile

ANNOTATE = yes
export ANNOTATE
DEBUG = yes
export DEBUG

OCAMLRUNPARAM=b
export OCAMLRUNPARAM

SOURCES=lifedb_rpc.ml server.ml
THREADS=yes
RESULT=lifedb_server
PACKS=netstring,netcgi2,unix,nethttpd-for-netcgi2,netplex,json-static,json-wheel

.PHONY: all
all: dc
	@ :

.PHONY: run
run:
	./$(RESULT) -conf ./netplex.cfg -fg

.PHONY: stop
stop:
	netplex-admin -shutdown

include $(OCAMLMAKEFILE)
