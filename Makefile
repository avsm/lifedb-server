OCAMLMAKEFILE = OCamlMakefile

PYTHON ?= python

ANNOTATE = yes
export ANNOTATE
DEBUG = yes
export DEBUG

OCAMLRUNPARAM=b
export OCAMLRUNPARAM

USE_CAMLP4 := yes
SOURCES=lifedb_rpc.ml lifedb_session.ml lifedb_cache.ml lifedb_dispatch.ml server.ml
THREADS=yes
RESULT=lifedb_server
PACKS=netstring netcgi2 unix nethttpd-for-netcgi2 netplex json-static json-wheel uuidm

PP=./camlp4find $(PACKS)
export PP

.PHONY: all
all: dnc
	@ :

.PHONY: run
run:
	./$(RESULT) -conf ./netplex.cfg -fg

.PHONY: stop
stop:
	netplex-admin -shutdown

.PHONY: test
test:
	cd client/python && $(PYTHON) setup.py test

include $(OCAMLMAKEFILE)
