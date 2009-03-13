OCAMLMAKEFILE = OCamlMakefile

PYTHON ?= python

ANNOTATE = yes
export ANNOTATE
DEBUG = yes
export DEBUG

OCAMLRUNPARAM=b
export OCAMLRUNPARAM

PLATFORM=macos

USE_CAMLP4 := yes
SOURCES= custom_unix_stubs.c fork_helper.ml platform.ml utils.ml lifedb_config.ml sql_access.ml log.ml \
	passwords.ml  \
	sql_mtype_map.ml sql_mirror.ml \
	lifedb_rpc.ml \
	lifedb_passwd.ml \
	lifedb_session.ml db_thread_access.ml lifedb_tasks.ml lifedb_plugin.ml \
	db_thread.ml lifedb_static.ml \
	lifedb_dispatch.ml \
        server.ml
THREADS=yes
RESULT=lifedb_server
PACKS=netstring netcgi2 unix nethttpd-for-netcgi2 netplex json-static json-wheel uuidm sqlite3 str ANSITerminal cryptokit
LIBDIRS=/opt/local/lib

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

.PHONY: scripts
scripts:
	rm -rf scripts
	mkdir scripts
	cd client/python && $(PYTHON) setup.py install_scripts --install-dir $(PWD)/scripts
	echo export PYTHONPATH=$(PWD)/client/python:$$PYTHONPATH > export-var.sh
	. ./export-var.sh

platform.ml: platform_$(PLATFORM).ml
	cp $< $@

include $(OCAMLMAKEFILE)
