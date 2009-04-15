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
SOURCES= client/ocaml/lifedb.ml \
	custom_unix_stubs.c fork_helper.ml platform.ml utils.ml magic_mime.ml \
	lifedb_config.ml sql_access.ml log.ml \
	passwords.ml  \
        lifedb_schema.ml sync_schema.ml \
	sql_mirror.ml \
	lifedb_rpc.ml \
	lifedb_passwd.ml \
	db_thread_access.ml \
	lifedb_plugin.ml lifedb_tasks.ml \
	lifedb_filter.ml lifedb_user.ml \
        lifedb_out_tasks.ml \
	db_thread.ml lifedb_static.ml \
	lifedb_query.ml \
	lifedb_dispatch.ml \
	http_server.ml \
        server.ml
THREADS=yes
RESULT=lifedb_server
PACKS=netstring netcgi2 unix nethttpd-for-netcgi2 netplex json-static json-wheel uuidm sqlite3 str ANSITerminal cryptokit netclient
LIBDIRS=/opt/local/lib

PP=./camlp4find $(PACKS)
export PP

.PHONY: all
all: dnc
	@ :

.PHONY: run
run:
	./$(RESULT)

.PHONY: stop
stop:
	netplex-admin -shutdown

.PHONY: test
test:
	cd client/python && \
	env LIFEDB_TEST_USERNAME=root LIFEDB_TEST_PASSWORD=`../../get_passphrase.sh` \
	$(PYTHON) setup.py test

.PHONY: scripts
scripts:
	rm -rf scripts
	mkdir scripts
	cd client/python && $(PYTHON) setup.py install_scripts --install-dir $(PWD)/scripts
	echo export PYTHONPATH=$(PWD)/client/python:$$PYTHONPATH > export-var.sh
	. ./export-var.sh

.PHONY: macdist
macdist: all
	rm -rf macdist
	mkdir -p macdist/bin
	mkdir -p macdist/lib
	mkdir -p macdist/etc
	cp ./$(RESULT) macdist/bin/
	cp /opt/local/lib/libpcre.0.dylib macdist/lib/
	cp /opt/local/lib/libreadline.5.2.dylib macdist/lib/
	cp ./config.json.in macdist/etc/lifedb.config
	cp ./mime.types macdist/bin/mime.types
	cp ./scripts/run.sh macdist/run_server
	cp ./scripts/set_passphrase.sh macdist/set_passphrase
	mkdir -p macdist/plugins
	cd ../lifedb-plugins.git && make install DEST=$(PWD)/macdist/plugins
	rm -f mac.tgz
	tar -zcvf mac.tgz -C macdist .

platform.ml: platform_$(PLATFORM).ml
	cp $< $@

include $(OCAMLMAKEFILE)
