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

.PHONY: x-%
x-%:
	@echo $($*)

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

WANTLIB=libz.1 libsqlite3.0 libpcre.0 libreadline.5.2 libncurses.5

.PHONY: macdist
macdist: all
	rm -rf macdist
	mkdir -p macdist/bin
	mkdir -p macdist/lib
	mkdir -p macdist/etc
	cp -r ./htdocs macdist/
	cp ./$(RESULT) macdist/bin/
	for i in $(WANTLIB); do cp /opt/local/lib/$$i.dylib macdist/lib/$$i.dylib; done
	cp ./config.json.in macdist/etc/lifedb.config.in
	cp ./mime.types macdist/bin/mime.types
	cp ./scripts/run.sh macdist/run_server
	cp ./scripts/set_passphrase.sh macdist/set_passphrase
	mkdir -p macdist/plugins
	cd ../plugins && make install DEST=$(PWD)/macdist/plugins ETC=$(PWD)/macdist/etc
	rm -f mac.tgz
	tar -zcvf mac.tgz -C macdist .

platform.ml: platform_$(PLATFORM).ml
	cp $< $@

include $(OCAMLMAKEFILE)
