#!/bin/bash
set -ex
cd $(dirname $0)

export DYLD_LIBRARY_PATH=$(pwd)/lib
.//lifedb_server -conf $(pwd)/etc/lifedb.config
