#!/bin/bash
set -ex
cd $(dirname $0)

export DYLD_LIBRARY_PATH=$(pwd)/lib
cd bin
./lifedb_server -conf $(pwd)/../etc/lifedb.config
