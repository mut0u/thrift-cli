#!/bin/bash -e
mkdir -p src-gen-thrift
thrift -gen hs -out src-gen-thrift/ -v -r -strict IDL/service/demo.thrift
