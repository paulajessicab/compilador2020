#!/usr/bin/env bash

filename="${@%.*}"
stack exec compiladores-exe -- --bytecompile $filename.pcf
if test -f $filename.pcf.byte; then
    ./Src/bvm $filename.pcf.byte
else 
    exit 1
fi
