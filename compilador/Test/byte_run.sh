#!/usr/bin/env bash

filename="${@%.*}"
stack exec compiladores-exe -- --bytecompile $filename.pcf
if test -f $filename.pcf.byte; then
    stack exec compiladores-exe -- --run $filename.pcf.byte
else 
    exit 1
fi
