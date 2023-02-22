#!/usr/bin/env bash

filename="${@%.*}"
stack exec compiladores-exe -- --bytecompile $filename.pcf
stack exec compiladores-exe -- --run $filename.pcf.byte