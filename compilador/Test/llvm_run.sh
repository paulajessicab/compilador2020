#!/usr/bin/env bash

filename="${@%.*}"
stack exec compiladores-exe -- --runllvm $filename.pcf
