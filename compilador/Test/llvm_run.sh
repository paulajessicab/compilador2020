#!/usr/bin/env bash

filename="${@%.*}"
stack run -- -runllvm "$@" 