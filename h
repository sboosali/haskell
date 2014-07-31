#!/bin/bash

FILE=$(basename "$1")
EXT="${FILE##*.}"
FILE="${FILE%.*}"

ARGS=${@:2}

ghc  $FILE.hs  -o $FILE.hx  $GHC_OPTS

if [ $? -eq 0 ] # on success
then ./$FILE.hx $ARGS # execute 
fi
