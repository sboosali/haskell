#!/bin/bash

FILE=$(basename "$1")
EXT="${FILE##*.}"
FILE="${FILE%.*}"

ARGS=${@:2}

ghc  $FILE.hs  -o $FILE.hout  $GHC_OPTS

if [ $? -eq 0 ] # on success
then ./$FILE.hout $ARGS # execute 
fi
