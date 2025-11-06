#!/bin/bash

# load this into terminal using
# source set-environment.sh

HERE=`pwd`
# here directory could have spaces in it so we quote expression
DEVELOPER="${HERE}"

# now we can export this
export DEVELOPER

# now we can do same here - so guile can find libpixelformat.so
export LTDL_LIBRARY_PATH="$DEVELOPER/pixelformat"



