#!/bin/bash

set -e
set -x

#  (main) ~/code/guile-scheme/guile-sdl-learning/src$ ls /usr/lib/x86_64-linux-gnu/libSDL
# libSDL-1.2.so.0               libSDL2.a                     libSDL2_image.so              libSDL2_mixer.a               libSDL2_ttf-2.0.so.0          libSDL3.so                    
# libSDL-1.2.so.1.2.68          libSDL2_image-2.0.so          libSDL2main.a                 libSDL2_mixer.so              libSDL2_ttf-2.0.so.0.2400.0   libSDL3.so.0                  
# libSDL2-2.0.so                libSDL2_image-2.0.so.0        libSDL2_mixer-2.0.so          libSDL2.so                    libSDL2_ttf.a                 libSDL3.so.0.2.20             
# libSDL2-2.0.so.0              libSDL2_image-2.0.so.0.800.8  libSDL2_mixer-2.0.so.0        libSDL2_test.a                libSDL2_ttf.so                libSDL3_test.a                
# libSDL2-2.0.so.0.3200.4       libSDL2_image.a               libSDL2_mixer-2.0.so.0.800.1  libSDL2_ttf-2.0.so            libSDL3.a                     
#  (main) ~/code/guile-scheme/guile-sdl-learning/src$ ls /usr/lib/x86_64-linux-gnu/libSDL2.so
# /usr/lib/x86_64-linux-gnu/libSDL2.so
#  (main) ~/code/guile-scheme/guile-sdl-learning/src$ ls /usr/lib/x86_64-linux-gnu/libfreetype.
# libfreetype.a          libfreetype.so         libfreetype.so.6       libfreetype.so.6.20.2  
#  (main) ~/code/guile-scheme/guile-sdl-learning/src$ ls /usr/lib/x86_64-linux-gnu/libfreetype.so
# /usr/lib/x86_64-linux-gnu/libfreetype.so
#  (main) ~/code/guile-scheme/guile-sdl-learning/src$ ls /usr/lib/x86_64-linux-gnu/libcairo
# libcairo-gobject.so                       libcairomm-1.0.so.1                       libcairomm-1.16.so.1.4.0                  libcairo-script-interpreter.so.2.11804.4  libcairo.so.2.11804.4
# libcairo-gobject.so.2                     libcairomm-1.0.so.1.4.0                   libcairo-script-interpreter.so            libcairo.so                               
# libcairo-gobject.so.2.11804.4             libcairomm-1.16.so.1                      libcairo-script-interpreter.so.2          libcairo.so.2                             
#  (main) ~/code/guile-scheme/guile-sdl-learning/src$ ls /usr/lib/x86_64-linux-gnu/libcairo
# libcairo-gobject.so                       libcairomm-1.0.so.1                       libcairomm-1.16.so.1.4.0                  libcairo-script-interpreter.so.2.11804.4  libcairo.so.2.11804.4
# libcairo-gobject.so.2                     libcairomm-1.0.so.1.4.0                   libcairo-script-interpreter.so            libcairo.so                               
# libcairo-gobject.so.2.11804.4             libcairomm-1.16.so.1                      libcairo-script-interpreter.so.2          libcairo.so.2                             
#  (main) ~/code/guile-scheme/guile-sdl-learning/src$ ls /usr/lib/x86_64-linux-gnu/libcairo.so
# /usr/lib/x86_64-linux-gnu/libcairo.so
#  (main) ~/code/guile-scheme/guile-sdl-learning/src$ ls /usr/lib/x86_64-linux-gnu/libguile-
# libguile-3.0.a          libguile-3.0.so         libguile-3.0.so.1       libguile-3.0.so.1.7.0   libguile-ssh.so         libguile-ssh.so.18      libguile-ssh.so.18.0.0  
#  (main) ~/code/guile-scheme/guile-sdl-learning/src$ ls /usr/lib/x86_64-linux-gnu/libguile-3.0.so
# /usr/lib/x86_64-linux-gnu/libguile-3.0.so
#  (main) ~/code/guile-scheme/guile-sdl-learning/src$ 
# 
# -Lfreetype  for  libfreetype.so
# -LSDL2      for  libSDL2.so
# -Lcairo     for  libcairo.so
# -Lguile-3.0  for libguile-3.0.so
#
#
#
#
#
#



CC=clang
CFLAGS_SDL=`pkg-config --cflags sdl2`
CFLAGS_CAIRO=`pkg-config --cflags cairo`
CFLAGS_GUILE=`pkg-config --cflags guile-3.0`

FLAGS_SDL=`pkg-config --cflags --libs sdl2`
FLAGS_CAIRO=`pkg-config --cflags --libs cairo`
FLAGS_GUILE=`pkg-config --cflags --libs guile-3.0`

cd "$DEVELOPER/pixelformat"
rm -f libpixelformat.so
rm -f pixelformat.o

# compile and link in one step 
${CC} ${FLAGS_SDL} ${FLAGS_GUILE} ${FLAGS_CAIRO} -Wall -fPIC -shared -lSDL2 -lcairo -lguile-3.0 -o libpixelformat2.so pixelformat.c 

# dont install until fully satisfied with the library
# install:
# 	cp -v libpixelformat.so /opt/guile/lib/guile/3.0/extensions

