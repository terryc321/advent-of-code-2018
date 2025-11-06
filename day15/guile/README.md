
# Graphical Ability

install the shared library to correct location , without this everything else will not work

```
> sudo cp -v pixelformat/libpixelformat.so /opt/guile/lib/guile/3.0/extensions
```

add the directory holds this file into guile load path

```
> guile
> (add-to-load-path "/home/terry/code/advent-code/advent-of-code-2018/day15/guile")
```

with the load path updated guile can now work with these instructions

```
(use-modules (macros fcase))
(use-modules (macros inc))
(use-modules ((graphics sdl2 sdl) #:prefix sdl:))
(use-modules ((graphics sdl2 image) #:prefix img:))
(use-modules ((graphics cairo cairo) #:prefix cairo:))
```

guile will look for fcase.scm inside macros directory on the load path 

```
(use-modules (macros fcase))
```

similarly for increment macros , really simple incf version from common lisp 

```
(use-modules (macros inc))
```


the shared library libpixelformat.so is a little more complex . it needs to be compiled and installed in
correct location
```
> sudo cp -v pixelformat/libpixelformat.so /opt/guile/lib/guile/3.0/extensions
```

this is because my guile is installed at
```
/opt/guile
/opt/guile/bin/guile
/opt/guile/lib/guile/3.0/extensions <-- where shared libraries should go
/opt/guile/share/guile/site/3.0 <--     where guile scheme code should go make up the modules
```


try use a guile SDL + CAIRO library to see if we can do something graphical 

model 

walls , caverns , elfs , goblins 

simple 2d grid 

smalltalk 

# THE BOOK on programming 

# SETUP guile scheme 

emacs <= geiser SETUP

guile binary installed in /opt/guile/bin/guile 

guile shared libraries go into ? 

libpixelformat is shared library that does little C magic.

guile module libraries go into ? 

can we fool emacs into loading our own "EXECUTABLE" which is really just a shell script instead of actual guile executable

this allow us to intevene and set required envrionemtn both for loading shared libraries , and guile itself



# aoc 2018 day 15 


