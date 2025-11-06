
# aoc 2018 day 15 

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

