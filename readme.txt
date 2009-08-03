= Ollibot OLLIBOT_VERSION = 
== Robert J. Simmons and Frank Pfenning == 

== Compiling Ollibot ==

Compiling Ollibot requires a recent version of MLton and SML of New Jersey.
You can use make to compile either the Ollibot command-line interface, web
interface, or both.

Make only the command-line interface:
   $ make ollibot

Make only the server interface:
   $ make server

Make both the command-line interface and the server:
   $ make

The following files will be produced:
   bin/ollibot (command line)
   bin/olliserver (web server version)

== Command line interface ==

Run the command-line version of Ollibot with the following command:
   $ bin/ollibot <file1> <file2> ....

This will run each of the listed files in turn. For example, the following
command will run all of the examples in the lics09 example directory.
   $ bin/ollibot examples/lics09/*olf

== Web interface ==

Run this command to start the web interface:
   $ bin/olliserver

This starts a web server on port 5315 with a root in the examples directory.
So, if you start your web browser and go to
   http://localhost:5315/
you will see the readme.txt file in the examples directory. If you go to 
   http://localhost:5315/lics09/cbneed.lolf
you will see the call by need example located in the 
examples/lics09/cbneed.lolf directory.

The web interface uses a simple wiki-like markup language described below.

== Ollibot files ==

There are two kinds of Ollibot files, regular Ollibot files (with a .olf 
extension), and Literate Ollibot files (with a .olf extension). 
The examples/lics09 directory has several examples of both kinds of files.

In regular Ollibot files, line comments start with the string %%

%% Comment
Ollibot code
%% Comment

In Literate Ollibot, everything is taken to be a comment except for lines that 
start with a >

Comment
> Ollibot code
Comment

There is also a markup language that the web server uses

= Heading 1 =
== Heading 2 ==
=== Heading 3 ===
You can make code `fixed-width`, ''italics'', '''bold''', or '''''bold 
italics''''', as well as creating links like this: [[http://www.google.com]]
or like [[http://www.google.com | this]].
