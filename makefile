
default: copy-mlton commandline-mlton 

server: copy-mlton server-mlton

copy-mlton: 
	rm -Rf escape-src
	sml -m tools/escape-sml/sources.cm < /dev/null

bin:
	mkdir bin

commandline-mlton: bin
	-mlton -default-ann 'sequenceNonUnit warn' -output bin/lollibot escape-src/commandline.mlb 
