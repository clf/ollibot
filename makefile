
default: copy-mlton commandline-mlton server-mlton

ollibot: copy-mlton commandline-mlton

server: copy-mlton server-mlton

copy-mlton: 
	rm -Rf escape-src
	sml -m tools/escape-sml/sources.cm < /dev/null

server-mlton:
	-mlton -default-ann 'allowFFI true' -default-ann 'sequenceNonUnit warn' -output bin/olliserver escape-src/server.mlb src/server/raw-network.c

commandline-mlton:
	-mlton -default-ann 'sequenceNonUnit warn' -output bin/ollibot escape-src/commandline.mlb 
