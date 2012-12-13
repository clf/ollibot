default: ollibot server

ollibot: bin/ollibot

server: bin/olliserver

.PHONY: copy-mlton
copy-mlton: 
	rm -Rf escape-src
	sml -m tools/escape-sml/sources.cm < /dev/null
	mkdir -p bin

bin/ollibot: copy-mlton
	-mlton -default-ann 'sequenceNonUnit warn' -output bin/ollibot escape-src/commandline.mlb

bin/olliserver: copy-mlton
	-mlton -default-ann 'allowFFI true' -default-ann 'sequenceNonUnit warn' -output bin/olliserver escape-src/server.mlb src/server/raw-network.c

clean:
	rm -rf bin escape-src
