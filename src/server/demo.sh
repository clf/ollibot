#!/bin/sh

./server5 @MLton max-heap 128m -- -codepath ../ml5pgh/demos/ -tdb "db-demo.txt" -port 7777
