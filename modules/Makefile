FILES=src/exports src/main src/scripts src/types
HS=$(FILES:=.h)
BLOBS=$(FILES:=.blob)

all: $(HS) $(BLOBS)

remake:
	rm -f $(HS) $(BLOBS)

%.h: %.rkt
	racket -e "(require (submod (file \"$^\") make-header))"
	cat $@

%.blob: %.rkt
	racket -e "(require (submod (file \"$^\") make-blob))"
	cat $@ 
