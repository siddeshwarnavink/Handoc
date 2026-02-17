MD_FILES   := $(wildcard tests/*.md)
HTML_FILES := $(MD_FILES:.md=.html)

.PHONY: all clean
all: Handoc $(HTML_FILES)

Handoc: Handoc.hs
	ghc Handoc.hs

%.html: %.md Handoc
	./Handoc $< > $@

clean:
	rm -f Handoc.hi Handoc.o Handoc
	rm -f $(HTML_FILES)
