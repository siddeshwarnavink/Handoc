.PHONY: all
all: Handoc test.html

Handoc: Handoc.hs
	ghc Handoc.hs

test.html: Handoc test.md
	./Handoc test.md > test.html
