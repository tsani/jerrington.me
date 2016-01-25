.PHONY: all

TEXS=$(shell find latex-src -name '*.tex')
PDFS=$(patsubst latex-src/%.tex,files/pdfs/%.pdf,$(TEXS))

all: $(PDFS)

$(shell mkdir -p files/pdfs)

files/pdfs/%.pdf: latex-src/%.tex
	pdflatex -output-directory files/pdfs $<
