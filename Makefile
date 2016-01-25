.PHONY: all

TEXS=$(shell find latex-src -name '*.tex')
PDFS=$(patsubst latex-src/%.tex,files/pdfs/%.pdf,$(TEXS))

all: $(PDFS) latex-clean

$(shell mkdir -p files/pdfs)

files/pdfs/%.pdf: latex-src/%.tex
	@pdflatex -output-directory files/pdfs $<

.PHONY: latex-clean
latex-clean:
	@find files/pdfs \( -name '*.log' -o -name '*.aux' \) -exec rm -v {} \;
