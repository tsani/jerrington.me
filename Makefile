.PHONY: build all_posts deploy

$(shell mkdir -p files/pdfs lidr)

TEXS=$(shell find latex-src -name '*.tex')
PDFS=$(patsubst latex-src/%.tex,files/pdfs/%.pdf,$(TEXS))
LIDR=$(shell find lidr -name '*.lidr')
LIDR_MD=$(patsubst lidr/%.lidr,posts/%.lidr.md,$(LIDR))

DEPLOYDEST="/srv/http/blog"

deploy: site-rebuild
	rsync -Pr _site/ /srv/http/blog

site-%: all_posts
	cabal run $(patsubst site-%,%,$@)

all_posts: $(PDFS) $(LIDR_MD) latex-clean

posts/%.lidr.md: lidr/%.lidr
	sed 's/^> /    /' < $< | sed 's/^>/    /' > $@

files/pdfs/%.pdf: latex-src/%.tex
	@pdflatex -output-directory files/pdfs $<

.PHONY: latex-clean
latex-clean:
	@find files/pdfs \( -name '*.log' -o -name '*.aux' \) -exec rm -v {} \;
