$(shell mkdir -p lidr)


.PHONY: all site-rebuild deploy
all: all_lidr_posts all_figures site-rebuild

site-%:
	stack run -- $(patsubst site-%,%,$@)

deploy: site-rebuild
	./local-deploy.sh

##### Compiling literate idris files into pandoc-compatible markdown. #####

LIDR=$(shell find lidr -name '*.lidr')
LIDR_MD=$(patsubst lidr/%.lidr,posts/%.lidr.md,$(LIDR))

.PHONY: all_lidr_posts
all_lidr_posts: $(LIDR_MD)
posts/%.lidr.md: lidr/%.lidr
	./convert-lidr.py $@ $<

##### Compiling latex figures into svg files #####

$(shell mkdir -p static/figures)
FIGURES=$(shell find figures -name '*.tex')
SVG_FIGURES=$(patsubst figures/%.tex,static/figures/%.svg,$(FIGURES))

.PHONY: all_figures
all_figures: $(SVG_FIGURES)

static/figures/%.svg: figures/%.pdf
	pdf2svg $< $@

figures/%.pdf: figures/%.tex
	pdflatex -interaction=batchmode -file-line-error -jobname=$(patsubst figures/%.pdf,figures/%,$@) $<

