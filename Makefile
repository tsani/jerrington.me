.PHONY: build all_posts deploy

$(shell mkdir -p lidr)

LIDR=$(shell find lidr -name '*.lidr')
LIDR_MD=$(patsubst lidr/%.lidr,posts/%.lidr.md,$(LIDR))

DEPLOYDEST="/srv/http/blog"

deploy: build site-rebuild
	rsync -Pr _site/ $(DEPLOYDEST)

build:
	stack build
	stack install
	stack exec site rebuild

site-%: all_posts
	stack exec site $(patsubst site-%,%,$@)

all_posts: $(LIDR_MD)

posts/%.lidr.md: lidr/%.lidr
	./convert-lidr.py $@ $<
