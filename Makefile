.PHONY: all build all_posts deploy

$(shell mkdir -p lidr)

LIDR=$(shell find lidr -name '*.lidr')
LIDR_MD=$(patsubst lidr/%.lidr,posts/%.lidr.md,$(LIDR))

all: site-rebuild

deploy: build site-rebuild
	./local-deploy.sh

site-%: all_posts
	stack run -- $(patsubst site-%,%,$@)

all_posts: $(LIDR_MD)

posts/%.lidr.md: lidr/%.lidr
	./convert-lidr.py $@ $<
