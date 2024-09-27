My web site: [https://jerrington.me][]
======================================

This is a static web site generated with [Hakyll](https://jaspervdj.be/hakyll).

To compile the site locally, just use the Makefile.

Requirements:

- pdf2svg to turn figures into SVG images
- stack
- python

## Static pages

All the static pages are under `pages`. These compile to HTML documents with the same name.

All static content under `/static` has that prefix removed in the final URLs.

## Blog

Posts are under the `posts` directory and follow a naming convention including the post date in the
filename. A small metadata section begins each post giving the page a nice title.

```
---
title: The title of the post
---
```

## Figures

Some posts involve figures. These are written in LaTeX using tikz. The source code of figures
appears in `figures`. These are compiled into SVG files under `/static/figures/`.
