PANDOC=pandoc
PANDOC_ARGS=--pdf-engine=xelatex

README.html README.pdf: README.md
	     $(PANDOC) $(PANDOC_ARGS) --from markdown --to $(subst .,,$(suffix $(@F))) -o $@ $<

