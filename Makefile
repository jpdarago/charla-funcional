DOTS=$(wildcard dot/*.dot)
IMGS=$(patsubst dot/%.dot,images/%.pdf,$(DOTS))

images/%.pdf: dot/%.dot
	dot -Tpdf -o$@ $^

.PHONY: clean Makefile header.tex
slides.pdf: slides.md $(IMGS) Makefile header.tex
	~/.cabal/bin/pandoc --slide-level=2 --toc -H header.tex --listings\
		-V theme:Warsaw -V colortheme:crane\
		-s -t beamer -f markdown -o $@ $<

clean:
	rm -rf slides.pdf $(IMGS)
