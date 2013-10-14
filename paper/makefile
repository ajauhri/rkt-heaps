MAIN= final-report
TEXPARTS= *.tex
BIBPARTS= *.bib
FIGPARTS= FIG*


$(MAIN).pdf: $(MAIN).tex
	pdflatex $(MAIN).tex
	bibtex $(MAIN)
	pdflatex $(MAIN).tex
	pdflatex $(MAIN).tex

clean:
	\rm -f *.dvi *.bbl *.aux *.log *.blg *.toc *.bak

spotless: clean
	\rm -f $(MAIN).ps
	\rm -f $(MAIN).pdf


all.tar:
	tar cvfh all.tar makefile $(MAIN).tex $(TEXPARTS) $(BIBPARTS) \
	$(FIGPARTS) README psfig.sty

