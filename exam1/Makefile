all: ex1.pdf

clean:
	rm -rf *.aux *.gz *.log *.tex .Rhistory *~ figure cache

ex1.pdf: ex1.tex
	pdflatex ex1.tex
	Rscript --vanilla -e "require(extrafont);embed_fonts('ex1.pdf')"

ex1.tex: ex1.rnw
	Rscript --vanilla -e "require(knitr);knit('ex1.rnw')"

