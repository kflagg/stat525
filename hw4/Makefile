all: hw4.pdf

clean:
	rm -rf *.aux *.gz *.log *.tex .Rhistory *~ figure cache

hw4.pdf: hw4.tex
	pdflatex hw4.tex
	Rscript --vanilla -e "require(extrafont);embed_fonts('hw4.pdf')"

hw4.tex: hw4.rnw
	Rscript --vanilla -e "require(knitr);knit('hw4.rnw')"

