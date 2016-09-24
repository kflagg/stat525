all: hw3.pdf

clean:
	rm -rf *.aux *.gz *.log *.tex .Rhistory *~ figure cache

hw3.pdf: hw3.tex
	if [ ! -e hw3.aux ] ; then pdflatex hw3.tex ; fi
	pdflatex hw3.tex
	Rscript --vanilla -e "require(extrafont);embed_fonts('hw3.pdf')"

hw3.tex: hw3.rnw
	Rscript --vanilla -e "require(knitr);knit('hw3.rnw')"

