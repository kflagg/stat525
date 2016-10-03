all: hw1.pdf

clean:
	rm -rf *.aux *.gz *.log *.tex .Rhistory *~ figure cache

hw1.pdf: hw1.tex
	pdflatex hw1.tex
	Rscript --vanilla -e "require(extrafont);embed_fonts('hw1.pdf')"

hw1.tex: hw1.rnw Homework1.pdf
	Rscript --vanilla -e "require(knitr);knit('hw1.rnw')"

Homework1.pdf:

