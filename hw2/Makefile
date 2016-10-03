all: hw2.pdf

clean:
	rm -rf *.aux *.gz *.log *.tex .Rhistory *~ figure cache

hw2.pdf: hw2.tex
	pdflatex hw2.tex
	Rscript --vanilla -e "require(extrafont);embed_fonts('hw2.pdf')"

hw2.tex: hw2.rnw
	Rscript --vanilla -e "require(knitr);knit('hw2.rnw')"

