all: hw5.pdf

clean:
	rm -rf *.aux *.gz *.log *.tex .Rhistory *~ figure cache

hw5.pdf: hw5.tex
	pdflatex hw5.tex
	Rscript --vanilla -e "require(extrafont);embed_fonts('hw5.pdf')"

hw5.tex: hw5.rnw
	Rscript --vanilla -e "require(knitr);knit('hw5.rnw')"

