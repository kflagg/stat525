filename=hw8

all: $(filename).pdf

clean:
	rm -rf *.aux *.gz *.log *.tex .Rhistory *~ figure cache

$(filename).pdf: $(filename).tex
	pdflatex $(filename).tex
	Rscript --vanilla -e "require(extrafont);embed_fonts('$(filename).pdf')"

$(filename).tex: $(filename).rnw Scotland.txt
	Rscript --vanilla -e "require(knitr);knit('$(filename).rnw')"

Scotland.txt:

