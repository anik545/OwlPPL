# This is the Makefile for the demonstration dissertation
# written by Martin Richards
#
# Note that continuation lines require '\'
# and that TAB is used after ':' and before unix commands.

DISS = diss.tex refs.bib propbody.tex figs/diagram.eps makefile.txt

PROP = proposal.tex propbody.tex

help:
	@echo
	@echo "USAGE:"
	@echo
	@echo "make          display help information"
	@echo "make prop     make the proposal and view it using xdvi"
	@echo "make diss.ps  make a postscript version of the dissertation"
	@echo "make diss.pdf make a .pdf version of the dissertation"
	@echo "make gv       view the dissertation with ghostview"
	@echo "make gs       view the dissertation with ghostscript"
	@echo "make all      construct proposal.dvi and diss.ps"
	@echo "make count    display an estimated word count"
	@echo "make pub      put demodiss.tar on my homepage"
	@echo "make clean    remove all remakeable files"
	@echo "make pr       print the dissertation"
	@echo

prop:	proposal.dvi
	xdvi proposal.dvi

diss.ps:	$(DISS)
	latex diss
	bibtex diss
	latex diss
	bibtex diss
	latex diss
	bibtex diss
	dvips -Ppdf -G0 -t a4 -pp 0-200 -o diss.ps diss.dvi

diss.pdf:	diss.ps
	ps2pdf diss.ps

makefile.txt:	Makefile
	expand Makefile >makefile.txt
count:
	detex diss.tex | tr -cd '0-9A-Za-z \n' | wc -w

proposal.dvi: $(PROP)
	latex proposal

all:	proposal.dvi diss.ps

pub:	diss.pdf
	cp diss.pdf /homes/mr/public_html/demodiss.pdf
	make clean
	(cd ..; tar cfv /homes/mr/public_html/demodiss.tar demodiss)

clean:
	rm -f diss.ps *.dvi *.aux *.log *.err
	rm -f core *~ *.lof *.toc *.blg *.bbl
	rm -f makefile.txt

gv:	diss.ps
	ghostview diss.ps

gs:	diss.ps
	gs diss.ps

pr:	diss.ps
	lpr diss.ps
