# This is the Makefile for the demonstration dissertation
# written by Martin Richards
#
# Note that continuation lines require '\'
# and that TAB is used after ':' and before unix commands.

DISS = diss.tex refs.bib propbody.tex figs/diagram.eps makefile.txt

PROP = proposal.tex propbody.tex

ALL_TEX = $(shell find data code_snippets chapters -type f) $(wilcard *.tex *.txt *.bib) figs data tikz code_snippets chapters

OUTDIR = out

help:
	@echo
	@echo "USAGE:"
	@echo
	@echo "make          display help information"
	@echo "make proposal"
	@echo "make diss"

	@echo "make linecount"
	@echo "make wordcount"
	@echo "make submit"
	@echo "make clean    remove all remakeable files"
	@echo

# diss.ps:	$(DISS)

LATEXMK_ARGS = -synctex=1 -interaction=nonstopmode -file-line-error -pdf --shell-escape -output-directory=$(OUTDIR) -aux-directory=$(OUTDIR)

.PHONY: diss
diss: diss.pdf
diss.pdf: diss.tex $(ALL_TEX)
	latexmk $(LATEXMK_ARGS) diss.tex

.PHONY: proposal
proposal: proposal.pdf
proposal.pdf: propbody.tex proposal.tex
	latexmk $(LATEXMK_ARGS) proposal.tex

wordcount:
	detex chapters/conclusion.tex chapters/evaluation.tex chapters/implementation.tex chapters/introduction.tex chapters/preparation.tex chapters/proforma.tex | tr -cd '0-9A-Za-z \n' | wc -w
	texcount -inc -sum -1 diss.tex

linecount:
	cloc  --yaml `git ls-files ..` | grep 'code: ' | tail -n 1 | sed  's/  code://g'

CODE_TO_SUBMIT = $(shell echo `git ls-files ../evaluation` `git ls-files ../ppl`)

.PHONY: code
code: ar899.tar.gz
ar899.tar.gz: $(CODE_TO_SUBMIT)
	echo $(CODE_TO_SUBMIT)
	tar -cvzf ar899.tar.gz $(CODE_TO_SUBMIT)

.PHONY: submit
submit: diss.pdf ar899.tar.gz
	cp diss.pdf ../ar899.pdf
	cp ar899.tar.gz ../ar899.tar.gz

.PHONY: clean

clean:
	# echo $(OUTDIR)
	rm -r out/*