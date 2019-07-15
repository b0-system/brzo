# N.B. doesn't handle changes in toc and bib removals.

# document base name
document = test

# prelude name
prelude = prelude-bera

# files to generate
products = $(document).pdf

# other deps (comment out document.bbl if you don't have a bib file)
deps = $(prelude).sty $(document).bbl

# signature size for booklets
sigsize = 40

LATEX = latex -file-line-error -halt-on-error
PDFLATEX = pdflatex -file-line-error -halt-on-error

default: preview

all: $(products)

$(document).pdf: LATEXC := $(PDFLATEX)
$(document).pdf: $(document).aux

$(document).ps $(document).dvi: LATEXC := $(LATEX)
$(document).ps $(document).dvi: $(document).aux

$(document).aux: $(deps)

$(document)-bk.pdf: $(document).pdf
	@echo "$(echopre) Generating $@ from $< using $(PDFLATEX)"
	printf "\\\documentclass[a4paper]{article}   \n\
	\\\usepackage{pdfpages}             \n\
	\\\begin{document}                  \n\
	\\\includepdf[frame,pages=-,signature=$(sigsize),landscape]{$<}  \n\
	\\\end{document}\n" | pdflatex &> /dev/null
	rm texput.log texput.aux
	mv texput.pdf $@

$(document)-bk.ps: $(document).ps
	@echo "$(echopre) Generating $@ from $< using psbook"
	psbook -s$(sigsize) -q $< | psnup -q -2 | pstops -q "2:0,1U(1w,1h)" > \
	$@

cleanaux:
	$(RM) -f $(document).aux $(document).log $(document).toc \
	$(document).bbl $(document).blg $(document).thm $(document).out \

clean: cleanaux
	rm -f $(document).pdf $(document).dvi $(document).ps \
	$(document)-bk.ps $(document)-bk.pdf

preview: pdfpreview

dvipreview: $(document).dvi
	$(DVIVIEW) $<

pspreview: $(document).ps
	$(PSVIEW) $<

pdfpreview: $(document).pdf
	$(PDFVIEW) $<

.PHONY: default all cleanaux  clean preview dvipreview pspreview pdfpreview

SHELL = /bin/sh
RM = /bin/rm
TOUCH = /usr/bin/touch
GREP = egrep

# latex compilers
LATEX = latex -file-line-error -halt-on-error
PDFLATEX = pdflatex -file-line-error -halt-on-error
HTMLLATEX =

# bibtex
BIBTEX = bibtex

# dvi converters
DVIPS = dvips
DVIPDF = dvipdf

# previewers
PSVIEW = gv
DVIVIEW = xdvi
PDFVIEW = open -a preview


# pattern rules

echopre = >>>>

rerun = "Rerun to get|No file .*\.toc"
citundef = "There were undefined references"
dviwritten = "written on $(document).dvi"
pdfwritten = "written on $(document).pdf"

%.aux: %.tex
	@echo "$(echopre) Generating $@ from $< using $(LATEXC)"
	$(LATEXC) $<


%.bbl: %.bib
	@echo "$(echopre) Generating $@ from $< using $(BIBTEX)"
	if [ -e $(<:.bib=.aux) ]; then \
	  $(BIBTEX) $(<:.bib=); \
	fi
	@echo "Rerun to get" >> $(<:.bib=.log)

%.dvi : %.tex
	@echo "$(echopre) Generating $@ from $< using $(LATEX)"
	@if $(GREP) -q $(citundef) $(<:.tex=.log); then \
	  $(BIBTEX) $(<:.tex=); \
	  $(LATEX) $<; \
	fi
	@if $(GREP) -q $(pdfwritten) $(<:.tex=.log); then \
	  $(LATEX) $<; \
	fi
	@while $(GREP) -q $(rerun) $(<:.tex=.log); do \
	  $(LATEX) $<; \
	done

%.ps : %.dvi
	@echo "$(echopre) Generating $@ from $< using $(DVIPS)"
	$(DVIPS) $< -o $@

%.pdf : %.tex
	@echo "$(echopre) Generating $@ from $< using $(PDFLATEX)"
	@if $(GREP) -q $(citundef) $(<:.tex=.log); then \
	  $(BIBTEX) $(<:.tex=); \
	  $(PDFLATEX) $<; \
	fi
	@if $(GREP) -q $(dviwritten) $(<:.tex=.log); then \
	  $(PDFLATEX) $<; \
	fi
	@while $(GREP) -q $(rerun) $(<:.tex=.log); do \
	  $(PDFLATEX) $<; \
	done
