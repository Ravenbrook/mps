# -*- makefile -*-
#
#
# This Makefile contains rules for converting the LaTeX sources of the
# CMUCL User's Manual into various formats: Postscript, PDF, DVI, HTML
# and info. The useful targets are:
#
#    make cmu-user.ps           (A4 paper)
#    make cmu-user-letter.ps    (letter paper)
#    make cmu-user.dvi
#    make cmu-user.pdf
#    make cmu-user.html
#    make cmu-user.info
#    make clean

# A number of addon LaTeX packages are used in the manual, to support
# features such as hyperlinks and multiple indexes. A current
# distribution of teTeX contains everything that is necessary to
# generate the Postscript, PDF and DVI formats. HTML output is
# generated using the Hevea tool, which is available from
# <URL:http://pauillac.inria.fr/~maranget/hevea/>. The HTML files are
# then split into sections using the hacha tool (distributed with
# hevea), and cleaned up using tidy.
#
# The generated DVI file should include clickable hyperlinks. The PDF
# output should include a hyperlinked table of contents, hyperlinked
# cross-references, and an index. The generated Postscript should use
# Postscript fonts that give good quality output at high resolutions.


SHELL = /bin/sh
LATEX ?= latex
BIBTEX ?= bibtex
PDFLATEX ?= pdflatex
HEVEA ?= hevea
HACHA ?= hacha
TIDY ?= tidy

FILES = *.tex

.SUFFIXES: 
.SUFFIXES: .tex .dvi .ps .ps1 .pdf .html .info


all: cmu-user.ps


# Runs LaTeX once, then reruns LaTeX as many times as necessary to get
# rid of the "undefined references" message, generates the indexes,
# the reruns LaTeX. The dependency on the .tex files means that the
# DVI file will be rebuilt only if one of the included LaTeX files has
# been modified.
%.dvi : %.tex $(FILES)
	$(LATEX) $<
	@while ( grep Rerun $*.log > /dev/null ); do \
	   $(LATEX) $<; \
	done
	if [ "$<" = "cmu-user.tex" ]; then $(MAKE) index; else $(MAKE) index-letter; fi

	$(LATEX) $<

%.pdf : %.tex $(FILES)
	$(PDFLATEX) $<
	@while ( grep Rerun $*.log > /dev/null ); do \
	   $(PDFLATEX) $<; \
	done
	if [ "$<" = "cmu-user.tex" ]; then $(MAKE) index; else $(MAKE) index-letter; fi
	$(PDFLATEX) $<

# the "-fix" option to hevea makes it run as many times as necessary
# to resolve all cross-references and generate an index.
%.html : %.tex $(FILES)
	$(HEVEA) -fix cmu-user.hva $<
	$(HACHA) -tocbis $@
	-$(TIDY) -m *.html

%.info : %.tex $(FILES)
	$(HEVEA) -fix -info cmu-user.hva $<

%.ps1 : %.dvi 
	dvips -o $@ $<

# convert the Postscript file to duplex (will print double-sided if
# the printer supports it)
%.ps : %.ps1
	psset -d -o $@ $<

# Tar up the html files
cmu-user-html.tgz : cmu-user.html
	mkdir cmu-user
	cp *.html *.gif cmu-user
	tar cf - cmu-user | gzip > cmu-user-html.tgz

# generate Postscript for letter format, instead of for A4 paper
.INTERMEDIATE: cmu-user-letter.tex
cmu-user-letter.tex: cmu-user.tex
	cp $< $@
	perl -pi -e 's/documentclass\[a4paper\]/documentclass\[letter\]/' $@


clean:
	rm -f *.log *.bbl *.blg *.ps *.pdf *.aux *.lof *.toc *.gif *.out *.ilg
	rm -f *.vdx *.cdx *.tdx *.fdx *.idx *.cnd *.fnd *.tnd *.vnd *.haux
	rm -f *.html *.hcnd *.htnd *.hvnd *.hfnd *.htoc

index:
	makeindex cmu-user.tdx -o cmu-user.tnd
	makeindex cmu-user.vdx -o cmu-user.vnd
	makeindex cmu-user.fdx -o cmu-user.fnd
	makeindex cmu-user.cdx -o cmu-user.cnd

index-letter:
	makeindex cmu-user-letter.tdx -o cmu-user-letter.tnd
	makeindex cmu-user-letter.vdx -o cmu-user-letter.vnd
	makeindex cmu-user-letter.fdx -o cmu-user-letter.fnd
	makeindex cmu-user-letter.cdx -o cmu-user-letter.cnd

.PHONY: clean index

# EOF
