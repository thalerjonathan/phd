all: qabs.tex
	pdflatex -shell-escape -synctex=1 -interaction=nonstopmode qabs.tex
	wait
	bibtex qabs.aux
	wait
	pdflatex -shell-escape -synctex=1 -interaction=nonstopmode qabs.tex
	wait
	pdflatex -shell-escape -synctex=1 -interaction=nonstopmode qabs.tex
	wait

clean:
	$(RM) comment.cut
	$(RM) *.aux
	$(RM) *.bbl
	$(RM) *.blg
	$(RM) *.log
	$(RM) *.out
	$(RM) *.gz
	$(RM) _minted* -r
