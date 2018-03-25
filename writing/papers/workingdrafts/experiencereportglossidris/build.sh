pdflatex -shell-escape -synctex=1 -interaction=nonstopmode epglossidris.tex
wait
bibtex epglossidris.aux
wait
pdflatex -shell-escape -synctex=1 -interaction=nonstopmode epglossidris.tex
wait
pdflatex -shell-escape -synctex=1 -interaction=nonstopmode epglossidris.tex
wait
