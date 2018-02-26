pdflatex -shell-escape -synctex=1 -interaction=nonstopmode $1.tex
wait
bibtex $1.aux
wait
pdflatex -shell-escape -synctex=1 -interaction=nonstopmode $1.tex
wait
pdflatex -shell-escape -synctex=1 -interaction=nonstopmode $1.tex
wait
