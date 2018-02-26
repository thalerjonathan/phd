pdflatex -shell-escape -synctex=1 -interaction=nonstopmode pfe.tex
wait
bibtex pfe.aux
wait
pdflatex -shell-escape -synctex=1 -interaction=nonstopmode pfe.tex
wait
pdflatex -shell-escape -synctex=1 -interaction=nonstopmode pfe.tex
wait
