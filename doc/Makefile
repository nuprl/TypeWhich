all:
	latexmk -pdf doc
	pandoc doc.tex --bibliography doc.bib -s -o doc.html
	pandoc doc.tex --bibliography doc.bib  -t gfm -o ../README.md
