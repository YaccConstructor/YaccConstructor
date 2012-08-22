cd DOT/
sh make.sh
cd ..

pdflatex SemenDiplomaTitle_en.tex

pdflatex --shell-escape Semen_Grigorev_Diploma_20_fbr.tex

evince Semen_Grigorev_Diploma_20_fbr.pdf &
