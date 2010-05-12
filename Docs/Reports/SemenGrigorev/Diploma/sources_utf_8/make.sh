cd DOT/
sh make.sh
cd ..

pdflatex --shell-escape Semen_Grigorev_Diploma_20_fbr.tex

acroread Semen_Grigorev_Diploma_20_fbr.pdf &
