dot2tex --template Template.tex createTNFACls.dot > createTNFACls.tex
dot2tex --template Template.tex createTNFAAlt.dot > createTNFAAlt.tex
dot2tex --template Template.tex createTNFASeq.dot > createTNFASeq.tex
dot2tex --template Template.tex createTNFALeaf.dot > createTNFALeaf.tex
dot2tex --template Template.tex TNFAExample.dot  > TNFAExample.tex
dot2tex --template Template.tex TNFA2TDFA_s1.dot > TNFA2TDFAs1.tex
dot2tex --template Template.tex tree.dot > tree.tex
dot2tex --prog dot --template Template.tex TNFA2TDFA_s2.dot > TNFA2TDFAs2.tex
pdflatex TNFA.tex
acroread TNFA.pdf &

