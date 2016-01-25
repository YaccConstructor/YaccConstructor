dot gss.dot -o gss.pdf -Tpdf

for %%i in (1,2,3,4,5,6,7,8,9,10,11) do (
  dot gss%%i.dot -o gss%%i.pdf -Tpdf
)

dot sppf.dot -o sppf.pdf -Tpdf  