del log.txt

for %%i in (AlternativeInMiddle, One, TwoManyNonTerms, TwoManyTerms, CalcEBNF, ManyAndOne) do (
		echo . >> log.txt
		echo %%i >> log.txt
    ..\..\Bin\Debug\v40\YC.YaccConstructor.exe -i %%i.yrd -c ExpandMeta ^
        -g "RNGLR.ReadBackGenerator -pos int -token int -module RNGLR.ReadBackParser.%%i -translateToAst my -table LR -o %%i.yrd.fs" >> log.txt
)