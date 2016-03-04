del log.txt

for %%i in (AlternativeInMiddle, CalcEBNF, Choice, ComplexRightNull, EpsilonCycle, ManyAndOne, ManyAndOpt, One, RightNull, SeqOfTwo, SimpleEpsilon, SimpleOpt, SimpleRightNull, SimpleSome, StackingConflict, TwoEpsilonsMiddle, TwoManyNonTerms, TwoManyTerms) do (
		echo . >> log.txt
		echo %%i >> log.txt
    ..\..\Bin\Debug\v40\YC.YaccConstructor.exe -i %%i.yrd -c ExpandMeta ^
        -g "RNGLR.ReadBackGenerator -pos int -token int -module RNGLR.ReadBackParser.%%i -translate true -table LR -o %%i.yrd.fs" >> log.txt
)