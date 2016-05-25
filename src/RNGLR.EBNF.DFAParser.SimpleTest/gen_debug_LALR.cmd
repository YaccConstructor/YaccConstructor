del log.txt

for %%i in (AlternativeInMiddle, CachedReduction, CalcEBNF, Choice, ComplexRightNull, Cond, EpsilonCycle, ManyAndOne, ManyAndOpt, One, RightNull, SeqOfTwo, SeqOfTwoNonTerms, SimpleEpsilon, SimpleOpt, SimpleRightNull, SimpleSome, StackingConflict, TwoEpsilonsMiddle, TwoManyNonTerms, TwoManyTerms, TwoParents) do (
		echo . >> log.txt
		echo %%i >> log.txt
    ..\..\Bin\Debug\v40\YC.YaccConstructor.exe -i %%i.yrd -c ExpandMeta ^
        -g "RNGLR.EBNF.DFAGenerator -pos int -token int -module RNGLR.EBNF.DFAParser.%%i -translate false -table LALR -o %%i.yrd.fs" >> log.txt
)