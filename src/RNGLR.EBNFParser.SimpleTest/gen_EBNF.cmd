del log.txt

for %%i in (AlternativeInMiddle, CalcEBNF, ComplexRightNull, Choice, LeftRec, EpsilonCycle, ManyAndOne, ManyAndOpt, RightNull, SimpleEpsilon, SimpleOpt, SimpleRightNull, SimpleSome, StackingConflict, TwoEpsilonsMiddle) do (
		echo . >> log.txt
		echo %%i >> log.txt
    ..\..\Bin\Release\v40\YC.YaccConstructor.exe -i %%i.yrd -c ExpandMeta ^
        -g "RNGLR.EBNFGenerator -pos int -token int -module RNGLR.Parse%%i -translate true -table LALR -o %%i.yrd.fs" >> log.txt
)