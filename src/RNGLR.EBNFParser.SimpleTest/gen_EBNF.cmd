del log.txt

for %%i in (CalcEBNF, Choice, ComplexRightNull, EpsilonCycle, LeftRec, ManyAndOne, ManyAndOpt, RightNull, SimpleEpsilon, SimpleOpt, SimpleRightNull, SimpleSome, StackingConflict, TwoEpsilonsMiddle) do (
		echo . >> log.txt
		echo %%i >> log.txt
    ..\..\Bin\Release\v40\YC.YaccConstructor.exe -i %%i.yrd -c ExpandMeta ^
        -g "RNGLR.EBNFGenerator -pos int -token int -module RNGLR.Parser%%i -translate true -table LR -o %%i.yrd.fs" >> log.txt
)