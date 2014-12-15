
del log.txt

for %%i in (Epsilon, Longest, InfEpsilon, LongCycle_BAD) do (
		echo . >> log.txt
		echo %%i >> log.txt
    ..\..\Bin\Release\v40\YC.YaccConstructor.exe -i ..\RNGLRParser.SimpleTest\%%i.yrd -c ExpandMeta ^
        -g "RNGLR.EBNFGenerator -pos int -token int -module RNGLR.EBNF.Parse%%i -o %%i.yrd.fs" >> log.txt
)

for %%i in (Order,Cond,Attrs,Calc,Counter,Cycle,LongCycle,Resolvers, LolCalc, Omit) do (
		echo . >> log.txt
		echo %%i >> log.txt
    ..\..\Bin\Release\v40\YC.YaccConstructor.exe -i ..\RNGLRParser.SimpleTest\%%i.yrd ^
        -g "RNGLR.EBNFGenerator -pos int -token int -module RNGLR.Parse%%i -o %%i.yrd.fs" >> log.txt
)

for %%i in (ComplexRightNull,Expr,First,List,SimpleRightNull) do (
		echo . >> log.txt
		echo %%i >> log.txt
    ..\..\Bin\Release\v40\YC.YaccConstructor.exe -i ..\RNGLRParser.SimpleTest\%%i.yrd ^
        -g "RNGLR.EBNFGenerator -pos int -token int -module RNGLR.Parse%%i -translate false -o %%i.yrd.fs" >> log.txt
)