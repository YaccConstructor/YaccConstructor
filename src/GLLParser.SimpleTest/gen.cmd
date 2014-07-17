
del log.txt

for %%i in (Epsilon, Longest, InfEpsilon) do (
		echo . >> log.txt
		echo %%i >> log.txt
    ..\..\bin\Release\v%1\YaccConstructor.exe -i %%i.yrd -c ExpandEbnf -c ExpandMeta ^
        -g "GLLGenerator -pos int -token int -module GLL.Parse%%i -o %%i.yrd.fs" >> log.txt
)

for %%i in (Order,Cond,Attrs,Calc,Counter,Cycle,LongCycle,Resolvers, LolCalc, Omit) do (
		echo . >> log.txt
		echo %%i >> log.txt
    ..\..\bin\Release\v%1\YaccConstructor.exe -i %%i.yrd ^
        -g "GLLGenerator -pos int -token int -module GLL.Parse%%i -o %%i.yrd.fs" >> log.txt
)

for %%i in (ComplexRightNull,Expr,First,List,SimpleRightNull) do (
		echo . >> log.txt
		echo %%i >> log.txt
    ..\..\bin\Release\v%1\YaccConstructor.exe -i %%i.yrd ^
        -g "GLLGenerator -pos int -token int -module GLL.Parse%%i -o %%i.yrd.fs" >> log.txt
)

