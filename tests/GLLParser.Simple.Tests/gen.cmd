
del log.txt

for %%i in (Epsilon, Longest, InfEpsilon) do (
    echo . >> log.txt
    echo %%i >> log.txt
    ..\..\Bin\Release\v40\YC.YaccConstructor.exe -i %%i.yrd ^
        -g "GLLGenerator -pos int -token int -abstract false -module GLL.Parse%%i -o %%i.yrd.fs" >> log.txt
)

for %%i in (Order,Cond,Attrs,Calc,Counter,Cycle,LongCycle, LolCalc, Omit) do (
    echo . >> log.txt
    echo %%i >> log.txt
    ..\..\Bin\Release\v40\YC.YaccConstructor.exe -i %%i.yrd ^
        -g "GLLGenerator -pos int -token int -abstract false -module GLL.Parse%%i -o %%i.yrd.fs" >> log.txt
)

for %%i in (ComplexRightNull,Expr,First,List,SimpleRightNull) do (
    echo . >> log.txt
    echo %%i >> log.txt
    ..\..\Bin\Release\v40\YC.YaccConstructor.exe -i %%i.yrd ^
        -g "GLLGenerator -pos int -token int -abstract false -module GLL.Parse%%i -o %%i.yrd.fs" >> log.txt
)

..\..\Bin\Release\v40\YC.YaccConstructor.exe -i Eps.yrd  ^
        -g "GLLGenerator -pos int -token int -abstract false -module GLL.Eps -o Eps.yrd.fs " >> log.txt

..\..\Bin\Release\v40\YC.YaccConstructor.exe -i Eps2.yrd  ^
        -g "GLLGenerator -pos int -token int -abstract false -module GLL.Eps2  -o Eps2.yrd.fs " >> log.txt

..\..\Bin\Release\v40\YC.YaccConstructor.exe -i ListEps.yrd  ^
        -g "GLLGenerator -pos int -token int -abstract false -module GLL.ListEps -o ListEps.yrd.fs " >> log.txt

..\..\Bin\Release\v40\YC.YaccConstructor.exe -i Brackets.yrd  ^
        -g "GLLGenerator -pos int -token int -abstract false -module GLL.Brackets -o Brackets.yrd.fs " >> log.txt
                                                                                                                                  

..\..\Bin\Release\v40\YC.YaccConstructor.exe -i BadLeftRecursion.yrd  ^
        -g "GLLGenerator -pos int -token int -abstract false -module GLL.BadLeftRecursion -o BadLeftRecursion.yrd.fs " >> log.txt


..\..\Bin\Release\v40\YC.YaccConstructor.exe -i SimpleLeftRecursion.yrd  ^
        -g "GLLGenerator -pos int -token int -abstract false -module GLL.SimpleLeftRecursion -o SimpleLeftRecursion.yrd.fs " >> log.txt


..\..\Bin\Release\v40\YC.YaccConstructor.exe -i SimpleRightRecursion.yrd  ^
        -g "GLLGenerator -pos int -token int -abstract false -module GLL.SimpleRightRecursion -o SimpleRightRecursion.yrd.fs " >> log.txt


..\..\Bin\Release\v40\YC.YaccConstructor.exe -i SimpleAmb.yrd  ^
        -g "GLLGenerator -pos int -token int -abstract false -module GLL.SimpleAmb -o SimpleAmb.yrd.fs " >> log.txt


..\..\Bin\Release\v40\YC.YaccConstructor.exe -i Mixed.yrd  ^
        -g "GLLGenerator -pos int -token int -abstract false -module GLL.Mixed -o Mixed.yrd.fs " >> log.txt

..\..\Bin\Release\v40\YC.YaccConstructor.exe -i PrettySimpleCalc.yrd  ^
        -g "GLLGenerator -pos int -token int -abstract false -module GLL.PrettySimpleCalc -o PrettySimpleCalc.yrd.fs " >> log.txt


