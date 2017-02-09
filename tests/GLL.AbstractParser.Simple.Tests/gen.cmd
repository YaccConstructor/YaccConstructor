..\..\Bin\Release\v40\YC.YaccConstructor.exe -i SimpleCalc.yrd -c ExpandEbnf -c ExpandMeta ^
        -g "GLLGenerator -pos int -token int -module GLL.ParseSimpleCalc  -o SimpleCalc.yrd.fs -abstract true" >> log.txt

..\..\Bin\Release\v40\YC.YaccConstructor.exe -i SimpleBranch.yrd -c ExpandEbnf -c ExpandMeta ^
        -g "GLLGenerator -pos int -token int -module GLL.ParseSimpleBranch  -o SimpleBranch.yrd.fs -abstract true" >> log.txt

..\..\Bin\Release\v40\YC.YaccConstructor.exe -i BadLeftRecursion.yrd -c ExpandEbnf -c ExpandMeta ^
        -g "GLLGenerator -pos int -token int -module GLL.BadLeftRecursion  -o BadLeftRecursion.yrd.fs -abstract true" >> log.txt

..\..\Bin\Release\v40\YC.YaccConstructor.exe -i SimpleAmb.yrd -c ExpandEbnf -c ExpandMeta ^
        -g "GLLGenerator -pos int -token int -module GLL.SimpleAmb  -o SimpleAmb.yrd.fs -abstract true" >> log.txt

..\..\Bin\Release\v40\YC.YaccConstructor.exe -i SimpleLeftRecursion.yrd -c ExpandEbnf -c ExpandMeta ^
        -g "GLLGenerator -pos int -token int -module GLL.SimpleLeftRecursion  -o SimpleLeftRecursion.yrd.fs -abstract true" >> log.txt

..\..\Bin\Release\v40\YC.YaccConstructor.exe -i SimpleRightRecursion.yrd -c ExpandEbnf -c ExpandMeta ^
        -g "GLLGenerator -pos int -token int -module GLL.SimpleRightRecursion  -o SimpleRightRecursion.yrd.fs -abstract true" >> log.txt

..\..\Bin\Release\v40\YC.YaccConstructor.exe -i SimpleRightNull.yrd -c ExpandEbnf -c ExpandMeta ^
        -g "GLLGenerator -pos int -token int -module GLL.SimpleRightNull  -o SimpleRightNull.yrd.fs -abstract true" >> log.txt

..\..\Bin\Release\v40\YC.YaccConstructor.exe -i Calc.yrd -c ExpandEbnf -c ExpandMeta ^
        -g "GLLGenerator -pos int -token int -module GLL.ParseCalc  -o Calc.yrd.fs -abstract true" >> log.txt

..\..\Bin\Release\v40\YC.YaccConstructor.exe -i simpleCalc_with_Nterms.yrd -c ExpandEbnf -c ExpandMeta ^
        -g "GLLGenerator -pos int -token int -module GLL.SimpleCalcWithNTerm  -o simpleCalc_with_Nterms.yrd.fs -abstract true" >> log.txt

..\..\Bin\Release\v40\YC.YaccConstructor.exe -i simpleCalc_with_Nterms_2.yrd -c ExpandEbnf -c ExpandMeta ^
        -g "GLLGenerator -pos int -token int -module GLL.SimpleCalcWithNTerms_2  -o simpleCalc_with_Nterms_2.yrd.fs -abstract true" >> log.txt

..\..\Bin\Release\v40\YC.YaccConstructor.exe -i simpleCalc_with_Nterms_3.yrd -c ExpandEbnf -c ExpandMeta ^
        -g "GLLGenerator -pos int -token int -module GLL.SimpleCalcWithNTerms_3  -o simpleCalc_with_Nterms_3.yrd.fs -abstract true" >> log.txt

..\..\Bin\Release\v40\YC.YaccConstructor.exe -i simpleCalc_with_Nterms_4.yrd -c ExpandEbnf -c ExpandMeta ^
        -g "GLLGenerator -pos int -token int -module GLL.SimpleCalcWithNTerms_4  -o simpleCalc_with_Nterms_4.yrd.fs -abstract true" >> log.txt

..\..\Bin\Release\v40\YC.YaccConstructor.exe -i PrettySimpleCalc.yrd -c ExpandEbnf -c ExpandMeta ^
        -g "GLLGenerator -pos int -token int -module GLL.PrettySimpleCalc -o PrettySimpleCalc.yrd.fs -abstract true" >> log.txt
                                                                                                                                                               
..\..\Bin\Release\v40\YC.YaccConstructor.exe -i NotAmbigousSimpleCalc.yrd -c ExpandEbnf -c ExpandMeta ^
        -g "GLLGenerator -pos int -token int -module GLL.NotAmbigousSimpleCalc -o NotAmbigousSimpleCalc.yrd.fs -abstract true" >> log.txt

..\..\Bin\Release\v40\YC.YaccConstructor.exe -i NotAmbigousSimpleCalcWith2Ops.yrd -c ExpandEbnf -c ExpandMeta ^
        -g "GLLGenerator -pos int -token int -module GLL.NotAmbigousSimpleCalcWith2Ops -o NotAmbigousSimpleCalcWith2Ops.yrd.fs -abstract true" >> log.txt

..\..\Bin\Release\v40\YC.YaccConstructor.exe -i Stars.yrd -c ExpandEbnf -c ExpandMeta ^
        -g "GLLGenerator -pos int -token int -module GLL.Stars -o Stars.yrd.fs -abstract true" >> log.txt

..\..\Bin\Release\v40\YC.YaccConstructor.exe -i Stars2.yrd -c ExpandEbnf -c ExpandMeta ^
        -g "GLLGenerator -pos int -token int -module GLL.Stars2 -o Stars2.yrd.fs -abstract true" >> log.txt

..\..\Bin\Release\v40\YC.YaccConstructor.exe -i Eps.yrd -c ExpandEbnf -c ExpandMeta ^
        -g "GLLGenerator -pos int -token int -module GLL.Eps -o Eps.yrd.fs -abstract true" >> log.txt

..\..\Bin\Release\v40\YC.YaccConstructor.exe -i List.yrd -c ExpandEbnf -c ExpandMeta ^
        -g "GLLGenerator -pos int -token int -module GLL.List -o List.yrd.fs -abstract true" >> _log.txt

..\..\Bin\Release\v40\YC.YaccConstructor.exe -i FirstEps.yrd -c ExpandEbnf -c ExpandMeta ^
        -g "GLLGenerator -pos int -token int -module GLL.FirstEps -o FirstEps.yrd.fs -abstract true" >> log.txt

..\..\Bin\Release\v40\YC.YaccConstructor.exe -i CroppedBrackets.yrd -c ExpandEbnf -c ExpandMeta ^
        -g "GLLGenerator -pos int -token int -module GLL.CroppedBrackets -o CroppedBrackets.yrd.fs -abstract true" >> log.txt

..\..\Bin\Release\v40\YC.YaccConstructor.exe -i Brackets.yrd -c ExpandEbnf -c ExpandMeta ^
        -g "GLLGenerator -pos int -token int -module GLL.Brackets -o Brackets.yrd.fs -abstract true" >> log.txt


..\..\Bin\Release\v40\YC.YaccConstructor.exe -i Brackets2.yrd -c ExpandEbnf -c ExpandMeta ^
        -g "GLLGenerator -pos int -token int -module GLL.Brackets2 -o Brackets2.yrd.fs -abstract true" >> log.txt



..\..\Bin\Release\v40\YC.YaccConstructor.exe -i Brackets2.yrd -c ExpandEbnf -c ExpandMeta ^
        -g "GLLGenerator -pos int -token int -module GLL.Brackets2 -translate false -o Brackets2.yrd.fs -abstract true" >> log.txt


..\..\Bin\Release\v40\YC.YaccConstructor.exe -i StrangeBrackets.yrd -c ExpandEbnf -c ExpandMeta ^
        -g "GLLGenerator -pos int -token int -module GLL.StrangeBrackets -o StrangeBrackets.yrd.fs -abstract true" >> log.txt

for %%i in (Attrs, Cond, Counter, Cycle, Eps2, Epsilon, Expr, First, ListEps, LolCalc, LongCycle, LongCycle_BAD, Longest, Mixed, Omit, Order) do (
    echo . >> log.txt
    echo %%i >> log.txt
    ..\..\Bin\Release\v40\YC.YaccConstructor.exe -i %%i.yrd ^
        -g "GLLGenerator -pos int -token int -abstract true -module GLL.Parse%%i -o %%i.yrd.fs" >> log.txt
)

..\..\Bin\Release\v40\YC.YaccConstructor.exe -i "..\YC.GrammarZOO\SQL\TSQL\mssql_abstract.yrd" -c ExpandEbnf -c Linearize -c ExpandMeta ^
        -g "GLLGenerator -pos int -token int -abstract true -module GLL.MsSqlParser -o MsSqlParser.yrd.fs " >> log.txt

..\..\Bin\Release\v40\YC.YaccConstructor.exe -i GPPerf1.yrd -c ExpandEbnf -c ExpandMeta ^
        -g "GLLGenerator -pos int -token int -module GLL.GPPerf1  -o GPPerf1.yrd.fs -abstract true" >> log.txt

..\..\Bin\Release\v40\YC.YaccConstructor.exe -i GPPerf2.yrd -c ExpandEbnf -c ExpandMeta ^
        -g "GLLGenerator -pos int -token int -module GLL.GPPerf2  -o GPPerf2.yrd.fs -abstract true" >> log.txt
