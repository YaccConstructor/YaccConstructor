..\..\Bin\Release\v40\YC.YaccConstructor.exe -i SimpleCalc.yrd -c ExpandEbnf -c ExpandMeta ^
        -g "GLLGenerator -pos int -token int -module GLL.ParseSimpleCalc -translate true -o SimpleCalc.yrd.fs -abstract true" >> log.txt

..\..\Bin\Release\v40\YC.YaccConstructor.exe -i SimpleBranch.yrd -c ExpandEbnf -c ExpandMeta ^
        -g "GLLGenerator -pos int -token int -module GLL.ParseSimpleBranch -translate true -o SimpleBranch.yrd.fs -abstract true" >> log.txt

..\..\Bin\Release\v40\YC.YaccConstructor.exe -i BadLeftRecursion.yrd -c ExpandEbnf -c ExpandMeta ^
        -g "GLLGenerator -pos int -token int -module GLL.BadLeftRecursion -translate true -o BadLeftRecursion.yrd.fs -abstract true" >> log.txt

..\..\Bin\Release\v40\YC.YaccConstructor.exe -i SimpleAmb.yrd -c ExpandEbnf -c ExpandMeta ^
        -g "GLLGenerator -pos int -token int -module GLL.SimpleAmb -translate true -o SimpleAmb.yrd.fs -abstract true" >> log.txt

..\..\Bin\Release\v40\YC.YaccConstructor.exe -i SimpleLeftRecursion.yrd -c ExpandEbnf -c ExpandMeta ^
        -g "GLLGenerator -pos int -token int -module GLL.SimpleLeftRecursion -translate true -o SimpleLeftRecursion.yrd.fs -abstract true" >> log.txt

..\..\Bin\Release\v40\YC.YaccConstructor.exe -i SimpleRightRecursion.yrd -c ExpandEbnf -c ExpandMeta ^
        -g "GLLGenerator -pos int -token int -module GLL.SimpleRightRecursion -translate true -o SimpleRightRecursion.yrd.fs -abstract true" >> log.txt

..\..\Bin\Release\v40\YC.YaccConstructor.exe -i SimpleRightNull.yrd -c ExpandEbnf -c ExpandMeta ^
        -g "GLLGenerator -pos int -token int -module GLL.SimpleRightNull -translate true -o SimpleRightNull.yrd.fs -abstract true" >> log.txt

..\..\Bin\Release\v40\YC.YaccConstructor.exe -i Calc.yrd -c ExpandEbnf -c ExpandMeta ^
        -g "GLLGenerator -pos int -token int -module GLL.ParseCalc -translate true -o Calc.yrd.fs -abstract true" >> log.txt

..\..\Bin\Release\v40\YC.YaccConstructor.exe -i simpleCalc_with_Nterms.yrd -c ExpandEbnf -c ExpandMeta ^
        -g "GLLGenerator -pos int -token int -module GLL.SimpleCalcWithNTerm -translate true -o simpleCalc_with_Nterms.yrd.fs -abstract true" >> log.txt

..\..\Bin\Release\v40\YC.YaccConstructor.exe -i simpleCalc_with_Nterms_2.yrd -c ExpandEbnf -c ExpandMeta ^
        -g "GLLGenerator -pos int -token int -module GLL.SimpleCalcWithNTerms_2 -translate true -o simpleCalc_with_Nterms_2.yrd.fs -abstract true" >> log.txt

..\..\Bin\Release\v40\YC.YaccConstructor.exe -i simpleCalc_with_Nterms_3.yrd -c ExpandEbnf -c ExpandMeta ^
        -g "GLLGenerator -pos int -token int -module GLL.SimpleCalcWithNTerms_3 -translate true -o simpleCalc_with_Nterms_3.yrd.fs -abstract true" >> log.txt

..\..\Bin\Release\v40\YC.YaccConstructor.exe -i simpleCalc_with_Nterms_4.yrd -c ExpandEbnf -c ExpandMeta ^
        -g "GLLGenerator -pos int -token int -module GLL.SimpleCalcWithNTerms_4 -translate true -o simpleCalc_with_Nterms_4.yrd.fs -abstract true" >> log.txt

..\..\Bin\Release\v40\YC.YaccConstructor.exe -i PrettySimpleCalc.yrd -c ExpandEbnf -c ExpandMeta ^
        -g "GLLGenerator -pos int -token int -module GLL.PrettySimpleCalc -translate false -o PrettySimpleCalc.yrd.fs -abstract true" >> log.txt
                                                                                                                                                               
..\..\Bin\Release\v40\YC.YaccConstructor.exe -i NotAmbigousSimpleCalc.yrd -c ExpandEbnf -c ExpandMeta ^
        -g "GLLGenerator -pos int -token int -module GLL.NotAmbigousSimpleCalc -translate false -o NotAmbigousSimpleCalc.yrd.fs -abstract true" >> log.txt

..\..\Bin\Release\v40\YC.YaccConstructor.exe -i NotAmbigousSimpleCalcWith2Ops.yrd -c ExpandEbnf -c ExpandMeta ^
        -g "GLLGenerator -pos int -token int -module GLL.NotAmbigousSimpleCalcWith2Ops -translate false -o NotAmbigousSimpleCalcWith2Ops.yrd.fs -abstract true" >> log.txt

..\..\Bin\Release\v40\YC.YaccConstructor.exe -i Stars.yrd -c ExpandEbnf -c ExpandMeta ^
        -g "GLLGenerator -pos int -token int -module GLL.Stars -translate false -o Stars.yrd.fs -abstract true" >> log.txt

..\..\Bin\Release\v40\YC.YaccConstructor.exe -i Stars2.yrd -c ExpandEbnf -c ExpandMeta ^
        -g "GLLGenerator -pos int -token int -module GLL.Stars2 -translate false -o Stars2.yrd.fs -abstract true" >> log.txt

..\..\Bin\Release\v40\YC.YaccConstructor.exe -i Eps.yrd -c ExpandEbnf -c ExpandMeta ^
        -g "GLLGenerator -pos int -token int -module GLL.Eps -translate false -o Eps.yrd.fs -abstract true" >> log.txt

..\..\Bin\Release\v40\YC.YaccConstructor.exe -i List.yrd -c ExpandEbnf -c ExpandMeta ^
        -g "GLLGenerator -pos int -token int -module GLL.List -translate false -o List.yrd.fs -abstract true" >> _log.txt

..\..\Bin\Release\v40\YC.YaccConstructor.exe -i FirstEps.yrd -c ExpandEbnf -c ExpandMeta ^
        -g "GLLGenerator -pos int -token int -module GLL.FirstEps -translate false -o FirstEps.yrd.fs -abstract true" >> log.txt

..\..\Bin\Release\v40\YC.YaccConstructor.exe -i CroppedBrackets.yrd -c ExpandEbnf -c ExpandMeta ^
        -g "GLLGenerator -pos int -token int -module GLL.CroppedBrackets -translate false -o CroppedBrackets.yrd.fs -abstract true" >> log.txt

..\..\Bin\Release\v40\YC.YaccConstructor.exe -i Brackets.yrd -c ExpandEbnf -c ExpandMeta ^
        -g "GLLGenerator -pos int -token int -module GLL.Brackets -translate false -o Brackets.yrd.fs -abstract true" >> log.txt

..\..\Bin\Release\v40\YC.YaccConstructor.exe -i StrangeBrackets.yrd -c ExpandEbnf -c ExpandMeta ^
        -g "GLLGenerator -pos int -token int -module GLL.StrangeBrackets -translate false -o StrangeBrackets.yrd.fs -abstract true" >> log.txt

for %%i in (Attrs, Cond, Counter, Cycle, Eps2, Epsilon, Expr, First, ListEps, LolCalc, LongCycle, LongCycle_BAD, Longest, Mixed, Omit, Order) do (
    echo . >> log.txt
    echo %%i >> log.txt
    ..\..\Bin\Release\v40\YC.YaccConstructor.exe -i %%i.yrd ^
        -g "GLLGenerator -pos int -token int -abstract true -module GLL.Parse%%i -o %%i.yrd.fs" >> log.txt
)

..\..\Bin\Release\v40\YC.YaccConstructor.exe -i "..\YC.GrammarZOO\SQL\TSQL\mssql_abstract.yrd" -c ExpandEbnf -c Linearize -c ExpandMeta ^
        -g "GLLGenerator -pos int -token int -abstract true -module GLL.MsSqlParser -o MsSqlParser.yrd.fs " >> log.txt

..\..\Bin\Release\v40\YC.YaccConstructor.exe -i ..\YC.GrammarZOO\Bio\tRNA.yrd -c ExpandRepeat -c ExpandEbnf -c ExpandMeta -g "GLLGenerator -pos int -token int -module GLL.tRNA -o tRNA.yrd.fs -abstract true" >> log.txt
..\..\Bin\Release\v40\YC.YaccConstructor.exe -i ..\YC.GrammarZOO\Bio\tests\shift_problem.yrd  -c ExpandEbnf -c ExpandMeta -g "GLLGenerator -pos int -token int -module GLL.shiftProblem -o shift_problem.yrd.fs -abstract true" >> log.txt
..\..\Bin\Release\v40\YC.YaccConstructor.exe -i ..\YC.GrammarZOO\Bio\tests\multiple_edges_problem.yrd  -c ExpandEbnf -c ExpandMeta -g "GLLGenerator -pos int -token int -module GLL.multipleEdgesProblem -o multiple_edges_problem.yrd.fs -abstract true" >> log.txt
..\..\Bin\Release\v40\YC.YaccConstructor.exe -i ..\YC.GrammarZOO\Bio\tests\very_very_small.yrd  -c ExpandEbnf -c ExpandMeta -g "GLLGenerator -pos int -token int -module GLL.VeryVerySmall -o very_very_small.yrd.fs -abstract true" >> log.txt
..\..\Bin\Release\v40\YC.YaccConstructor.exe -i ..\YC.GrammarZOO\Bio\tests\intersection_small.yrd -c ExpandEbnf -c ExpandMeta -g "GLLGenerator -pos int -token int -module GLL.IntersectionSmall -o intersection_small.yrd.fs -abstract true" >> log.txt