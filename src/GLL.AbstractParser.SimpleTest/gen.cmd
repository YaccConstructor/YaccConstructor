..\..\Bin\Release\v40\YC.YaccConstructor.exe -i SimpleCalc.yrd  ^
        -g "GLLFSAGenerator -pos int -token int -module GLLFSA.ParseSimpleCalc -translate true -o SimpleCalc.yrd.fs -abstract true" >> log.txt

..\..\Bin\Release\v40\YC.YaccConstructor.exe -i SimpleBranch.yrd  ^
        -g "GLLFSAGenerator -pos int -token int -module GLLFSA.ParseSimpleBranch -translate true -o SimpleBranch.yrd.fs -abstract true" >> log.txt

..\..\Bin\Release\v40\YC.YaccConstructor.exe -i BadLeftRecursion.yrd  ^
        -g "GLLFSAGenerator -pos int -token int -module GLLFSA.BadLeftRecursion -translate true -o BadLeftRecursion.yrd.fs -abstract true" >> log.txt

..\..\Bin\Release\v40\YC.YaccConstructor.exe -i SimpleAmb.yrd  ^
        -g "GLLFSAGenerator -pos int -token int -module GLLFSA.SimpleAmb -translate true -o SimpleAmb.yrd.fs -abstract true" >> log.txt

..\..\Bin\Release\v40\YC.YaccConstructor.exe -i SimpleLeftRecursion.yrd  ^
        -g "GLLFSAGenerator -pos int -token int -module GLLFSA.SimpleLeftRecursion -translate true -o SimpleLeftRecursion.yrd.fs -abstract true" >> log.txt

..\..\Bin\Release\v40\YC.YaccConstructor.exe -i SimpleRightRecursion.yrd  ^
        -g "GLLFSAGenerator -pos int -token int -module GLLFSA.SimpleRightRecursion -translate true -o SimpleRightRecursion.yrd.fs -abstract true" >> log.txt

..\..\Bin\Release\v40\YC.YaccConstructor.exe -i SimpleRightNull.yrd  ^
        -g "GLLFSAGenerator -pos int -token int -module GLLFSA.SimpleRightNull -translate true -o SimpleRightNull.yrd.fs -abstract true" >> log.txt

..\..\Bin\Release\v40\YC.YaccConstructor.exe -i Calc.yrd  ^
        -g "GLLFSAGenerator -pos int -token int -module GLLFSA.ParseCalc -translate true -o Calc.yrd.fs -abstract true" >> log.txt

..\..\Bin\Release\v40\YC.YaccConstructor.exe -i simpleCalc_with_Nterms.yrd  ^
        -g "GLLFSAGenerator -pos int -token int -module GLLFSA.SimpleCalcWithNTerm -translate true -o simpleCalc_with_Nterms.yrd.fs -abstract true" >> log.txt

..\..\Bin\Release\v40\YC.YaccConstructor.exe -i simpleCalc_with_Nterms_2.yrd  ^
        -g "GLLFSAGenerator -pos int -token int -module GLLFSA.SimpleCalcWithNTerms_2 -translate true -o simpleCalc_with_Nterms_2.yrd.fs -abstract true" >> log.txt

..\..\Bin\Release\v40\YC.YaccConstructor.exe -i simpleCalc_with_Nterms_3.yrd  ^
        -g "GLLFSAGenerator -pos int -token int -module GLLFSA.SimpleCalcWithNTerms_3 -translate true -o simpleCalc_with_Nterms_3.yrd.fs -abstract true" >> log.txt

..\..\Bin\Release\v40\YC.YaccConstructor.exe -i simpleCalc_with_Nterms_4.yrd  ^
        -g "GLLFSAGenerator -pos int -token int -module GLLFSA.SimpleCalcWithNTerms_4 -translate true -o simpleCalc_with_Nterms_4.yrd.fs -abstract true" >> log.txt

..\..\Bin\Release\v40\YC.YaccConstructor.exe -i PrettySimpleCalc.yrd  ^
        -g "GLLFSAGenerator -pos int -token int -module GLLFSA.PrettySimpleCalc -translate false -o PrettySimpleCalc.yrd.fs -abstract true" >> log.txt
                                                                                                                                                               
..\..\Bin\Release\v40\YC.YaccConstructor.exe -i NotAmbigousSimpleCalc.yrd  ^
        -g "GLLFSAGenerator -pos int -token int -module GLLFSA.NotAmbigousSimpleCalc -translate false -o NotAmbigousSimpleCalc.yrd.fs -abstract true" >> log.txt

..\..\Bin\Release\v40\YC.YaccConstructor.exe -i NotAmbigousSimpleCalcWith2Ops.yrd  ^
        -g "GLLFSAGenerator -pos int -token int -module GLLFSA.NotAmbigousSimpleCalcWith2Ops -translate false -o NotAmbigousSimpleCalcWith2Ops.yrd.fs -abstract true" >> log.txt

..\..\Bin\Release\v40\YC.YaccConstructor.exe -i Stars.yrd  ^
        -g "GLLFSAGenerator -pos int -token int -module GLLFSA.Stars -translate false -o Stars.yrd.fs -abstract true" >> log.txt

..\..\Bin\Release\v40\YC.YaccConstructor.exe -i Stars2.yrd  ^
        -g "GLLFSAGenerator -pos int -token int -module GLLFSA.Stars2 -translate false -o Stars2.yrd.fs -abstract true" >> log.txt

..\..\Bin\Release\v40\YC.YaccConstructor.exe -i Eps.yrd  ^
        -g "GLLFSAGenerator -pos int -token int -module GLLFSA.Eps -translate false -o Eps.yrd.fs -abstract true" >> log.txt

..\..\Bin\Release\v40\YC.YaccConstructor.exe -i List.yrd  ^
        -g "GLLFSAGenerator -pos int -token int -module GLLFSA.List -translate false -o List.yrd.fs -abstract true" >> _log.txt

..\..\Bin\Release\v40\YC.YaccConstructor.exe -i FirstEps.yrd  ^
        -g "GLLFSAGenerator -pos int -token int -module GLLFSA.FirstEps -translate false -o FirstEps.yrd.fs -abstract true" >> log.txt

..\..\Bin\Release\v40\YC.YaccConstructor.exe -i CroppedBrackets.yrd  ^
        -g "GLLFSAGenerator -pos int -token int -module GLLFSA.CroppedBrackets -translate false -o CroppedBrackets.yrd.fs -abstract true" >> log.txt

..\..\Bin\Release\v40\YC.YaccConstructor.exe -i Brackets.yrd  ^
        -g "GLLFSAGenerator -pos int -token int -module GLLFSA.Brackets -translate false -o Brackets.yrd.fs -abstract true" >> log.txt

..\..\Bin\Release\v40\YC.YaccConstructor.exe -i StrangeBrackets.yrd  ^
        -g "GLLFSAGenerator -pos int -token int -module GLLFSA.StrangeBrackets -translate false -o StrangeBrackets.yrd.fs -abstract true" >> log.txt

for %%i in (Attrs, Cond, Counter, Cycle, Eps2, Epsilon, Expr, First, ListEps, LolCalc, LongCycle, LongCycle_BAD, Longest, Mixed, Omit, Order) do (
    echo . >> log.txt
    echo %%i >> log.txt
    ..\..\Bin\Release\v40\YC.YaccConstructor.exe -i %%i.yrd ^
        -g "GLLFSAGenerator -pos int -token int -abstract true -module GLLFSA.Parse%%i -o %%i.yrd.fs" >> log.txt
)

..\..\Bin\Release\v40\YC.YaccConstructor.exe -i "..\YC.GrammarZOO\SQL\TSQL\mssql_abstract.yrd" -c ExpandEbnf -c Linearize -c ExpandMeta ^
        -g "GLLFSAGenerator -pos int -token int -abstract true -module GLLFSA.MsSqlParser -o MsSqlParser.yrd.fs " >> log.txt

..\..\Bin\Release\v40\YC.YaccConstructor.exe -i ..\YC.GrammarZOO\Bio\tRNA.yrd -c ExpandRepeat  -g "GLLFSAGenerator -pos int -token int -module GLLFSA.tRNA -o tRNA.yrd.fs -abstract true" >> log.txt
..\..\Bin\Release\v40\YC.YaccConstructor.exe -i ..\YC.GrammarZOO\Bio\tests\shift_problem.yrd   -g "GLLFSAGenerator -pos int -token int -module GLLFSA.shiftProblem -o shift_problem.yrd.fs -abstract true" >> log.txt
..\..\Bin\Release\v40\YC.YaccConstructor.exe -i ..\YC.GrammarZOO\Bio\tests\multiple_edges_problem.yrd   -g "GLLFSAGenerator -pos int -token int -module GLLFSA.multipleEdgesProblem -o multiple_edges_problem.yrd.fs -abstract true" >> log.txt
..\..\Bin\Release\v40\YC.YaccConstructor.exe -i ..\YC.GrammarZOO\Bio\tests\very_very_small.yrd   -g "GLLFSAGenerator -pos int -token int -module GLLFSA.VeryVerySmall -o very_very_small.yrd.fs -abstract true" >> log.txt
..\..\Bin\Release\v40\YC.YaccConstructor.exe -i ..\YC.GrammarZOO\Bio\tests\intersection_small.yrd  -g "GLLFSAGenerator -pos int -token int -module GLLFSA.IntersectionSmall -o intersection_small.yrd.fs -abstract true" >> log.txt
