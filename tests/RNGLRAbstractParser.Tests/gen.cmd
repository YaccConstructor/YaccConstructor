..\..\bin\Release\v40\YC.AbstractLexer.Generator.exe Lexer.fsl -o Lexer.fs

del log.txt

for %%i in (AandB, SimpleCalc, Calc, EpsilonKiller, simpleCalc_with_Nterms, simpleCalc_with_Nterms_2, simpleCalc_with_Nterms_3, simpleCalc_with_Nterms_4, PrettySimpleCalc, NotAmbigousSimpleCalc, NotAmbigousSimpleCalcWith2Ops, Stars, Stars2, Eps, List, FirstEps, CroppedBrackets, Brackets, Brackets1, StrangeBrackets) do (
    echo . >> log.txt
    echo %%i >> log.txt
    ..\..\Bin\Release\v40\YC.YaccConstructor.exe -i %%i.yrd ^
        -g "RNGLRGenerator -pos int -token int -module RNGLR.%%i -translate false -o %%i.yrd.fs -abstract true" >> log.txt
)