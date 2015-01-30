..\..\Bin\Release\v40\YC.YaccConstructor.exe -i SimpleCalc.yrd -c ExpandEbnf -c ExpandMeta ^
        -g "RNGLRGenerator -pos int -token int -module RNGLR.ParseSimpleCalc -translate true -table LR -o SimpleCalc.yrd.fs -abstract true" >> log.txt

..\..\Bin\Release\v40\YC.YaccConstructor.exe -i Calc.yrd -c ExpandEbnf -c ExpandMeta ^
        -g "RNGLRGenerator -pos int -token int -module RNGLR.ParseCalc -translate true -table LR -o Calc.yrd.fs -abstract true" >> log.txt

..\..\Bin\Release\v40\YC.YaccConstructor.exe -i simpleCalc_with_Nterms.yrd -c ExpandEbnf -c ExpandMeta ^
        -g "RNGLRGenerator -pos int -token int -module RNGLR.SimpleCalcWithNTerm -translate true -table LR -o simpleCalc_with_Nterms.yrd.fs -abstract true" >> log.txt

..\..\Bin\Release\v40\YC.YaccConstructor.exe -i simpleCalc_with_Nterms_2.yrd -c ExpandEbnf -c ExpandMeta ^
        -g "RNGLRGenerator -pos int -token int -module RNGLR.SimpleCalcWithNTerms_2 -translate true -table LR -o simpleCalc_with_Nterms_2.yrd.fs -abstract true" >> log.txt

..\..\Bin\Release\v40\YC.YaccConstructor.exe -i simpleCalc_with_Nterms_3.yrd -c ExpandEbnf -c ExpandMeta ^
        -g "RNGLRGenerator -pos int -token int -module RNGLR.SimpleCalcWithNTerms_3 -translate true -table LR -o simpleCalc_with_Nterms_3.yrd.fs -abstract true" >> log.txt

..\..\Bin\Release\v40\YC.YaccConstructor.exe -i simpleCalc_with_Nterms_4.yrd -c ExpandEbnf -c ExpandMeta ^
        -g "RNGLRGenerator -pos int -token int -module RNGLR.SimpleCalcWithNTerms_4 -translate true -table LR -o simpleCalc_with_Nterms_4.yrd.fs -abstract true" >> log.txt

..\..\Bin\Release\v40\YC.YaccConstructor.exe -i PrettySimpleCalc.yrd -c ExpandEbnf -c ExpandMeta ^
        -g "RNGLRGenerator -pos int -token int -module RNGLR.PrettySimpleCalc -translate false -table LR -o PrettySimpleCalc.yrd.fs -abstract true" >> log.txt

..\..\Bin\Release\v40\YC.YaccConstructor.exe -i NotAmbigousSimpleCalc.yrd -c ExpandEbnf -c ExpandMeta ^
        -g "RNGLRGenerator -pos int -token int -module RNGLR.NotAmbigousSimpleCalc -translate false -table LR -o NotAmbigousSimpleCalc.yrd.fs -abstract true" >> log.txt

..\..\Bin\Release\v40\YC.YaccConstructor.exe -i NotAmbigousSimpleCalcWith2Ops.yrd -c ExpandEbnf -c ExpandMeta ^
        -g "RNGLRGenerator -pos int -token int -module RNGLR.NotAmbigousSimpleCalcWith2Ops -translate false -table LR -o NotAmbigousSimpleCalcWith2Ops.yrd.fs -abstract true" >> log.txt

..\..\Bin\Release\v40\YC.YaccConstructor.exe -i Stars.yrd -c ExpandEbnf -c ExpandMeta ^
        -g "RNGLRGenerator -pos int -token int -module RNGLR.Stars -translate false -table LR -o Stars.yrd.fs -abstract true" >> log.txt

..\..\Bin\Release\v40\YC.YaccConstructor.exe -i Stars2.yrd -c ExpandEbnf -c ExpandMeta ^
        -g "RNGLRGenerator -pos int -token int -module RNGLR.Stars2 -translate false -table LR -o Stars2.yrd.fs -abstract true" >> log.txt

..\..\Bin\Release\v40\YC.YaccConstructor.exe -i Eps.yrd -c ExpandEbnf -c ExpandMeta ^
        -g "RNGLRGenerator -pos int -token int -module RNGLR.Eps -translate false -table LR -o Eps.yrd.fs -abstract true" >> log.txt

..\..\Bin\Release\v40\YC.YaccConstructor.exe -i List.yrd -c ExpandEbnf -c ExpandMeta ^
        -g "RNGLRGenerator -pos int -token int -module RNGLR.List -translate false -table LR -o List.yrd.fs -abstract true" >> _log.txt

..\..\Bin\Release\v40\YC.YaccConstructor.exe -i FirstEps.yrd -c ExpandEbnf -c ExpandMeta ^
        -g "RNGLRGenerator -pos int -token int -module RNGLR.FirstEps -translate false -table LR -o FirstEps.yrd.fs -abstract true" >> log.txt