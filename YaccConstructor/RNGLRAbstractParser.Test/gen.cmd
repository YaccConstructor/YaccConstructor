..\..\bin\Release\v40\YaccConstructor_min_base.exe -i SimpleCalc.yrd -c ExpandEbnf -c ExpandMeta ^
        -g "RNGLRGenerator -pos int -token int -module RNGLR.ParseSimpleCalc -translate true -table LR -o SimpleCalc.yrd.fs" >> log.txt

..\..\bin\Release\v40\YaccConstructor_min_base.exe -i Calc.yrd -c ExpandEbnf -c ExpandMeta ^
        -g "RNGLRGenerator -pos int -token int -module RNGLR.ParseCalc -translate true -table LR -o Calc.yrd.fs" >> log.txt

..\..\bin\Release\v40\YaccConstructor_min_base.exe -i simpleCalc_with_Nterms.yrd -c ExpandEbnf -c ExpandMeta ^
        -g "RNGLRGenerator -pos int -token int -module RNGLR.SimpleCalcWithNTerm -translate true -table LR -o simpleCalc_with_Nterms.yrd.fs" >> log.txt

..\..\bin\Release\v40\YaccConstructor_min_base.exe -i simpleCalc_with_Nterms_2.yrd -c ExpandEbnf -c ExpandMeta ^
        -g "RNGLRGenerator -pos int -token int -module RNGLR.SimpleCalcWithNTerms_2 -translate true -table LR -o simpleCalc_with_Nterms_2.yrd.fs" >> log.txt

..\..\bin\Release\v40\YaccConstructor_min_base.exe -i simpleCalc_with_Nterms_3.yrd -c ExpandEbnf -c ExpandMeta ^
        -g "RNGLRGenerator -pos int -token int -module RNGLR.SimpleCalcWithNTerms_3 -translate true -table LR -o simpleCalc_with_Nterms_3.yrd.fs" >> log.txt

..\..\bin\Release\v40\YaccConstructor_min_base.exe -i simpleCalc_with_Nterms_4.yrd -c ExpandEbnf -c ExpandMeta ^
        -g "RNGLRGenerator -pos int -token int -module RNGLR.SimpleCalcWithNTerms_4 -translate true -table LR -o simpleCalc_with_Nterms_4.yrd.fs" >> log.txt

..\..\bin\Release\v40\YaccConstructor_min_base.exe -i PrettySimpleCalc.yrd -c ExpandEbnf -c ExpandMeta ^
        -g "RNGLRGenerator -pos int -token int -module RNGLR.PrettySimpleCalc -translate false -table LR -o PrettySimpleCalc.yrd.fs" >> log.txt
