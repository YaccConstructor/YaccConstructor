
rm log.txt

for i in Epsilon Longest InfEpsilon
do
    echo . >> log.txt
    echo $i >> log.txt
    mono ..\..\Bin\Release\v40\YC.YaccConstructor.exe -i $i.yrd -c ExpandEbnf -c ExpandMeta -g "RNGLRGenerator -pos int -token int -module RNGLR.Parse$i -o $i.yrd.fs" >> log.txt
done

for i in Order Cond Attrs Calc Counter Cycle LongCycle Resolvers LolCalc Omit
do
    echo . >> log.txt
    echo $i >> log.txt
    mono ..\..\Bin\Release\v40\YC.YaccConstructor.exe -i $i.yrd -g "RNGLRGenerator -pos int -token int -module RNGLR.Parse$i -o $i.yrd.fs" >> log.txt
done

for i in ComplexRightNull Expr First List SimpleRightNull
do
    echo . >> log.txt
    echo $i >> log.txt
    mono ..\..\Bin\Release\v40\YC.YaccConstructor.exe -i $i.yrd -g "RNGLRGenerator -pos int -token int -module RNGLR.Parse$i -translate false -o $i.yrd.fs" >> log.txt
done

mono ..\..\Bin\Release\v40\YC.YaccConstructor.exe -i Eps.yrd -c ExpandEbnf -c ExpandMeta -g "RNGLRGenerator -pos int -token int -module RNGLR.Eps -translate false -table LR -o Eps.yrd.fs " >> log.txt

mono ..\..\Bin\Release\v40\YC.YaccConstructor.exe -i Eps2.yrd -c ExpandEbnf -c ExpandMeta -g "RNGLRGenerator -pos int -token int -module RNGLR.Eps2 -translate false -table LR -o Eps2.yrd.fs " >> log.txt

mono ..\..\Bin\Release\v40\YC.YaccConstructor.exe -i ListEps.yrd -c ExpandEbnf -c ExpandMeta -g "RNGLRGenerator -pos int -token int -module RNGLR.ListEps -translate false -table LR -o ListEps.yrd.fs " >> log.txt

mono ..\..\Bin\Release\v40\YC.YaccConstructor.exe -i Brackets.yrd -c ExpandEbnf -c ExpandMeta -g "RNGLRGenerator -pos int -token int -module RNGLR.Brackets -translate false -table LR -o Brackets.yrd.fs " >> log.txt

mono ..\..\Bin\Release\v40\YC.YaccConstructor.exe -i _Brackets.yrd -c ExpandEbnf -c ExpandMeta -g "RNGLRGenerator -pos int -token int -module RNGLR._Brackets -translate false -table LR -o _Brackets.yrd.fs " >> log.txt