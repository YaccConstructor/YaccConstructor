del log.txt

for %%i in (ComplexRightNull) do (
		echo . >> log.txt
		echo %%i >> log.txt
    ..\..\Bin\Release\v40\YC.YaccConstructor.exe -i %%i.yrd -c ExpandMeta ^
        -g "RNGLR.EBNF.DFAGenerator -pos int -token int -module RNGLR.EBNF.DFAParser%%i -translate true -table LR -o %%i.yrd.fs" >> log.txt
)