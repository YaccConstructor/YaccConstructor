del log.txt

for %%i in (One) do (
		echo . >> log.txt
		echo %%i >> log.txt
    ..\..\Bin\Debug\v40\YC.YaccConstructor.exe -i %%i.yrd -c ExpandMeta ^
        -g "RNGLR.ReadBackGenerator -pos int -token int -module RNGLR.ReadBackParser.%%i -translate true -table LR -o %%i.yrd.fs" >> log.txt
)