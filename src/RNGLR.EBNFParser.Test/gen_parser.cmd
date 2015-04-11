del log.txt

..\..\Bin\Release\v40\YC.YaccConstructor.exe -i .\..\..\..\YC.Abstract.SQL\src\TSQL\mssqlNonAbstract.yrd -c Linearize -g "RNGLR.Generator -table LALR -o RNGLR.sql.yrd.fs"
..\..\Bin\Release\v40\YC.YaccConstructor.exe -i .\..\..\..\YC.Abstract.SQL\src\TSQL\mssqlNonAbstract.yrd -c Linearize -g "RNGLR.EBNFGenerator -table LALR -o RNGLREBNF.sql.yrd.fs"