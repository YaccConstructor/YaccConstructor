..\..\..\YaccConstructor\Main\bin\Release\YaccConstructor.exe -c ExpandMeta -c ExpandEbnf -c "ReplaceLiterals KW_%%s" -c ExpandInnerAlt -c ExpandBrackets -c LeaveLast -c AddDefaultAC ^
     -g "RNGLRGenerator -token string -module Yard.Examples.MSParser -o MSParser.fs" -i mssql.yrd > log.txt
