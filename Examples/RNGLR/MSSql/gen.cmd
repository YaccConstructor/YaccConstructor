echo off
echo "##teamcity[testStarted name='Generate MS-SQL grammar.' captureStandardOutput='true']"

..\..\yc\YaccConstructor.exe -c ExpandMeta -c ExpandEbnf -c "ReplaceLiterals KW_%%s" -c ExpandInnerAlt -c ExpandBrackets -c LeaveLast  ^
     -g "RNGLRGenerator -translate false -token string*(Position*Position) -module Yard.Examples.MSParser -infEpsPath epsilons -o MSParser.fs" -i mssql.yrd > log.txt

set el=%errorlevel%
echo %el%
if "%el%"=="0" (
     echo "##teamcity[testFinished name='Generate MS-SQL grammar.']"
) else (
     echo "##teamcity[testFailed name='Generate MS-SQL grammar.']"
)
exit %el%