rem ======================================
rem Copy sources

if  exist .\workdir\src (
    rd /s /q .\workdir\src )

mkdir .\workdir\src

xcopy /s /i /Y ..\YaccConstructor .\workdir\src

rem ======================================
rem Copy 3rdParty

if  exist .\workdir\3rdParty (
    rd /s /q .\workdir\3rdParty )

mkdir .\workdir\3rdParty

xcopy /s /i /Y ..\3rdParty .\workdir\3rdParty

rem ======================================
rem Copy exaples

if  exist .\workdir\Examples (
    rd /s /q .\workdir\Examples )

mkdir .\workdir\Examples

xcopy /s /i /Y ..\Examples .\workdir\Examples


rem ======================================
rem Copy grammar templatres

if  exist .\workdir\GrammarTemplates (
    rd /s /q .\workdir\GrammarTemplates )

mkdir .\workdir\GrammarTemplates

xcopy /s /i /Y ..\GrammarTemplates .\workdir\GrammarTemplates

rem ======================================


cd scripts

fsi prepare.fsx