if  exist .\workdir\src (
    rd /s /q .\workdir\src )

mkdir .\workdir\src

xcopy /s /i /Y ..\YaccConstructor .\workdir\src

if  exist .\workdir\3rdParty (
    rd /s /q .\workdir\3rdParty )

mkdir .\workdir\3rdParty

xcopy /s /i /Y ..\3rdParty .\workdir\3rdParty


cd scripts

fsi prepare.fsx