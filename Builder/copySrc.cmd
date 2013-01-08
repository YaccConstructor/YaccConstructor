if  exist .\workdir\src (
    rd /s /q .\workdir\src )

mkdir .\workdir\src

xcopy /s /i /Y ..\YaccConstructor .\workdir\src

cd scripts

fsi prepare.fsx