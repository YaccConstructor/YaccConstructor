if  exist yc (
    rd /s /q yc )

mkdir yc

xcopy /Y ..\Bin\Release\v40\*.dll yc
xcopy /Y ..\Bin\Release\v40\*.exe yc
xcopy /Y ..\Bin\Release\v40\*.targets yc

xcopy /Y ..\YaccConstructor\GLLCommon\bin\Release\*.dll yc
xcopy /Y ..\YaccConstructor\GLLParser\bin\Release\*.dll yc
