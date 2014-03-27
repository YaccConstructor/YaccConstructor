if  exist yc (
    rd /s /q yc )

mkdir yc

xcopy /Y ..\Bin\Release\v40\*.dll yc
xcopy /Y ..\Bin\Release\v40\*.exe yc
xcopy /Y ..\Bin\Release\v40\*.targets yc

