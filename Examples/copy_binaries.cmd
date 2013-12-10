if  exist yc (
    rd /s /q yc )

mkdir yc

xcopy /Y ..\Bin\Release\v45\*.dll yc
xcopy /Y ..\Bin\Release\v45\*.exe yc
xcopy /Y ..\Bin\Release\v45\*.targets yc

