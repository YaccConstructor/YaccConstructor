cls
set failo=yards\a1.yrd

del /F %failo%.actions.fs
del /F %failo%.regexp.fs
del /F %failo%.tables.fs

..\..\..\YaccConstructor\Main\bin\Debug\YaccConstructor.exe -c ExpandMeta -f YardFrontend -g GNESCCGenerator -i %failo%

IF exist %failo%.actions.fs copy %failo%.actions.fs %failo%\..\..\Actions.fs
IF exist %failo%.tables.fs copy %failo%.tables.fs %failo%\..\..\Tables.fs
IF exist %failo%.regexp.fs copy %failo%.regexp.fs %failo%\..\..\Regexp.fs
