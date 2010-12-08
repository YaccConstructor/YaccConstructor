Main\bin\Debug\Main.exe -f YardFrontend -g RACCGenerator -t test_seq.yrd --testpath ..\Tests\RACC\test_seq\
Main\bin\Debug\Main.exe -f YardFrontend -g RACCGenerator -t test_alt.yrd --testpath ..\Tests\RACC\test_alt\
Main\bin\Debug\Main.exe -f YardFrontend -g RACCGenerator -t test_cls.yrd --testpath ..\Tests\RACC\test_cls\
RD /S /Q RACCGeneratedTests
mkdir RACCGeneratedTests
move ..\Tests\RACC\test_seq\*.fs  RACCGeneratedTests/
move ..\Tests\RACC\test_alt\*.fs  RACCGeneratedTests/
move ..\Tests\RACC\test_cls\*.fs  RACCGeneratedTests/
