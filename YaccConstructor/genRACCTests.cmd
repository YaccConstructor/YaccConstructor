Main\bin\Debug\Main.exe -f YardFrontend -g RACCGenerator -t test_seq.yrd --testpath ..\Tests\RACC\test_seq\
Main\bin\Debug\Main.exe -f YardFrontend -g RACCGenerator -t test_alt.yrd --testpath ..\Tests\RACC\test_alt\
Main\bin\Debug\Main.exe -f YardFrontend -g RACCGenerator -t test_cls.yrd --testpath ..\Tests\RACC\test_cls\
Main\bin\Debug\Main.exe -f YardFrontend -g RACCGenerator -t test_alt_in_cls.yrd --testpath ..\Tests\RACC\test_alt_in_cls\
Main\bin\Debug\Main.exe -f YardFrontend -g RACCGenerator -t test_cls_with_tail.yrd --testpath ..\Tests\RACC\test_cls_with_tail\
Main\bin\Debug\Main.exe -f YardFrontend -g RACCGenerator -t test_cls_with_head.yrd --testpath ..\Tests\RACC\test_cls_with_head\

RD /S /Q RACCGeneratedTests
mkdir RACCGeneratedTests
move ..\Tests\RACC\test_seq\*.fs  RACCGeneratedTests/
move ..\Tests\RACC\test_alt\*.fs  RACCGeneratedTests/
move ..\Tests\RACC\test_cls\*.fs  RACCGeneratedTests/
move ..\Tests\RACC\test_alt_in_cls\*.fs  RACCGeneratedTests/
move ..\Tests\RACC\test_cls_with_tail\*.fs  RACCGeneratedTests/
move ..\Tests\RACC\test_cls_with_head\*.fs  RACCGeneratedTests/
