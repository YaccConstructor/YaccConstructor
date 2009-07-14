@echo off
echo get new Generator.dll

md logs

if .%1 == .-r (
   xcopy /y /s Generator\bin\Release\Generator.dll Runner\bin\Release\Generator.dll 
) else       (
   if .%1 == .-d (
      xcopy /y /s Generator\bin\Debug\Generator.dll Runner\bin\Debug\Generator.dll
   ) else       (
      echo Incorrect first argument.
   )
)

echo Generating...

if .%1 == .-r (
   if .%2 == .-o (
      Runner\bin\Release\Runner.exe >> logs\generator_trace_r.txt
   ) else      (
      Runner\bin\Release\Runner.exe
   )   
) else       (
   if .%1 == .-d (
      if .%2 == .-o (
         Runner\bin\Debug\Runner.exe >> logs\generator_trace_d.txt
      ) else      (
         Runner\bin\Debug\Runner.exe
      )      
   ) else       ( 
      echo Incorrect first argument.
   )
)

rem pause

echo Copy data files...

if .%1 == .-r (
   xcopy /y  goto.dta  Pasrser\bin\Release\
   xcopy /y  items.dta  Pasrser\bin\Release\
) else       (
   if .%1 == .-d (
      xcopy /y  goto.dta  Pasrser\bin\Debug\
      xcopy /y  items.dta  Pasrser\bin\Debug\
   ) else       ( 
      echo Incorrect first argument.
   )
)

echo Parsing...

if .%1 == .-r (
   if .%2 == .-o (
      Pasrser\bin\Release\pasrser.exe >> logs\parser_result_r.txt
   ) else      (
      Pasrser\bin\Release\pasrser.exe
   )
) else       (
   if .%1 == .-d (
      if .%2 == .-o (
         Pasrser\bin\Debug\pasrser.exe >> logs\parser_result_d.txt
      ) else      (
         Pasrser\bin\Debug\pasrser.exe
      )
   ) else       (
      echo Incorrect first argument.
   )
)

echo Parsing is finished.