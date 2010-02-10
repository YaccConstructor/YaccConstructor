// Tests.fs
//
// Copyright 2009-2010 Semen Grigorev
//
// This program is free software; you can redistribute it and/or
// modify it under the terms of the GNU General Public License
// as published by the Free Software Foundation.


module Test

open Yard.Core.Lexeme


//tests for grammar test001.yrd
let test001 = [{name = "NUMBER";value = "1"};{name = "$";value = "1"}]

//tests for grammar test002.yrd
let test002 = [{name = "NUMBER";value = "1"};{name = "PLUS";value = "+"};{name = "NUMBER";value = "2"};{name = "$";value = "$"}]

//tests for grammar test003.yrd
let test003_0 = [{name = "NUMBER";value = "1"};{name = "$";value = "$"}]
let test003_1 = [{name = "LITERAL";value = "go!"};{name = "PLUS";value = "+"};{name = "$";value = "$"}]


//tests for grammar test005.yrd
let test005_0 = [{name = "NUMBER";value = "1"};{name = "PLUS";value = "+"};{name = "NUMBER";value = "2"};{name = "$";value = "$"}]
let test005_1 = [{name = "NUMBER";value = "1"};{name = "PLUS";value = "+"};{name = "NUMBER";value = "2"};{name = "PLUS";value = "+"};{name = "NUMBER";value = "3"};{name = "$";value = "$"}]


//tests for grammar test011.yrd
let test011_0 = 
    [{name = "NUMBER";value = "2"};{name = "MULT";value = "*"};{name = "NUMBER";value = "3"};{name = "$";value = "$"}]
let test011_1 = 
  [{name = "NUMBER";value = "1"};{name = "PLUS";value = "+"};{name = "NUMBER";value = "2"};{name = "$";value = "$"}]
let test011_2 = 
  [{name = "NUMBER";value = "1"};{name = "PLUS";value = "+"};{name = "NUMBER";value = "2"};{name = "PLUS";value = "+"};{name = "NUMBER";value = "3"};{name = "$";value = "$"}]

//tests for grammar test012.yrd
let test012_0 = 
  [{name = "NUMBER";value = "2"};{name = "MULT";value = "*"};{name = "NUMBER";value = "3"};{name = "$";value = "$"}]
let test012_1 = 
    [{name = "NUMBER";value = "2"};{name = "MULT";value = "*"};{name = "NUMBER";value = "3"};{name = "PLUS";value = "+"};{name = "NUMBER";value = "4"};{name = "MULT";value = "*"};{name = "NUMBER";value = "3"};{name = "PLUS";value = "+"};{name = "NUMBER";value = "4"}
    ;{name = "MULT";value = "*"};{name = "NUMBER";value = "3"};{name = "PLUS";value = "+"};{name = "NUMBER";value = "4"};{name = "MULT";value = "*"};{name = "NUMBER";value = "3"};{name = "PLUS";value = "+"};{name = "NUMBER";value = "4"}
    ;{name = "MULT";value = "*"};{name = "NUMBER";value = "3"};{name = "PLUS";value = "+"};{name = "NUMBER";value = "4"};{name = "MULT";value = "*"};{name = "NUMBER";value = "3"};{name = "PLUS";value = "+"};{name = "NUMBER";value = "4"}
    ;{name = "MULT";value = "*"};{name = "NUMBER";value = "3"};{name = "PLUS";value = "+"};{name = "NUMBER";value = "4"};{name = "MULT";value = "*"};{name = "NUMBER";value = "3"};{name = "PLUS";value = "+"};{name = "NUMBER";value = "4"}
    ;{name = "MULT";value = "*"};{name = "NUMBER";value = "3"};{name = "PLUS";value = "+"};{name = "NUMBER";value = "4"};{name = "MULT";value = "*"};{name = "NUMBER";value = "3"};{name = "PLUS";value = "+"};{name = "NUMBER";value = "4"}
    ;{name = "MULT";value = "*"};{name = "NUMBER";value = "3"};{name = "PLUS";value = "+"};{name = "NUMBER";value = "4"};{name = "MULT";value = "*"};{name = "NUMBER";value = "3"};{name = "PLUS";value = "+"};{name = "NUMBER";value = "4"}
    ;{name = "MULT";value = "*"};{name = "NUMBER";value = "3"};{name = "PLUS";value = "+"};{name = "NUMBER";value = "4"};{name = "MULT";value = "*"};{name = "NUMBER";value = "3"};{name = "PLUS";value = "+"};{name = "NUMBER";value = "4"};{name = "$";value = "$"}]

//unsorted  

let test0 = [{name = "$";value = 1}]  
let test1_1 = [{name = "NUMBER";value = 1};{name = "MULT";value = 1};{name = "NUMBER";value = 1};{name = "$";value = 1}]
let test1_2 = [{name = "NUMBER";value = 1};{name = "PLUS";value = 1};{name = "NUMBER";value = 1};{name = "PLUS";value = 1};{name = "NUMBER";value = 1};{name = "$";value = 1}]
let test2 = [{name = "NUMBER";value = "1"};{name = "MULT";value = "*"};{name = "NUMBER";value = "2"};{name = "PLUS";value = "+"};{name = "NUMBER";value = "4"};{name = "$";value = "$"}]
let test3 = [{name = "NUMBER";value = 1};{name = "PLUS";value = 1};{name = "NUMBER";value = 1};{name = "MULT";value = 1};{name = "LEFT";value = 1};{name = "NUMBER";value = 1};{name = "PLUS";value = 1};{name = "NUMBER";value = 1};{name = "RIGHT";value = 1};{name = "$";value = 1}]
let test7_1 = [{name = "NUMBER";value = 1};{name = "$";value = 1}]
let test7_2 = [{name = "CHAR";value = 1};{name = "$";value = 1}]
let test7_3 = [{name = "NUMBER";value = 1};{name = "NUMBER";value = 1};{name = "$";value = 1}]
let test7_4 = [{name = "CHAR";value = 1};{name = "CHAR";value = 1};{name = "$";value = 1}]
let test8_1 = [{name = "CHAR";value = 1};{name = "NUMBER";value = 1};{name = "$";value = 1}]
let test6_1 = [{name = "CHAR";value = 1};{name = "PLUS";value = 1};{name = "$";value = 1}]
let test6_2 = [{name = "CHAR";value = 1};{name = "PLUS";value = 1};{name = "MINUS";value = 1};{name = "$";value = 1}]


    