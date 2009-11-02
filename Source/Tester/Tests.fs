// Tests.fs
//
// Copyright 2009 Semen Grigorev
//
// This program is free software; you can redistribute it and/or
// modify it under the terms of the GNU General Public License
// as published by the Free Software Foundation.

#light

open Lexeme.Lexeme


let test00 = [{name = "$";value = 1}]
let test0 = [{name = "NUMBER";value = 1};{name = "$";value = 1}]
let test1 = [{name = "NUMBER";value = "1"};{name = "PLUS";value = "+"};{name = "NUMBER";value = "1"};{name = "$";value = "$"}]
let test1_0_1 = [{name = "NUMBER";value = 1};{name = "PLUS";value = 1};{name = "NUMBER";value = 1};{name = "PLUS";value = 1};{name = "NUMBER";value = 1};{name = "$";value = 1}]
let test1_1 = [{name = "NUMBER";value = 1};{name = "MULT";value = 1};{name = "NUMBER";value = 1};{name = "$";value = 1}]
let test1_2 = [{name = "NUMBER";value = 1};{name = "PLUS";value = 1};{name = "NUMBER";value = 1};{name = "PLUS";value = 1};{name = "NUMBER";value = 1};{name = "$";value = 1}]
let test2 = [{name = "NUMBER";value = 1};{name = "MULT";value = 1};{name = "NUMBER";value = 1};{name = "PLUS";value = 1};{name = "NUMBER";value = 1};{name = "$";value = 1}]
let test3 = [{name = "NUMBER";value = 1};{name = "PLUS";value = 1};{name = "NUMBER";value = 1};{name = "MULT";value = 1};{name = "LEFT";value = 1};{name = "NUMBER";value = 1};{name = "PLUS";value = 1};{name = "NUMBER";value = 1};{name = "RIGHT";value = 1};{name = "$";value = 1}]
let test7_1 = [{name = "NUMBER";value = 1};{name = "$";value = 1}]
let test7_2 = [{name = "CHAR";value = 1};{name = "$";value = 1}]
let test7_3 = [{name = "NUMBER";value = 1};{name = "NUMBER";value = 1};{name = "$";value = 1}]
let test7_4 = [{name = "CHAR";value = 1};{name = "CHAR";value = 1};{name = "$";value = 1}]
let test8_1 = [{name = "CHAR";value = 1};{name = "NUMBER";value = 1};{name = "$";value = 1}]
let test6_1 = [{name = "CHAR";value = 1};{name = "PLUS";value = 1};{name = "$";value = 1}]
let test6_2 = [{name = "CHAR";value = 1};{name = "PLUS";value = 1};{name = "MINUS";value = 1};{name = "$";value = 1}]

let test2_w_a = 
    [{name = "NUMBER";value = "2"};{name = "MULT";value = "*"};{name = "NUMBER";value = "3"};{name = "$";value = "$"}]

let test2_w_a_3 = 
    [{name = "NUMBER";value = "2"};{name = "MULT";value = "*"};{name = "NUMBER";value = "3"};{name = "PLUS";value = "+"};{name = "NUMBER";value = "4"};{name = "MULT";value = "*"};{name = "NUMBER";value = "3"};{name = "PLUS";value = "+"};{name = "NUMBER";value = "4"}
    ;{name = "MULT";value = "*"};{name = "NUMBER";value = "3"};{name = "PLUS";value = "+"};{name = "NUMBER";value = "4"};{name = "MULT";value = "*"};{name = "NUMBER";value = "3"};{name = "PLUS";value = "+"};{name = "NUMBER";value = "4"}
    ;{name = "MULT";value = "*"};{name = "NUMBER";value = "3"};{name = "PLUS";value = "+"};{name = "NUMBER";value = "4"};{name = "MULT";value = "*"};{name = "NUMBER";value = "3"};{name = "PLUS";value = "+"};{name = "NUMBER";value = "4"}
    ;{name = "MULT";value = "*"};{name = "NUMBER";value = "3"};{name = "PLUS";value = "+"};{name = "NUMBER";value = "4"};{name = "MULT";value = "*"};{name = "NUMBER";value = "3"};{name = "PLUS";value = "+"};{name = "NUMBER";value = "4"}
    ;{name = "MULT";value = "*"};{name = "NUMBER";value = "3"};{name = "PLUS";value = "+"};{name = "NUMBER";value = "4"};{name = "MULT";value = "*"};{name = "NUMBER";value = "3"};{name = "PLUS";value = "+"};{name = "NUMBER";value = "4"}
    ;{name = "MULT";value = "*"};{name = "NUMBER";value = "3"};{name = "PLUS";value = "+"};{name = "NUMBER";value = "4"};{name = "MULT";value = "*"};{name = "NUMBER";value = "3"};{name = "PLUS";value = "+"};{name = "NUMBER";value = "4"}
    ;{name = "MULT";value = "*"};{name = "NUMBER";value = "3"};{name = "PLUS";value = "+"};{name = "NUMBER";value = "4"};{name = "MULT";value = "*"};{name = "NUMBER";value = "3"};{name = "PLUS";value = "+"};{name = "NUMBER";value = "4"};{name = "$";value = "$"}]