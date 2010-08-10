// Tests.fs
//
// Copyright 2009-2010 Semen Grigorev
//
// This program is free software; you can redistribute it and/or
// modify it under the terms of the GNU General Public License
// as published by the Free Software Foundation.


module Test

open Yard.Generators.RecursiveAscent


//tests for grammar test001.yrd
let test001 = [{name = "NUMBER";value = "1"};{name = "$";value = "1"}]

//tests for grammar test002.yrd
let test002 = [{name = "NUMBER";value = "1"};{name = "PLUS";value = "+"};{name = "NUMBER";value = "2"};{name = "$";value = "$"}]

//tests for grammar test003.yrd
let test003_0 = [{name = "NUMBER";value = "1"};{name = "$";value = "$"}]
let test003_1 = [{name = "LITERAL";value = "go!"};{name = "PLUS";value = "+"};{name = "$";value = "$"}]


//tests for grammar test005.yrd
let test005_0 = [{name = "NUMBER";value = "1"};{name = "PLUS";value = "+"};{name = "NUMBER";value = "2"};{name = "$";value = "$"}]

let test005_1 = 
    [{name = "NUMBER";value = "1"};{name = "PLUS";value = "+"};{name = "NUMBER";value = "2"};{name = "PLUS";value = "+"};{name = "NUMBER";value = "3"};{name = "$";value = "$"}]

let test005_2 = 
    [ {name = "NUMBER";value = "1"};{name = "PLUS";value = "+"};{name = "NUMBER";value = "2"};{name = "PLUS";value = "+"};{name = "NUMBER";value = "3"}
     ;{name = "PLUS";value = "+"};{name = "NUMBER";value = "4"};{name = "PLUS";value = "+"};{name = "NUMBER";value = "5"};{name = "$";value = "$"}]


//tests for grammar test011.yrd
let test011_0 = 
    [{name = "NUMBER";value = "2"};{name = "MULT";value = "*"};{name = "NUMBER";value = "3"};{name = "$";value = "$"}]
    
let test011_1 = 
    [{name = "NUMBER";value = "1"};{name = "PLUS";value = "+"};{name = "NUMBER";value = "2"};{name = "$";value = "$"}]
    
let test011_2 = 
    [{name = "NUMBER";value = "1"};{name = "PLUS";value = "+"};{name = "NUMBER";value = "2"};{name = "PLUS";value = "+"};{name = "NUMBER";value = "3"};{name = "$";value = "$"}]

let test011_3 = 
    [{name = "NUMBER";value = "2"};{name = "MULT";value = "*"};{name = "NUMBER";value = "2"};{name = "MULT";value = "*"};{name = "NUMBER";value = "3"};{name = "$";value = "$"}]
    
let test011_4 = 
    [{name = "NUMBER";value = "2"};{name = "PLUS";value = "+"};{name = "NUMBER";value = "2"};{name = "MULT";value = "*"};{name = "NUMBER";value = "3"};{name = "$";value = "$"}]
    



//tests for grammar test012.yrd
let test012_0 = 
    [{name = "NUMBER";value = "2"};{name = "MULT";value = "*"};{name = "NUMBER";value = "3"};{name = "$";value = "$"}]

let test012_1 = 
    [{name = "NUMBER";value = "2"};{name = "PLUS";value = "+"};{name = "NUMBER";value = "3"};{name = "$";value = "$"}]    

let test012_2 = 
    [{name = "NUMBER";value = "4"};{name = "PLUS";value = "+"};{name = "NUMBER";value = "3"};{name = "PLUS";value = "+"};{name = "NUMBER";value = "3"};{name = "$";value = "$"}]    

let test012_3 = 
    [{name = "NUMBER";value = "1"};{name = "PLUS";value = "+"};{name = "NUMBER";value = "3"};{name = "MULT";value = "*"};{name = "NUMBER";value = "3"};{name = "$";value = "$"}]    

let test012_4 = 
    [{name = "NUMBER";value = "1"};{name = "PLUS";value = "+"};{name = "NUMBER";value = "2"};{name = "MULT";value = "*"};{name = "NUMBER";value = "3"}
    ;{name = "PLUS";value = "+"};{name = "NUMBER";value = "3"};{name = "$";value = "$"}]    

let test012_5 = 
    [{name = "NUMBER";value = "7"};{name = "PLUS";value = "+"};{name = "NUMBER";value = "2"};{name = "MULT";value = "*"};{name = "NUMBER";value = "3"}
    ;{name = "PLUS";value = "+"};{name = "NUMBER";value = "-3"};{name = "$";value = "$"}]    

let test012_6 = 
    [{name = "NUMBER";value = "2"};{name = "MULT";value = "*"};{name = "LEFT";value = "("};{name = "NUMBER";value = "2"};{name = "PLUS";value = "+"}
    ;{name = "NUMBER";value = "3"};{name = "RIGHT";value = ")"};{name = "$";value = "$"}]    

    
let test012_7 = 
    [{name = "NUMBER";value = "2"};{name = "MULT";value = "*"};{name = "NUMBER";value = "3"};{name = "PLUS";value = "+"};{name = "NUMBER";value = "4"};{name = "MULT";value = "*"};{name = "NUMBER";value = "3"};{name = "PLUS";value = "+"};{name = "NUMBER";value = "4"}
    ;{name = "MULT";value = "*"};{name = "NUMBER";value = "3"};{name = "PLUS";value = "+"};{name = "NUMBER";value = "4"};{name = "MULT";value = "*"};{name = "NUMBER";value = "3"};{name = "PLUS";value = "+"};{name = "NUMBER";value = "4"}
    ;{name = "MULT";value = "*"};{name = "NUMBER";value = "3"};{name = "PLUS";value = "+"};{name = "NUMBER";value = "4"};{name = "MULT";value = "*"};{name = "NUMBER";value = "3"};{name = "PLUS";value = "+"};{name = "NUMBER";value = "4"}
    ;{name = "MULT";value = "*"};{name = "NUMBER";value = "3"};{name = "PLUS";value = "+"};{name = "NUMBER";value = "4"};{name = "MULT";value = "*"};{name = "NUMBER";value = "3"};{name = "PLUS";value = "+"};{name = "NUMBER";value = "4"}
    ;{name = "MULT";value = "*"};{name = "NUMBER";value = "3"};{name = "PLUS";value = "+"};{name = "NUMBER";value = "4"};{name = "MULT";value = "*"};{name = "NUMBER";value = "3"};{name = "PLUS";value = "+"};{name = "NUMBER";value = "4"}
    ;{name = "MULT";value = "*"};{name = "NUMBER";value = "3"};{name = "PLUS";value = "+"};{name = "NUMBER";value = "4"};{name = "MULT";value = "*"};{name = "NUMBER";value = "3"};{name = "PLUS";value = "+"};{name = "NUMBER";value = "4"}
    ;{name = "MULT";value = "*"};{name = "NUMBER";value = "3"};{name = "PLUS";value = "+"};{name = "NUMBER";value = "4"};{name = "MULT";value = "*"};{name = "NUMBER";value = "3"};{name = "PLUS";value = "+"};{name = "NUMBER";value = "4"};{name = "$";value = "$"}]


//tests for grammar test014.yrd
let test014_0 = [{name = "NUMBER";value = "5"};{name = "MINUS";value = "-"};{name = "NUMBER";value = "3"};{name = "$";value = "$"}]
let test014_1 = [{name = "NUMBER";value = "1"};{name = "PLUS";value = "+"};{name = "NUMBER";value = "2"};{name = "$";value = "$"}]

//tests for grammar test015.yrd
let test015_0 = [{name = "NUMBER";value = "5"};{name = "$";value = "$"}]
let test015_1 = [{name = "NUMBER";value = "1"};{name = "NUMBER";value = "2"};{name = "$";value = "$"}]

//tests for grammar test016.yrd
let test016_0 = [{name = "NUMBER";value = "1"};{name = "$";value = "$"}]
let test016_1 = [{name = "MINUS";value = "-"};{name = "$";value = "$"}]


//tests for grammar test017.yrd
let test017_0 = [{name = "NUMBER";value = "5"};{name = "$";value = "$"}]
let test017_1 = [{name = "NUMBER";value = "5"};{name = "PLUS";value = "5"};{name = "$";value = "$"}]

//tests for grammar test018.yrd
let test018_0 = [{name = "NUMBER";value = "2"};{name = "MULT";value = "*"};{name = "NUMBER";value = "3"};{name = "$";value = "$"}]
let test018_1 = [{name = "NUMBER";value = "2"};{name = "PLUS";value = "+"};{name = "NUMBER";value = "3"};{name = "$";value = "$"}]  

let test018_2 = 
    [{name = "NUMBER";value = "2"};{name = "MULT";value = "*"};{name = "LEFT";value = "("};{name = "NUMBER";value = "2"};{name = "PLUS";value = "+"}
    ;{name = "NUMBER";value = "3"};{name = "RIGHT";value = ")"};{name = "$";value = "$"}]    

let test018_3 = 
    [{name = "NUMBER";value = "8"};{name = "DIV";value = "/"};{name = "NUMBER";value = "2"};{name = "MULT";value = "*"};{name = "NUMBER";value = "3"}
    ;{name = "MINUS";value = "-"};{name = "NUMBER";value = "2"};{name = "$";value = "$"}]        


let testGen23 = 
    let e = [{name = "$";value = "$"}]
    let l1 = [{name = "LEFT";value = "("}]
    let l2 = [{name = "RIGHT";value = ")"};{name = "MULT";value = "*"};{name = "NUMBER";value = "2"}]
    let hd = [{name = "NUMBER";value = "2"};{name = "PLUS";value = "+"};
              {name = "NUMBER";value = "3"}]
    let rec add lst i = 
        if i = 0
        then (lst@e)
        else (add (l1@lst@l2) (i-1))
    List.map (add hd) [3]   
    
let testGen23_1 = 
    let e = [{name = "$";value = "$"}]
    let l1 = [{name = "LEFT";value = "("}]
    let l2 = [{name = "RIGHT";value = ")"};{name = "MULT";value = "*"};{name = "NUMBER";value = "2"}]
    let hd = [{name = "LEFT";value = "("};{name = "NUMBER";value = "2"};{name = "PLUS";value = "+"};
              {name = "NUMBER";value = "3"};{name = "RIGHT";value = ")"}]
    let rec add lst i = 
        if i = 0
        then (lst@e)
        else (add (l1@lst@l2) (i-1))
    List.map (add hd) [0..100]   
    
let testGen24 = 
    let e = [{name = "$";value = "$"}]
    let l1 = [{name = "LEFT";value = "("}]
    let l2 = [{name = "RIGHT";value = ")"};{name = "MULT";value = "*"};{name = "NUMBER";value = "2"}]
    let hd = [{name = "NUMBER";value = "2"};{name = "PLUS";value = "+"};{name = "NUMBER";value = "3"}]
    let rec add lst i = 
        if i = 0
        then (lst@e)
        else (add (l1@lst@l2) (i-1))
    List.map (add hd) [10]   
    
let test023_0 = [{name = "NUMBER";value = "2"};{name = "$";value = "$"}]     

let test024_b_0 = [{name = "NUMBER";value = "2"};{name = "NUMBER";value = "2"};{name = "$";value = "$"}]
let test024_b_1 = [{name = "NUMBER";value = "2"};{name = "PLUS";value = "+"};{name = "$";value = "$"}]
    
let testGen11 = 
    let e = [{name = "$";value = "$"}]
    let l1 = [{name = "LEFT";value = "("}]
    let l2 = [{name = "PLUS";value = "+"};{name = "NUMBER";value = "2"}]
    let hd = [{name = "NUMBER";value = "2"};{name = "PLUS";value = "+"};
              {name = "NUMBER";value = "3"}]
    let rec add lst i = 
        if i = 0
        then (lst@e)
        else (add (lst@l2) (i-1))
    List.map (add hd) [500]    

let bt = [[{name = "$";value = "$"}]]
    
let badTests = 
    [ [{name = "NUMBER";value = "2"};{name = "PLUS";value = "+"};{name = "NUMBER";value = "3"};{name = "NUMBER";value = "3"};{name = "$";value = "$"}];
      [{name = "NUMBER";value = "2"};{name = "PLUS";value = "+"};{name = "NUMBER";value = "3"};{name = "RIGHT";value = "3"};{name = "$";value = "$"}];
      [{name = "LEFT";value = "3"};{name = "NUMBER";value = "2"};{name = "PLUS";value = "+"};{name = "NUMBER";value = "3"};{name = "$";value = "$"}];
      [{name = "NUMBER";value = "2"};{name = "PLUS";value = "+"};{name = "NUMBER";value = "3"};{name = "MULT";value = "3"};{name = "$";value = "$"}];
      [{name = "NUMBER";value = "2"};{name = "PLUS";value = "+"};{name = "LEFT";value = "3"};{name = "RIGHT";value = "3"};{name = "$";value = "$"}]]    
    
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


    