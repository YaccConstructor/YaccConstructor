﻿// Test.fs
//
// Copyright 2009 Semen Grigorev
//
// This program is free software; you can redistribute it and/or
// modify it under the terms of the GNU General Public License
// as published by the Free Software Foundation.


#light

module Test
open Yard.Core
open IL.Production
open IL.Rule
open IL
open Grammar.Item


let test_lexem = [PLiteral("b",(1,1));PLiteral("a",(1,1));PLiteral(":",(1,1));PLiteral(";",(1,1));PLiteral("c",(1,1));PLiteral("E",(1,1));PLiteral("T",(1,1));PLiteral("F",(1,1));PLiteral("+",(1,1));PLiteral("*",(1,1));PLiteral("(",(1,1));PLiteral(")",(1,1))]
                 //[PLiteral("NUM",(1,1));PLiteral("MINUS",(1,1));PLiteral("PLUS",(1,1));
                 // PLiteral("DIV",(1,1));PLiteral("POW",(1,1));PLiteral("MULT",(1,1));
                 // PLiteral("LEFT",(1,1));PLiteral("RIGHT",(1,1));PLiteral("+",(1,1));PLiteral("*",(1,1));PLiteral("(",(1,1));PLiteral(")",(1,1))] 
                 
let production1 = PSeq([{omit=false;
                         rule= PToken("E",(1,1));
                         binding = None;
                         checker = None}],None)
let production2 = PSeq([{omit=false;
                         rule= PToken("E",(1,1));
                         binding = None;
                         checker = None};
                         {omit=false;
                         rule= PAlt(PLiteral("+",(1,1)),PLiteral("*",(1,1)));
                         binding = None;
                         checker = None};
                         {omit=false;
                         rule= PToken("E",(1,1));
                         binding = None;
                         checker = None}],None)
let production3 = PSeq([{omit=false;
                         rule= PLiteral("(",(1,1));
                         binding = None;
                         checker = None};
                         {omit=false;
                         rule= PToken("E",(1,1));
                         binding = None;
                         checker = None};
                         {omit=false;
                         rule= PLiteral(")",(1,1));
                         binding = None;
                         checker = None}],None)
let production4 = PSeq([{omit=false;
                         rule= PSome(PAlt(PLiteral("a",(1,1)),PLiteral("b",(1,1))));
                         binding = None;
                         checker = None}],None)
let production5 = PSeq([{omit=false;
                         rule= PAlt(PAlt(production2,production3),production4);
                         binding = None;
                         checker = None}],None) 

let production7 = PSeq([{omit=false;
                         rule= PLiteral("(",(1,1));
                         binding = None;
                         checker = None};
                         {omit=false;
                         rule= PToken("E",(1,1));
                         binding = None;
                         checker = None};
                         {omit=false;
                         rule= PLiteral(")",(1,1));
                         binding = None;
                         checker = None}],None)                         
let production6 = PSeq([{omit=false;
                         rule= PSome(PAlt(PLiteral("a",(1,1)),PLiteral("b",(1,1))));
                         binding = None;
                         checker = None}],None)                                                
                         
let rules = 
    [ {name = "S";
       args = [];
       body = production1;
       _public = true; 
       metaArgs = []};
       {name = "E";
       args = [];
       body = production5;
       _public = true; 
       metaArgs = []}
     ] 



let production1_1 = PSeq([{omit=false;
                         rule= PToken("E",(1,1));
                         binding = None;
                         checker = None}],None)
let production2_1 = PSeq([{omit=false;
                         rule= PToken("E",(1,1));
                         binding = None;
                         checker = None};
                         {omit=false;
                         rule=PLiteral("+",(1,1));
                         binding = None;
                         checker = None};
                         {omit=false;
                         rule= PToken("T",(1,1));
                         binding = None;
                         checker = None}],None)
let production7_1 = PSeq([{omit=false;
                         rule= PToken("T",(1,1));
                         binding = None;
                         checker = None};
                         {omit=false;
                         rule=PLiteral("*",(1,1));
                         binding = None;
                         checker = None};
                         {omit=false;
                         rule= PToken("F",(1,1));
                         binding = None;
                         checker = None}],None)                         
let production3_1 = PSeq([{omit=false;
                         rule= PLiteral("(",(1,1));
                         binding = None;
                         checker = None};
                         {omit=false;
                         rule= PToken("E",(1,1));
                         binding = None;
                         checker = None};
                         {omit=false;
                         rule= PLiteral(")",(1,1));
                         binding = None;
                         checker = None}],None)
let production4_1 = PSeq([{omit=false;
                         rule=PAlt(PLiteral("a",(1,1)),PLiteral("b",(1,1)));
                         binding = None;
                         checker = None}],None)
let production5_1 = PSeq([{omit=false;
                         rule= PToken("T",(1,1));
                         binding = None;
                         checker = None}],None)
                         
let production6_1 = PSeq([{omit=false;
                         rule= PToken("F",(1,1));
                         binding = None;
                         checker = None}],None)                         


let rules_1 = 
    [ {name = "S";
       args = [];
       body = production1;
       _public = true; 
       metaArgs = []};
       {name = "E";
       args = [];
       body = production2_1;
       _public = false; 
       metaArgs = []};
       {name = "E";
       args = [];
       body = production5_1;
       _public = false; 
       metaArgs = []};
       {name = "T";
       args = [];
       body = production6_1;
       _public = false; 
       metaArgs = []};
       {name = "T";
       args = [];
       body = production7_1;
       _public = false; 
       metaArgs = []};
       {name = "F";
       args = [];
       body = production4_1;
       _public = false; 
       metaArgs = []};
       {name = "F";
       args = [];
       body = production3_1;
       _public = false; 
       metaArgs = []}
     ] 


let test_grammar = rules

let test_grammar_2 = rules_1

//aa+b*(b+ba)+b*(b+ba)....
//правильная строка
let test1 = [PLiteral("NUMBER",(1,1));PLiteral("PLUS",(1,1));PLiteral("NUMBER",(1,1));PLiteral("$",(1,1))]  
//[PLiteral("NUMBER",(1,1));PLiteral("$",(1,1))]  
   //[PLiteral("a",(1,1));PLiteral("+",(1,1));PLiteral("b",(1,1));PLiteral("$",(1,1))]  
//неправильная строка
let test2 = [PLiteral("a",(1,1));PLiteral("a",(1,1));PLiteral("+",(1,1));PLiteral("b",(1,1));PLiteral("*",(1,1));PLiteral("b",(1,1));PLiteral("+",(1,1));PLiteral("b",(1,1));PLiteral("a",(1,1));PLiteral(")",(1,1));PLiteral("+",(1,1));PLiteral("b",(1,1));PLiteral("*",(1,1));PLiteral("(",(1,1));PLiteral("b",(1,1));PLiteral("+",(1,1));PLiteral("b",(1,1));PLiteral("a",(1,1));PLiteral(")",(1,1));PLiteral("+",(1,1));PLiteral("b",(1,1));PLiteral("*",(1,1));PLiteral("(",(1,1));PLiteral("b",(1,1));PLiteral("+",(1,1));PLiteral("b",(1,1));PLiteral("a",(1,1));PLiteral(")",(1,1));PLiteral("+",(1,1));PLiteral("b",(1,1));PLiteral("*",(1,1));PLiteral("(",(1,1));PLiteral("b",(1,1));PLiteral("+",(1,1));PLiteral("b",(1,1));PLiteral("a",(1,1));PLiteral(")",(1,1));PLiteral("+",(1,1));PLiteral("b",(1,1));PLiteral("*",(1,1));PLiteral("(",(1,1));PLiteral("b",(1,1));PLiteral("+",(1,1));PLiteral("b",(1,1));PLiteral("a",(1,1));PLiteral(")",(1,1));PLiteral("+",(1,1));PLiteral("b",(1,1));PLiteral("*",(1,1));PLiteral("(",(1,1));PLiteral("b",(1,1));PLiteral("+",(1,1));PLiteral("b",(1,1));PLiteral("a",(1,1));PLiteral(")",(1,1));PLiteral("+",(1,1));PLiteral("b",(1,1));PLiteral("*",(1,1));PLiteral("(",(1,1));PLiteral("b",(1,1));PLiteral("+",(1,1));PLiteral("b",(1,1));PLiteral("a",(1,1));PLiteral(")",(1,1));PLiteral("+",(1,1));PLiteral("b",(1,1));PLiteral("*",(1,1));PLiteral("(",(1,1));PLiteral("b",(1,1));PLiteral("+",(1,1));PLiteral("b",(1,1));PLiteral("b",(1,1));PLiteral(")",(1,1));PLiteral("$",(1,1))]    

let test3 = [PLiteral("a",(1,1));PLiteral("+",(1,1));PLiteral("(",(1,1));PLiteral("b",(1,1));
             PLiteral("+",(1,1));PLiteral("a",(1,1));PLiteral(")",(1,1));PLiteral("+",(1,1));
             PLiteral("(",(1,1));PLiteral("a",(1,1));PLiteral("+",(1,1));PLiteral("a",(1,1));
             PLiteral("+",(1,1));PLiteral("b",(1,1));PLiteral(")",(1,1));PLiteral("+",(1,1));
             PLiteral("a",(1,1));PLiteral("+",(1,1));PLiteral("(",(1,1));PLiteral("a",(1,1));
             PLiteral("+",(1,1));PLiteral("b",(1,1));PLiteral(")",(1,1));PLiteral("+",(1,1));
             PLiteral("(",(1,1));PLiteral("b",(1,1));
             PLiteral("+",(1,1));PLiteral("a",(1,1));PLiteral(")",(1,1));PLiteral("+",(1,1));
             PLiteral("(",(1,1));PLiteral("a",(1,1));PLiteral("+",(1,1));PLiteral("a",(1,1));
             PLiteral("+",(1,1));PLiteral("b",(1,1));PLiteral(")",(1,1));PLiteral("+",(1,1));
             PLiteral("a",(1,1));PLiteral("+",(1,1));PLiteral("(",(1,1));PLiteral("a",(1,1));
             PLiteral("+",(1,1));PLiteral("b",(1,1));PLiteral(")",(1,1));PLiteral("$",(1,1))] 
             


let test4= [PLiteral("(",(1,1));PLiteral("a",(1,1));PLiteral("+",(1,1));PLiteral("b",(1,1));PLiteral(")",(1,1));PLiteral("+",(1,1));PLiteral("a",(1,1));PLiteral("+",(1,1));PLiteral("b",(1,1));PLiteral("$",(1,1))] 