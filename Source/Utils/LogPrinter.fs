// LogPrinter.fs
//
// Copyright 2009 Semen Grigorev
//
// This program is free software; you can redistribute it and/or
// modify it under the terms of the GNU General Public License
// as published by the Free Software Foundation.

#light 

module Log

open System


let print_goto x q cl= 
    Console.WriteLine( " GOTO "); 
    Console.Write( "GOTO x : ");
    printf "%A " x ;
    Console.WriteLine( "");
    Console.Write( "GOTO cl : ");
    printf "%A " cl;
    Console.WriteLine( "")
    
let print_climb_1 i x q =  
    Console.WriteLine( " CLIMB "); 
    Console.Write( "Climb i : "); 
    printf "%A " i;
    Console.WriteLine( "");
    Console.Write( "Climb x : "); 
    printf "%A " x;
    Console.WriteLine( "");
    Console.Write( "Climb q : "); 
    printf "%A " q;
    Console.WriteLine( "")
    
let print_climb_2 gt =      
    Console.Write( "Climb gt : ");
    printf "%A " gt;
    Console.WriteLine( "")
     
let print_climb_3 new_q =     
    Console.Write( "Climb new_q : ");
    printf "%A " new_q;
    Console.WriteLine( "")   

let print_climb_info i x q gt new_q =
    print_climb_1 i x q;
    print_climb_2 gt;
    print_climb_3 new_q
    
let print_parse q i=     
    printf "\n PARSER \n Parser q : \n %A \n" q;
    printf "Parser i : %A \n" i;
    
let print_item itm s f =    
    Set.iter (printf "%A ") itm ;
    Console.WriteLine();
    printf "%A " (s,f);
    Console.WriteLine()

let print_items items = 
    Console.WriteLine("Items:");
    Set.iter (printf "%A ") items;
    Console.WriteLine()
    
let print_goto_c y x gt=
    printf "%A " (y,x) ; 
    printf "%A " " -> ";
    printf "%A " gt
    
let print_autonaton new_states clean_new_automata new_start_state new_finale_state closure_set states =
     Console.WriteLine("\n new_states:");
     Set.iter (printf "%A ") new_states;
     Console.WriteLine("\n new_automata:");
     Set.iter (printf "%A ") clean_new_automata;
     Console.WriteLine("\n new_start_state:");
     printf "%A " new_start_state;
     Console.WriteLine("\n new_finale_state:");
     Set.iter (printf "%A ") new_finale_state;
     Console.WriteLine("\n Closure_set:");
     printf "%A " closure_set;
     Console.WriteLine("\n States:");
     printf "%A " states;
     Console.WriteLine()   

let print_result (start_time:System.DateTime) end_time result=
    printfn "Result : %A" result;
    printfn "End parsing time: %A Total: %A" end_time (end_time - start_time);
    printfn "End working time: %A Total: %A" System.DateTime.Now (System.DateTime.Now - start_time)             