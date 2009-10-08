// LogPrinter.fs
//
// Copyright 2009 Semen Grigorev
//
// This program is free software; you can redistribute it and/or
// modify it under the terms of the GNU General Public License
// as published by the Free Software Foundation.

#light 

module Log

let print_goto x q cl = printf "\n GOTO \n GOTO x : %A \n GOTO cl : %A \n" x cl
    
let print_climb_1 i x q = printf "\n CLIMB \n Climb i : %A \n Climb x : %A \n Climb q : %A \n" i x q
    
let print_climb_2 gt = printf "\n Climb gt : \n %A \n" gt
     
let print_climb_3 new_q = printf "\n Climb new_q : \n %A \n" new_q

let print_climb_info i x q gt new_q =
    print_climb_1 i x q;
    print_climb_2 gt;
    print_climb_3 new_q
    
let print_parse q i=     
    printf "\n PARSER \n Parser q : \n %A \n" q
    printf "Parser i : %A \n" i
    
let print_item itm s f =    
    Set.iter (printf "%A ") itm 
    printf "\n %A \n" (s,f)    

let print_items items = 
    printf "\n Items: \n"
    Set.iter (printf "%A ") items
    printf"\n"
    
let print_goto_c y x gt=
    printf "%A " (y,x)  
    printf "%A " " -> "
    printf "%A " gt
    
let print_autonaton new_states clean_new_automata new_start_state new_finale_state closure_set states =
     printf "\n new_states:\n"
     Set.iter (printf "%A ") new_states
     printf "\n new_automata:\n"
     Set.iter (printf "%A ") clean_new_automata
     printf "\n new_start_state: \n %A \n\n new_finale_state:\n %A \n" new_start_state new_finale_state
     printf "\n Closure_set:\n %A \n\n States:\n %A \n" closure_set states
     
let print_result (start_time:System.DateTime) end_time result=
    printfn "Result : %A" result;
    printfn "End parsing time: %A Total: %A" end_time (end_time - start_time);
    printfn "End working time: %A Total: %A" System.DateTime.Now (System.DateTime.Now - start_time)             