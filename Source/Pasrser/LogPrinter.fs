#light "off"

module Log

open System


let print_goto x q cl= 
    Console.WriteLine( " GOTO "); 
    Console.Write( "GOTO x : ");
    print_any x ;
    Console.WriteLine( "");
    Console.Write( "GOTO cl : ");
    print_any cl;
    Console.WriteLine( "")
    
let print_climb_1 i x q =  
    Console.WriteLine( " CLIMB "); 
    Console.Write( "Climb i : "); 
    print_any i;
    Console.WriteLine( "");
    Console.Write( "Climb x : "); 
    print_any x;
    Console.WriteLine( "");
    Console.Write( "Climb q : "); 
    print_any q;
    Console.WriteLine( "")
    
let print_climb_2 gt =      
    Console.Write( "Climb gt : ");
    print_any gt;
    Console.WriteLine( "")
     
let print_climb_3 new_q =     
    Console.Write( "Climb new_q : ");
    print_any new_q;
    Console.WriteLine( "")   

let print_parse q i=     
    Console.WriteLine( " PARSER "); 
    Console.Write( "Parser q : "); 
    print_any q;
    Console.WriteLine( "");
    Console.Write( "Parser i : "); 
    print_any i;
    Console.WriteLine( "")
    
let print_item itm s f =    
    Set.iter print_any itm ;
    Console.WriteLine();
    print_any (s,f);
    Console.WriteLine()

let print_items items = 
    Console.WriteLine("Items:");
    Set.iter print_any items;
    Console.WriteLine()
    
let print_goto_c gt y x =
    print_any (y,x) ; 
    print_any " -> ";
    print_any gt
    
let print_autonaton new_states clean_new_automata new_start_state new_finale_state closure_set states =
     Console.WriteLine("new_states:");
     Set.iter print_any new_states;
     Console.WriteLine("new_automata:");
     Set.iter print_any clean_new_automata;
     Console.WriteLine("new_start_state:");
     print_any new_start_state;
     Console.WriteLine("new_finale_state:");
     Set.iter print_any new_finale_state;
     Console.WriteLine("Closure_set:");
     print_any closure_set;
     Console.WriteLine("States:");
     print_any states;
     Console.WriteLine()        


