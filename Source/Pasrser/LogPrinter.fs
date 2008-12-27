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
    Console.WriteLine( "");
    

