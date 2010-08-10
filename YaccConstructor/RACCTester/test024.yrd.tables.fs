module Tables 

open Yard.Generators.RecursiveAscent.Grammar.Item 
open Yard.Generators.RecursiveAscent

let items =
   set [|
       {
        prod_num       = 0;
        prod_name      = "_yard_start";
        item_num       = 0;
        symb           = Some("s");
        next_num       = Some(1);
        seq_number     = 0;
        s              = 0;
        f              = set [|1|];
        fromStateTrace = (List.ofArray [|((List.ofArray [| Some("s")|]),(List.ofArray [|(List.ofArray [|TSeqS(4);TSmbS(2)|])|]));((List.ofArray [|None|]),(List.ofArray [|(List.ofArray [|TSmbE(2);TSeqE(4)|])|]))|]);
        toStateTrace   = (List.ofArray [|((List.ofArray [| Some("s")|]),(List.ofArray [|(List.ofArray [|TSeqS(4);TSmbS(2)|])|]));((List.ofArray [|None|]),(List.ofArray [|(List.ofArray [|TSmbE(2);TSeqE(4)|])|]))|]);
       };
       {
        prod_num       = 0;
        prod_name      = "_yard_start";
        item_num       = 1;
        symb           = Some("s");
        next_num       = None;
        seq_number     = 0;
        s              = 0;
        f              = set [|1|];
        fromStateTrace = (List.ofArray [|((List.ofArray [| Some("s")|]),(List.ofArray [|(List.ofArray [|TSeqS(4);TSmbS(2)|])|]));((List.ofArray [|None|]),(List.ofArray [|(List.ofArray [|TSmbE(2);TSeqE(4)|])|]))|]);
        toStateTrace   = (List.ofArray [|((List.ofArray [| Some("s")|]),(List.ofArray [|(List.ofArray [|TSeqS(4);TSmbS(2)|])|]));((List.ofArray [|None|]),(List.ofArray [|(List.ofArray [|TSmbE(2);TSeqE(4)|])|]))|]);
       };
       {
        prod_num       = 10;
        prod_name      = "b";
        item_num       = 0;
        symb           = Some("NUMBER");
        next_num       = Some(1);
        seq_number     = 0;
        s              = 0;
        f              = set [|1|];
        fromStateTrace = (List.ofArray [|((List.ofArray [| Some("NUMBER")|]),(List.ofArray [|(List.ofArray [|TSeqS(72);TSmbS(70)|])|]));((List.ofArray [|None|]),(List.ofArray [|(List.ofArray [|TSmbE(70);TSeqE(72)|])|]))|]);
        toStateTrace   = (List.ofArray [|((List.ofArray [| Some("NUMBER")|]),(List.ofArray [|(List.ofArray [|TSeqS(72);TSmbS(70)|])|]));((List.ofArray [|None|]),(List.ofArray [|(List.ofArray [|TSmbE(70);TSeqE(72)|])|]))|]);
       };
       {
        prod_num       = 10;
        prod_name      = "b";
        item_num       = 1;
        symb           = Some("NUMBER");
        next_num       = None;
        seq_number     = 0;
        s              = 0;
        f              = set [|1|];
        fromStateTrace = (List.ofArray [|((List.ofArray [| Some("NUMBER")|]),(List.ofArray [|(List.ofArray [|TSeqS(72);TSmbS(70)|])|]));((List.ofArray [|None|]),(List.ofArray [|(List.ofArray [|TSmbE(70);TSeqE(72)|])|]))|]);
        toStateTrace   = (List.ofArray [|((List.ofArray [| Some("NUMBER")|]),(List.ofArray [|(List.ofArray [|TSeqS(72);TSmbS(70)|])|]));((List.ofArray [|None|]),(List.ofArray [|(List.ofArray [|TSmbE(70);TSeqE(72)|])|]))|]);
       };
       {
        prod_num       = 11;
        prod_name      = "c";
        item_num       = 0;
        symb           = Some("LEFT");
        next_num       = Some(1);
        seq_number     = 0;
        s              = 0;
        f              = set [|3|];
        fromStateTrace = (List.ofArray [|((List.ofArray [| Some("LEFT")|]),(List.ofArray [|(List.ofArray [|TSeqS(86);TSmbS(76)|])|]))|]);
        toStateTrace   = (List.ofArray [|((List.ofArray [| Some("LEFT")|]),(List.ofArray [|(List.ofArray [|TSeqS(86);TSmbS(76)|])|]))|]);
       };
       {
        prod_num       = 11;
        prod_name      = "c";
        item_num       = 1;
        symb           = Some("t");
        next_num       = Some(2);
        seq_number     = 1;
        s              = 0;
        f              = set [|3|];
        fromStateTrace = (List.ofArray [|((List.ofArray [| Some("t")|]),(List.ofArray [|(List.ofArray [|TSmbE(76);TSmbS(80)|])|]))|]);
        toStateTrace   = (List.ofArray [|((List.ofArray [| Some("t")|]),(List.ofArray [|(List.ofArray [|TSmbE(76);TSmbS(80)|])|]))|]);
       };
       {
        prod_num       = 11;
        prod_name      = "c";
        item_num       = 2;
        symb           = Some("RIGHT");
        next_num       = Some(3);
        seq_number     = 2;
        s              = 0;
        f              = set [|3|];
        fromStateTrace = (List.ofArray [|((List.ofArray [| Some("RIGHT")|]),(List.ofArray [|(List.ofArray [|TSmbE(80);TSmbS(84)|])|]));((List.ofArray [|None|]),(List.ofArray [|(List.ofArray [|TSmbE(84);TSeqE(86)|])|]))|]);
        toStateTrace   = (List.ofArray [|((List.ofArray [| Some("RIGHT")|]),(List.ofArray [|(List.ofArray [|TSmbE(80);TSmbS(84)|])|]));((List.ofArray [|None|]),(List.ofArray [|(List.ofArray [|TSmbE(84);TSeqE(86)|])|]))|]);
       };
       {
        prod_num       = 11;
        prod_name      = "c";
        item_num       = 3;
        symb           = Some("RIGHT");
        next_num       = None;
        seq_number     = 2;
        s              = 0;
        f              = set [|3|];
        fromStateTrace = (List.ofArray [|((List.ofArray [| Some("RIGHT")|]),(List.ofArray [|(List.ofArray [|TSmbE(80);TSmbS(84)|])|]));((List.ofArray [|None|]),(List.ofArray [|(List.ofArray [|TSmbE(84);TSeqE(86)|])|]))|]);
        toStateTrace   = (List.ofArray [|((List.ofArray [| Some("RIGHT")|]),(List.ofArray [|(List.ofArray [|TSmbE(80);TSmbS(84)|])|]));((List.ofArray [|None|]),(List.ofArray [|(List.ofArray [|TSmbE(84);TSeqE(86)|])|]))|]);
       };
       {
        prod_num       = 12;
        prod_name      = "c";
        item_num       = 0;
        symb           = Some("LEFT");
        next_num       = Some(1);
        seq_number     = 0;
        s              = 0;
        f              = set [|3|];
        fromStateTrace = (List.ofArray [|((List.ofArray [| Some("LEFT")|]),(List.ofArray [|(List.ofArray [|TSeqS(100);TSmbS(90)|])|]))|]);
        toStateTrace   = (List.ofArray [|((List.ofArray [| Some("LEFT")|]),(List.ofArray [|(List.ofArray [|TSeqS(100);TSmbS(90)|])|]))|]);
       };
       {
        prod_num       = 12;
        prod_name      = "c";
        item_num       = 1;
        symb           = Some("f");
        next_num       = Some(2);
        seq_number     = 1;
        s              = 0;
        f              = set [|3|];
        fromStateTrace = (List.ofArray [|((List.ofArray [| Some("f")|]),(List.ofArray [|(List.ofArray [|TSmbE(90);TSmbS(94)|])|]))|]);
        toStateTrace   = (List.ofArray [|((List.ofArray [| Some("f")|]),(List.ofArray [|(List.ofArray [|TSmbE(90);TSmbS(94)|])|]))|]);
       };
       {
        prod_num       = 12;
        prod_name      = "c";
        item_num       = 2;
        symb           = Some("RIGHT");
        next_num       = Some(3);
        seq_number     = 2;
        s              = 0;
        f              = set [|3|];
        fromStateTrace = (List.ofArray [|((List.ofArray [| Some("RIGHT")|]),(List.ofArray [|(List.ofArray [|TSmbE(94);TSmbS(98)|])|]));((List.ofArray [|None|]),(List.ofArray [|(List.ofArray [|TSmbE(98);TSeqE(100)|])|]))|]);
        toStateTrace   = (List.ofArray [|((List.ofArray [| Some("RIGHT")|]),(List.ofArray [|(List.ofArray [|TSmbE(94);TSmbS(98)|])|]));((List.ofArray [|None|]),(List.ofArray [|(List.ofArray [|TSmbE(98);TSeqE(100)|])|]))|]);
       };
       {
        prod_num       = 12;
        prod_name      = "c";
        item_num       = 3;
        symb           = Some("RIGHT");
        next_num       = None;
        seq_number     = 2;
        s              = 0;
        f              = set [|3|];
        fromStateTrace = (List.ofArray [|((List.ofArray [| Some("RIGHT")|]),(List.ofArray [|(List.ofArray [|TSmbE(94);TSmbS(98)|])|]));((List.ofArray [|None|]),(List.ofArray [|(List.ofArray [|TSmbE(98);TSeqE(100)|])|]))|]);
        toStateTrace   = (List.ofArray [|((List.ofArray [| Some("RIGHT")|]),(List.ofArray [|(List.ofArray [|TSmbE(94);TSmbS(98)|])|]));((List.ofArray [|None|]),(List.ofArray [|(List.ofArray [|TSmbE(98);TSeqE(100)|])|]))|]);
       };
       {
        prod_num       = 13;
        prod_name      = "c";
        item_num       = 0;
        symb           = Some("NUMBER");
        next_num       = Some(1);
        seq_number     = 0;
        s              = 0;
        f              = set [|1|];
        fromStateTrace = (List.ofArray [|((List.ofArray [| Some("NUMBER")|]),(List.ofArray [|(List.ofArray [|TSeqS(106);TSmbS(104)|])|]));((List.ofArray [|None|]),(List.ofArray [|(List.ofArray [|TSmbE(104);TSeqE(106)|])|]))|]);
        toStateTrace   = (List.ofArray [|((List.ofArray [| Some("NUMBER")|]),(List.ofArray [|(List.ofArray [|TSeqS(106);TSmbS(104)|])|]));((List.ofArray [|None|]),(List.ofArray [|(List.ofArray [|TSmbE(104);TSeqE(106)|])|]))|]);
       };
       {
        prod_num       = 13;
        prod_name      = "c";
        item_num       = 1;
        symb           = Some("NUMBER");
        next_num       = None;
        seq_number     = 0;
        s              = 0;
        f              = set [|1|];
        fromStateTrace = (List.ofArray [|((List.ofArray [| Some("NUMBER")|]),(List.ofArray [|(List.ofArray [|TSeqS(106);TSmbS(104)|])|]));((List.ofArray [|None|]),(List.ofArray [|(List.ofArray [|TSmbE(104);TSeqE(106)|])|]))|]);
        toStateTrace   = (List.ofArray [|((List.ofArray [| Some("NUMBER")|]),(List.ofArray [|(List.ofArray [|TSeqS(106);TSmbS(104)|])|]));((List.ofArray [|None|]),(List.ofArray [|(List.ofArray [|TSmbE(104);TSeqE(106)|])|]))|]);
       };
       {
        prod_num       = 14;
        prod_name      = "t";
        item_num       = 0;
        symb           = Some("a");
        next_num       = Some(1);
        seq_number     = 0;
        s              = 0;
        f              = set [|3|];
        fromStateTrace = (List.ofArray [|((List.ofArray [| Some("a")|]),(List.ofArray [|(List.ofArray [|TSeqS(120);TSmbS(110)|])|]))|]);
        toStateTrace   = (List.ofArray [|((List.ofArray [| Some("a")|]),(List.ofArray [|(List.ofArray [|TSeqS(120);TSmbS(110)|])|]))|]);
       };
       {
        prod_num       = 14;
        prod_name      = "t";
        item_num       = 1;
        symb           = Some("PLUS");
        next_num       = Some(2);
        seq_number     = 1;
        s              = 0;
        f              = set [|3|];
        fromStateTrace = (List.ofArray [|((List.ofArray [| Some("PLUS")|]),(List.ofArray [|(List.ofArray [|TSmbE(110);TSmbS(114)|])|]))|]);
        toStateTrace   = (List.ofArray [|((List.ofArray [| Some("PLUS")|]),(List.ofArray [|(List.ofArray [|TSmbE(110);TSmbS(114)|])|]))|]);
       };
       {
        prod_num       = 14;
        prod_name      = "t";
        item_num       = 2;
        symb           = Some("b");
        next_num       = Some(3);
        seq_number     = 2;
        s              = 0;
        f              = set [|3|];
        fromStateTrace = (List.ofArray [|((List.ofArray [| Some("b")|]),(List.ofArray [|(List.ofArray [|TSmbE(114);TSmbS(118)|])|]));((List.ofArray [|None|]),(List.ofArray [|(List.ofArray [|TSmbE(118);TSeqE(120)|])|]))|]);
        toStateTrace   = (List.ofArray [|((List.ofArray [| Some("b")|]),(List.ofArray [|(List.ofArray [|TSmbE(114);TSmbS(118)|])|]));((List.ofArray [|None|]),(List.ofArray [|(List.ofArray [|TSmbE(118);TSeqE(120)|])|]))|]);
       };
       {
        prod_num       = 14;
        prod_name      = "t";
        item_num       = 3;
        symb           = Some("b");
        next_num       = None;
        seq_number     = 2;
        s              = 0;
        f              = set [|3|];
        fromStateTrace = (List.ofArray [|((List.ofArray [| Some("b")|]),(List.ofArray [|(List.ofArray [|TSmbE(114);TSmbS(118)|])|]));((List.ofArray [|None|]),(List.ofArray [|(List.ofArray [|TSmbE(118);TSeqE(120)|])|]))|]);
        toStateTrace   = (List.ofArray [|((List.ofArray [| Some("b")|]),(List.ofArray [|(List.ofArray [|TSmbE(114);TSmbS(118)|])|]));((List.ofArray [|None|]),(List.ofArray [|(List.ofArray [|TSmbE(118);TSeqE(120)|])|]))|]);
       };
       {
        prod_num       = 15;
        prod_name      = "f";
        item_num       = 0;
        symb           = Some("b");
        next_num       = Some(1);
        seq_number     = 0;
        s              = 0;
        f              = set [|3|];
        fromStateTrace = (List.ofArray [|((List.ofArray [| Some("b")|]),(List.ofArray [|(List.ofArray [|TSeqS(134);TSmbS(124)|])|]))|]);
        toStateTrace   = (List.ofArray [|((List.ofArray [| Some("b")|]),(List.ofArray [|(List.ofArray [|TSeqS(134);TSmbS(124)|])|]))|]);
       };
       {
        prod_num       = 15;
        prod_name      = "f";
        item_num       = 1;
        symb           = Some("MULT");
        next_num       = Some(2);
        seq_number     = 1;
        s              = 0;
        f              = set [|3|];
        fromStateTrace = (List.ofArray [|((List.ofArray [| Some("MULT")|]),(List.ofArray [|(List.ofArray [|TSmbE(124);TSmbS(128)|])|]))|]);
        toStateTrace   = (List.ofArray [|((List.ofArray [| Some("MULT")|]),(List.ofArray [|(List.ofArray [|TSmbE(124);TSmbS(128)|])|]))|]);
       };
       {
        prod_num       = 15;
        prod_name      = "f";
        item_num       = 2;
        symb           = Some("c");
        next_num       = Some(3);
        seq_number     = 2;
        s              = 0;
        f              = set [|3|];
        fromStateTrace = (List.ofArray [|((List.ofArray [| Some("c")|]),(List.ofArray [|(List.ofArray [|TSmbE(128);TSmbS(132)|])|]));((List.ofArray [|None|]),(List.ofArray [|(List.ofArray [|TSmbE(132);TSeqE(134)|])|]))|]);
        toStateTrace   = (List.ofArray [|((List.ofArray [| Some("c")|]),(List.ofArray [|(List.ofArray [|TSmbE(128);TSmbS(132)|])|]));((List.ofArray [|None|]),(List.ofArray [|(List.ofArray [|TSmbE(132);TSeqE(134)|])|]))|]);
       };
       {
        prod_num       = 15;
        prod_name      = "f";
        item_num       = 3;
        symb           = Some("c");
        next_num       = None;
        seq_number     = 2;
        s              = 0;
        f              = set [|3|];
        fromStateTrace = (List.ofArray [|((List.ofArray [| Some("c")|]),(List.ofArray [|(List.ofArray [|TSmbE(128);TSmbS(132)|])|]));((List.ofArray [|None|]),(List.ofArray [|(List.ofArray [|TSmbE(132);TSeqE(134)|])|]))|]);
        toStateTrace   = (List.ofArray [|((List.ofArray [| Some("c")|]),(List.ofArray [|(List.ofArray [|TSmbE(128);TSmbS(132)|])|]));((List.ofArray [|None|]),(List.ofArray [|(List.ofArray [|TSmbE(132);TSeqE(134)|])|]))|]);
       };
       {
        prod_num       = 1;
        prod_name      = "s";
        item_num       = 0;
        symb           = Some("e");
        next_num       = Some(1);
        seq_number     = 0;
        s              = 0;
        f              = set [|1|];
        fromStateTrace = (List.ofArray [|((List.ofArray [| Some("e")|]),(List.ofArray [|(List.ofArray [|TSeqS(10);TSmbS(8)|])|]));((List.ofArray [|None|]),(List.ofArray [|(List.ofArray [|TSmbE(8);TSeqE(10)|])|]))|]);
        toStateTrace   = (List.ofArray [|((List.ofArray [| Some("e")|]),(List.ofArray [|(List.ofArray [|TSeqS(10);TSmbS(8)|])|]));((List.ofArray [|None|]),(List.ofArray [|(List.ofArray [|TSmbE(8);TSeqE(10)|])|]))|]);
       };
       {
        prod_num       = 1;
        prod_name      = "s";
        item_num       = 1;
        symb           = Some("e");
        next_num       = None;
        seq_number     = 0;
        s              = 0;
        f              = set [|1|];
        fromStateTrace = (List.ofArray [|((List.ofArray [| Some("e")|]),(List.ofArray [|(List.ofArray [|TSeqS(10);TSmbS(8)|])|]));((List.ofArray [|None|]),(List.ofArray [|(List.ofArray [|TSmbE(8);TSeqE(10)|])|]))|]);
        toStateTrace   = (List.ofArray [|((List.ofArray [| Some("e")|]),(List.ofArray [|(List.ofArray [|TSeqS(10);TSmbS(8)|])|]));((List.ofArray [|None|]),(List.ofArray [|(List.ofArray [|TSmbE(8);TSeqE(10)|])|]))|]);
       };
       {
        prod_num       = 2;
        prod_name      = "e";
        item_num       = 0;
        symb           = Some("t");
        next_num       = Some(1);
        seq_number     = 0;
        s              = 0;
        f              = set [|1|];
        fromStateTrace = (List.ofArray [|((List.ofArray [| Some("t")|]),(List.ofArray [|(List.ofArray [|TSeqS(16);TSmbS(14)|])|]));((List.ofArray [|None|]),(List.ofArray [|(List.ofArray [|TSmbE(14);TSeqE(16)|])|]))|]);
        toStateTrace   = (List.ofArray [|((List.ofArray [| Some("t")|]),(List.ofArray [|(List.ofArray [|TSeqS(16);TSmbS(14)|])|]));((List.ofArray [|None|]),(List.ofArray [|(List.ofArray [|TSmbE(14);TSeqE(16)|])|]))|]);
       };
       {
        prod_num       = 2;
        prod_name      = "e";
        item_num       = 1;
        symb           = Some("t");
        next_num       = None;
        seq_number     = 0;
        s              = 0;
        f              = set [|1|];
        fromStateTrace = (List.ofArray [|((List.ofArray [| Some("t")|]),(List.ofArray [|(List.ofArray [|TSeqS(16);TSmbS(14)|])|]));((List.ofArray [|None|]),(List.ofArray [|(List.ofArray [|TSmbE(14);TSeqE(16)|])|]))|]);
        toStateTrace   = (List.ofArray [|((List.ofArray [| Some("t")|]),(List.ofArray [|(List.ofArray [|TSeqS(16);TSmbS(14)|])|]));((List.ofArray [|None|]),(List.ofArray [|(List.ofArray [|TSmbE(14);TSeqE(16)|])|]))|]);
       };
       {
        prod_num       = 3;
        prod_name      = "e";
        item_num       = 0;
        symb           = Some("f");
        next_num       = Some(1);
        seq_number     = 0;
        s              = 0;
        f              = set [|1|];
        fromStateTrace = (List.ofArray [|((List.ofArray [| Some("f")|]),(List.ofArray [|(List.ofArray [|TSeqS(22);TSmbS(20)|])|]));((List.ofArray [|None|]),(List.ofArray [|(List.ofArray [|TSmbE(20);TSeqE(22)|])|]))|]);
        toStateTrace   = (List.ofArray [|((List.ofArray [| Some("f")|]),(List.ofArray [|(List.ofArray [|TSeqS(22);TSmbS(20)|])|]));((List.ofArray [|None|]),(List.ofArray [|(List.ofArray [|TSmbE(20);TSeqE(22)|])|]))|]);
       };
       {
        prod_num       = 3;
        prod_name      = "e";
        item_num       = 1;
        symb           = Some("f");
        next_num       = None;
        seq_number     = 0;
        s              = 0;
        f              = set [|1|];
        fromStateTrace = (List.ofArray [|((List.ofArray [| Some("f")|]),(List.ofArray [|(List.ofArray [|TSeqS(22);TSmbS(20)|])|]));((List.ofArray [|None|]),(List.ofArray [|(List.ofArray [|TSmbE(20);TSeqE(22)|])|]))|]);
        toStateTrace   = (List.ofArray [|((List.ofArray [| Some("f")|]),(List.ofArray [|(List.ofArray [|TSeqS(22);TSmbS(20)|])|]));((List.ofArray [|None|]),(List.ofArray [|(List.ofArray [|TSmbE(20);TSeqE(22)|])|]))|]);
       };
       {
        prod_num       = 4;
        prod_name      = "e";
        item_num       = 0;
        symb           = Some("NUMBER");
        next_num       = Some(1);
        seq_number     = 0;
        s              = 0;
        f              = set [|1|];
        fromStateTrace = (List.ofArray [|((List.ofArray [| Some("NUMBER")|]),(List.ofArray [|(List.ofArray [|TSeqS(28);TSmbS(26)|])|]));((List.ofArray [|None|]),(List.ofArray [|(List.ofArray [|TSmbE(26);TSeqE(28)|])|]))|]);
        toStateTrace   = (List.ofArray [|((List.ofArray [| Some("NUMBER")|]),(List.ofArray [|(List.ofArray [|TSeqS(28);TSmbS(26)|])|]));((List.ofArray [|None|]),(List.ofArray [|(List.ofArray [|TSmbE(26);TSeqE(28)|])|]))|]);
       };
       {
        prod_num       = 4;
        prod_name      = "e";
        item_num       = 1;
        symb           = Some("NUMBER");
        next_num       = None;
        seq_number     = 0;
        s              = 0;
        f              = set [|1|];
        fromStateTrace = (List.ofArray [|((List.ofArray [| Some("NUMBER")|]),(List.ofArray [|(List.ofArray [|TSeqS(28);TSmbS(26)|])|]));((List.ofArray [|None|]),(List.ofArray [|(List.ofArray [|TSmbE(26);TSeqE(28)|])|]))|]);
        toStateTrace   = (List.ofArray [|((List.ofArray [| Some("NUMBER")|]),(List.ofArray [|(List.ofArray [|TSeqS(28);TSmbS(26)|])|]));((List.ofArray [|None|]),(List.ofArray [|(List.ofArray [|TSmbE(26);TSeqE(28)|])|]))|]);
       };
       {
        prod_num       = 5;
        prod_name      = "a";
        item_num       = 0;
        symb           = Some("t");
        next_num       = Some(1);
        seq_number     = 0;
        s              = 0;
        f              = set [|1|];
        fromStateTrace = (List.ofArray [|((List.ofArray [| Some("t")|]),(List.ofArray [|(List.ofArray [|TSeqS(34);TSmbS(32)|])|]));((List.ofArray [|None|]),(List.ofArray [|(List.ofArray [|TSmbE(32);TSeqE(34)|])|]))|]);
        toStateTrace   = (List.ofArray [|((List.ofArray [| Some("t")|]),(List.ofArray [|(List.ofArray [|TSeqS(34);TSmbS(32)|])|]));((List.ofArray [|None|]),(List.ofArray [|(List.ofArray [|TSmbE(32);TSeqE(34)|])|]))|]);
       };
       {
        prod_num       = 5;
        prod_name      = "a";
        item_num       = 1;
        symb           = Some("t");
        next_num       = None;
        seq_number     = 0;
        s              = 0;
        f              = set [|1|];
        fromStateTrace = (List.ofArray [|((List.ofArray [| Some("t")|]),(List.ofArray [|(List.ofArray [|TSeqS(34);TSmbS(32)|])|]));((List.ofArray [|None|]),(List.ofArray [|(List.ofArray [|TSmbE(32);TSeqE(34)|])|]))|]);
        toStateTrace   = (List.ofArray [|((List.ofArray [| Some("t")|]),(List.ofArray [|(List.ofArray [|TSeqS(34);TSmbS(32)|])|]));((List.ofArray [|None|]),(List.ofArray [|(List.ofArray [|TSmbE(32);TSeqE(34)|])|]))|]);
       };
       {
        prod_num       = 6;
        prod_name      = "a";
        item_num       = 0;
        symb           = Some("f");
        next_num       = Some(1);
        seq_number     = 0;
        s              = 0;
        f              = set [|1|];
        fromStateTrace = (List.ofArray [|((List.ofArray [| Some("f")|]),(List.ofArray [|(List.ofArray [|TSeqS(40);TSmbS(38)|])|]));((List.ofArray [|None|]),(List.ofArray [|(List.ofArray [|TSmbE(38);TSeqE(40)|])|]))|]);
        toStateTrace   = (List.ofArray [|((List.ofArray [| Some("f")|]),(List.ofArray [|(List.ofArray [|TSeqS(40);TSmbS(38)|])|]));((List.ofArray [|None|]),(List.ofArray [|(List.ofArray [|TSmbE(38);TSeqE(40)|])|]))|]);
       };
       {
        prod_num       = 6;
        prod_name      = "a";
        item_num       = 1;
        symb           = Some("f");
        next_num       = None;
        seq_number     = 0;
        s              = 0;
        f              = set [|1|];
        fromStateTrace = (List.ofArray [|((List.ofArray [| Some("f")|]),(List.ofArray [|(List.ofArray [|TSeqS(40);TSmbS(38)|])|]));((List.ofArray [|None|]),(List.ofArray [|(List.ofArray [|TSmbE(38);TSeqE(40)|])|]))|]);
        toStateTrace   = (List.ofArray [|((List.ofArray [| Some("f")|]),(List.ofArray [|(List.ofArray [|TSeqS(40);TSmbS(38)|])|]));((List.ofArray [|None|]),(List.ofArray [|(List.ofArray [|TSmbE(38);TSeqE(40)|])|]))|]);
       };
       {
        prod_num       = 7;
        prod_name      = "a";
        item_num       = 0;
        symb           = Some("NUMBER");
        next_num       = Some(1);
        seq_number     = 0;
        s              = 0;
        f              = set [|1|];
        fromStateTrace = (List.ofArray [|((List.ofArray [| Some("NUMBER")|]),(List.ofArray [|(List.ofArray [|TSeqS(46);TSmbS(44)|])|]));((List.ofArray [|None|]),(List.ofArray [|(List.ofArray [|TSmbE(44);TSeqE(46)|])|]))|]);
        toStateTrace   = (List.ofArray [|((List.ofArray [| Some("NUMBER")|]),(List.ofArray [|(List.ofArray [|TSeqS(46);TSmbS(44)|])|]));((List.ofArray [|None|]),(List.ofArray [|(List.ofArray [|TSmbE(44);TSeqE(46)|])|]))|]);
       };
       {
        prod_num       = 7;
        prod_name      = "a";
        item_num       = 1;
        symb           = Some("NUMBER");
        next_num       = None;
        seq_number     = 0;
        s              = 0;
        f              = set [|1|];
        fromStateTrace = (List.ofArray [|((List.ofArray [| Some("NUMBER")|]),(List.ofArray [|(List.ofArray [|TSeqS(46);TSmbS(44)|])|]));((List.ofArray [|None|]),(List.ofArray [|(List.ofArray [|TSmbE(44);TSeqE(46)|])|]))|]);
        toStateTrace   = (List.ofArray [|((List.ofArray [| Some("NUMBER")|]),(List.ofArray [|(List.ofArray [|TSeqS(46);TSmbS(44)|])|]));((List.ofArray [|None|]),(List.ofArray [|(List.ofArray [|TSmbE(44);TSeqE(46)|])|]))|]);
       };
       {
        prod_num       = 8;
        prod_name      = "b";
        item_num       = 0;
        symb           = Some("LEFT");
        next_num       = Some(1);
        seq_number     = 0;
        s              = 0;
        f              = set [|3|];
        fromStateTrace = (List.ofArray [|((List.ofArray [| Some("LEFT")|]),(List.ofArray [|(List.ofArray [|TSeqS(60);TSmbS(50)|])|]))|]);
        toStateTrace   = (List.ofArray [|((List.ofArray [| Some("LEFT")|]),(List.ofArray [|(List.ofArray [|TSeqS(60);TSmbS(50)|])|]))|]);
       };
       {
        prod_num       = 8;
        prod_name      = "b";
        item_num       = 1;
        symb           = Some("t");
        next_num       = Some(2);
        seq_number     = 1;
        s              = 0;
        f              = set [|3|];
        fromStateTrace = (List.ofArray [|((List.ofArray [| Some("t")|]),(List.ofArray [|(List.ofArray [|TSmbE(50);TSmbS(54)|])|]))|]);
        toStateTrace   = (List.ofArray [|((List.ofArray [| Some("t")|]),(List.ofArray [|(List.ofArray [|TSmbE(50);TSmbS(54)|])|]))|]);
       };
       {
        prod_num       = 8;
        prod_name      = "b";
        item_num       = 2;
        symb           = Some("RIGHT");
        next_num       = Some(3);
        seq_number     = 2;
        s              = 0;
        f              = set [|3|];
        fromStateTrace = (List.ofArray [|((List.ofArray [| Some("RIGHT")|]),(List.ofArray [|(List.ofArray [|TSmbE(54);TSmbS(58)|])|]));((List.ofArray [|None|]),(List.ofArray [|(List.ofArray [|TSmbE(58);TSeqE(60)|])|]))|]);
        toStateTrace   = (List.ofArray [|((List.ofArray [| Some("RIGHT")|]),(List.ofArray [|(List.ofArray [|TSmbE(54);TSmbS(58)|])|]));((List.ofArray [|None|]),(List.ofArray [|(List.ofArray [|TSmbE(58);TSeqE(60)|])|]))|]);
       };
       {
        prod_num       = 8;
        prod_name      = "b";
        item_num       = 3;
        symb           = Some("RIGHT");
        next_num       = None;
        seq_number     = 2;
        s              = 0;
        f              = set [|3|];
        fromStateTrace = (List.ofArray [|((List.ofArray [| Some("RIGHT")|]),(List.ofArray [|(List.ofArray [|TSmbE(54);TSmbS(58)|])|]));((List.ofArray [|None|]),(List.ofArray [|(List.ofArray [|TSmbE(58);TSeqE(60)|])|]))|]);
        toStateTrace   = (List.ofArray [|((List.ofArray [| Some("RIGHT")|]),(List.ofArray [|(List.ofArray [|TSmbE(54);TSmbS(58)|])|]));((List.ofArray [|None|]),(List.ofArray [|(List.ofArray [|TSmbE(58);TSeqE(60)|])|]))|]);
       };
       {
        prod_num       = 9;
        prod_name      = "b";
        item_num       = 0;
        symb           = Some("f");
        next_num       = Some(1);
        seq_number     = 0;
        s              = 0;
        f              = set [|1|];
        fromStateTrace = (List.ofArray [|((List.ofArray [| Some("f")|]),(List.ofArray [|(List.ofArray [|TSeqS(66);TSmbS(64)|])|]));((List.ofArray [|None|]),(List.ofArray [|(List.ofArray [|TSmbE(64);TSeqE(66)|])|]))|]);
        toStateTrace   = (List.ofArray [|((List.ofArray [| Some("f")|]),(List.ofArray [|(List.ofArray [|TSeqS(66);TSmbS(64)|])|]));((List.ofArray [|None|]),(List.ofArray [|(List.ofArray [|TSmbE(64);TSeqE(66)|])|]))|]);
       };
       {
        prod_num       = 9;
        prod_name      = "b";
        item_num       = 1;
        symb           = Some("f");
        next_num       = None;
        seq_number     = 0;
        s              = 0;
        f              = set [|1|];
        fromStateTrace = (List.ofArray [|((List.ofArray [| Some("f")|]),(List.ofArray [|(List.ofArray [|TSeqS(66);TSmbS(64)|])|]));((List.ofArray [|None|]),(List.ofArray [|(List.ofArray [|TSmbE(64);TSeqE(66)|])|]))|]);
        toStateTrace   = (List.ofArray [|((List.ofArray [| Some("f")|]),(List.ofArray [|(List.ofArray [|TSeqS(66);TSmbS(64)|])|]));((List.ofArray [|None|]),(List.ofArray [|(List.ofArray [|TSmbE(64);TSeqE(66)|])|]))|]);
       }|]

let gotoSet =
   dict <| (List.ofArray [|(-683466676,set [|
       {
        prod_num       = 8;
        prod_name      = "b";
        item_num       = 1;
        symb           = Some("t");
        next_num       = Some(2);
        seq_number     = 1;
        s              = 0;
        f              = set [|3|];
        fromStateTrace = (List.ofArray [|((List.ofArray [| Some("t")|]),(List.ofArray [|(List.ofArray [|TSmbE(50);TSmbS(54)|])|]))|]);
        toStateTrace   = (List.ofArray [|((List.ofArray [| Some("t")|]),(List.ofArray [|(List.ofArray [|TSmbE(50);TSmbS(54)|])|]))|]);
       }|]);(1618418999,set [|
       {
        prod_num       = 8;
        prod_name      = "b";
        item_num       = 1;
        symb           = Some("t");
        next_num       = Some(2);
        seq_number     = 1;
        s              = 0;
        f              = set [|3|];
        fromStateTrace = (List.ofArray [|((List.ofArray [| Some("t")|]),(List.ofArray [|(List.ofArray [|TSmbE(50);TSmbS(54)|])|]))|]);
        toStateTrace   = (List.ofArray [|((List.ofArray [| Some("t")|]),(List.ofArray [|(List.ofArray [|TSmbE(50);TSmbS(54)|])|]))|]);
       }|]);(-683448865,set [|
       {
        prod_num       = 8;
        prod_name      = "b";
        item_num       = 1;
        symb           = Some("t");
        next_num       = Some(2);
        seq_number     = 1;
        s              = 0;
        f              = set [|3|];
        fromStateTrace = (List.ofArray [|((List.ofArray [| Some("t")|]),(List.ofArray [|(List.ofArray [|TSmbE(50);TSmbS(54)|])|]))|]);
        toStateTrace   = (List.ofArray [|((List.ofArray [| Some("t")|]),(List.ofArray [|(List.ofArray [|TSmbE(50);TSmbS(54)|])|]))|]);
       }|]);(1618431012,set [|
       {
        prod_num       = 8;
        prod_name      = "b";
        item_num       = 1;
        symb           = Some("t");
        next_num       = Some(2);
        seq_number     = 1;
        s              = 0;
        f              = set [|3|];
        fromStateTrace = (List.ofArray [|((List.ofArray [| Some("t")|]),(List.ofArray [|(List.ofArray [|TSmbE(50);TSmbS(54)|])|]))|]);
        toStateTrace   = (List.ofArray [|((List.ofArray [| Some("t")|]),(List.ofArray [|(List.ofArray [|TSmbE(50);TSmbS(54)|])|]))|]);
       }|]);(-683615219,set [|
       {
        prod_num       = 8;
        prod_name      = "b";
        item_num       = 1;
        symb           = Some("t");
        next_num       = Some(2);
        seq_number     = 1;
        s              = 0;
        f              = set [|3|];
        fromStateTrace = (List.ofArray [|((List.ofArray [| Some("t")|]),(List.ofArray [|(List.ofArray [|TSmbE(50);TSmbS(54)|])|]))|]);
        toStateTrace   = (List.ofArray [|((List.ofArray [| Some("t")|]),(List.ofArray [|(List.ofArray [|TSmbE(50);TSmbS(54)|])|]))|]);
       }|]);(1618352438,set [|
       {
        prod_num       = 8;
        prod_name      = "b";
        item_num       = 1;
        symb           = Some("t");
        next_num       = Some(2);
        seq_number     = 1;
        s              = 0;
        f              = set [|3|];
        fromStateTrace = (List.ofArray [|((List.ofArray [| Some("t")|]),(List.ofArray [|(List.ofArray [|TSmbE(50);TSmbS(54)|])|]))|]);
        toStateTrace   = (List.ofArray [|((List.ofArray [| Some("t")|]),(List.ofArray [|(List.ofArray [|TSmbE(50);TSmbS(54)|])|]))|]);
       }|]);(-683646758,set [|
       {
        prod_num       = 8;
        prod_name      = "b";
        item_num       = 1;
        symb           = Some("t");
        next_num       = Some(2);
        seq_number     = 1;
        s              = 0;
        f              = set [|3|];
        fromStateTrace = (List.ofArray [|((List.ofArray [| Some("t")|]),(List.ofArray [|(List.ofArray [|TSmbE(50);TSmbS(54)|])|]))|]);
        toStateTrace   = (List.ofArray [|((List.ofArray [| Some("t")|]),(List.ofArray [|(List.ofArray [|TSmbE(50);TSmbS(54)|])|]))|]);
       }|]);(1618368609,set [|
       {
        prod_num       = 8;
        prod_name      = "b";
        item_num       = 1;
        symb           = Some("t");
        next_num       = Some(2);
        seq_number     = 1;
        s              = 0;
        f              = set [|3|];
        fromStateTrace = (List.ofArray [|((List.ofArray [| Some("t")|]),(List.ofArray [|(List.ofArray [|TSmbE(50);TSmbS(54)|])|]))|]);
        toStateTrace   = (List.ofArray [|((List.ofArray [| Some("t")|]),(List.ofArray [|(List.ofArray [|TSmbE(50);TSmbS(54)|])|]))|]);
       }|]);(-116846325,set [||]);(1589551920,set [||]);(-679657302,set [|
       {
        prod_num       = 8;
        prod_name      = "b";
        item_num       = 1;
        symb           = Some("t");
        next_num       = Some(2);
        seq_number     = 1;
        s              = 0;
        f              = set [|3|];
        fromStateTrace = (List.ofArray [|((List.ofArray [| Some("t")|]),(List.ofArray [|(List.ofArray [|TSmbE(50);TSmbS(54)|])|]))|]);
        toStateTrace   = (List.ofArray [|((List.ofArray [| Some("t")|]),(List.ofArray [|(List.ofArray [|TSmbE(50);TSmbS(54)|])|]))|]);
       }|]);(1618658769,set [|
       {
        prod_num       = 8;
        prod_name      = "b";
        item_num       = 1;
        symb           = Some("t");
        next_num       = Some(2);
        seq_number     = 1;
        s              = 0;
        f              = set [|3|];
        fromStateTrace = (List.ofArray [|((List.ofArray [| Some("t")|]),(List.ofArray [|(List.ofArray [|TSmbE(50);TSmbS(54)|])|]))|]);
        toStateTrace   = (List.ofArray [|((List.ofArray [| Some("t")|]),(List.ofArray [|(List.ofArray [|TSmbE(50);TSmbS(54)|])|]))|]);
       }|]);(-683494145,set [|
       {
        prod_num       = 8;
        prod_name      = "b";
        item_num       = 1;
        symb           = Some("t");
        next_num       = Some(2);
        seq_number     = 1;
        s              = 0;
        f              = set [|3|];
        fromStateTrace = (List.ofArray [|((List.ofArray [| Some("t")|]),(List.ofArray [|(List.ofArray [|TSmbE(50);TSmbS(54)|])|]))|]);
        toStateTrace   = (List.ofArray [|((List.ofArray [| Some("t")|]),(List.ofArray [|(List.ofArray [|TSmbE(50);TSmbS(54)|])|]))|]);
       }|]);(1618217604,set [|
       {
        prod_num       = 8;
        prod_name      = "b";
        item_num       = 1;
        symb           = Some("t");
        next_num       = Some(2);
        seq_number     = 1;
        s              = 0;
        f              = set [|3|];
        fromStateTrace = (List.ofArray [|((List.ofArray [| Some("t")|]),(List.ofArray [|(List.ofArray [|TSmbE(50);TSmbS(54)|])|]))|]);
        toStateTrace   = (List.ofArray [|((List.ofArray [| Some("t")|]),(List.ofArray [|(List.ofArray [|TSmbE(50);TSmbS(54)|])|]))|]);
       }|]);(-116812504,set [||]);(1589632979,set [||]);(-2094361813,set [|
       {
        prod_num       = 8;
        prod_name      = "b";
        item_num       = 1;
        symb           = Some("t");
        next_num       = Some(2);
        seq_number     = 1;
        s              = 0;
        f              = set [|3|];
        fromStateTrace = (List.ofArray [|((List.ofArray [| Some("t")|]),(List.ofArray [|(List.ofArray [|TSmbE(50);TSmbS(54)|])|]))|]);
        toStateTrace   = (List.ofArray [|((List.ofArray [| Some("t")|]),(List.ofArray [|(List.ofArray [|TSmbE(50);TSmbS(54)|])|]))|]);
       }|]);(-683161049,set [|
       {
        prod_num       = 8;
        prod_name      = "b";
        item_num       = 1;
        symb           = Some("t");
        next_num       = Some(2);
        seq_number     = 1;
        s              = 0;
        f              = set [|3|];
        fromStateTrace = (List.ofArray [|((List.ofArray [| Some("t")|]),(List.ofArray [|(List.ofArray [|TSmbE(50);TSmbS(54)|])|]))|]);
        toStateTrace   = (List.ofArray [|((List.ofArray [| Some("t")|]),(List.ofArray [|(List.ofArray [|TSmbE(50);TSmbS(54)|])|]))|]);
       }|]);(-382976770,set [||]);(-1366573821,set [||]);(-683374320,set [|
       {
        prod_num       = 8;
        prod_name      = "b";
        item_num       = 1;
        symb           = Some("t");
        next_num       = Some(2);
        seq_number     = 1;
        s              = 0;
        f              = set [|3|];
        fromStateTrace = (List.ofArray [|((List.ofArray [| Some("t")|]),(List.ofArray [|(List.ofArray [|TSmbE(50);TSmbS(54)|])|]))|]);
        toStateTrace   = (List.ofArray [|((List.ofArray [| Some("t")|]),(List.ofArray [|(List.ofArray [|TSmbE(50);TSmbS(54)|])|]))|]);
       }|]);(1617999659,set [|
       {
        prod_num       = 8;
        prod_name      = "b";
        item_num       = 1;
        symb           = Some("t");
        next_num       = Some(2);
        seq_number     = 1;
        s              = 0;
        f              = set [|3|];
        fromStateTrace = (List.ofArray [|((List.ofArray [| Some("t")|]),(List.ofArray [|(List.ofArray [|TSmbE(50);TSmbS(54)|])|]))|]);
        toStateTrace   = (List.ofArray [|((List.ofArray [| Some("t")|]),(List.ofArray [|(List.ofArray [|TSmbE(50);TSmbS(54)|])|]))|]);
       }|]);(-116675387,set [||]);(1589479806,set [||]);(-2094700728,set [|
       {
        prod_num       = 11;
        prod_name      = "c";
        item_num       = 1;
        symb           = Some("t");
        next_num       = Some(2);
        seq_number     = 1;
        s              = 0;
        f              = set [|3|];
        fromStateTrace = (List.ofArray [|((List.ofArray [| Some("t")|]),(List.ofArray [|(List.ofArray [|TSmbE(76);TSmbS(80)|])|]))|]);
        toStateTrace   = (List.ofArray [|((List.ofArray [| Some("t")|]),(List.ofArray [|(List.ofArray [|TSmbE(76);TSmbS(80)|])|]))|]);
       }|]);(-683392764,set [|
       {
        prod_num       = 8;
        prod_name      = "b";
        item_num       = 1;
        symb           = Some("t");
        next_num       = Some(2);
        seq_number     = 1;
        s              = 0;
        f              = set [|3|];
        fromStateTrace = (List.ofArray [|((List.ofArray [| Some("t")|]),(List.ofArray [|(List.ofArray [|TSmbE(50);TSmbS(54)|])|]))|]);
        toStateTrace   = (List.ofArray [|((List.ofArray [| Some("t")|]),(List.ofArray [|(List.ofArray [|TSmbE(50);TSmbS(54)|])|]))|]);
       }|]);(-382751907,set [||]);(-1366621728,set [||]);(-2094480977,set [|
       {
        prod_num       = 12;
        prod_name      = "c";
        item_num       = 1;
        symb           = Some("f");
        next_num       = Some(2);
        seq_number     = 1;
        s              = 0;
        f              = set [|3|];
        fromStateTrace = (List.ofArray [|((List.ofArray [| Some("f")|]),(List.ofArray [|(List.ofArray [|TSmbE(90);TSmbS(94)|])|]))|]);
        toStateTrace   = (List.ofArray [|((List.ofArray [| Some("f")|]),(List.ofArray [|(List.ofArray [|TSmbE(90);TSmbS(94)|])|]))|]);
       }|]);(-683550251,set [|
       {
        prod_num       = 8;
        prod_name      = "b";
        item_num       = 1;
        symb           = Some("t");
        next_num       = Some(2);
        seq_number     = 1;
        s              = 0;
        f              = set [|3|];
        fromStateTrace = (List.ofArray [|((List.ofArray [| Some("t")|]),(List.ofArray [|(List.ofArray [|TSmbE(50);TSmbS(54)|])|]))|]);
        toStateTrace   = (List.ofArray [|((List.ofArray [| Some("t")|]),(List.ofArray [|(List.ofArray [|TSmbE(50);TSmbS(54)|])|]))|]);
       }|]);(-382571398,set [||]);(-1367061113,set [||]);(-116722974,set [||]);(1585596953,set [||]);(-683268356,set [|
       {
        prod_num       = 8;
        prod_name      = "b";
        item_num       = 1;
        symb           = Some("t");
        next_num       = Some(2);
        seq_number     = 1;
        s              = 0;
        f              = set [|3|];
        fromStateTrace = (List.ofArray [|((List.ofArray [| Some("t")|]),(List.ofArray [|(List.ofArray [|TSmbE(50);TSmbS(54)|])|]))|]);
        toStateTrace   = (List.ofArray [|((List.ofArray [| Some("t")|]),(List.ofArray [|(List.ofArray [|TSmbE(50);TSmbS(54)|])|]))|]);
       }|]);(1074596622,set [||]);(-683225673,set [|
       {
        prod_num       = 8;
        prod_name      = "b";
        item_num       = 1;
        symb           = Some("t");
        next_num       = Some(2);
        seq_number     = 1;
        s              = 0;
        f              = set [|3|];
        fromStateTrace = (List.ofArray [|((List.ofArray [| Some("t")|]),(List.ofArray [|(List.ofArray [|TSmbE(50);TSmbS(54)|])|]))|]);
        toStateTrace   = (List.ofArray [|((List.ofArray [| Some("t")|]),(List.ofArray [|(List.ofArray [|TSmbE(50);TSmbS(54)|])|]))|]);
       }|]);(1617981834,set [|
       {
        prod_num       = 8;
        prod_name      = "b";
        item_num       = 1;
        symb           = Some("t");
        next_num       = Some(2);
        seq_number     = 1;
        s              = 0;
        f              = set [|3|];
        fromStateTrace = (List.ofArray [|((List.ofArray [| Some("t")|]),(List.ofArray [|(List.ofArray [|TSmbE(50);TSmbS(54)|])|]))|]);
        toStateTrace   = (List.ofArray [|((List.ofArray [| Some("t")|]),(List.ofArray [|(List.ofArray [|TSmbE(50);TSmbS(54)|])|]))|]);
       }|]);(-683171758,set [|
       {
        prod_num       = 8;
        prod_name      = "b";
        item_num       = 1;
        symb           = Some("t");
        next_num       = Some(2);
        seq_number     = 1;
        s              = 0;
        f              = set [|3|];
        fromStateTrace = (List.ofArray [|((List.ofArray [| Some("t")|]),(List.ofArray [|(List.ofArray [|TSmbE(50);TSmbS(54)|])|]))|]);
        toStateTrace   = (List.ofArray [|((List.ofArray [| Some("t")|]),(List.ofArray [|(List.ofArray [|TSmbE(50);TSmbS(54)|])|]))|]);
       }|]);(1384758239,set [||]);(-683258793,set [|
       {
        prod_num       = 11;
        prod_name      = "c";
        item_num       = 1;
        symb           = Some("t");
        next_num       = Some(2);
        seq_number     = 1;
        s              = 0;
        f              = set [|3|];
        fromStateTrace = (List.ofArray [|((List.ofArray [| Some("t")|]),(List.ofArray [|(List.ofArray [|TSmbE(76);TSmbS(80)|])|]))|]);
        toStateTrace   = (List.ofArray [|((List.ofArray [| Some("t")|]),(List.ofArray [|(List.ofArray [|TSmbE(76);TSmbS(80)|])|]))|]);
       };
       {
        prod_num       = 12;
        prod_name      = "c";
        item_num       = 1;
        symb           = Some("f");
        next_num       = Some(2);
        seq_number     = 1;
        s              = 0;
        f              = set [|3|];
        fromStateTrace = (List.ofArray [|((List.ofArray [| Some("f")|]),(List.ofArray [|(List.ofArray [|TSmbE(90);TSmbS(94)|])|]))|]);
        toStateTrace   = (List.ofArray [|((List.ofArray [| Some("f")|]),(List.ofArray [|(List.ofArray [|TSmbE(90);TSmbS(94)|])|]))|]);
       }|]);(1618057450,set [|
       {
        prod_num       = 11;
        prod_name      = "c";
        item_num       = 1;
        symb           = Some("t");
        next_num       = Some(2);
        seq_number     = 1;
        s              = 0;
        f              = set [|3|];
        fromStateTrace = (List.ofArray [|((List.ofArray [| Some("t")|]),(List.ofArray [|(List.ofArray [|TSmbE(76);TSmbS(80)|])|]))|]);
        toStateTrace   = (List.ofArray [|((List.ofArray [| Some("t")|]),(List.ofArray [|(List.ofArray [|TSmbE(76);TSmbS(80)|])|]))|]);
       };
       {
        prod_num       = 12;
        prod_name      = "c";
        item_num       = 1;
        symb           = Some("f");
        next_num       = Some(2);
        seq_number     = 1;
        s              = 0;
        f              = set [|3|];
        fromStateTrace = (List.ofArray [|((List.ofArray [| Some("f")|]),(List.ofArray [|(List.ofArray [|TSmbE(90);TSmbS(94)|])|]))|]);
        toStateTrace   = (List.ofArray [|((List.ofArray [| Some("f")|]),(List.ofArray [|(List.ofArray [|TSmbE(90);TSmbS(94)|])|]))|]);
       }|]);(2026477149,set [||]);(-805459162,set [||]);(2026492878,set [||]);(-805438923,set [||]);(2026531356,set [||]);(-805523673,set [||]);(2026559179,set [||]);(-805507472,set [||]);(1451468570,set [||]);(-248244959,set [||]);(2029516475,set [||]);(-806325312,set [||]);(2026373870,set [||]);(-805389163,set [||]);(1451434809,set [||]);(-248157758,set [||]);(748692794,set [||]);(2026732598,set [||]);(1185319663,set [||]);(16845586,set [||]);(2026782465,set [||]);(-805699270,set [||]);(1451301588,set [||]);(-248037521,set [||]);(749719897,set [||]);(2026829589,set [||]);(1185226060,set [||]);(16889841,set [||]);(748844990,set [||]);(2026598340,set [||]);(1186519659,set [||]);(17759126,set [||]);(1451246835,set [||]);(-251069432,set [||]);(2026672365,set [||]);(-276316897,set [||]);(2026666918,set [||]);(-805648485,set [||]);(2026706499,set [||]);(-50131506,set [|
       {
        prod_num       = 15;
        prod_name      = "f";
        item_num       = 2;
        symb           = Some("c");
        next_num       = Some(3);
        seq_number     = 2;
        s              = 0;
        f              = set [|3|];
        fromStateTrace = (List.ofArray [|((List.ofArray [| Some("c")|]),(List.ofArray [|(List.ofArray [|TSmbE(128);TSmbS(132)|])|]));((List.ofArray [|None|]),(List.ofArray [|(List.ofArray [|TSmbE(132);TSeqE(134)|])|]))|]);
        toStateTrace   = (List.ofArray [|((List.ofArray [| Some("c")|]),(List.ofArray [|(List.ofArray [|TSmbE(128);TSmbS(132)|])|]));((List.ofArray [|None|]),(List.ofArray [|(List.ofArray [|TSmbE(132);TSeqE(134)|])|]))|]);
       }|]);(2026699334,set [||]);(-805622021,set [||]);(-1181498128,set [|
       {
        prod_num       = 10;
        prod_name      = "b";
        item_num       = 1;
        symb           = Some("NUMBER");
        next_num       = None;
        seq_number     = 0;
        s              = 0;
        f              = set [|1|];
        fromStateTrace = (List.ofArray [|((List.ofArray [| Some("NUMBER")|]),(List.ofArray [|(List.ofArray [|TSeqS(72);TSmbS(70)|])|]));((List.ofArray [|None|]),(List.ofArray [|(List.ofArray [|TSmbE(70);TSeqE(72)|])|]))|]);
        toStateTrace   = (List.ofArray [|((List.ofArray [| Some("NUMBER")|]),(List.ofArray [|(List.ofArray [|TSeqS(72);TSmbS(70)|])|]));((List.ofArray [|None|]),(List.ofArray [|(List.ofArray [|TSmbE(70);TSeqE(72)|])|]))|]);
       };
       {
        prod_num       = 4;
        prod_name      = "e";
        item_num       = 1;
        symb           = Some("NUMBER");
        next_num       = None;
        seq_number     = 0;
        s              = 0;
        f              = set [|1|];
        fromStateTrace = (List.ofArray [|((List.ofArray [| Some("NUMBER")|]),(List.ofArray [|(List.ofArray [|TSeqS(28);TSmbS(26)|])|]));((List.ofArray [|None|]),(List.ofArray [|(List.ofArray [|TSmbE(26);TSeqE(28)|])|]))|]);
        toStateTrace   = (List.ofArray [|((List.ofArray [| Some("NUMBER")|]),(List.ofArray [|(List.ofArray [|TSeqS(28);TSmbS(26)|])|]));((List.ofArray [|None|]),(List.ofArray [|(List.ofArray [|TSmbE(26);TSeqE(28)|])|]))|]);
       };
       {
        prod_num       = 7;
        prod_name      = "a";
        item_num       = 1;
        symb           = Some("NUMBER");
        next_num       = None;
        seq_number     = 0;
        s              = 0;
        f              = set [|1|];
        fromStateTrace = (List.ofArray [|((List.ofArray [| Some("NUMBER")|]),(List.ofArray [|(List.ofArray [|TSeqS(46);TSmbS(44)|])|]));((List.ofArray [|None|]),(List.ofArray [|(List.ofArray [|TSmbE(44);TSeqE(46)|])|]))|]);
        toStateTrace   = (List.ofArray [|((List.ofArray [| Some("NUMBER")|]),(List.ofArray [|(List.ofArray [|TSeqS(46);TSmbS(44)|])|]));((List.ofArray [|None|]),(List.ofArray [|(List.ofArray [|TSmbE(44);TSeqE(46)|])|]))|]);
       }|]);(245889419,set [|
       {
        prod_num       = 10;
        prod_name      = "b";
        item_num       = 1;
        symb           = Some("NUMBER");
        next_num       = None;
        seq_number     = 0;
        s              = 0;
        f              = set [|1|];
        fromStateTrace = (List.ofArray [|((List.ofArray [| Some("NUMBER")|]),(List.ofArray [|(List.ofArray [|TSeqS(72);TSmbS(70)|])|]));((List.ofArray [|None|]),(List.ofArray [|(List.ofArray [|TSmbE(70);TSeqE(72)|])|]))|]);
        toStateTrace   = (List.ofArray [|((List.ofArray [| Some("NUMBER")|]),(List.ofArray [|(List.ofArray [|TSeqS(72);TSmbS(70)|])|]));((List.ofArray [|None|]),(List.ofArray [|(List.ofArray [|TSmbE(70);TSeqE(72)|])|]))|]);
       };
       {
        prod_num       = 4;
        prod_name      = "e";
        item_num       = 1;
        symb           = Some("NUMBER");
        next_num       = None;
        seq_number     = 0;
        s              = 0;
        f              = set [|1|];
        fromStateTrace = (List.ofArray [|((List.ofArray [| Some("NUMBER")|]),(List.ofArray [|(List.ofArray [|TSeqS(28);TSmbS(26)|])|]));((List.ofArray [|None|]),(List.ofArray [|(List.ofArray [|TSmbE(26);TSeqE(28)|])|]))|]);
        toStateTrace   = (List.ofArray [|((List.ofArray [| Some("NUMBER")|]),(List.ofArray [|(List.ofArray [|TSeqS(28);TSmbS(26)|])|]));((List.ofArray [|None|]),(List.ofArray [|(List.ofArray [|TSmbE(26);TSeqE(28)|])|]))|]);
       };
       {
        prod_num       = 7;
        prod_name      = "a";
        item_num       = 1;
        symb           = Some("NUMBER");
        next_num       = None;
        seq_number     = 0;
        s              = 0;
        f              = set [|1|];
        fromStateTrace = (List.ofArray [|((List.ofArray [| Some("NUMBER")|]),(List.ofArray [|(List.ofArray [|TSeqS(46);TSmbS(44)|])|]));((List.ofArray [|None|]),(List.ofArray [|(List.ofArray [|TSmbE(44);TSeqE(46)|])|]))|]);
        toStateTrace   = (List.ofArray [|((List.ofArray [| Some("NUMBER")|]),(List.ofArray [|(List.ofArray [|TSeqS(46);TSmbS(44)|])|]));((List.ofArray [|None|]),(List.ofArray [|(List.ofArray [|TSmbE(44);TSeqE(46)|])|]))|]);
       }|]);(-1181515421,set [|
       {
        prod_num       = 10;
        prod_name      = "b";
        item_num       = 1;
        symb           = Some("NUMBER");
        next_num       = None;
        seq_number     = 0;
        s              = 0;
        f              = set [|1|];
        fromStateTrace = (List.ofArray [|((List.ofArray [| Some("NUMBER")|]),(List.ofArray [|(List.ofArray [|TSeqS(72);TSmbS(70)|])|]));((List.ofArray [|None|]),(List.ofArray [|(List.ofArray [|TSmbE(70);TSeqE(72)|])|]))|]);
        toStateTrace   = (List.ofArray [|((List.ofArray [| Some("NUMBER")|]),(List.ofArray [|(List.ofArray [|TSeqS(72);TSmbS(70)|])|]));((List.ofArray [|None|]),(List.ofArray [|(List.ofArray [|TSmbE(70);TSeqE(72)|])|]))|]);
       };
       {
        prod_num       = 4;
        prod_name      = "e";
        item_num       = 1;
        symb           = Some("NUMBER");
        next_num       = None;
        seq_number     = 0;
        s              = 0;
        f              = set [|1|];
        fromStateTrace = (List.ofArray [|((List.ofArray [| Some("NUMBER")|]),(List.ofArray [|(List.ofArray [|TSeqS(28);TSmbS(26)|])|]));((List.ofArray [|None|]),(List.ofArray [|(List.ofArray [|TSmbE(26);TSeqE(28)|])|]))|]);
        toStateTrace   = (List.ofArray [|((List.ofArray [| Some("NUMBER")|]),(List.ofArray [|(List.ofArray [|TSeqS(28);TSmbS(26)|])|]));((List.ofArray [|None|]),(List.ofArray [|(List.ofArray [|TSmbE(26);TSeqE(28)|])|]))|]);
       };
       {
        prod_num       = 7;
        prod_name      = "a";
        item_num       = 1;
        symb           = Some("NUMBER");
        next_num       = None;
        seq_number     = 0;
        s              = 0;
        f              = set [|1|];
        fromStateTrace = (List.ofArray [|((List.ofArray [| Some("NUMBER")|]),(List.ofArray [|(List.ofArray [|TSeqS(46);TSmbS(44)|])|]));((List.ofArray [|None|]),(List.ofArray [|(List.ofArray [|TSmbE(44);TSeqE(46)|])|]))|]);
        toStateTrace   = (List.ofArray [|((List.ofArray [| Some("NUMBER")|]),(List.ofArray [|(List.ofArray [|TSeqS(46);TSmbS(44)|])|]));((List.ofArray [|None|]),(List.ofArray [|(List.ofArray [|TSmbE(44);TSeqE(46)|])|]))|]);
       }|]);(245868696,set [|
       {
        prod_num       = 10;
        prod_name      = "b";
        item_num       = 1;
        symb           = Some("NUMBER");
        next_num       = None;
        seq_number     = 0;
        s              = 0;
        f              = set [|1|];
        fromStateTrace = (List.ofArray [|((List.ofArray [| Some("NUMBER")|]),(List.ofArray [|(List.ofArray [|TSeqS(72);TSmbS(70)|])|]));((List.ofArray [|None|]),(List.ofArray [|(List.ofArray [|TSmbE(70);TSeqE(72)|])|]))|]);
        toStateTrace   = (List.ofArray [|((List.ofArray [| Some("NUMBER")|]),(List.ofArray [|(List.ofArray [|TSeqS(72);TSmbS(70)|])|]));((List.ofArray [|None|]),(List.ofArray [|(List.ofArray [|TSmbE(70);TSeqE(72)|])|]))|]);
       };
       {
        prod_num       = 4;
        prod_name      = "e";
        item_num       = 1;
        symb           = Some("NUMBER");
        next_num       = None;
        seq_number     = 0;
        s              = 0;
        f              = set [|1|];
        fromStateTrace = (List.ofArray [|((List.ofArray [| Some("NUMBER")|]),(List.ofArray [|(List.ofArray [|TSeqS(28);TSmbS(26)|])|]));((List.ofArray [|None|]),(List.ofArray [|(List.ofArray [|TSmbE(26);TSeqE(28)|])|]))|]);
        toStateTrace   = (List.ofArray [|((List.ofArray [| Some("NUMBER")|]),(List.ofArray [|(List.ofArray [|TSeqS(28);TSmbS(26)|])|]));((List.ofArray [|None|]),(List.ofArray [|(List.ofArray [|TSmbE(26);TSeqE(28)|])|]))|]);
       };
       {
        prod_num       = 7;
        prod_name      = "a";
        item_num       = 1;
        symb           = Some("NUMBER");
        next_num       = None;
        seq_number     = 0;
        s              = 0;
        f              = set [|1|];
        fromStateTrace = (List.ofArray [|((List.ofArray [| Some("NUMBER")|]),(List.ofArray [|(List.ofArray [|TSeqS(46);TSmbS(44)|])|]));((List.ofArray [|None|]),(List.ofArray [|(List.ofArray [|TSmbE(44);TSeqE(46)|])|]))|]);
        toStateTrace   = (List.ofArray [|((List.ofArray [| Some("NUMBER")|]),(List.ofArray [|(List.ofArray [|TSeqS(46);TSmbS(44)|])|]));((List.ofArray [|None|]),(List.ofArray [|(List.ofArray [|TSmbE(44);TSeqE(46)|])|]))|]);
       }|]);(-1181730639,set [|
       {
        prod_num       = 10;
        prod_name      = "b";
        item_num       = 1;
        symb           = Some("NUMBER");
        next_num       = None;
        seq_number     = 0;
        s              = 0;
        f              = set [|1|];
        fromStateTrace = (List.ofArray [|((List.ofArray [| Some("NUMBER")|]),(List.ofArray [|(List.ofArray [|TSeqS(72);TSmbS(70)|])|]));((List.ofArray [|None|]),(List.ofArray [|(List.ofArray [|TSmbE(70);TSeqE(72)|])|]))|]);
        toStateTrace   = (List.ofArray [|((List.ofArray [| Some("NUMBER")|]),(List.ofArray [|(List.ofArray [|TSeqS(72);TSmbS(70)|])|]));((List.ofArray [|None|]),(List.ofArray [|(List.ofArray [|TSmbE(70);TSeqE(72)|])|]))|]);
       };
       {
        prod_num       = 7;
        prod_name      = "a";
        item_num       = 1;
        symb           = Some("NUMBER");
        next_num       = None;
        seq_number     = 0;
        s              = 0;
        f              = set [|1|];
        fromStateTrace = (List.ofArray [|((List.ofArray [| Some("NUMBER")|]),(List.ofArray [|(List.ofArray [|TSeqS(46);TSmbS(44)|])|]));((List.ofArray [|None|]),(List.ofArray [|(List.ofArray [|TSmbE(44);TSeqE(46)|])|]))|]);
        toStateTrace   = (List.ofArray [|((List.ofArray [| Some("NUMBER")|]),(List.ofArray [|(List.ofArray [|TSeqS(46);TSmbS(44)|])|]));((List.ofArray [|None|]),(List.ofArray [|(List.ofArray [|TSmbE(44);TSeqE(46)|])|]))|]);
       }|]);(245824906,set [|
       {
        prod_num       = 10;
        prod_name      = "b";
        item_num       = 1;
        symb           = Some("NUMBER");
        next_num       = None;
        seq_number     = 0;
        s              = 0;
        f              = set [|1|];
        fromStateTrace = (List.ofArray [|((List.ofArray [| Some("NUMBER")|]),(List.ofArray [|(List.ofArray [|TSeqS(72);TSmbS(70)|])|]));((List.ofArray [|None|]),(List.ofArray [|(List.ofArray [|TSmbE(70);TSeqE(72)|])|]))|]);
        toStateTrace   = (List.ofArray [|((List.ofArray [| Some("NUMBER")|]),(List.ofArray [|(List.ofArray [|TSeqS(72);TSmbS(70)|])|]));((List.ofArray [|None|]),(List.ofArray [|(List.ofArray [|TSmbE(70);TSeqE(72)|])|]))|]);
       };
       {
        prod_num       = 7;
        prod_name      = "a";
        item_num       = 1;
        symb           = Some("NUMBER");
        next_num       = None;
        seq_number     = 0;
        s              = 0;
        f              = set [|1|];
        fromStateTrace = (List.ofArray [|((List.ofArray [| Some("NUMBER")|]),(List.ofArray [|(List.ofArray [|TSeqS(46);TSmbS(44)|])|]));((List.ofArray [|None|]),(List.ofArray [|(List.ofArray [|TSmbE(44);TSeqE(46)|])|]))|]);
        toStateTrace   = (List.ofArray [|((List.ofArray [| Some("NUMBER")|]),(List.ofArray [|(List.ofArray [|TSeqS(46);TSmbS(44)|])|]));((List.ofArray [|None|]),(List.ofArray [|(List.ofArray [|TSmbE(44);TSeqE(46)|])|]))|]);
       }|]);(-1181711258,set [|
       {
        prod_num       = 10;
        prod_name      = "b";
        item_num       = 1;
        symb           = Some("NUMBER");
        next_num       = None;
        seq_number     = 0;
        s              = 0;
        f              = set [|1|];
        fromStateTrace = (List.ofArray [|((List.ofArray [| Some("NUMBER")|]),(List.ofArray [|(List.ofArray [|TSeqS(72);TSmbS(70)|])|]));((List.ofArray [|None|]),(List.ofArray [|(List.ofArray [|TSmbE(70);TSeqE(72)|])|]))|]);
        toStateTrace   = (List.ofArray [|((List.ofArray [| Some("NUMBER")|]),(List.ofArray [|(List.ofArray [|TSeqS(72);TSmbS(70)|])|]));((List.ofArray [|None|]),(List.ofArray [|(List.ofArray [|TSmbE(70);TSeqE(72)|])|]))|]);
       }|]);(245808349,set [|
       {
        prod_num       = 10;
        prod_name      = "b";
        item_num       = 1;
        symb           = Some("NUMBER");
        next_num       = None;
        seq_number     = 0;
        s              = 0;
        f              = set [|1|];
        fromStateTrace = (List.ofArray [|((List.ofArray [| Some("NUMBER")|]),(List.ofArray [|(List.ofArray [|TSeqS(72);TSmbS(70)|])|]));((List.ofArray [|None|]),(List.ofArray [|(List.ofArray [|TSmbE(70);TSeqE(72)|])|]))|]);
        toStateTrace   = (List.ofArray [|((List.ofArray [| Some("NUMBER")|]),(List.ofArray [|(List.ofArray [|TSeqS(72);TSmbS(70)|])|]));((List.ofArray [|None|]),(List.ofArray [|(List.ofArray [|TSmbE(70);TSeqE(72)|])|]))|]);
       }|]);(-1747323465,set [|
       {
        prod_num       = 4;
        prod_name      = "e";
        item_num       = 1;
        symb           = Some("NUMBER");
        next_num       = None;
        seq_number     = 0;
        s              = 0;
        f              = set [|1|];
        fromStateTrace = (List.ofArray [|((List.ofArray [| Some("NUMBER")|]),(List.ofArray [|(List.ofArray [|TSeqS(28);TSmbS(26)|])|]));((List.ofArray [|None|]),(List.ofArray [|(List.ofArray [|TSmbE(26);TSeqE(28)|])|]))|]);
        toStateTrace   = (List.ofArray [|((List.ofArray [| Some("NUMBER")|]),(List.ofArray [|(List.ofArray [|TSeqS(28);TSmbS(26)|])|]));((List.ofArray [|None|]),(List.ofArray [|(List.ofArray [|TSmbE(26);TSeqE(28)|])|]))|]);
       }|]);(812533644,set [||]);(-1179802602,set [|
       {
        prod_num       = 10;
        prod_name      = "b";
        item_num       = 1;
        symb           = Some("NUMBER");
        next_num       = None;
        seq_number     = 0;
        s              = 0;
        f              = set [|1|];
        fromStateTrace = (List.ofArray [|((List.ofArray [| Some("NUMBER")|]),(List.ofArray [|(List.ofArray [|TSeqS(72);TSmbS(70)|])|]));((List.ofArray [|None|]),(List.ofArray [|(List.ofArray [|TSmbE(70);TSeqE(72)|])|]))|]);
        toStateTrace   = (List.ofArray [|((List.ofArray [| Some("NUMBER")|]),(List.ofArray [|(List.ofArray [|TSeqS(72);TSmbS(70)|])|]));((List.ofArray [|None|]),(List.ofArray [|(List.ofArray [|TSmbE(70);TSeqE(72)|])|]))|]);
       };
       {
        prod_num       = 7;
        prod_name      = "a";
        item_num       = 1;
        symb           = Some("NUMBER");
        next_num       = None;
        seq_number     = 0;
        s              = 0;
        f              = set [|1|];
        fromStateTrace = (List.ofArray [|((List.ofArray [| Some("NUMBER")|]),(List.ofArray [|(List.ofArray [|TSeqS(46);TSmbS(44)|])|]));((List.ofArray [|None|]),(List.ofArray [|(List.ofArray [|TSmbE(44);TSeqE(46)|])|]))|]);
        toStateTrace   = (List.ofArray [|((List.ofArray [| Some("NUMBER")|]),(List.ofArray [|(List.ofArray [|TSeqS(46);TSmbS(44)|])|]));((List.ofArray [|None|]),(List.ofArray [|(List.ofArray [|TSmbE(44);TSeqE(46)|])|]))|]);
       }|]);(246030701,set [|
       {
        prod_num       = 10;
        prod_name      = "b";
        item_num       = 1;
        symb           = Some("NUMBER");
        next_num       = None;
        seq_number     = 0;
        s              = 0;
        f              = set [|1|];
        fromStateTrace = (List.ofArray [|((List.ofArray [| Some("NUMBER")|]),(List.ofArray [|(List.ofArray [|TSeqS(72);TSmbS(70)|])|]));((List.ofArray [|None|]),(List.ofArray [|(List.ofArray [|TSmbE(70);TSeqE(72)|])|]))|]);
        toStateTrace   = (List.ofArray [|((List.ofArray [| Some("NUMBER")|]),(List.ofArray [|(List.ofArray [|TSeqS(72);TSmbS(70)|])|]));((List.ofArray [|None|]),(List.ofArray [|(List.ofArray [|TSmbE(70);TSeqE(72)|])|]))|]);
       };
       {
        prod_num       = 7;
        prod_name      = "a";
        item_num       = 1;
        symb           = Some("NUMBER");
        next_num       = None;
        seq_number     = 0;
        s              = 0;
        f              = set [|1|];
        fromStateTrace = (List.ofArray [|((List.ofArray [| Some("NUMBER")|]),(List.ofArray [|(List.ofArray [|TSeqS(46);TSmbS(44)|])|]));((List.ofArray [|None|]),(List.ofArray [|(List.ofArray [|TSmbE(44);TSeqE(46)|])|]))|]);
        toStateTrace   = (List.ofArray [|((List.ofArray [| Some("NUMBER")|]),(List.ofArray [|(List.ofArray [|TSeqS(46);TSmbS(44)|])|]));((List.ofArray [|None|]),(List.ofArray [|(List.ofArray [|TSmbE(44);TSeqE(46)|])|]))|]);
       }|]);(-1181593533,set [|
       {
        prod_num       = 10;
        prod_name      = "b";
        item_num       = 1;
        symb           = Some("NUMBER");
        next_num       = None;
        seq_number     = 0;
        s              = 0;
        f              = set [|1|];
        fromStateTrace = (List.ofArray [|((List.ofArray [| Some("NUMBER")|]),(List.ofArray [|(List.ofArray [|TSeqS(72);TSmbS(70)|])|]));((List.ofArray [|None|]),(List.ofArray [|(List.ofArray [|TSmbE(70);TSeqE(72)|])|]))|]);
        toStateTrace   = (List.ofArray [|((List.ofArray [| Some("NUMBER")|]),(List.ofArray [|(List.ofArray [|TSeqS(72);TSmbS(70)|])|]));((List.ofArray [|None|]),(List.ofArray [|(List.ofArray [|TSmbE(70);TSeqE(72)|])|]))|]);
       }|]);(245689912,set [|
       {
        prod_num       = 10;
        prod_name      = "b";
        item_num       = 1;
        symb           = Some("NUMBER");
        next_num       = None;
        seq_number     = 0;
        s              = 0;
        f              = set [|1|];
        fromStateTrace = (List.ofArray [|((List.ofArray [| Some("NUMBER")|]),(List.ofArray [|(List.ofArray [|TSeqS(72);TSmbS(70)|])|]));((List.ofArray [|None|]),(List.ofArray [|(List.ofArray [|TSmbE(70);TSeqE(72)|])|]))|]);
        toStateTrace   = (List.ofArray [|((List.ofArray [| Some("NUMBER")|]),(List.ofArray [|(List.ofArray [|TSeqS(72);TSmbS(70)|])|]));((List.ofArray [|None|]),(List.ofArray [|(List.ofArray [|TSmbE(70);TSeqE(72)|])|]))|]);
       }|]);(-1747357292,set [|
       {
        prod_num       = 7;
        prod_name      = "a";
        item_num       = 1;
        symb           = Some("NUMBER");
        next_num       = None;
        seq_number     = 0;
        s              = 0;
        f              = set [|1|];
        fromStateTrace = (List.ofArray [|((List.ofArray [| Some("NUMBER")|]),(List.ofArray [|(List.ofArray [|TSeqS(46);TSmbS(44)|])|]));((List.ofArray [|None|]),(List.ofArray [|(List.ofArray [|TSmbE(44);TSeqE(46)|])|]))|]);
        toStateTrace   = (List.ofArray [|((List.ofArray [| Some("NUMBER")|]),(List.ofArray [|(List.ofArray [|TSeqS(46);TSmbS(44)|])|]));((List.ofArray [|None|]),(List.ofArray [|(List.ofArray [|TSmbE(44);TSeqE(46)|])|]))|]);
       }|]);(812596079,set [||]);(-302352489,set [||]);(-1181274469,set [|
       {
        prod_num       = 10;
        prod_name      = "b";
        item_num       = 1;
        symb           = Some("NUMBER");
        next_num       = None;
        seq_number     = 0;
        s              = 0;
        f              = set [|1|];
        fromStateTrace = (List.ofArray [|((List.ofArray [| Some("NUMBER")|]),(List.ofArray [|(List.ofArray [|TSeqS(72);TSmbS(70)|])|]));((List.ofArray [|None|]),(List.ofArray [|(List.ofArray [|TSmbE(70);TSeqE(72)|])|]))|]);
        toStateTrace   = (List.ofArray [|((List.ofArray [| Some("NUMBER")|]),(List.ofArray [|(List.ofArray [|TSeqS(72);TSmbS(70)|])|]));((List.ofArray [|None|]),(List.ofArray [|(List.ofArray [|TSmbE(70);TSeqE(72)|])|]))|]);
       };
       {
        prod_num       = 7;
        prod_name      = "a";
        item_num       = 1;
        symb           = Some("NUMBER");
        next_num       = None;
        seq_number     = 0;
        s              = 0;
        f              = set [|1|];
        fromStateTrace = (List.ofArray [|((List.ofArray [| Some("NUMBER")|]),(List.ofArray [|(List.ofArray [|TSeqS(46);TSmbS(44)|])|]));((List.ofArray [|None|]),(List.ofArray [|(List.ofArray [|TSmbE(44);TSeqE(46)|])|]))|]);
        toStateTrace   = (List.ofArray [|((List.ofArray [| Some("NUMBER")|]),(List.ofArray [|(List.ofArray [|TSeqS(46);TSmbS(44)|])|]));((List.ofArray [|None|]),(List.ofArray [|(List.ofArray [|TSmbE(44);TSeqE(46)|])|]))|]);
       }|]);(-2013472702,set [||]);(-1067755073,set [||]);(-1181454932,set [|
       {
        prod_num       = 10;
        prod_name      = "b";
        item_num       = 1;
        symb           = Some("NUMBER");
        next_num       = None;
        seq_number     = 0;
        s              = 0;
        f              = set [|1|];
        fromStateTrace = (List.ofArray [|((List.ofArray [| Some("NUMBER")|]),(List.ofArray [|(List.ofArray [|TSeqS(72);TSmbS(70)|])|]));((List.ofArray [|None|]),(List.ofArray [|(List.ofArray [|TSmbE(70);TSeqE(72)|])|]))|]);
        toStateTrace   = (List.ofArray [|((List.ofArray [| Some("NUMBER")|]),(List.ofArray [|(List.ofArray [|TSeqS(72);TSmbS(70)|])|]));((List.ofArray [|None|]),(List.ofArray [|(List.ofArray [|TSmbE(70);TSeqE(72)|])|]))|]);
       }|]);(245388183,set [|
       {
        prod_num       = 10;
        prod_name      = "b";
        item_num       = 1;
        symb           = Some("NUMBER");
        next_num       = None;
        seq_number     = 0;
        s              = 0;
        f              = set [|1|];
        fromStateTrace = (List.ofArray [|((List.ofArray [| Some("NUMBER")|]),(List.ofArray [|(List.ofArray [|TSeqS(72);TSmbS(70)|])|]));((List.ofArray [|None|]),(List.ofArray [|(List.ofArray [|TSmbE(70);TSeqE(72)|])|]))|]);
        toStateTrace   = (List.ofArray [|((List.ofArray [| Some("NUMBER")|]),(List.ofArray [|(List.ofArray [|TSeqS(72);TSmbS(70)|])|]));((List.ofArray [|None|]),(List.ofArray [|(List.ofArray [|TSmbE(70);TSeqE(72)|])|]))|]);
       }|]);(-1747236743,set [|
       {
        prod_num       = 10;
        prod_name      = "b";
        item_num       = 1;
        symb           = Some("NUMBER");
        next_num       = None;
        seq_number     = 0;
        s              = 0;
        f              = set [|1|];
        fromStateTrace = (List.ofArray [|((List.ofArray [| Some("NUMBER")|]),(List.ofArray [|(List.ofArray [|TSeqS(72);TSmbS(70)|])|]));((List.ofArray [|None|]),(List.ofArray [|(List.ofArray [|TSmbE(70);TSeqE(72)|])|]))|]);
        toStateTrace   = (List.ofArray [|((List.ofArray [| Some("NUMBER")|]),(List.ofArray [|(List.ofArray [|TSeqS(72);TSmbS(70)|])|]));((List.ofArray [|None|]),(List.ofArray [|(List.ofArray [|TSmbE(70);TSeqE(72)|])|]))|]);
       }|]);(812477890,set [||]);(-302677004,set [||]);(-1181440584,set [|
       {
        prod_num       = 10;
        prod_name      = "b";
        item_num       = 1;
        symb           = Some("NUMBER");
        next_num       = None;
        seq_number     = 0;
        s              = 0;
        f              = set [|1|];
        fromStateTrace = (List.ofArray [|((List.ofArray [| Some("NUMBER")|]),(List.ofArray [|(List.ofArray [|TSeqS(72);TSmbS(70)|])|]));((List.ofArray [|None|]),(List.ofArray [|(List.ofArray [|TSmbE(70);TSeqE(72)|])|]))|]);
        toStateTrace   = (List.ofArray [|((List.ofArray [| Some("NUMBER")|]),(List.ofArray [|(List.ofArray [|TSeqS(72);TSmbS(70)|])|]));((List.ofArray [|None|]),(List.ofArray [|(List.ofArray [|TSmbE(70);TSeqE(72)|])|]))|]);
       };
       {
        prod_num       = 7;
        prod_name      = "a";
        item_num       = 1;
        symb           = Some("NUMBER");
        next_num       = None;
        seq_number     = 0;
        s              = 0;
        f              = set [|1|];
        fromStateTrace = (List.ofArray [|((List.ofArray [| Some("NUMBER")|]),(List.ofArray [|(List.ofArray [|TSeqS(46);TSmbS(44)|])|]));((List.ofArray [|None|]),(List.ofArray [|(List.ofArray [|TSmbE(44);TSeqE(46)|])|]))|]);
        toStateTrace   = (List.ofArray [|((List.ofArray [| Some("NUMBER")|]),(List.ofArray [|(List.ofArray [|TSeqS(46);TSmbS(44)|])|]));((List.ofArray [|None|]),(List.ofArray [|(List.ofArray [|TSmbE(44);TSeqE(46)|])|]))|]);
       }|]);(-2013311007,set [||]);(-1067719332,set [||]);(-302504685,set [||]);(-1181663895,set [|
       {
        prod_num       = 10;
        prod_name      = "b";
        item_num       = 1;
        symb           = Some("NUMBER");
        next_num       = None;
        seq_number     = 0;
        s              = 0;
        f              = set [|1|];
        fromStateTrace = (List.ofArray [|((List.ofArray [| Some("NUMBER")|]),(List.ofArray [|(List.ofArray [|TSeqS(72);TSmbS(70)|])|]));((List.ofArray [|None|]),(List.ofArray [|(List.ofArray [|TSmbE(70);TSeqE(72)|])|]))|]);
        toStateTrace   = (List.ofArray [|((List.ofArray [| Some("NUMBER")|]),(List.ofArray [|(List.ofArray [|TSeqS(72);TSmbS(70)|])|]));((List.ofArray [|None|]),(List.ofArray [|(List.ofArray [|TSmbE(70);TSeqE(72)|])|]))|]);
       }|]);(-2015196986,set [||]);(-1068193477,set [||]);(-1747315106,set [|
       {
        prod_num       = 13;
        prod_name      = "c";
        item_num       = 1;
        symb           = Some("NUMBER");
        next_num       = None;
        seq_number     = 0;
        s              = 0;
        f              = set [|1|];
        fromStateTrace = (List.ofArray [|((List.ofArray [| Some("NUMBER")|]),(List.ofArray [|(List.ofArray [|TSeqS(106);TSmbS(104)|])|]));((List.ofArray [|None|]),(List.ofArray [|(List.ofArray [|TSmbE(104);TSeqE(106)|])|]))|]);
        toStateTrace   = (List.ofArray [|((List.ofArray [| Some("NUMBER")|]),(List.ofArray [|(List.ofArray [|TSeqS(106);TSmbS(104)|])|]));((List.ofArray [|None|]),(List.ofArray [|(List.ofArray [|TSmbE(104);TSeqE(106)|])|]))|]);
       }|]);(810725029,set [||]);(-1181302208,set [|
       {
        prod_num       = 10;
        prod_name      = "b";
        item_num       = 1;
        symb           = Some("NUMBER");
        next_num       = None;
        seq_number     = 0;
        s              = 0;
        f              = set [|1|];
        fromStateTrace = (List.ofArray [|((List.ofArray [| Some("NUMBER")|]),(List.ofArray [|(List.ofArray [|TSeqS(72);TSmbS(70)|])|]));((List.ofArray [|None|]),(List.ofArray [|(List.ofArray [|TSmbE(70);TSeqE(72)|])|]))|]);
        toStateTrace   = (List.ofArray [|((List.ofArray [| Some("NUMBER")|]),(List.ofArray [|(List.ofArray [|TSeqS(72);TSmbS(70)|])|]));((List.ofArray [|None|]),(List.ofArray [|(List.ofArray [|TSmbE(70);TSeqE(72)|])|]))|]);
       };
       {
        prod_num       = 7;
        prod_name      = "a";
        item_num       = 1;
        symb           = Some("NUMBER");
        next_num       = None;
        seq_number     = 0;
        s              = 0;
        f              = set [|1|];
        fromStateTrace = (List.ofArray [|((List.ofArray [| Some("NUMBER")|]),(List.ofArray [|(List.ofArray [|TSeqS(46);TSmbS(44)|])|]));((List.ofArray [|None|]),(List.ofArray [|(List.ofArray [|TSmbE(44);TSeqE(46)|])|]))|]);
        toStateTrace   = (List.ofArray [|((List.ofArray [| Some("NUMBER")|]),(List.ofArray [|(List.ofArray [|TSeqS(46);TSmbS(44)|])|]));((List.ofArray [|None|]),(List.ofArray [|(List.ofArray [|TSmbE(44);TSeqE(46)|])|]))|]);
       }|]);(786296754,set [||]);(-1181341429,set [|
       {
        prod_num       = 10;
        prod_name      = "b";
        item_num       = 1;
        symb           = Some("NUMBER");
        next_num       = None;
        seq_number     = 0;
        s              = 0;
        f              = set [|1|];
        fromStateTrace = (List.ofArray [|((List.ofArray [| Some("NUMBER")|]),(List.ofArray [|(List.ofArray [|TSeqS(72);TSmbS(70)|])|]));((List.ofArray [|None|]),(List.ofArray [|(List.ofArray [|TSmbE(70);TSeqE(72)|])|]))|]);
        toStateTrace   = (List.ofArray [|((List.ofArray [| Some("NUMBER")|]),(List.ofArray [|(List.ofArray [|TSeqS(72);TSmbS(70)|])|]));((List.ofArray [|None|]),(List.ofArray [|(List.ofArray [|TSmbE(70);TSeqE(72)|])|]))|]);
       }|]);(245404982,set [|
       {
        prod_num       = 10;
        prod_name      = "b";
        item_num       = 1;
        symb           = Some("NUMBER");
        next_num       = None;
        seq_number     = 0;
        s              = 0;
        f              = set [|1|];
        fromStateTrace = (List.ofArray [|((List.ofArray [| Some("NUMBER")|]),(List.ofArray [|(List.ofArray [|TSeqS(72);TSmbS(70)|])|]));((List.ofArray [|None|]),(List.ofArray [|(List.ofArray [|TSmbE(70);TSeqE(72)|])|]))|]);
        toStateTrace   = (List.ofArray [|((List.ofArray [| Some("NUMBER")|]),(List.ofArray [|(List.ofArray [|TSeqS(72);TSmbS(70)|])|]));((List.ofArray [|None|]),(List.ofArray [|(List.ofArray [|TSmbE(70);TSeqE(72)|])|]))|]);
       }|]);(-1181268754,set [|
       {
        prod_num       = 10;
        prod_name      = "b";
        item_num       = 1;
        symb           = Some("NUMBER");
        next_num       = None;
        seq_number     = 0;
        s              = 0;
        f              = set [|1|];
        fromStateTrace = (List.ofArray [|((List.ofArray [| Some("NUMBER")|]),(List.ofArray [|(List.ofArray [|TSeqS(72);TSmbS(70)|])|]));((List.ofArray [|None|]),(List.ofArray [|(List.ofArray [|TSmbE(70);TSeqE(72)|])|]))|]);
        toStateTrace   = (List.ofArray [|((List.ofArray [| Some("NUMBER")|]),(List.ofArray [|(List.ofArray [|TSeqS(72);TSmbS(70)|])|]));((List.ofArray [|None|]),(List.ofArray [|(List.ofArray [|TSmbE(70);TSeqE(72)|])|]))|]);
       }|]);(1012490083,set [||]);(-1181308693,set [|
       {
        prod_num       = 13;
        prod_name      = "c";
        item_num       = 1;
        symb           = Some("NUMBER");
        next_num       = None;
        seq_number     = 0;
        s              = 0;
        f              = set [|1|];
        fromStateTrace = (List.ofArray [|((List.ofArray [| Some("NUMBER")|]),(List.ofArray [|(List.ofArray [|TSeqS(106);TSmbS(104)|])|]));((List.ofArray [|None|]),(List.ofArray [|(List.ofArray [|TSmbE(104);TSeqE(106)|])|]))|]);
        toStateTrace   = (List.ofArray [|((List.ofArray [| Some("NUMBER")|]),(List.ofArray [|(List.ofArray [|TSeqS(106);TSmbS(104)|])|]));((List.ofArray [|None|]),(List.ofArray [|(List.ofArray [|TSmbE(104);TSeqE(106)|])|]))|]);
       }|]);(245464150,set [|
       {
        prod_num       = 13;
        prod_name      = "c";
        item_num       = 1;
        symb           = Some("NUMBER");
        next_num       = None;
        seq_number     = 0;
        s              = 0;
        f              = set [|1|];
        fromStateTrace = (List.ofArray [|((List.ofArray [| Some("NUMBER")|]),(List.ofArray [|(List.ofArray [|TSeqS(106);TSmbS(104)|])|]));((List.ofArray [|None|]),(List.ofArray [|(List.ofArray [|TSmbE(104);TSeqE(106)|])|]))|]);
        toStateTrace   = (List.ofArray [|((List.ofArray [| Some("NUMBER")|]),(List.ofArray [|(List.ofArray [|TSeqS(106);TSmbS(104)|])|]));((List.ofArray [|None|]),(List.ofArray [|(List.ofArray [|TSmbE(104);TSeqE(106)|])|]))|]);
       }|]);(286287501,set [||]);(-1507566602,set [||]);(286270238,set [||]);(-1507587355,set [||]);(286497484,set [||]);(-1507500041,set [||]);(286467611,set [||]);(-1507516768,set [||]);(1062885322,set [||]);(-1729239567,set [||]);(288228971,set [||]);(-1507228912,set [||]);(286388798,set [||]);(-1507374011,set [||]);(1062917097,set [||]);(-1729324782,set [||]);(1165612522,set [||]);(286559462,set [||]);(796883519,set [||]);(1759050690,set [||]);(286773201,set [||]);(-1507593750,set [||]);(1062791684,set [||]);(-1729179713,set [||]);(1165369737,set [||]);(286721989,set [||]);(796716444,set [||]);(1759004449,set [||]);(1165723502,set [||]);(286433044,set [||]);(794897083,set [||]);(1758923590,set [||]);(1062843427,set [||]);(-1731130152,set [||]);(286613565,set [||]);(-2040640049,set [|
       {
        prod_num       = 14;
        prod_name      = "t";
        item_num       = 2;
        symb           = Some("b");
        next_num       = Some(3);
        seq_number     = 2;
        s              = 0;
        f              = set [|3|];
        fromStateTrace = (List.ofArray [|((List.ofArray [| Some("b")|]),(List.ofArray [|(List.ofArray [|TSmbE(114);TSmbS(118)|])|]));((List.ofArray [|None|]),(List.ofArray [|(List.ofArray [|TSmbE(118);TSeqE(120)|])|]))|]);
        toStateTrace   = (List.ofArray [|((List.ofArray [| Some("b")|]),(List.ofArray [|(List.ofArray [|TSmbE(114);TSmbS(118)|])|]));((List.ofArray [|None|]),(List.ofArray [|(List.ofArray [|TSmbE(118);TSeqE(120)|])|]))|]);
       }|]);(286624630,set [||]);(-1507641525,set [||]);(286582419,set [||]);(-1797587682,set [||]);(286591638,set [||]);(-1507664341,set [||]);(-1971759933,set [||]);(1028483512,set [||]);(-1971777200,set [||]);(1028470955,set [||]);(-1971672958,set [||]);(1028419001,set [||]);(-1971710891,set [||]);(1028402414,set [||]);(-1540135548,set [||]);(59037631,set [||]);(-1975045083,set [||]);(1027649886,set [||]);(-1971797904,set [||]);(1028554251,set [||]);(-1540103769,set [||]);(59091804,set [||]);(-569317468,set [||]);(-1971470680,set [||]);(-1273608079,set [|
       {
        prod_num       = 8;
        prod_name      = "b";
        item_num       = 3;
        symb           = Some("RIGHT");
        next_num       = None;
        seq_number     = 2;
        s              = 0;
        f              = set [|3|];
        fromStateTrace = (List.ofArray [|((List.ofArray [| Some("RIGHT")|]),(List.ofArray [|(List.ofArray [|TSmbE(54);TSmbS(58)|])|]));((List.ofArray [|None|]),(List.ofArray [|(List.ofArray [|TSmbE(58);TSeqE(60)|])|]))|]);
        toStateTrace   = (List.ofArray [|((List.ofArray [| Some("RIGHT")|]),(List.ofArray [|(List.ofArray [|TSmbE(54);TSmbS(58)|])|]));((List.ofArray [|None|]),(List.ofArray [|(List.ofArray [|TSmbE(58);TSeqE(60)|])|]))|]);
       }|]);(-206452340,set [||]);(-1971389025,set [||]);(1028309924,set [||]);(-1540237238,set [||]);(59244017,set [||]);(-568388665,set [||]);(-1971440245,set [||]);(-1273634862,set [|
       {
        prod_num       = 11;
        prod_name      = "c";
        item_num       = 3;
        symb           = Some("RIGHT");
        next_num       = None;
        seq_number     = 2;
        s              = 0;
        f              = set [|3|];
        fromStateTrace = (List.ofArray [|((List.ofArray [| Some("RIGHT")|]),(List.ofArray [|(List.ofArray [|TSmbE(80);TSmbS(84)|])|]));((List.ofArray [|None|]),(List.ofArray [|(List.ofArray [|TSmbE(84);TSeqE(86)|])|]))|]);
        toStateTrace   = (List.ofArray [|((List.ofArray [| Some("RIGHT")|]),(List.ofArray [|(List.ofArray [|TSmbE(80);TSmbS(84)|])|]));((List.ofArray [|None|]),(List.ofArray [|(List.ofArray [|TSmbE(84);TSeqE(86)|])|]))|]);
       }|]);(-206473873,set [||]);(-569199328,set [||]);(-1971606182,set [||]);(-1274537739,set [|
       {
        prod_num       = 12;
        prod_name      = "c";
        item_num       = 3;
        symb           = Some("RIGHT");
        next_num       = None;
        seq_number     = 2;
        s              = 0;
        f              = set [|3|];
        fromStateTrace = (List.ofArray [|((List.ofArray [| Some("RIGHT")|]),(List.ofArray [|(List.ofArray [|TSmbE(94);TSmbS(98)|])|]));((List.ofArray [|None|]),(List.ofArray [|(List.ofArray [|TSmbE(98);TSeqE(100)|])|]))|]);
        toStateTrace   = (List.ofArray [|((List.ofArray [| Some("RIGHT")|]),(List.ofArray [|(List.ofArray [|TSmbE(94);TSmbS(98)|])|]));((List.ofArray [|None|]),(List.ofArray [|(List.ofArray [|TSmbE(98);TSeqE(100)|])|]))|]);
       }|]);(-205637368,set [||]);(-1540323731,set [||]);(62406294,set [||]);(-1971563917,set [||]);(490176385,set [||]);(-1971537608,set [||]);(1028261125,set [||]);(-1971464995,set [||]);(263442256,set [||]);(-1971570472,set [||]);(1028385893,set [||]);(262878531,set [|
       {
        prod_num       = 14;
        prod_name      = "t";
        item_num       = 1;
        symb           = Some("PLUS");
        next_num       = Some(2);
        seq_number     = 1;
        s              = 0;
        f              = set [|3|];
        fromStateTrace = (List.ofArray [|((List.ofArray [| Some("PLUS")|]),(List.ofArray [|(List.ofArray [|TSmbE(110);TSmbS(114)|])|]))|]);
        toStateTrace   = (List.ofArray [|((List.ofArray [| Some("PLUS")|]),(List.ofArray [|(List.ofArray [|TSmbE(110);TSmbS(114)|])|]))|]);
       }|]);(-1197537224,set [|
       {
        prod_num       = 14;
        prod_name      = "t";
        item_num       = 1;
        symb           = Some("PLUS");
        next_num       = Some(2);
        seq_number     = 1;
        s              = 0;
        f              = set [|3|];
        fromStateTrace = (List.ofArray [|((List.ofArray [| Some("PLUS")|]),(List.ofArray [|(List.ofArray [|TSmbE(110);TSmbS(114)|])|]))|]);
        toStateTrace   = (List.ofArray [|((List.ofArray [| Some("PLUS")|]),(List.ofArray [|(List.ofArray [|TSmbE(110);TSmbS(114)|])|]))|]);
       }|]);(262893776,set [|
       {
        prod_num       = 14;
        prod_name      = "t";
        item_num       = 1;
        symb           = Some("PLUS");
        next_num       = Some(2);
        seq_number     = 1;
        s              = 0;
        f              = set [|3|];
        fromStateTrace = (List.ofArray [|((List.ofArray [| Some("PLUS")|]),(List.ofArray [|(List.ofArray [|TSmbE(110);TSmbS(114)|])|]))|]);
        toStateTrace   = (List.ofArray [|((List.ofArray [| Some("PLUS")|]),(List.ofArray [|(List.ofArray [|TSmbE(110);TSmbS(114)|])|]))|]);
       }|]);(-1197516501,set [|
       {
        prod_num       = 14;
        prod_name      = "t";
        item_num       = 1;
        symb           = Some("PLUS");
        next_num       = Some(2);
        seq_number     = 1;
        s              = 0;
        f              = set [|3|];
        fromStateTrace = (List.ofArray [|((List.ofArray [| Some("PLUS")|]),(List.ofArray [|(List.ofArray [|TSmbE(110);TSmbS(114)|])|]))|]);
        toStateTrace   = (List.ofArray [|((List.ofArray [| Some("PLUS")|]),(List.ofArray [|(List.ofArray [|TSmbE(110);TSmbS(114)|])|]))|]);
       }|]);(262719746,set [|
       {
        prod_num       = 14;
        prod_name      = "t";
        item_num       = 1;
        symb           = Some("PLUS");
        next_num       = Some(2);
        seq_number     = 1;
        s              = 0;
        f              = set [|3|];
        fromStateTrace = (List.ofArray [|((List.ofArray [| Some("PLUS")|]),(List.ofArray [|(List.ofArray [|TSmbE(110);TSmbS(114)|])|]))|]);
        toStateTrace   = (List.ofArray [|((List.ofArray [| Some("PLUS")|]),(List.ofArray [|(List.ofArray [|TSmbE(110);TSmbS(114)|])|]))|]);
       }|]);(-1197601735,set [|
       {
        prod_num       = 14;
        prod_name      = "t";
        item_num       = 1;
        symb           = Some("PLUS");
        next_num       = Some(2);
        seq_number     = 1;
        s              = 0;
        f              = set [|3|];
        fromStateTrace = (List.ofArray [|((List.ofArray [| Some("PLUS")|]),(List.ofArray [|(List.ofArray [|TSmbE(110);TSmbS(114)|])|]))|]);
        toStateTrace   = (List.ofArray [|((List.ofArray [| Some("PLUS")|]),(List.ofArray [|(List.ofArray [|TSmbE(110);TSmbS(114)|])|]))|]);
       }|]);(262698453,set [||]);(-1197585042,set [||]);(568394756,set [||]);(-2041137601,set [||]);(261445029,set [|
       {
        prod_num       = 14;
        prod_name      = "t";
        item_num       = 1;
        symb           = Some("PLUS");
        next_num       = Some(2);
        seq_number     = 1;
        s              = 0;
        f              = set [|3|];
        fromStateTrace = (List.ofArray [|((List.ofArray [| Some("PLUS")|]),(List.ofArray [|(List.ofArray [|TSmbE(110);TSmbS(114)|])|]))|]);
        toStateTrace   = (List.ofArray [|((List.ofArray [| Some("PLUS")|]),(List.ofArray [|(List.ofArray [|TSmbE(110);TSmbS(114)|])|]))|]);
       }|]);(-1198337826,set [|
       {
        prod_num       = 14;
        prod_name      = "t";
        item_num       = 1;
        symb           = Some("PLUS");
        next_num       = Some(2);
        seq_number     = 1;
        s              = 0;
        f              = set [|3|];
        fromStateTrace = (List.ofArray [|((List.ofArray [| Some("PLUS")|]),(List.ofArray [|(List.ofArray [|TSmbE(110);TSmbS(114)|])|]))|]);
        toStateTrace   = (List.ofArray [|((List.ofArray [| Some("PLUS")|]),(List.ofArray [|(List.ofArray [|TSmbE(110);TSmbS(114)|])|]))|]);
       }|]);(262840816,set [||]);(-1197727861,set [||]);(568426535,set [||]);(-2041066788,set [||]);(1539474980,set [||]);(263184168,set [|
       {
        prod_num       = 14;
        prod_name      = "t";
        item_num       = 1;
        symb           = Some("PLUS");
        next_num       = Some(2);
        seq_number     = 1;
        s              = 0;
        f              = set [|3|];
        fromStateTrace = (List.ofArray [|((List.ofArray [| Some("PLUS")|]),(List.ofArray [|(List.ofArray [|TSmbE(110);TSmbS(114)|])|]))|]);
        toStateTrace   = (List.ofArray [|((List.ofArray [| Some("PLUS")|]),(List.ofArray [|(List.ofArray [|TSmbE(110);TSmbS(114)|])|]))|]);
       }|]);(834939377,set [||]);(1986242572,set [||]);(262970399,set [||]);(-1197956572,set [||]);(568572362,set [||]);(-2041209743,set [||]);(1540190791,set [||]);(262951947,set [|
       {
        prod_num       = 14;
        prod_name      = "t";
        item_num       = 1;
        symb           = Some("PLUS");
        next_num       = Some(2);
        seq_number     = 1;
        s              = 0;
        f              = set [|3|];
        fromStateTrace = (List.ofArray [|((List.ofArray [| Some("PLUS")|]),(List.ofArray [|(List.ofArray [|TSmbE(110);TSmbS(114)|])|]))|]);
        toStateTrace   = (List.ofArray [|((List.ofArray [| Some("PLUS")|]),(List.ofArray [|(List.ofArray [|TSmbE(110);TSmbS(114)|])|]))|]);
       }|]);(835173970,set [||]);(1986204911,set [||]);(1539364000,set [||]);(262786266,set [||]);(836401525,set [||]);(1986812040,set [||]);(568518637,set [||]);(-2039849194,set [||]);(263074803,set [|
       {
        prod_num       = 14;
        prod_name      = "t";
        item_num       = 1;
        symb           = Some("PLUS");
        next_num       = Some(2);
        seq_number     = 1;
        s              = 0;
        f              = set [|3|];
        fromStateTrace = (List.ofArray [|((List.ofArray [| Some("PLUS")|]),(List.ofArray [|(List.ofArray [|TSmbE(110);TSmbS(114)|])|]))|]);
        toStateTrace   = (List.ofArray [|((List.ofArray [| Some("PLUS")|]),(List.ofArray [|(List.ofArray [|TSmbE(110);TSmbS(114)|])|]))|]);
       }|]);(-1729815039,set [||]);(263116984,set [||]);(-1197972347,set [||]);(263173469,set [||]);(-1973309744,set [||]);(263084376,set [||]);(-1197896219,set [||]);(262878530,set [|
       {
        prod_num       = 15;
        prod_name      = "f";
        item_num       = 1;
        symb           = Some("MULT");
        next_num       = Some(2);
        seq_number     = 1;
        s              = 0;
        f              = set [|3|];
        fromStateTrace = (List.ofArray [|((List.ofArray [| Some("MULT")|]),(List.ofArray [|(List.ofArray [|TSmbE(124);TSmbS(128)|])|]))|]);
        toStateTrace   = (List.ofArray [|((List.ofArray [| Some("MULT")|]),(List.ofArray [|(List.ofArray [|TSmbE(124);TSmbS(128)|])|]))|]);
       }|]);(-1197537223,set [|
       {
        prod_num       = 15;
        prod_name      = "f";
        item_num       = 1;
        symb           = Some("MULT");
        next_num       = Some(2);
        seq_number     = 1;
        s              = 0;
        f              = set [|3|];
        fromStateTrace = (List.ofArray [|((List.ofArray [| Some("MULT")|]),(List.ofArray [|(List.ofArray [|TSmbE(124);TSmbS(128)|])|]))|]);
        toStateTrace   = (List.ofArray [|((List.ofArray [| Some("MULT")|]),(List.ofArray [|(List.ofArray [|TSmbE(124);TSmbS(128)|])|]))|]);
       }|]);(262893777,set [|
       {
        prod_num       = 15;
        prod_name      = "f";
        item_num       = 1;
        symb           = Some("MULT");
        next_num       = Some(2);
        seq_number     = 1;
        s              = 0;
        f              = set [|3|];
        fromStateTrace = (List.ofArray [|((List.ofArray [| Some("MULT")|]),(List.ofArray [|(List.ofArray [|TSmbE(124);TSmbS(128)|])|]))|]);
        toStateTrace   = (List.ofArray [|((List.ofArray [| Some("MULT")|]),(List.ofArray [|(List.ofArray [|TSmbE(124);TSmbS(128)|])|]))|]);
       }|]);(-1197516502,set [|
       {
        prod_num       = 15;
        prod_name      = "f";
        item_num       = 1;
        symb           = Some("MULT");
        next_num       = Some(2);
        seq_number     = 1;
        s              = 0;
        f              = set [|3|];
        fromStateTrace = (List.ofArray [|((List.ofArray [| Some("MULT")|]),(List.ofArray [|(List.ofArray [|TSmbE(124);TSmbS(128)|])|]))|]);
        toStateTrace   = (List.ofArray [|((List.ofArray [| Some("MULT")|]),(List.ofArray [|(List.ofArray [|TSmbE(124);TSmbS(128)|])|]))|]);
       }|]);(262719747,set [|
       {
        prod_num       = 15;
        prod_name      = "f";
        item_num       = 1;
        symb           = Some("MULT");
        next_num       = Some(2);
        seq_number     = 1;
        s              = 0;
        f              = set [|3|];
        fromStateTrace = (List.ofArray [|((List.ofArray [| Some("MULT")|]),(List.ofArray [|(List.ofArray [|TSmbE(124);TSmbS(128)|])|]))|]);
        toStateTrace   = (List.ofArray [|((List.ofArray [| Some("MULT")|]),(List.ofArray [|(List.ofArray [|TSmbE(124);TSmbS(128)|])|]))|]);
       }|]);(-1197601736,set [|
       {
        prod_num       = 15;
        prod_name      = "f";
        item_num       = 1;
        symb           = Some("MULT");
        next_num       = Some(2);
        seq_number     = 1;
        s              = 0;
        f              = set [|3|];
        fromStateTrace = (List.ofArray [|((List.ofArray [| Some("MULT")|]),(List.ofArray [|(List.ofArray [|TSmbE(124);TSmbS(128)|])|]))|]);
        toStateTrace   = (List.ofArray [|((List.ofArray [| Some("MULT")|]),(List.ofArray [|(List.ofArray [|TSmbE(124);TSmbS(128)|])|]))|]);
       }|]);(262698452,set [|
       {
        prod_num       = 15;
        prod_name      = "f";
        item_num       = 1;
        symb           = Some("MULT");
        next_num       = Some(2);
        seq_number     = 1;
        s              = 0;
        f              = set [|3|];
        fromStateTrace = (List.ofArray [|((List.ofArray [| Some("MULT")|]),(List.ofArray [|(List.ofArray [|TSmbE(124);TSmbS(128)|])|]))|]);
        toStateTrace   = (List.ofArray [|((List.ofArray [| Some("MULT")|]),(List.ofArray [|(List.ofArray [|TSmbE(124);TSmbS(128)|])|]))|]);
       }|]);(-1197585041,set [|
       {
        prod_num       = 15;
        prod_name      = "f";
        item_num       = 1;
        symb           = Some("MULT");
        next_num       = Some(2);
        seq_number     = 1;
        s              = 0;
        f              = set [|3|];
        fromStateTrace = (List.ofArray [|((List.ofArray [| Some("MULT")|]),(List.ofArray [|(List.ofArray [|TSmbE(124);TSmbS(128)|])|]))|]);
        toStateTrace   = (List.ofArray [|((List.ofArray [| Some("MULT")|]),(List.ofArray [|(List.ofArray [|TSmbE(124);TSmbS(128)|])|]))|]);
       }|]);(568394757,set [||]);(-2041137602,set [||]);(261445028,set [|
       {
        prod_num       = 15;
        prod_name      = "f";
        item_num       = 1;
        symb           = Some("MULT");
        next_num       = Some(2);
        seq_number     = 1;
        s              = 0;
        f              = set [|3|];
        fromStateTrace = (List.ofArray [|((List.ofArray [| Some("MULT")|]),(List.ofArray [|(List.ofArray [|TSmbE(124);TSmbS(128)|])|]))|]);
        toStateTrace   = (List.ofArray [|((List.ofArray [| Some("MULT")|]),(List.ofArray [|(List.ofArray [|TSmbE(124);TSmbS(128)|])|]))|]);
       }|]);(-1198337825,set [|
       {
        prod_num       = 15;
        prod_name      = "f";
        item_num       = 1;
        symb           = Some("MULT");
        next_num       = Some(2);
        seq_number     = 1;
        s              = 0;
        f              = set [|3|];
        fromStateTrace = (List.ofArray [|((List.ofArray [| Some("MULT")|]),(List.ofArray [|(List.ofArray [|TSmbE(124);TSmbS(128)|])|]))|]);
        toStateTrace   = (List.ofArray [|((List.ofArray [| Some("MULT")|]),(List.ofArray [|(List.ofArray [|TSmbE(124);TSmbS(128)|])|]))|]);
       }|]);(262840817,set [|
       {
        prod_num       = 15;
        prod_name      = "f";
        item_num       = 1;
        symb           = Some("MULT");
        next_num       = Some(2);
        seq_number     = 1;
        s              = 0;
        f              = set [|3|];
        fromStateTrace = (List.ofArray [|((List.ofArray [| Some("MULT")|]),(List.ofArray [|(List.ofArray [|TSmbE(124);TSmbS(128)|])|]))|]);
        toStateTrace   = (List.ofArray [|((List.ofArray [| Some("MULT")|]),(List.ofArray [|(List.ofArray [|TSmbE(124);TSmbS(128)|])|]))|]);
       }|]);(-1197727862,set [|
       {
        prod_num       = 15;
        prod_name      = "f";
        item_num       = 1;
        symb           = Some("MULT");
        next_num       = Some(2);
        seq_number     = 1;
        s              = 0;
        f              = set [|3|];
        fromStateTrace = (List.ofArray [|((List.ofArray [| Some("MULT")|]),(List.ofArray [|(List.ofArray [|TSmbE(124);TSmbS(128)|])|]))|]);
        toStateTrace   = (List.ofArray [|((List.ofArray [| Some("MULT")|]),(List.ofArray [|(List.ofArray [|TSmbE(124);TSmbS(128)|])|]))|]);
       }|]);(568426534,set [||]);(-2041066787,set [||]);(1539474981,set [||]);(263184169,set [|
       {
        prod_num       = 15;
        prod_name      = "f";
        item_num       = 1;
        symb           = Some("MULT");
        next_num       = Some(2);
        seq_number     = 1;
        s              = 0;
        f              = set [|3|];
        fromStateTrace = (List.ofArray [|((List.ofArray [| Some("MULT")|]),(List.ofArray [|(List.ofArray [|TSmbE(124);TSmbS(128)|])|]))|]);
        toStateTrace   = (List.ofArray [|((List.ofArray [| Some("MULT")|]),(List.ofArray [|(List.ofArray [|TSmbE(124);TSmbS(128)|])|]))|]);
       }|]);(834939376,set [||]);(1986242573,set [||]);(262970398,set [|
       {
        prod_num       = 15;
        prod_name      = "f";
        item_num       = 1;
        symb           = Some("MULT");
        next_num       = Some(2);
        seq_number     = 1;
        s              = 0;
        f              = set [|3|];
        fromStateTrace = (List.ofArray [|((List.ofArray [| Some("MULT")|]),(List.ofArray [|(List.ofArray [|TSmbE(124);TSmbS(128)|])|]))|]);
        toStateTrace   = (List.ofArray [|((List.ofArray [| Some("MULT")|]),(List.ofArray [|(List.ofArray [|TSmbE(124);TSmbS(128)|])|]))|]);
       }|]);(-1197956571,set [|
       {
        prod_num       = 15;
        prod_name      = "f";
        item_num       = 1;
        symb           = Some("MULT");
        next_num       = Some(2);
        seq_number     = 1;
        s              = 0;
        f              = set [|3|];
        fromStateTrace = (List.ofArray [|((List.ofArray [| Some("MULT")|]),(List.ofArray [|(List.ofArray [|TSmbE(124);TSmbS(128)|])|]))|]);
        toStateTrace   = (List.ofArray [|((List.ofArray [| Some("MULT")|]),(List.ofArray [|(List.ofArray [|TSmbE(124);TSmbS(128)|])|]))|]);
       }|]);(568572363,set [||]);(-2041209744,set [||]);(1540190790,set [||]);(262951946,set [|
       {
        prod_num       = 15;
        prod_name      = "f";
        item_num       = 1;
        symb           = Some("MULT");
        next_num       = Some(2);
        seq_number     = 1;
        s              = 0;
        f              = set [|3|];
        fromStateTrace = (List.ofArray [|((List.ofArray [| Some("MULT")|]),(List.ofArray [|(List.ofArray [|TSmbE(124);TSmbS(128)|])|]))|]);
        toStateTrace   = (List.ofArray [|((List.ofArray [| Some("MULT")|]),(List.ofArray [|(List.ofArray [|TSmbE(124);TSmbS(128)|])|]))|]);
       }|]);(835173971,set [||]);(1986204910,set [||]);(1539364001,set [||]);(262786267,set [|
       {
        prod_num       = 15;
        prod_name      = "f";
        item_num       = 1;
        symb           = Some("MULT");
        next_num       = Some(2);
        seq_number     = 1;
        s              = 0;
        f              = set [|3|];
        fromStateTrace = (List.ofArray [|((List.ofArray [| Some("MULT")|]),(List.ofArray [|(List.ofArray [|TSmbE(124);TSmbS(128)|])|]))|]);
        toStateTrace   = (List.ofArray [|((List.ofArray [| Some("MULT")|]),(List.ofArray [|(List.ofArray [|TSmbE(124);TSmbS(128)|])|]))|]);
       }|]);(836401524,set [||]);(1986812041,set [||]);(568518636,set [||]);(-2039849193,set [||]);(263074802,set [|
       {
        prod_num       = 15;
        prod_name      = "f";
        item_num       = 1;
        symb           = Some("MULT");
        next_num       = Some(2);
        seq_number     = 1;
        s              = 0;
        f              = set [|3|];
        fromStateTrace = (List.ofArray [|((List.ofArray [| Some("MULT")|]),(List.ofArray [|(List.ofArray [|TSmbE(124);TSmbS(128)|])|]))|]);
        toStateTrace   = (List.ofArray [|((List.ofArray [| Some("MULT")|]),(List.ofArray [|(List.ofArray [|TSmbE(124);TSmbS(128)|])|]))|]);
       }|]);(-1729815040,set [||]);(263116985,set [|
       {
        prod_num       = 14;
        prod_name      = "t";
        item_num       = 3;
        symb           = Some("b");
        next_num       = None;
        seq_number     = 2;
        s              = 0;
        f              = set [|3|];
        fromStateTrace = (List.ofArray [|((List.ofArray [| Some("b")|]),(List.ofArray [|(List.ofArray [|TSmbE(114);TSmbS(118)|])|]));((List.ofArray [|None|]),(List.ofArray [|(List.ofArray [|TSmbE(118);TSeqE(120)|])|]))|]);
        toStateTrace   = (List.ofArray [|((List.ofArray [| Some("b")|]),(List.ofArray [|(List.ofArray [|TSmbE(114);TSmbS(118)|])|]));((List.ofArray [|None|]),(List.ofArray [|(List.ofArray [|TSmbE(118);TSeqE(120)|])|]))|]);
       };
       {
        prod_num       = 15;
        prod_name      = "f";
        item_num       = 1;
        symb           = Some("MULT");
        next_num       = Some(2);
        seq_number     = 1;
        s              = 0;
        f              = set [|3|];
        fromStateTrace = (List.ofArray [|((List.ofArray [| Some("MULT")|]),(List.ofArray [|(List.ofArray [|TSmbE(124);TSmbS(128)|])|]))|]);
        toStateTrace   = (List.ofArray [|((List.ofArray [| Some("MULT")|]),(List.ofArray [|(List.ofArray [|TSmbE(124);TSmbS(128)|])|]))|]);
       }|]);(-1197972348,set [|
       {
        prod_num       = 15;
        prod_name      = "f";
        item_num       = 1;
        symb           = Some("MULT");
        next_num       = Some(2);
        seq_number     = 1;
        s              = 0;
        f              = set [|3|];
        fromStateTrace = (List.ofArray [|((List.ofArray [| Some("MULT")|]),(List.ofArray [|(List.ofArray [|TSmbE(124);TSmbS(128)|])|]))|]);
        toStateTrace   = (List.ofArray [|((List.ofArray [| Some("MULT")|]),(List.ofArray [|(List.ofArray [|TSmbE(124);TSmbS(128)|])|]))|]);
       }|]);(263173468,set [|
       {
        prod_num       = 15;
        prod_name      = "f";
        item_num       = 1;
        symb           = Some("MULT");
        next_num       = Some(2);
        seq_number     = 1;
        s              = 0;
        f              = set [|3|];
        fromStateTrace = (List.ofArray [|((List.ofArray [| Some("MULT")|]),(List.ofArray [|(List.ofArray [|TSmbE(124);TSmbS(128)|])|]))|]);
        toStateTrace   = (List.ofArray [|((List.ofArray [| Some("MULT")|]),(List.ofArray [|(List.ofArray [|TSmbE(124);TSmbS(128)|])|]))|]);
       }|]);(-1973309743,set [||]);(263084377,set [||]);(-1197896220,set [||]);(262878529,set [||]);(-1197537222,set [||]);(262893778,set [||]);(-1197516503,set [||]);(262719744,set [||]);(-1197601733,set [||]);(262698455,set [||]);(-1197585044,set [||]);(568394758,set [||]);(-2041137603,set [||]);(261445031,set [||]);(-1198337828,set [||]);(262840818,set [||]);(-1197727863,set [||]);(568426533,set [||]);(-2041066786,set [||]);(1539474982,set [||]);(263184170,set [||]);(834939379,set [||]);(1986242574,set [||]);(262970397,set [||]);(-1197956570,set [||]);(568572360,set [||]);(-2041209741,set [||]);(1540190789,set [||]);(262951945,set [||]);(835173968,set [||]);(1986204909,set [||]);(1539364002,set [||]);(262786264,set [||]);(836401527,set [||]);(1986812042,set [||]);(568518639,set [||]);(-2039849196,set [||]);(263074801,set [||]);(-1729815037,set [||]);(263116986,set [||]);(-1197972345,set [||]);(263173471,set [||]);(-1973309742,set [||]);(263084378,set [|
       {
        prod_num       = 15;
        prod_name      = "f";
        item_num       = 3;
        symb           = Some("c");
        next_num       = None;
        seq_number     = 2;
        s              = 0;
        f              = set [|3|];
        fromStateTrace = (List.ofArray [|((List.ofArray [| Some("c")|]),(List.ofArray [|(List.ofArray [|TSmbE(128);TSmbS(132)|])|]));((List.ofArray [|None|]),(List.ofArray [|(List.ofArray [|TSmbE(132);TSeqE(134)|])|]))|]);
        toStateTrace   = (List.ofArray [|((List.ofArray [| Some("c")|]),(List.ofArray [|(List.ofArray [|TSmbE(128);TSmbS(132)|])|]));((List.ofArray [|None|]),(List.ofArray [|(List.ofArray [|TSmbE(132);TSeqE(134)|])|]))|]);
       }|]);(-1197896217,set [||]);(262878535,set [|
       {
        prod_num       = 1;
        prod_name      = "s";
        item_num       = 1;
        symb           = Some("e");
        next_num       = None;
        seq_number     = 0;
        s              = 0;
        f              = set [|1|];
        fromStateTrace = (List.ofArray [|((List.ofArray [| Some("e")|]),(List.ofArray [|(List.ofArray [|TSeqS(10);TSmbS(8)|])|]));((List.ofArray [|None|]),(List.ofArray [|(List.ofArray [|TSmbE(8);TSeqE(10)|])|]))|]);
        toStateTrace   = (List.ofArray [|((List.ofArray [| Some("e")|]),(List.ofArray [|(List.ofArray [|TSeqS(10);TSmbS(8)|])|]));((List.ofArray [|None|]),(List.ofArray [|(List.ofArray [|TSmbE(8);TSeqE(10)|])|]))|]);
       }|]);(-1197537220,set [|
       {
        prod_num       = 1;
        prod_name      = "s";
        item_num       = 1;
        symb           = Some("e");
        next_num       = None;
        seq_number     = 0;
        s              = 0;
        f              = set [|1|];
        fromStateTrace = (List.ofArray [|((List.ofArray [| Some("e")|]),(List.ofArray [|(List.ofArray [|TSeqS(10);TSmbS(8)|])|]));((List.ofArray [|None|]),(List.ofArray [|(List.ofArray [|TSmbE(8);TSeqE(10)|])|]))|]);
        toStateTrace   = (List.ofArray [|((List.ofArray [| Some("e")|]),(List.ofArray [|(List.ofArray [|TSeqS(10);TSmbS(8)|])|]));((List.ofArray [|None|]),(List.ofArray [|(List.ofArray [|TSmbE(8);TSeqE(10)|])|]))|]);
       }|]);(262893780,set [|
       {
        prod_num       = 1;
        prod_name      = "s";
        item_num       = 1;
        symb           = Some("e");
        next_num       = None;
        seq_number     = 0;
        s              = 0;
        f              = set [|1|];
        fromStateTrace = (List.ofArray [|((List.ofArray [| Some("e")|]),(List.ofArray [|(List.ofArray [|TSeqS(10);TSmbS(8)|])|]));((List.ofArray [|None|]),(List.ofArray [|(List.ofArray [|TSmbE(8);TSeqE(10)|])|]))|]);
        toStateTrace   = (List.ofArray [|((List.ofArray [| Some("e")|]),(List.ofArray [|(List.ofArray [|TSeqS(10);TSmbS(8)|])|]));((List.ofArray [|None|]),(List.ofArray [|(List.ofArray [|TSmbE(8);TSeqE(10)|])|]))|]);
       }|]);(-1197516497,set [||]);(262719750,set [||]);(-1197601731,set [||]);(262698449,set [||]);(-1197585046,set [||]);(568394752,set [||]);(-2041137605,set [||]);(261445025,set [||]);(-1198337830,set [||]);(262840820,set [||]);(-1197727857,set [||]);(568426531,set [||]);(-2041066792,set [||]);(1539474976,set [||]);(263184172,set [||]);(834939381,set [||]);(1986242568,set [||]);(262970395,set [||]);(-1197956576,set [||]);(568572366,set [||]);(-2041209739,set [||]);(1540190787,set [||]);(262951951,set [||]);(835173974,set [||]);(1986204907,set [||]);(1539364004,set [||]);(262786270,set [||]);(836401521,set [||]);(1986812044,set [||]);(568518633,set [||]);(-2039849198,set [||]);(263074807,set [||]);(-1729815035,set [||]);(263116988,set [||]);(-1197972351,set [||]);(263173465,set [||]);(-1973309740,set [||]);(263084380,set [||]);(-1197896223,set [||]);(262878534,set [|
       {
        prod_num       = 3;
        prod_name      = "e";
        item_num       = 1;
        symb           = Some("f");
        next_num       = None;
        seq_number     = 0;
        s              = 0;
        f              = set [|1|];
        fromStateTrace = (List.ofArray [|((List.ofArray [| Some("f")|]),(List.ofArray [|(List.ofArray [|TSeqS(22);TSmbS(20)|])|]));((List.ofArray [|None|]),(List.ofArray [|(List.ofArray [|TSmbE(20);TSeqE(22)|])|]))|]);
        toStateTrace   = (List.ofArray [|((List.ofArray [| Some("f")|]),(List.ofArray [|(List.ofArray [|TSeqS(22);TSmbS(20)|])|]));((List.ofArray [|None|]),(List.ofArray [|(List.ofArray [|TSmbE(20);TSeqE(22)|])|]))|]);
       };
       {
        prod_num       = 6;
        prod_name      = "a";
        item_num       = 1;
        symb           = Some("f");
        next_num       = None;
        seq_number     = 0;
        s              = 0;
        f              = set [|1|];
        fromStateTrace = (List.ofArray [|((List.ofArray [| Some("f")|]),(List.ofArray [|(List.ofArray [|TSeqS(40);TSmbS(38)|])|]));((List.ofArray [|None|]),(List.ofArray [|(List.ofArray [|TSmbE(38);TSeqE(40)|])|]))|]);
        toStateTrace   = (List.ofArray [|((List.ofArray [| Some("f")|]),(List.ofArray [|(List.ofArray [|TSeqS(40);TSmbS(38)|])|]));((List.ofArray [|None|]),(List.ofArray [|(List.ofArray [|TSmbE(38);TSeqE(40)|])|]))|]);
       };
       {
        prod_num       = 9;
        prod_name      = "b";
        item_num       = 1;
        symb           = Some("f");
        next_num       = None;
        seq_number     = 0;
        s              = 0;
        f              = set [|1|];
        fromStateTrace = (List.ofArray [|((List.ofArray [| Some("f")|]),(List.ofArray [|(List.ofArray [|TSeqS(66);TSmbS(64)|])|]));((List.ofArray [|None|]),(List.ofArray [|(List.ofArray [|TSmbE(64);TSeqE(66)|])|]))|]);
        toStateTrace   = (List.ofArray [|((List.ofArray [| Some("f")|]),(List.ofArray [|(List.ofArray [|TSeqS(66);TSmbS(64)|])|]));((List.ofArray [|None|]),(List.ofArray [|(List.ofArray [|TSmbE(64);TSeqE(66)|])|]))|]);
       }|]);(-1197537219,set [|
       {
        prod_num       = 3;
        prod_name      = "e";
        item_num       = 1;
        symb           = Some("f");
        next_num       = None;
        seq_number     = 0;
        s              = 0;
        f              = set [|1|];
        fromStateTrace = (List.ofArray [|((List.ofArray [| Some("f")|]),(List.ofArray [|(List.ofArray [|TSeqS(22);TSmbS(20)|])|]));((List.ofArray [|None|]),(List.ofArray [|(List.ofArray [|TSmbE(20);TSeqE(22)|])|]))|]);
        toStateTrace   = (List.ofArray [|((List.ofArray [| Some("f")|]),(List.ofArray [|(List.ofArray [|TSeqS(22);TSmbS(20)|])|]));((List.ofArray [|None|]),(List.ofArray [|(List.ofArray [|TSmbE(20);TSeqE(22)|])|]))|]);
       };
       {
        prod_num       = 6;
        prod_name      = "a";
        item_num       = 1;
        symb           = Some("f");
        next_num       = None;
        seq_number     = 0;
        s              = 0;
        f              = set [|1|];
        fromStateTrace = (List.ofArray [|((List.ofArray [| Some("f")|]),(List.ofArray [|(List.ofArray [|TSeqS(40);TSmbS(38)|])|]));((List.ofArray [|None|]),(List.ofArray [|(List.ofArray [|TSmbE(38);TSeqE(40)|])|]))|]);
        toStateTrace   = (List.ofArray [|((List.ofArray [| Some("f")|]),(List.ofArray [|(List.ofArray [|TSeqS(40);TSmbS(38)|])|]));((List.ofArray [|None|]),(List.ofArray [|(List.ofArray [|TSmbE(38);TSeqE(40)|])|]))|]);
       };
       {
        prod_num       = 9;
        prod_name      = "b";
        item_num       = 1;
        symb           = Some("f");
        next_num       = None;
        seq_number     = 0;
        s              = 0;
        f              = set [|1|];
        fromStateTrace = (List.ofArray [|((List.ofArray [| Some("f")|]),(List.ofArray [|(List.ofArray [|TSeqS(66);TSmbS(64)|])|]));((List.ofArray [|None|]),(List.ofArray [|(List.ofArray [|TSmbE(64);TSeqE(66)|])|]))|]);
        toStateTrace   = (List.ofArray [|((List.ofArray [| Some("f")|]),(List.ofArray [|(List.ofArray [|TSeqS(66);TSmbS(64)|])|]));((List.ofArray [|None|]),(List.ofArray [|(List.ofArray [|TSmbE(64);TSeqE(66)|])|]))|]);
       }|]);(262893781,set [|
       {
        prod_num       = 3;
        prod_name      = "e";
        item_num       = 1;
        symb           = Some("f");
        next_num       = None;
        seq_number     = 0;
        s              = 0;
        f              = set [|1|];
        fromStateTrace = (List.ofArray [|((List.ofArray [| Some("f")|]),(List.ofArray [|(List.ofArray [|TSeqS(22);TSmbS(20)|])|]));((List.ofArray [|None|]),(List.ofArray [|(List.ofArray [|TSmbE(20);TSeqE(22)|])|]))|]);
        toStateTrace   = (List.ofArray [|((List.ofArray [| Some("f")|]),(List.ofArray [|(List.ofArray [|TSeqS(22);TSmbS(20)|])|]));((List.ofArray [|None|]),(List.ofArray [|(List.ofArray [|TSmbE(20);TSeqE(22)|])|]))|]);
       };
       {
        prod_num       = 6;
        prod_name      = "a";
        item_num       = 1;
        symb           = Some("f");
        next_num       = None;
        seq_number     = 0;
        s              = 0;
        f              = set [|1|];
        fromStateTrace = (List.ofArray [|((List.ofArray [| Some("f")|]),(List.ofArray [|(List.ofArray [|TSeqS(40);TSmbS(38)|])|]));((List.ofArray [|None|]),(List.ofArray [|(List.ofArray [|TSmbE(38);TSeqE(40)|])|]))|]);
        toStateTrace   = (List.ofArray [|((List.ofArray [| Some("f")|]),(List.ofArray [|(List.ofArray [|TSeqS(40);TSmbS(38)|])|]));((List.ofArray [|None|]),(List.ofArray [|(List.ofArray [|TSmbE(38);TSeqE(40)|])|]))|]);
       };
       {
        prod_num       = 9;
        prod_name      = "b";
        item_num       = 1;
        symb           = Some("f");
        next_num       = None;
        seq_number     = 0;
        s              = 0;
        f              = set [|1|];
        fromStateTrace = (List.ofArray [|((List.ofArray [| Some("f")|]),(List.ofArray [|(List.ofArray [|TSeqS(66);TSmbS(64)|])|]));((List.ofArray [|None|]),(List.ofArray [|(List.ofArray [|TSmbE(64);TSeqE(66)|])|]))|]);
        toStateTrace   = (List.ofArray [|((List.ofArray [| Some("f")|]),(List.ofArray [|(List.ofArray [|TSeqS(66);TSmbS(64)|])|]));((List.ofArray [|None|]),(List.ofArray [|(List.ofArray [|TSmbE(64);TSeqE(66)|])|]))|]);
       }|]);(-1197516498,set [|
       {
        prod_num       = 3;
        prod_name      = "e";
        item_num       = 1;
        symb           = Some("f");
        next_num       = None;
        seq_number     = 0;
        s              = 0;
        f              = set [|1|];
        fromStateTrace = (List.ofArray [|((List.ofArray [| Some("f")|]),(List.ofArray [|(List.ofArray [|TSeqS(22);TSmbS(20)|])|]));((List.ofArray [|None|]),(List.ofArray [|(List.ofArray [|TSmbE(20);TSeqE(22)|])|]))|]);
        toStateTrace   = (List.ofArray [|((List.ofArray [| Some("f")|]),(List.ofArray [|(List.ofArray [|TSeqS(22);TSmbS(20)|])|]));((List.ofArray [|None|]),(List.ofArray [|(List.ofArray [|TSmbE(20);TSeqE(22)|])|]))|]);
       };
       {
        prod_num       = 6;
        prod_name      = "a";
        item_num       = 1;
        symb           = Some("f");
        next_num       = None;
        seq_number     = 0;
        s              = 0;
        f              = set [|1|];
        fromStateTrace = (List.ofArray [|((List.ofArray [| Some("f")|]),(List.ofArray [|(List.ofArray [|TSeqS(40);TSmbS(38)|])|]));((List.ofArray [|None|]),(List.ofArray [|(List.ofArray [|TSmbE(38);TSeqE(40)|])|]))|]);
        toStateTrace   = (List.ofArray [|((List.ofArray [| Some("f")|]),(List.ofArray [|(List.ofArray [|TSeqS(40);TSmbS(38)|])|]));((List.ofArray [|None|]),(List.ofArray [|(List.ofArray [|TSmbE(38);TSeqE(40)|])|]))|]);
       };
       {
        prod_num       = 9;
        prod_name      = "b";
        item_num       = 1;
        symb           = Some("f");
        next_num       = None;
        seq_number     = 0;
        s              = 0;
        f              = set [|1|];
        fromStateTrace = (List.ofArray [|((List.ofArray [| Some("f")|]),(List.ofArray [|(List.ofArray [|TSeqS(66);TSmbS(64)|])|]));((List.ofArray [|None|]),(List.ofArray [|(List.ofArray [|TSmbE(64);TSeqE(66)|])|]))|]);
        toStateTrace   = (List.ofArray [|((List.ofArray [| Some("f")|]),(List.ofArray [|(List.ofArray [|TSeqS(66);TSmbS(64)|])|]));((List.ofArray [|None|]),(List.ofArray [|(List.ofArray [|TSmbE(64);TSeqE(66)|])|]))|]);
       }|]);(262719751,set [|
       {
        prod_num       = 6;
        prod_name      = "a";
        item_num       = 1;
        symb           = Some("f");
        next_num       = None;
        seq_number     = 0;
        s              = 0;
        f              = set [|1|];
        fromStateTrace = (List.ofArray [|((List.ofArray [| Some("f")|]),(List.ofArray [|(List.ofArray [|TSeqS(40);TSmbS(38)|])|]));((List.ofArray [|None|]),(List.ofArray [|(List.ofArray [|TSmbE(38);TSeqE(40)|])|]))|]);
        toStateTrace   = (List.ofArray [|((List.ofArray [| Some("f")|]),(List.ofArray [|(List.ofArray [|TSeqS(40);TSmbS(38)|])|]));((List.ofArray [|None|]),(List.ofArray [|(List.ofArray [|TSmbE(38);TSeqE(40)|])|]))|]);
       };
       {
        prod_num       = 9;
        prod_name      = "b";
        item_num       = 1;
        symb           = Some("f");
        next_num       = None;
        seq_number     = 0;
        s              = 0;
        f              = set [|1|];
        fromStateTrace = (List.ofArray [|((List.ofArray [| Some("f")|]),(List.ofArray [|(List.ofArray [|TSeqS(66);TSmbS(64)|])|]));((List.ofArray [|None|]),(List.ofArray [|(List.ofArray [|TSmbE(64);TSeqE(66)|])|]))|]);
        toStateTrace   = (List.ofArray [|((List.ofArray [| Some("f")|]),(List.ofArray [|(List.ofArray [|TSeqS(66);TSmbS(64)|])|]));((List.ofArray [|None|]),(List.ofArray [|(List.ofArray [|TSmbE(64);TSeqE(66)|])|]))|]);
       }|]);(-1197601732,set [|
       {
        prod_num       = 6;
        prod_name      = "a";
        item_num       = 1;
        symb           = Some("f");
        next_num       = None;
        seq_number     = 0;
        s              = 0;
        f              = set [|1|];
        fromStateTrace = (List.ofArray [|((List.ofArray [| Some("f")|]),(List.ofArray [|(List.ofArray [|TSeqS(40);TSmbS(38)|])|]));((List.ofArray [|None|]),(List.ofArray [|(List.ofArray [|TSmbE(38);TSeqE(40)|])|]))|]);
        toStateTrace   = (List.ofArray [|((List.ofArray [| Some("f")|]),(List.ofArray [|(List.ofArray [|TSeqS(40);TSmbS(38)|])|]));((List.ofArray [|None|]),(List.ofArray [|(List.ofArray [|TSmbE(38);TSeqE(40)|])|]))|]);
       };
       {
        prod_num       = 9;
        prod_name      = "b";
        item_num       = 1;
        symb           = Some("f");
        next_num       = None;
        seq_number     = 0;
        s              = 0;
        f              = set [|1|];
        fromStateTrace = (List.ofArray [|((List.ofArray [| Some("f")|]),(List.ofArray [|(List.ofArray [|TSeqS(66);TSmbS(64)|])|]));((List.ofArray [|None|]),(List.ofArray [|(List.ofArray [|TSmbE(64);TSeqE(66)|])|]))|]);
        toStateTrace   = (List.ofArray [|((List.ofArray [| Some("f")|]),(List.ofArray [|(List.ofArray [|TSeqS(66);TSmbS(64)|])|]));((List.ofArray [|None|]),(List.ofArray [|(List.ofArray [|TSmbE(64);TSeqE(66)|])|]))|]);
       }|]);(262698448,set [|
       {
        prod_num       = 3;
        prod_name      = "e";
        item_num       = 1;
        symb           = Some("f");
        next_num       = None;
        seq_number     = 0;
        s              = 0;
        f              = set [|1|];
        fromStateTrace = (List.ofArray [|((List.ofArray [| Some("f")|]),(List.ofArray [|(List.ofArray [|TSeqS(22);TSmbS(20)|])|]));((List.ofArray [|None|]),(List.ofArray [|(List.ofArray [|TSmbE(20);TSeqE(22)|])|]))|]);
        toStateTrace   = (List.ofArray [|((List.ofArray [| Some("f")|]),(List.ofArray [|(List.ofArray [|TSeqS(22);TSmbS(20)|])|]));((List.ofArray [|None|]),(List.ofArray [|(List.ofArray [|TSmbE(20);TSeqE(22)|])|]))|]);
       };
       {
        prod_num       = 9;
        prod_name      = "b";
        item_num       = 1;
        symb           = Some("f");
        next_num       = None;
        seq_number     = 0;
        s              = 0;
        f              = set [|1|];
        fromStateTrace = (List.ofArray [|((List.ofArray [| Some("f")|]),(List.ofArray [|(List.ofArray [|TSeqS(66);TSmbS(64)|])|]));((List.ofArray [|None|]),(List.ofArray [|(List.ofArray [|TSmbE(64);TSeqE(66)|])|]))|]);
        toStateTrace   = (List.ofArray [|((List.ofArray [| Some("f")|]),(List.ofArray [|(List.ofArray [|TSeqS(66);TSmbS(64)|])|]));((List.ofArray [|None|]),(List.ofArray [|(List.ofArray [|TSmbE(64);TSeqE(66)|])|]))|]);
       }|]);(-1197585045,set [|
       {
        prod_num       = 9;
        prod_name      = "b";
        item_num       = 1;
        symb           = Some("f");
        next_num       = None;
        seq_number     = 0;
        s              = 0;
        f              = set [|1|];
        fromStateTrace = (List.ofArray [|((List.ofArray [| Some("f")|]),(List.ofArray [|(List.ofArray [|TSeqS(66);TSmbS(64)|])|]));((List.ofArray [|None|]),(List.ofArray [|(List.ofArray [|TSmbE(64);TSeqE(66)|])|]))|]);
        toStateTrace   = (List.ofArray [|((List.ofArray [| Some("f")|]),(List.ofArray [|(List.ofArray [|TSeqS(66);TSmbS(64)|])|]));((List.ofArray [|None|]),(List.ofArray [|(List.ofArray [|TSmbE(64);TSeqE(66)|])|]))|]);
       }|]);(568394753,set [||]);(-2041137606,set [||]);(261445024,set [|
       {
        prod_num       = 6;
        prod_name      = "a";
        item_num       = 1;
        symb           = Some("f");
        next_num       = None;
        seq_number     = 0;
        s              = 0;
        f              = set [|1|];
        fromStateTrace = (List.ofArray [|((List.ofArray [| Some("f")|]),(List.ofArray [|(List.ofArray [|TSeqS(40);TSmbS(38)|])|]));((List.ofArray [|None|]),(List.ofArray [|(List.ofArray [|TSmbE(38);TSeqE(40)|])|]))|]);
        toStateTrace   = (List.ofArray [|((List.ofArray [| Some("f")|]),(List.ofArray [|(List.ofArray [|TSeqS(40);TSmbS(38)|])|]));((List.ofArray [|None|]),(List.ofArray [|(List.ofArray [|TSmbE(38);TSeqE(40)|])|]))|]);
       };
       {
        prod_num       = 9;
        prod_name      = "b";
        item_num       = 1;
        symb           = Some("f");
        next_num       = None;
        seq_number     = 0;
        s              = 0;
        f              = set [|1|];
        fromStateTrace = (List.ofArray [|((List.ofArray [| Some("f")|]),(List.ofArray [|(List.ofArray [|TSeqS(66);TSmbS(64)|])|]));((List.ofArray [|None|]),(List.ofArray [|(List.ofArray [|TSmbE(64);TSeqE(66)|])|]))|]);
        toStateTrace   = (List.ofArray [|((List.ofArray [| Some("f")|]),(List.ofArray [|(List.ofArray [|TSeqS(66);TSmbS(64)|])|]));((List.ofArray [|None|]),(List.ofArray [|(List.ofArray [|TSmbE(64);TSeqE(66)|])|]))|]);
       }|]);(-1198337829,set [|
       {
        prod_num       = 6;
        prod_name      = "a";
        item_num       = 1;
        symb           = Some("f");
        next_num       = None;
        seq_number     = 0;
        s              = 0;
        f              = set [|1|];
        fromStateTrace = (List.ofArray [|((List.ofArray [| Some("f")|]),(List.ofArray [|(List.ofArray [|TSeqS(40);TSmbS(38)|])|]));((List.ofArray [|None|]),(List.ofArray [|(List.ofArray [|TSmbE(38);TSeqE(40)|])|]))|]);
        toStateTrace   = (List.ofArray [|((List.ofArray [| Some("f")|]),(List.ofArray [|(List.ofArray [|TSeqS(40);TSmbS(38)|])|]));((List.ofArray [|None|]),(List.ofArray [|(List.ofArray [|TSmbE(38);TSeqE(40)|])|]))|]);
       };
       {
        prod_num       = 9;
        prod_name      = "b";
        item_num       = 1;
        symb           = Some("f");
        next_num       = None;
        seq_number     = 0;
        s              = 0;
        f              = set [|1|];
        fromStateTrace = (List.ofArray [|((List.ofArray [| Some("f")|]),(List.ofArray [|(List.ofArray [|TSeqS(66);TSmbS(64)|])|]));((List.ofArray [|None|]),(List.ofArray [|(List.ofArray [|TSmbE(64);TSeqE(66)|])|]))|]);
        toStateTrace   = (List.ofArray [|((List.ofArray [| Some("f")|]),(List.ofArray [|(List.ofArray [|TSeqS(66);TSmbS(64)|])|]));((List.ofArray [|None|]),(List.ofArray [|(List.ofArray [|TSmbE(64);TSeqE(66)|])|]))|]);
       }|]);(262840821,set [|
       {
        prod_num       = 6;
        prod_name      = "a";
        item_num       = 1;
        symb           = Some("f");
        next_num       = None;
        seq_number     = 0;
        s              = 0;
        f              = set [|1|];
        fromStateTrace = (List.ofArray [|((List.ofArray [| Some("f")|]),(List.ofArray [|(List.ofArray [|TSeqS(40);TSmbS(38)|])|]));((List.ofArray [|None|]),(List.ofArray [|(List.ofArray [|TSmbE(38);TSeqE(40)|])|]))|]);
        toStateTrace   = (List.ofArray [|((List.ofArray [| Some("f")|]),(List.ofArray [|(List.ofArray [|TSeqS(40);TSmbS(38)|])|]));((List.ofArray [|None|]),(List.ofArray [|(List.ofArray [|TSmbE(38);TSeqE(40)|])|]))|]);
       };
       {
        prod_num       = 9;
        prod_name      = "b";
        item_num       = 1;
        symb           = Some("f");
        next_num       = None;
        seq_number     = 0;
        s              = 0;
        f              = set [|1|];
        fromStateTrace = (List.ofArray [|((List.ofArray [| Some("f")|]),(List.ofArray [|(List.ofArray [|TSeqS(66);TSmbS(64)|])|]));((List.ofArray [|None|]),(List.ofArray [|(List.ofArray [|TSmbE(64);TSeqE(66)|])|]))|]);
        toStateTrace   = (List.ofArray [|((List.ofArray [| Some("f")|]),(List.ofArray [|(List.ofArray [|TSeqS(66);TSmbS(64)|])|]));((List.ofArray [|None|]),(List.ofArray [|(List.ofArray [|TSmbE(64);TSeqE(66)|])|]))|]);
       }|]);(-1197727858,set [|
       {
        prod_num       = 9;
        prod_name      = "b";
        item_num       = 1;
        symb           = Some("f");
        next_num       = None;
        seq_number     = 0;
        s              = 0;
        f              = set [|1|];
        fromStateTrace = (List.ofArray [|((List.ofArray [| Some("f")|]),(List.ofArray [|(List.ofArray [|TSeqS(66);TSmbS(64)|])|]));((List.ofArray [|None|]),(List.ofArray [|(List.ofArray [|TSmbE(64);TSeqE(66)|])|]))|]);
        toStateTrace   = (List.ofArray [|((List.ofArray [| Some("f")|]),(List.ofArray [|(List.ofArray [|TSeqS(66);TSmbS(64)|])|]));((List.ofArray [|None|]),(List.ofArray [|(List.ofArray [|TSmbE(64);TSeqE(66)|])|]))|]);
       }|]);(568426530,set [||]);(-2041066791,set [||]);(1539474977,set [||]);(263184173,set [|
       {
        prod_num       = 6;
        prod_name      = "a";
        item_num       = 1;
        symb           = Some("f");
        next_num       = None;
        seq_number     = 0;
        s              = 0;
        f              = set [|1|];
        fromStateTrace = (List.ofArray [|((List.ofArray [| Some("f")|]),(List.ofArray [|(List.ofArray [|TSeqS(40);TSmbS(38)|])|]));((List.ofArray [|None|]),(List.ofArray [|(List.ofArray [|TSmbE(38);TSeqE(40)|])|]))|]);
        toStateTrace   = (List.ofArray [|((List.ofArray [| Some("f")|]),(List.ofArray [|(List.ofArray [|TSeqS(40);TSmbS(38)|])|]));((List.ofArray [|None|]),(List.ofArray [|(List.ofArray [|TSmbE(38);TSeqE(40)|])|]))|]);
       };
       {
        prod_num       = 9;
        prod_name      = "b";
        item_num       = 1;
        symb           = Some("f");
        next_num       = None;
        seq_number     = 0;
        s              = 0;
        f              = set [|1|];
        fromStateTrace = (List.ofArray [|((List.ofArray [| Some("f")|]),(List.ofArray [|(List.ofArray [|TSeqS(66);TSmbS(64)|])|]));((List.ofArray [|None|]),(List.ofArray [|(List.ofArray [|TSmbE(64);TSeqE(66)|])|]))|]);
        toStateTrace   = (List.ofArray [|((List.ofArray [| Some("f")|]),(List.ofArray [|(List.ofArray [|TSeqS(66);TSmbS(64)|])|]));((List.ofArray [|None|]),(List.ofArray [|(List.ofArray [|TSmbE(64);TSeqE(66)|])|]))|]);
       }|]);(834939380,set [||]);(1986242569,set [||]);(262970394,set [|
       {
        prod_num       = 9;
        prod_name      = "b";
        item_num       = 1;
        symb           = Some("f");
        next_num       = None;
        seq_number     = 0;
        s              = 0;
        f              = set [|1|];
        fromStateTrace = (List.ofArray [|((List.ofArray [| Some("f")|]),(List.ofArray [|(List.ofArray [|TSeqS(66);TSmbS(64)|])|]));((List.ofArray [|None|]),(List.ofArray [|(List.ofArray [|TSmbE(64);TSeqE(66)|])|]))|]);
        toStateTrace   = (List.ofArray [|((List.ofArray [| Some("f")|]),(List.ofArray [|(List.ofArray [|TSeqS(66);TSmbS(64)|])|]));((List.ofArray [|None|]),(List.ofArray [|(List.ofArray [|TSmbE(64);TSeqE(66)|])|]))|]);
       }|]);(-1197956575,set [|
       {
        prod_num       = 9;
        prod_name      = "b";
        item_num       = 1;
        symb           = Some("f");
        next_num       = None;
        seq_number     = 0;
        s              = 0;
        f              = set [|1|];
        fromStateTrace = (List.ofArray [|((List.ofArray [| Some("f")|]),(List.ofArray [|(List.ofArray [|TSeqS(66);TSmbS(64)|])|]));((List.ofArray [|None|]),(List.ofArray [|(List.ofArray [|TSmbE(64);TSeqE(66)|])|]))|]);
        toStateTrace   = (List.ofArray [|((List.ofArray [| Some("f")|]),(List.ofArray [|(List.ofArray [|TSeqS(66);TSmbS(64)|])|]));((List.ofArray [|None|]),(List.ofArray [|(List.ofArray [|TSmbE(64);TSeqE(66)|])|]))|]);
       }|]);(568572367,set [||]);(-2041209740,set [||]);(1540190786,set [||]);(262951950,set [|
       {
        prod_num       = 6;
        prod_name      = "a";
        item_num       = 1;
        symb           = Some("f");
        next_num       = None;
        seq_number     = 0;
        s              = 0;
        f              = set [|1|];
        fromStateTrace = (List.ofArray [|((List.ofArray [| Some("f")|]),(List.ofArray [|(List.ofArray [|TSeqS(40);TSmbS(38)|])|]));((List.ofArray [|None|]),(List.ofArray [|(List.ofArray [|TSmbE(38);TSeqE(40)|])|]))|]);
        toStateTrace   = (List.ofArray [|((List.ofArray [| Some("f")|]),(List.ofArray [|(List.ofArray [|TSeqS(40);TSmbS(38)|])|]));((List.ofArray [|None|]),(List.ofArray [|(List.ofArray [|TSmbE(38);TSeqE(40)|])|]))|]);
       };
       {
        prod_num       = 9;
        prod_name      = "b";
        item_num       = 1;
        symb           = Some("f");
        next_num       = None;
        seq_number     = 0;
        s              = 0;
        f              = set [|1|];
        fromStateTrace = (List.ofArray [|((List.ofArray [| Some("f")|]),(List.ofArray [|(List.ofArray [|TSeqS(66);TSmbS(64)|])|]));((List.ofArray [|None|]),(List.ofArray [|(List.ofArray [|TSmbE(64);TSeqE(66)|])|]))|]);
        toStateTrace   = (List.ofArray [|((List.ofArray [| Some("f")|]),(List.ofArray [|(List.ofArray [|TSeqS(66);TSmbS(64)|])|]));((List.ofArray [|None|]),(List.ofArray [|(List.ofArray [|TSmbE(64);TSeqE(66)|])|]))|]);
       }|]);(835173975,set [||]);(1986204906,set [||]);(1539364005,set [||]);(262786271,set [|
       {
        prod_num       = 12;
        prod_name      = "c";
        item_num       = 2;
        symb           = Some("RIGHT");
        next_num       = Some(3);
        seq_number     = 2;
        s              = 0;
        f              = set [|3|];
        fromStateTrace = (List.ofArray [|((List.ofArray [| Some("RIGHT")|]),(List.ofArray [|(List.ofArray [|TSmbE(94);TSmbS(98)|])|]));((List.ofArray [|None|]),(List.ofArray [|(List.ofArray [|TSmbE(98);TSeqE(100)|])|]))|]);
        toStateTrace   = (List.ofArray [|((List.ofArray [| Some("RIGHT")|]),(List.ofArray [|(List.ofArray [|TSmbE(94);TSmbS(98)|])|]));((List.ofArray [|None|]),(List.ofArray [|(List.ofArray [|TSmbE(98);TSeqE(100)|])|]))|]);
       };
       {
        prod_num       = 9;
        prod_name      = "b";
        item_num       = 1;
        symb           = Some("f");
        next_num       = None;
        seq_number     = 0;
        s              = 0;
        f              = set [|1|];
        fromStateTrace = (List.ofArray [|((List.ofArray [| Some("f")|]),(List.ofArray [|(List.ofArray [|TSeqS(66);TSmbS(64)|])|]));((List.ofArray [|None|]),(List.ofArray [|(List.ofArray [|TSmbE(64);TSeqE(66)|])|]))|]);
        toStateTrace   = (List.ofArray [|((List.ofArray [| Some("f")|]),(List.ofArray [|(List.ofArray [|TSeqS(66);TSmbS(64)|])|]));((List.ofArray [|None|]),(List.ofArray [|(List.ofArray [|TSmbE(64);TSeqE(66)|])|]))|]);
       }|]);(836401520,set [||]);(1986812045,set [||]);(568518632,set [||]);(-2039849197,set [||]);(263074806,set [|
       {
        prod_num       = 6;
        prod_name      = "a";
        item_num       = 1;
        symb           = Some("f");
        next_num       = None;
        seq_number     = 0;
        s              = 0;
        f              = set [|1|];
        fromStateTrace = (List.ofArray [|((List.ofArray [| Some("f")|]),(List.ofArray [|(List.ofArray [|TSeqS(40);TSmbS(38)|])|]));((List.ofArray [|None|]),(List.ofArray [|(List.ofArray [|TSmbE(38);TSeqE(40)|])|]))|]);
        toStateTrace   = (List.ofArray [|((List.ofArray [| Some("f")|]),(List.ofArray [|(List.ofArray [|TSeqS(40);TSmbS(38)|])|]));((List.ofArray [|None|]),(List.ofArray [|(List.ofArray [|TSmbE(38);TSeqE(40)|])|]))|]);
       };
       {
        prod_num       = 9;
        prod_name      = "b";
        item_num       = 1;
        symb           = Some("f");
        next_num       = None;
        seq_number     = 0;
        s              = 0;
        f              = set [|1|];
        fromStateTrace = (List.ofArray [|((List.ofArray [| Some("f")|]),(List.ofArray [|(List.ofArray [|TSeqS(66);TSmbS(64)|])|]));((List.ofArray [|None|]),(List.ofArray [|(List.ofArray [|TSmbE(64);TSeqE(66)|])|]))|]);
        toStateTrace   = (List.ofArray [|((List.ofArray [| Some("f")|]),(List.ofArray [|(List.ofArray [|TSeqS(66);TSmbS(64)|])|]));((List.ofArray [|None|]),(List.ofArray [|(List.ofArray [|TSmbE(64);TSeqE(66)|])|]))|]);
       }|]);(-1729815036,set [||]);(263116989,set [|
       {
        prod_num       = 9;
        prod_name      = "b";
        item_num       = 1;
        symb           = Some("f");
        next_num       = None;
        seq_number     = 0;
        s              = 0;
        f              = set [|1|];
        fromStateTrace = (List.ofArray [|((List.ofArray [| Some("f")|]),(List.ofArray [|(List.ofArray [|TSeqS(66);TSmbS(64)|])|]));((List.ofArray [|None|]),(List.ofArray [|(List.ofArray [|TSmbE(64);TSeqE(66)|])|]))|]);
        toStateTrace   = (List.ofArray [|((List.ofArray [| Some("f")|]),(List.ofArray [|(List.ofArray [|TSeqS(66);TSmbS(64)|])|]));((List.ofArray [|None|]),(List.ofArray [|(List.ofArray [|TSmbE(64);TSeqE(66)|])|]))|]);
       }|]);(-1197972352,set [|
       {
        prod_num       = 9;
        prod_name      = "b";
        item_num       = 1;
        symb           = Some("f");
        next_num       = None;
        seq_number     = 0;
        s              = 0;
        f              = set [|1|];
        fromStateTrace = (List.ofArray [|((List.ofArray [| Some("f")|]),(List.ofArray [|(List.ofArray [|TSeqS(66);TSmbS(64)|])|]));((List.ofArray [|None|]),(List.ofArray [|(List.ofArray [|TSmbE(64);TSeqE(66)|])|]))|]);
        toStateTrace   = (List.ofArray [|((List.ofArray [| Some("f")|]),(List.ofArray [|(List.ofArray [|TSeqS(66);TSmbS(64)|])|]));((List.ofArray [|None|]),(List.ofArray [|(List.ofArray [|TSmbE(64);TSeqE(66)|])|]))|]);
       }|]);(263173464,set [|
       {
        prod_num       = 9;
        prod_name      = "b";
        item_num       = 1;
        symb           = Some("f");
        next_num       = None;
        seq_number     = 0;
        s              = 0;
        f              = set [|1|];
        fromStateTrace = (List.ofArray [|((List.ofArray [| Some("f")|]),(List.ofArray [|(List.ofArray [|TSeqS(66);TSmbS(64)|])|]));((List.ofArray [|None|]),(List.ofArray [|(List.ofArray [|TSmbE(64);TSeqE(66)|])|]))|]);
        toStateTrace   = (List.ofArray [|((List.ofArray [| Some("f")|]),(List.ofArray [|(List.ofArray [|TSeqS(66);TSmbS(64)|])|]));((List.ofArray [|None|]),(List.ofArray [|(List.ofArray [|TSmbE(64);TSeqE(66)|])|]))|]);
       }|]);(-1973309739,set [||]);(263084381,set [||]);(-1197896224,set [||]);(262878513,set [|
       {
        prod_num       = 0;
        prod_name      = "_yard_start";
        item_num       = 1;
        symb           = Some("s");
        next_num       = None;
        seq_number     = 0;
        s              = 0;
        f              = set [|1|];
        fromStateTrace = (List.ofArray [|((List.ofArray [| Some("s")|]),(List.ofArray [|(List.ofArray [|TSeqS(4);TSmbS(2)|])|]));((List.ofArray [|None|]),(List.ofArray [|(List.ofArray [|TSmbE(2);TSeqE(4)|])|]))|]);
        toStateTrace   = (List.ofArray [|((List.ofArray [| Some("s")|]),(List.ofArray [|(List.ofArray [|TSeqS(4);TSmbS(2)|])|]));((List.ofArray [|None|]),(List.ofArray [|(List.ofArray [|TSmbE(2);TSeqE(4)|])|]))|]);
       }|]);(-1197537206,set [||]);(262893730,set [||]);(-1197516455,set [||]);(262719856,set [||]);(-1197601717,set [||]);(262698407,set [||]);(-1197585124,set [||]);(568394870,set [||]);(-2041137587,set [||]);(261445079,set [||]);(-1198337876,set [||]);(262840706,set [||]);(-1197727751,set [||]);(568426581,set [||]);(-2041066834,set [||]);(1539475030,set [||]);(263184218,set [||]);(834939267,set [||]);(1986242686,set [||]);(262970477,set [||]);(-1197956522,set [||]);(568572344,set [||]);(-2041209853,set [||]);(1540190773,set [||]);(262952057,set [||]);(835173920,set [||]);(1986204829,set [||]);(1539364050,set [||]);(262786216,set [||]);(836401415,set [||]);(1986812154,set [||]);(568518559,set [||]);(-2039849116,set [||]);(263074689,set [||]);(-1729814925,set [||]);(263117002,set [||]);(-1197972233,set [||]);(263173423,set [||]);(-1973309790,set [||]);(263084330,set [||]);(-1197896297,set [||]);(262878512,set [|
       {
        prod_num       = 2;
        prod_name      = "e";
        item_num       = 1;
        symb           = Some("t");
        next_num       = None;
        seq_number     = 0;
        s              = 0;
        f              = set [|1|];
        fromStateTrace = (List.ofArray [|((List.ofArray [| Some("t")|]),(List.ofArray [|(List.ofArray [|TSeqS(16);TSmbS(14)|])|]));((List.ofArray [|None|]),(List.ofArray [|(List.ofArray [|TSmbE(14);TSeqE(16)|])|]))|]);
        toStateTrace   = (List.ofArray [|((List.ofArray [| Some("t")|]),(List.ofArray [|(List.ofArray [|TSeqS(16);TSmbS(14)|])|]));((List.ofArray [|None|]),(List.ofArray [|(List.ofArray [|TSmbE(14);TSeqE(16)|])|]))|]);
       };
       {
        prod_num       = 5;
        prod_name      = "a";
        item_num       = 1;
        symb           = Some("t");
        next_num       = None;
        seq_number     = 0;
        s              = 0;
        f              = set [|1|];
        fromStateTrace = (List.ofArray [|((List.ofArray [| Some("t")|]),(List.ofArray [|(List.ofArray [|TSeqS(34);TSmbS(32)|])|]));((List.ofArray [|None|]),(List.ofArray [|(List.ofArray [|TSmbE(32);TSeqE(34)|])|]))|]);
        toStateTrace   = (List.ofArray [|((List.ofArray [| Some("t")|]),(List.ofArray [|(List.ofArray [|TSeqS(34);TSmbS(32)|])|]));((List.ofArray [|None|]),(List.ofArray [|(List.ofArray [|TSmbE(32);TSeqE(34)|])|]))|]);
       }|]);(-1197537205,set [|
       {
        prod_num       = 2;
        prod_name      = "e";
        item_num       = 1;
        symb           = Some("t");
        next_num       = None;
        seq_number     = 0;
        s              = 0;
        f              = set [|1|];
        fromStateTrace = (List.ofArray [|((List.ofArray [| Some("t")|]),(List.ofArray [|(List.ofArray [|TSeqS(16);TSmbS(14)|])|]));((List.ofArray [|None|]),(List.ofArray [|(List.ofArray [|TSmbE(14);TSeqE(16)|])|]))|]);
        toStateTrace   = (List.ofArray [|((List.ofArray [| Some("t")|]),(List.ofArray [|(List.ofArray [|TSeqS(16);TSmbS(14)|])|]));((List.ofArray [|None|]),(List.ofArray [|(List.ofArray [|TSmbE(14);TSeqE(16)|])|]))|]);
       };
       {
        prod_num       = 5;
        prod_name      = "a";
        item_num       = 1;
        symb           = Some("t");
        next_num       = None;
        seq_number     = 0;
        s              = 0;
        f              = set [|1|];
        fromStateTrace = (List.ofArray [|((List.ofArray [| Some("t")|]),(List.ofArray [|(List.ofArray [|TSeqS(34);TSmbS(32)|])|]));((List.ofArray [|None|]),(List.ofArray [|(List.ofArray [|TSmbE(32);TSeqE(34)|])|]))|]);
        toStateTrace   = (List.ofArray [|((List.ofArray [| Some("t")|]),(List.ofArray [|(List.ofArray [|TSeqS(34);TSmbS(32)|])|]));((List.ofArray [|None|]),(List.ofArray [|(List.ofArray [|TSmbE(32);TSeqE(34)|])|]))|]);
       }|]);(262893731,set [|
       {
        prod_num       = 2;
        prod_name      = "e";
        item_num       = 1;
        symb           = Some("t");
        next_num       = None;
        seq_number     = 0;
        s              = 0;
        f              = set [|1|];
        fromStateTrace = (List.ofArray [|((List.ofArray [| Some("t")|]),(List.ofArray [|(List.ofArray [|TSeqS(16);TSmbS(14)|])|]));((List.ofArray [|None|]),(List.ofArray [|(List.ofArray [|TSmbE(14);TSeqE(16)|])|]))|]);
        toStateTrace   = (List.ofArray [|((List.ofArray [| Some("t")|]),(List.ofArray [|(List.ofArray [|TSeqS(16);TSmbS(14)|])|]));((List.ofArray [|None|]),(List.ofArray [|(List.ofArray [|TSmbE(14);TSeqE(16)|])|]))|]);
       };
       {
        prod_num       = 5;
        prod_name      = "a";
        item_num       = 1;
        symb           = Some("t");
        next_num       = None;
        seq_number     = 0;
        s              = 0;
        f              = set [|1|];
        fromStateTrace = (List.ofArray [|((List.ofArray [| Some("t")|]),(List.ofArray [|(List.ofArray [|TSeqS(34);TSmbS(32)|])|]));((List.ofArray [|None|]),(List.ofArray [|(List.ofArray [|TSmbE(32);TSeqE(34)|])|]))|]);
        toStateTrace   = (List.ofArray [|((List.ofArray [| Some("t")|]),(List.ofArray [|(List.ofArray [|TSeqS(34);TSmbS(32)|])|]));((List.ofArray [|None|]),(List.ofArray [|(List.ofArray [|TSmbE(32);TSeqE(34)|])|]))|]);
       }|]);(-1197516456,set [|
       {
        prod_num       = 2;
        prod_name      = "e";
        item_num       = 1;
        symb           = Some("t");
        next_num       = None;
        seq_number     = 0;
        s              = 0;
        f              = set [|1|];
        fromStateTrace = (List.ofArray [|((List.ofArray [| Some("t")|]),(List.ofArray [|(List.ofArray [|TSeqS(16);TSmbS(14)|])|]));((List.ofArray [|None|]),(List.ofArray [|(List.ofArray [|TSmbE(14);TSeqE(16)|])|]))|]);
        toStateTrace   = (List.ofArray [|((List.ofArray [| Some("t")|]),(List.ofArray [|(List.ofArray [|TSeqS(16);TSmbS(14)|])|]));((List.ofArray [|None|]),(List.ofArray [|(List.ofArray [|TSmbE(14);TSeqE(16)|])|]))|]);
       };
       {
        prod_num       = 5;
        prod_name      = "a";
        item_num       = 1;
        symb           = Some("t");
        next_num       = None;
        seq_number     = 0;
        s              = 0;
        f              = set [|1|];
        fromStateTrace = (List.ofArray [|((List.ofArray [| Some("t")|]),(List.ofArray [|(List.ofArray [|TSeqS(34);TSmbS(32)|])|]));((List.ofArray [|None|]),(List.ofArray [|(List.ofArray [|TSmbE(32);TSeqE(34)|])|]))|]);
        toStateTrace   = (List.ofArray [|((List.ofArray [| Some("t")|]),(List.ofArray [|(List.ofArray [|TSeqS(34);TSmbS(32)|])|]));((List.ofArray [|None|]),(List.ofArray [|(List.ofArray [|TSmbE(32);TSeqE(34)|])|]))|]);
       }|]);(262719857,set [|
       {
        prod_num       = 2;
        prod_name      = "e";
        item_num       = 1;
        symb           = Some("t");
        next_num       = None;
        seq_number     = 0;
        s              = 0;
        f              = set [|1|];
        fromStateTrace = (List.ofArray [|((List.ofArray [| Some("t")|]),(List.ofArray [|(List.ofArray [|TSeqS(16);TSmbS(14)|])|]));((List.ofArray [|None|]),(List.ofArray [|(List.ofArray [|TSmbE(14);TSeqE(16)|])|]))|]);
        toStateTrace   = (List.ofArray [|((List.ofArray [| Some("t")|]),(List.ofArray [|(List.ofArray [|TSeqS(16);TSmbS(14)|])|]));((List.ofArray [|None|]),(List.ofArray [|(List.ofArray [|TSmbE(14);TSeqE(16)|])|]))|]);
       };
       {
        prod_num       = 5;
        prod_name      = "a";
        item_num       = 1;
        symb           = Some("t");
        next_num       = None;
        seq_number     = 0;
        s              = 0;
        f              = set [|1|];
        fromStateTrace = (List.ofArray [|((List.ofArray [| Some("t")|]),(List.ofArray [|(List.ofArray [|TSeqS(34);TSmbS(32)|])|]));((List.ofArray [|None|]),(List.ofArray [|(List.ofArray [|TSmbE(32);TSeqE(34)|])|]))|]);
        toStateTrace   = (List.ofArray [|((List.ofArray [| Some("t")|]),(List.ofArray [|(List.ofArray [|TSeqS(34);TSmbS(32)|])|]));((List.ofArray [|None|]),(List.ofArray [|(List.ofArray [|TSmbE(32);TSeqE(34)|])|]))|]);
       }|]);(-1197601718,set [|
       {
        prod_num       = 5;
        prod_name      = "a";
        item_num       = 1;
        symb           = Some("t");
        next_num       = None;
        seq_number     = 0;
        s              = 0;
        f              = set [|1|];
        fromStateTrace = (List.ofArray [|((List.ofArray [| Some("t")|]),(List.ofArray [|(List.ofArray [|TSeqS(34);TSmbS(32)|])|]));((List.ofArray [|None|]),(List.ofArray [|(List.ofArray [|TSmbE(32);TSeqE(34)|])|]))|]);
        toStateTrace   = (List.ofArray [|((List.ofArray [| Some("t")|]),(List.ofArray [|(List.ofArray [|TSeqS(34);TSmbS(32)|])|]));((List.ofArray [|None|]),(List.ofArray [|(List.ofArray [|TSmbE(32);TSeqE(34)|])|]))|]);
       }|]);(262698406,set [||]);(-1197585123,set [||]);(568394871,set [||]);(-2041137588,set [||]);(261445078,set [|
       {
        prod_num       = 5;
        prod_name      = "a";
        item_num       = 1;
        symb           = Some("t");
        next_num       = None;
        seq_number     = 0;
        s              = 0;
        f              = set [|1|];
        fromStateTrace = (List.ofArray [|((List.ofArray [| Some("t")|]),(List.ofArray [|(List.ofArray [|TSeqS(34);TSmbS(32)|])|]));((List.ofArray [|None|]),(List.ofArray [|(List.ofArray [|TSmbE(32);TSeqE(34)|])|]))|]);
        toStateTrace   = (List.ofArray [|((List.ofArray [| Some("t")|]),(List.ofArray [|(List.ofArray [|TSeqS(34);TSmbS(32)|])|]));((List.ofArray [|None|]),(List.ofArray [|(List.ofArray [|TSmbE(32);TSeqE(34)|])|]))|]);
       }|]);(-1198337875,set [|
       {
        prod_num       = 5;
        prod_name      = "a";
        item_num       = 1;
        symb           = Some("t");
        next_num       = None;
        seq_number     = 0;
        s              = 0;
        f              = set [|1|];
        fromStateTrace = (List.ofArray [|((List.ofArray [| Some("t")|]),(List.ofArray [|(List.ofArray [|TSeqS(34);TSmbS(32)|])|]));((List.ofArray [|None|]),(List.ofArray [|(List.ofArray [|TSmbE(32);TSeqE(34)|])|]))|]);
        toStateTrace   = (List.ofArray [|((List.ofArray [| Some("t")|]),(List.ofArray [|(List.ofArray [|TSeqS(34);TSmbS(32)|])|]));((List.ofArray [|None|]),(List.ofArray [|(List.ofArray [|TSmbE(32);TSeqE(34)|])|]))|]);
       }|]);(262840707,set [||]);(-1197727752,set [||]);(568426580,set [||]);(-2041066833,set [||]);(1539475031,set [||]);(263184219,set [|
       {
        prod_num       = 5;
        prod_name      = "a";
        item_num       = 1;
        symb           = Some("t");
        next_num       = None;
        seq_number     = 0;
        s              = 0;
        f              = set [|1|];
        fromStateTrace = (List.ofArray [|((List.ofArray [| Some("t")|]),(List.ofArray [|(List.ofArray [|TSeqS(34);TSmbS(32)|])|]));((List.ofArray [|None|]),(List.ofArray [|(List.ofArray [|TSmbE(32);TSeqE(34)|])|]))|]);
        toStateTrace   = (List.ofArray [|((List.ofArray [| Some("t")|]),(List.ofArray [|(List.ofArray [|TSeqS(34);TSmbS(32)|])|]));((List.ofArray [|None|]),(List.ofArray [|(List.ofArray [|TSmbE(32);TSeqE(34)|])|]))|]);
       };
       {
        prod_num       = 8;
        prod_name      = "b";
        item_num       = 2;
        symb           = Some("RIGHT");
        next_num       = Some(3);
        seq_number     = 2;
        s              = 0;
        f              = set [|3|];
        fromStateTrace = (List.ofArray [|((List.ofArray [| Some("RIGHT")|]),(List.ofArray [|(List.ofArray [|TSmbE(54);TSmbS(58)|])|]));((List.ofArray [|None|]),(List.ofArray [|(List.ofArray [|TSmbE(58);TSeqE(60)|])|]))|]);
        toStateTrace   = (List.ofArray [|((List.ofArray [| Some("RIGHT")|]),(List.ofArray [|(List.ofArray [|TSmbE(54);TSmbS(58)|])|]));((List.ofArray [|None|]),(List.ofArray [|(List.ofArray [|TSmbE(58);TSeqE(60)|])|]))|]);
       }|]);(834939266,set [||]);(1986242687,set [||]);(262970476,set [||]);(-1197956521,set [||]);(568572345,set [||]);(-2041209854,set [||]);(1540190772,set [||]);(262952056,set [|
       {
        prod_num       = 11;
        prod_name      = "c";
        item_num       = 2;
        symb           = Some("RIGHT");
        next_num       = Some(3);
        seq_number     = 2;
        s              = 0;
        f              = set [|3|];
        fromStateTrace = (List.ofArray [|((List.ofArray [| Some("RIGHT")|]),(List.ofArray [|(List.ofArray [|TSmbE(80);TSmbS(84)|])|]));((List.ofArray [|None|]),(List.ofArray [|(List.ofArray [|TSmbE(84);TSeqE(86)|])|]))|]);
        toStateTrace   = (List.ofArray [|((List.ofArray [| Some("RIGHT")|]),(List.ofArray [|(List.ofArray [|TSmbE(80);TSmbS(84)|])|]));((List.ofArray [|None|]),(List.ofArray [|(List.ofArray [|TSmbE(84);TSeqE(86)|])|]))|]);
       };
       {
        prod_num       = 5;
        prod_name      = "a";
        item_num       = 1;
        symb           = Some("t");
        next_num       = None;
        seq_number     = 0;
        s              = 0;
        f              = set [|1|];
        fromStateTrace = (List.ofArray [|((List.ofArray [| Some("t")|]),(List.ofArray [|(List.ofArray [|TSeqS(34);TSmbS(32)|])|]));((List.ofArray [|None|]),(List.ofArray [|(List.ofArray [|TSmbE(32);TSeqE(34)|])|]))|]);
        toStateTrace   = (List.ofArray [|((List.ofArray [| Some("t")|]),(List.ofArray [|(List.ofArray [|TSeqS(34);TSmbS(32)|])|]));((List.ofArray [|None|]),(List.ofArray [|(List.ofArray [|TSmbE(32);TSeqE(34)|])|]))|]);
       }|]);(835173921,set [||]);(1986204828,set [||]);(1539364051,set [||]);(262786217,set [||]);(836401414,set [||]);(1986812155,set [||]);(568518558,set [||]);(-2039849115,set [||]);(263074688,set [|
       {
        prod_num       = 5;
        prod_name      = "a";
        item_num       = 1;
        symb           = Some("t");
        next_num       = None;
        seq_number     = 0;
        s              = 0;
        f              = set [|1|];
        fromStateTrace = (List.ofArray [|((List.ofArray [| Some("t")|]),(List.ofArray [|(List.ofArray [|TSeqS(34);TSmbS(32)|])|]));((List.ofArray [|None|]),(List.ofArray [|(List.ofArray [|TSmbE(32);TSeqE(34)|])|]))|]);
        toStateTrace   = (List.ofArray [|((List.ofArray [| Some("t")|]),(List.ofArray [|(List.ofArray [|TSeqS(34);TSmbS(32)|])|]));((List.ofArray [|None|]),(List.ofArray [|(List.ofArray [|TSmbE(32);TSeqE(34)|])|]))|]);
       }|]);(-1729814926,set [||]);(263117003,set [||]);(-1197972234,set [||]);(263173422,set [||]);(-1973309789,set [||]);(263084331,set [||]);(-1197896298,set [||])|])

let startNterms =
    (List.ofArray [|"_yard_start"|])

let ruleToActionMap =
    dict <| (List.ofArray [|(15,"f15_action");(14,"t14_action");(13,"c13_action");(12,"c12_action");(11,"c11_action");(10,"b10_action");(9,"b9_action");(8,"b8_action");(7,"a7_action");(6,"a6_action");(5,"a5_action");(4,"e4_action");(3,"e3_action");(2,"e2_action");(1,"s1_action");(0,"_yard_start0_action")|])