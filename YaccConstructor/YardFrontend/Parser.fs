
# 2 "Parser.fs"
module Yard.Frontends.YardFrontend.GrammarParser
#nowarn "64";; // From fsyacc: turn off warnings that type variables used in production annotations are instantiated to concrete type
open Yard.Generators.RNGLR.Parser
open Yard.Generators.RNGLR
open Yard.Generators.RNGLR.AST

# 1 "Parser.fsy"

//  Copyright 2009 Jake Kirilenko
//
//  This file is part of YaccConctructor.
//
//  YaccConstructor is free software: you can redistribute it and/or modify
//  it under the terms of the GNU General Public License as published by
//  the Free Software Foundation, either version 3 of the License, or
//  (at your option) any later version.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//  GNU General Public License for more details.
//
//  You should have received a copy of the GNU General Public License
//  along with this program.  If not, see <http://www.gnu.org/licenses/>.

#nowarn "62";; 
open Microsoft.FSharp.Text
open Yard.Core.IL
open Yard.Core
open Yard.Core.IL.Production
open Yard.Core.IL.Grammar
open Yard.Core.IL.Definition
open System.Text.RegularExpressions
 
type Range = struct
    val Start: Lexing.Position
    val End: Lexing.Position

    new (start,end_) = {Start = start; End = end_}
end

exception ParseError of Source.t * string
let parseFile = ref Unchecked.defaultof<_>
let FrontendsManager = Yard.Core.FrontendsManager.FrontendsManager()  
let currentFilename = ref ""
let allPublic = ref false
let o2l = function Some x -> [x] | None -> []
let getList = function Some x -> x | None -> []
let fst_ (x,_,_) = x
let snd_ (_,x,_) = x
let trd_ (_,_,x) = x

let joinMaps (p:Map<'a,'b>) (q:Map<'a,'b>) = 
    Map(Seq.concat [ (Map.toSeq p) ; (Map.toSeq q) ])

let makeNewSeq seq (lbl:Source.t) (w:Option<Source.t>) = 
    match seq,w with 
     | PSeq(els,ac,_),_ -> match w with
                           | None -> PSeq (els,ac,Some {label=lbl.text; weight=None})
                           | _ -> let wVal = w.Value
                                  try
                                      PSeq (els,ac,Some {label=lbl.text; weight = Some (float wVal.text)})
                                  with 
                                  | :? System.FormatException as ex ->
                                      failwithf "Parse error on position (%i,%i) on token %s: %s" wVal.startPos.line wVal.startPos.column wVal.text "illegal weight. Number expected."
     | x,_ -> x

let missing name = System.Console.WriteLine("Missing " + name)
let createSeqElem bnd omitted r check =
    { binding = bnd; omit = omitted; rule = r; checker = check }

let parseRules (filename:string) : Definition.t<Source.t, Source.t> =
    let oldFileName = !currentFilename
    currentFilename := filename
    let ext = filename.Substring(filename.LastIndexOf(".") + 1)    
    let userDefs =
        let args = oldFileName.Trim().Split('%') in
        if args.Length = 2
        then args.[1]
        else ""
    let sameDirFilename = System.IO.Path.Combine(System.IO.Path.GetDirectoryName oldFileName, filename) in
    let res = !parseFile (sameDirFilename + "%" + userDefs)
    currentFilename := oldFileName
    res

# 88 "Parser.fs"
type Token =
    | ACTION of Source.t
    | ALL_PUBLIC of Source.t
    | BAR of Source.t
    | BLOCK_END of Source.t
    | COLON of Source.t
    | COMMA of Source.t
    | DLABEL of Source.t
    | EOF of Source.t
    | EQUAL of Source.t
    | GREAT of Source.t
    | INCLUDE of Source.t
    | LESS of Source.t
    | LIDENT of Source.t
    | LPAREN of Source.t
    | MINUS of Source.t
    | MODULE of Source.t
    | NUMBER of Source.t
    | OPEN of Source.t
    | OPTIONS_START of Source.t
    | PARAM of Source.t
    | PLUS of Source.t
    | PREDICATE of Source.t
    | PRIVATE of Source.t
    | PUBLIC of Source.t
    | QUESTION of Source.t
    | RPAREN of Source.t
    | SEMICOLON of Source.t
    | SHARPLINE of Source.t
    | SQR_LBR of Source.t
    | SQR_RBR of Source.t
    | STAR of Source.t
    | START_RULE_SIGN of Source.t
    | STRING of Source.t
    | TOKENS_BLOCK of Source.t
    | UIDENT of Source.t

let numToString = function
    | 0 -> "access_modifier_opt"
    | 1 -> "action_opt"
    | 2 -> "alts"
    | 3 -> "bar_seq_nlist"
    | 4 -> "bound"
    | 5 -> "call"
    | 6 -> "file"
    | 7 -> "foot_opt"
    | 8 -> "formal_meta_list"
    | 9 -> "formal_meta_param_opt"
    | 10 -> "ident"
    | 11 -> "include_"
    | 12 -> "includes_or_options_or_tokens"
    | 13 -> "kw"
    | 14 -> "lbl_seq"
    | 15 -> "meta_param"
    | 16 -> "meta_param_opt"
    | 17 -> "meta_params"
    | 18 -> "module_"
    | 19 -> "module_header"
    | 20 -> "modules"
    | 21 -> "no_lbl_seq"
    | 22 -> "omit_opt"
    | 23 -> "open_list"
    | 24 -> "openings"
    | 25 -> "option"
    | 26 -> "option_block"
    | 27 -> "option_value"
    | 28 -> "opts"
    | 29 -> "param_list"
    | 30 -> "param_opt"
    | 31 -> "patt"
    | 32 -> "predicate_opt"
    | 33 -> "prim"
    | 34 -> "rule"
    | 35 -> "rule_nlist"
    | 36 -> "semi_opt"
    | 37 -> "seq"
    | 38 -> "seq_elem"
    | 39 -> "seq_elem_list"
    | 40 -> "start_rule_sign_opt"
    | 41 -> "tada_rule"
    | 42 -> "tokens_block"
    | 43 -> "unnamed_module_opt"
    | 44 -> "weight_opt"
    | 45 -> "yard_start_rule"
    | 46 -> "ACTION"
    | 47 -> "ALL_PUBLIC"
    | 48 -> "BAR"
    | 49 -> "BLOCK_END"
    | 50 -> "COLON"
    | 51 -> "COMMA"
    | 52 -> "DLABEL"
    | 53 -> "EOF"
    | 54 -> "EQUAL"
    | 55 -> "GREAT"
    | 56 -> "INCLUDE"
    | 57 -> "LESS"
    | 58 -> "LIDENT"
    | 59 -> "LPAREN"
    | 60 -> "MINUS"
    | 61 -> "MODULE"
    | 62 -> "NUMBER"
    | 63 -> "OPEN"
    | 64 -> "OPTIONS_START"
    | 65 -> "PARAM"
    | 66 -> "PLUS"
    | 67 -> "PREDICATE"
    | 68 -> "PRIVATE"
    | 69 -> "PUBLIC"
    | 70 -> "QUESTION"
    | 71 -> "RPAREN"
    | 72 -> "SEMICOLON"
    | 73 -> "SHARPLINE"
    | 74 -> "SQR_LBR"
    | 75 -> "SQR_RBR"
    | 76 -> "STAR"
    | 77 -> "START_RULE_SIGN"
    | 78 -> "STRING"
    | 79 -> "TOKENS_BLOCK"
    | 80 -> "UIDENT"
    | _ -> ""
let tokenToNumber = function
    | ACTION _ -> 46
    | ALL_PUBLIC _ -> 47
    | BAR _ -> 48
    | BLOCK_END _ -> 49
    | COLON _ -> 50
    | COMMA _ -> 51
    | DLABEL _ -> 52
    | EOF _ -> 53
    | EQUAL _ -> 54
    | GREAT _ -> 55
    | INCLUDE _ -> 56
    | LESS _ -> 57
    | LIDENT _ -> 58
    | LPAREN _ -> 59
    | MINUS _ -> 60
    | MODULE _ -> 61
    | NUMBER _ -> 62
    | OPEN _ -> 63
    | OPTIONS_START _ -> 64
    | PARAM _ -> 65
    | PLUS _ -> 66
    | PREDICATE _ -> 67
    | PRIVATE _ -> 68
    | PUBLIC _ -> 69
    | QUESTION _ -> 70
    | RPAREN _ -> 71
    | SEMICOLON _ -> 72
    | SHARPLINE _ -> 73
    | SQR_LBR _ -> 74
    | SQR_RBR _ -> 75
    | STAR _ -> 76
    | START_RULE_SIGN _ -> 77
    | STRING _ -> 78
    | TOKENS_BLOCK _ -> 79
    | UIDENT _ -> 80

let mutable private cur = 0
let leftSide = [|13; 13; 13; 13; 13; 6; 45; 12; 12; 12; 12; 42; 11; 26; 28; 28; 25; 27; 27; 27; 43; 20; 20; 18; 10; 10; 19; 19; 24; 24; 23; 23; 1; 1; 7; 7; 35; 35; 34; 40; 40; 0; 0; 0; 9; 9; 8; 8; 30; 30; 29; 29; 44; 44; 2; 2; 3; 3; 37; 37; 21; 21; 14; 39; 39; 38; 22; 22; 36; 36; 32; 32; 4; 4; 31; 31; 33; 33; 33; 33; 33; 33; 33; 33; 15; 17; 17; 16; 16; 5; 5; 41; 41|]
let private rules = [|68; 69; 63; 56; 61; 1; 12; 43; 20; 7; 6; 42; 12; 26; 12; 11; 12; 79; 56; 78; 64; 28; 49; 25; 28; 10; 54; 27; 13; 78; 10; 35; 18; 20; 19; 10; 24; 35; 58; 80; 61; 47; 61; 63; 10; 23; 51; 10; 23; 46; 72; 46; 34; 36; 35; 40; 0; 58; 9; 29; 50; 2; 77; 68; 69; 57; 8; 55; 58; 8; 58; 65; 65; 29; 74; 62; 75; 37; 3; 37; 48; 37; 48; 37; 3; 14; 21; 46; 38; 39; 1; 52; 44; 59; 21; 71; 38; 39; 22; 4; 32; 60; 72; 67; 33; 31; 54; 33; 46; 58; 78; 14; 5; 59; 2; 71; 74; 33; 75; 33; 70; 33; 66; 33; 76; 33; 15; 17; 15; 57; 17; 55; 58; 16; 30; 80; 53; 73|]
let private rulesStart = [|0; 1; 2; 3; 4; 5; 10; 11; 13; 15; 17; 17; 18; 20; 23; 23; 25; 28; 29; 30; 31; 32; 32; 34; 38; 39; 40; 41; 43; 46; 46; 46; 49; 50; 50; 52; 52; 52; 55; 62; 63; 63; 63; 64; 65; 68; 68; 70; 71; 72; 72; 74; 74; 77; 77; 79; 80; 82; 85; 86; 87; 88; 91; 96; 98; 98; 101; 102; 102; 103; 103; 104; 104; 105; 108; 109; 110; 111; 112; 113; 116; 119; 121; 123; 125; 126; 128; 129; 132; 132; 135; 136; 137; 138|]
let startRule = 6

let acceptEmptyInput = true

let defaultAstToDot =
    (fun (tree : Yard.Generators.RNGLR.AST.Tree<Token>) -> tree.AstToDot numToString tokenToNumber leftSide)

let private lists_gotos = [|1; 128; 83; 2; 29; 4; 6; 8; 10; 28; 3; 5; 7; 9; 11; 24; 26; 17; 23; 12; 13; 14; 15; 16; 18; 19; 20; 21; 22; 25; 27; 30; 107; 33; 108; 105; 31; 106; 32; 34; 103; 104; 35; 36; 98; 37; 96; 38; 39; 40; 41; 42; 72; 77; 84; 54; 80; 43; 46; 47; 48; 90; 95; 91; 69; 85; 88; 89; 44; 45; 49; 50; 59; 51; 52; 53; 55; 92; 56; 57; 58; 60; 63; 61; 62; 64; 67; 66; 65; 68; 70; 71; 73; 74; 75; 76; 78; 81; 79; 82; 86; 87; 93; 94; 97; 99; 101; 100; 102; 109; 110; 124; 121; 123; 120; 111; 112; 114; 113; 115; 116; 117; 118; 119; 122; 125; 126; 127|]
let private small_gotos =
        [|3; 65536; 393217; 3014658; 65543; 720899; 786436; 1703941; 2752518; 3670023; 4194312; 5177353; 131079; 720899; 786442; 1703941; 2752518; 3670023; 4194312; 5177353; 262151; 720899; 786443; 1703941; 2752518; 3670023; 4194312; 5177353; 393223; 720899; 786444; 1703941; 2752518; 3670023; 4194312; 5177353; 524289; 5111821; 655365; 655374; 1638415; 1835024; 3801105; 5242898; 720897; 3538963; 786443; 655380; 851989; 1769494; 3670039; 3801105; 3997720; 4128793; 4456474; 4522011; 5111836; 5242898; 1572869; 655374; 1638415; 1835037; 3801105; 5242898; 1703937; 3211294; 1900549; 2228255; 2293792; 2621473; 2818082; 5046307; 1966082; 2359332; 4718629; 2031620; 2228255; 2293798; 2621473; 5046307; 2162691; 39; 4456488; 4522025; 2228225; 3801130; 2293762; 589867; 3735596; 2359298; 1900589; 4259886; 2424833; 3276847; 2490377; 131120; 917553; 1376306; 1441843; 2424884; 2490421; 3014710; 3407927; 3932216; 2752524; 262201; 327738; 917563; 2031676; 2162749; 3014718; 3407927; 3801151; 3866688; 4849729; 5111874; 5242947; 2818050; 2097220; 4390981; 3145729; 3539014; 3211273; 327738; 917563; 2162759; 3407927; 3801160; 3866688; 4849729; 5111874; 5242947; 3276803; 4325449; 4587594; 4980811; 3538946; 2883660; 4849741; 3604481; 3866702; 3670021; 1376335; 1441843; 2490421; 3014710; 3932216; 3735553; 4653136; 3866626; 1048657; 3735634; 3932162; 1966163; 4259924; 4128779; 327738; 917563; 983125; 1114198; 2162775; 3407927; 3801160; 3866688; 4849729; 5111874; 5242947; 4194315; 327738; 917563; 983125; 1114200; 2162775; 3407927; 3801160; 3866688; 4849729; 5111874; 5242947; 4325379; 4325449; 4587594; 4980811; 4390913; 3604569; 4521993; 131162; 917553; 1376306; 1441843; 2424884; 2490421; 3014710; 3407927; 3932216; 4587521; 4653147; 4718594; 196700; 3145821; 4849672; 917553; 1376306; 1441843; 2424926; 2490421; 3014710; 3407927; 3932216; 4915202; 196703; 3145821; 5046276; 1441843; 2490464; 2556001; 3932216; 5111812; 1441843; 2490464; 2556002; 3932216; 5308418; 65635; 3014658; 5570569; 327738; 917563; 2162788; 3407927; 3801160; 3866688; 4849729; 5111874; 5242947; 5636100; 4325449; 4587594; 4915301; 4980811; 5898243; 4325449; 4587594; 4980811; 5963778; 1048657; 3735634; 6029313; 4063334; 6094849; 4915303; 6291458; 1900648; 4259886; 6422530; 524393; 3801194; 6488065; 3604587; 6619138; 524396; 3801194; 7077893; 1179757; 1245294; 1310831; 3080304; 3997809; 7143429; 1179757; 1245294; 1310834; 3080304; 3997809; 7208963; 655475; 3801105; 5242898; 7274498; 1572980; 4128885; 7340036; 2228255; 2293878; 2621473; 5046307; 7471107; 655479; 3801105; 5242898; 7536642; 1507448; 3342457; 7667715; 655482; 3801105; 5242898; 7733250; 1507451; 3342457; 7929857; 3997820; 8126466; 458877; 4718718; 8257537; 3014783|]
let gotos = Array.zeroCreate 129
for i = 0 to 128 do
        gotos.[i] <- Array.zeroCreate 81
cur <- 0
while cur < small_gotos.Length do
    let i = small_gotos.[cur] >>> 16
    let length = small_gotos.[cur] &&& 65535
    cur <- cur + 1
    for k = 0 to length-1 do
        let j = small_gotos.[cur + k] >>> 16
        let x = small_gotos.[cur + k] &&& 65535
        gotos.[i].[j] <- lists_gotos.[x]
    cur <- cur + length
let private lists_reduces = [|[|5,1|]; [|9,1|]; [|9,2|]; [|8,1|]; [|8,2|]; [|7,1|]; [|7,2|]; [|12,2|]; [|19,1|]; [|17,1|]; [|16,3|]; [|3,1|]; [|24,1|]; [|4,1|]; [|2,1|]; [|0,1|]; [|1,1|]; [|18,1|]; [|25,1|]; [|15,1|]; [|15,2|]; [|13,3|]; [|11,1|]; [|5,2|]; [|37,1|]; [|37,2|]; [|37,3|]; [|38,7|]; [|58,1|]; [|59,1|]; [|65,2|]; [|65,3|]; [|70,1|]; [|78,1|]; [|77,1|]; [|73,3|]; [|82,2|]; [|81,2|]; [|83,2|]; [|62,5|]; [|89,1|]; [|89,2|]; [|89,3|]; [|48,1|]; [|86,1|]; [|85,2|]; [|84,1|]; [|87,3|]; [|79,3|]; [|55,1|]; [|54,2|]; [|56,2|]; [|57,3|]; [|61,1|]; [|63,1|]; [|63,2|]; [|66,1|]; [|61,2|]; [|61,3|]; [|32,1|]; [|60,1|]; [|80,3|]; [|76,1|]; [|90,1|]; [|72,1|]; [|75,1|]; [|52,3|]; [|74,1|]; [|50,1|]; [|50,2|]; [|44,3|]; [|47,1|]; [|46,2|]; [|42,1|]; [|43,1|]; [|39,1|]; [|68,1|]; [|20,1|]; [|5,3|]; [|22,1|]; [|23,2|]; [|23,3|]; [|23,4|]; [|28,2|]; [|28,3|]; [|31,2|]; [|31,3|]; [|22,2|]; [|27,2|]; [|26,1|]; [|5,4|]; [|5,5|]; [|34,2|]|]
let private small_reduces =
        [|65537; 3473408; 131080; 3080193; 3473409; 3801089; 3997697; 4456449; 4521985; 4718593; 5046273; 196616; 3080194; 3473410; 3801090; 3997698; 4456450; 4521986; 4718594; 5046274; 262152; 3080195; 3473411; 3801091; 3997699; 4456451; 4521987; 4718595; 5046275; 327688; 3080196; 3473412; 3801092; 3997700; 4456452; 4521988; 4718596; 5046276; 393224; 3080197; 3473413; 3801093; 3997701; 4456453; 4521989; 4718597; 5046277; 458760; 3080198; 3473414; 3801094; 3997702; 4456454; 4521990; 4718598; 5046278; 589835; 3080199; 3473415; 3670023; 3801095; 3997703; 4194311; 4456455; 4521991; 4718599; 5046279; 5177351; 851971; 3211272; 3801096; 5242888; 917507; 3211273; 3801097; 5242889; 983043; 3211274; 3801098; 5242890; 1048579; 3211275; 3801099; 5242891; 1114125; 3080204; 3211276; 3342348; 3473420; 3538956; 3801100; 3997708; 4128780; 4456460; 4521996; 4718604; 5046284; 5242892; 1179651; 3211277; 3801101; 5242893; 1245187; 3211278; 3801102; 5242894; 1310723; 3211279; 3801103; 5242895; 1376259; 3211280; 3801104; 5242896; 1441795; 3211281; 3801105; 5242897; 1507341; 3080210; 3211282; 3342354; 3473426; 3538962; 3801106; 3997714; 4128786; 4456466; 4522002; 4718610; 5046290; 5242898; 1572865; 3211283; 1638401; 3211284; 1769483; 3080213; 3473429; 3670037; 3801109; 3997717; 4194325; 4456469; 4522005; 4718613; 5046293; 5177365; 1835019; 3080214; 3473430; 3670038; 3801110; 3997718; 4194326; 4456470; 4522006; 4718614; 5046294; 5177366; 1900545; 3473431; 1966084; 3080216; 3473432; 3997720; 4718616; 2031620; 3080217; 3473433; 3997721; 4718617; 2097156; 3080218; 3473434; 3997722; 4718618; 2555912; 3080219; 3473435; 3801115; 3997723; 4456475; 4522011; 4718619; 5046299; 2621450; 3080220; 3145756; 3473436; 3801116; 3997724; 4456476; 4522012; 4653084; 4718620; 5046300; 2686986; 3080221; 3145757; 3473437; 3801117; 3997725; 4456477; 4522013; 4653085; 4718621; 5046301; 2818065; 3014686; 3080222; 3145758; 3407902; 3473438; 3801118; 3866654; 3932190; 3997726; 4456478; 4522014; 4653086; 4718622; 4849694; 5046302; 5111838; 5242910; 2883601; 3014687; 3080223; 3145759; 3407903; 3473439; 3801119; 3866655; 3932191; 3997727; 4456479; 4522015; 4653087; 4718623; 4849695; 5046303; 5111839; 5242911; 2949137; 3014688; 3080224; 3145760; 3407904; 3473440; 3801120; 3866656; 3932192; 3997728; 4456480; 4522016; 4653088; 4718624; 4849696; 5046304; 5111840; 5242912; 3014679; 3014689; 3080225; 3145761; 3407905; 3473441; 3604513; 3801121; 3866657; 3932193; 3997729; 4325409; 4390945; 4456481; 4522017; 4587553; 4653089; 4718625; 4849697; 4915233; 4980769; 5046305; 5111841; 5242913; 3080215; 3014690; 3080226; 3145762; 3407906; 3473442; 3604514; 3801122; 3866658; 3932194; 3997730; 4325410; 4390946; 4456482; 4522018; 4587554; 4653090; 4718626; 4849698; 4915234; 4980770; 5046306; 5111842; 5242914; 3276818; 3014691; 3080227; 3145763; 3407907; 3473443; 3801123; 3866659; 3932195; 3997731; 4390947; 4456483; 4522019; 4653091; 4718627; 4849699; 5046307; 5111843; 5242915; 3342359; 3014692; 3080228; 3145764; 3407908; 3473444; 3604516; 3801124; 3866660; 3932196; 3997732; 4325412; 4390948; 4456484; 4522020; 4587556; 4653092; 4718628; 4849700; 4915236; 4980772; 5046308; 5111844; 5242916; 3407895; 3014693; 3080229; 3145765; 3407909; 3473445; 3604517; 3801125; 3866661; 3932197; 3997733; 4325413; 4390949; 4456485; 4522021; 4587557; 4653093; 4718629; 4849701; 4915237; 4980773; 5046309; 5111845; 5242917; 3473431; 3014694; 3080230; 3145766; 3407910; 3473446; 3604518; 3801126; 3866662; 3932198; 3997734; 4325414; 4390950; 4456486; 4522022; 4587558; 4653094; 4718630; 4849702; 4915238; 4980774; 5046310; 5111846; 5242918; 3801111; 3014695; 3080231; 3145767; 3407911; 3473447; 3604519; 3801127; 3866663; 3932199; 3997735; 4325415; 4390951; 4456487; 4522023; 4587559; 4653095; 4718631; 4849703; 4915239; 4980775; 5046311; 5111847; 5242919; 3866647; 3014696; 3080232; 3145768; 3407912; 3473448; 3604520; 3801128; 3866664; 3932200; 3997736; 4325416; 4390952; 4456488; 4522024; 4587560; 4653096; 4718632; 4849704; 4915240; 4980776; 5046312; 5111848; 5242920; 3932183; 3014697; 3080233; 3145769; 3407913; 3473449; 3604521; 3801129; 3866665; 3932201; 3997737; 4325417; 4390953; 4456489; 4522025; 4587561; 4653097; 4718633; 4849705; 4915241; 4980777; 5046313; 5111849; 5242921; 3997719; 3014698; 3080234; 3145770; 3407914; 3473450; 3604522; 3801130; 3866666; 3932202; 3997738; 4325418; 4390954; 4456490; 4522026; 4587562; 4653098; 4718634; 4849706; 4915242; 4980778; 5046314; 5111850; 5242922; 4063255; 3014699; 3080235; 3145771; 3407915; 3473451; 3604523; 3801131; 3866667; 3932203; 3997739; 4325419; 4390955; 4456491; 4522027; 4587563; 4653099; 4718635; 4849707; 4915243; 4980779; 5046315; 5111851; 5242923; 4194305; 3604524; 4259841; 3604525; 4325383; 3407918; 3604526; 3801134; 3866670; 4849710; 5111854; 5242926; 4456472; 3014703; 3080239; 3145775; 3407919; 3473455; 3604527; 3801135; 3866671; 3932207; 3997743; 4259887; 4325423; 4390959; 4456495; 4522031; 4587567; 4653103; 4718639; 4849711; 4915247; 4980783; 5046319; 5111855; 5242927; 4653079; 3014704; 3080240; 3145776; 3407920; 3473456; 3604528; 3801136; 3866672; 3932208; 3997744; 4325424; 4390960; 4456496; 4522032; 4587568; 4653104; 4718640; 4849712; 4915248; 4980784; 5046320; 5111856; 5242928; 4718601; 3080241; 3473457; 3801137; 3997745; 4456497; 4522033; 4653105; 4718641; 5046321; 4784137; 3080242; 3473458; 3801138; 3997746; 4456498; 4522034; 4653106; 4718642; 5046322; 4915209; 3080243; 3473459; 3801139; 3997747; 4456499; 4522035; 4653107; 4718643; 5046323; 4980745; 3080244; 3473460; 3801140; 3997748; 4456500; 4522036; 4653108; 4718644; 5046324; 5046282; 3080245; 3145781; 3473461; 3801141; 3997749; 4456501; 4522037; 4653109; 4718645; 5046325; 5111819; 3014710; 3080246; 3145782; 3473462; 3801142; 3997750; 4456502; 4522038; 4653110; 4718646; 5046326; 5177355; 3014711; 3080247; 3145783; 3473463; 3801143; 3997751; 4456503; 4522039; 4653111; 4718647; 5046327; 5242887; 3014712; 3407928; 3801144; 3866680; 4849720; 5111864; 5242936; 5308426; 3080249; 3145785; 3473465; 3801145; 3997753; 4456505; 4522041; 4653113; 4718649; 5046329; 5373962; 3080250; 3145786; 3473466; 3801146; 3997754; 4456506; 4522042; 4653114; 4718650; 5046330; 5439501; 3080251; 3145787; 3473467; 3670075; 3801147; 3997755; 4194363; 4456507; 4522043; 4653115; 4718651; 5046331; 5177403; 5505034; 3080252; 3145788; 3473468; 3801148; 3997756; 4456508; 4522044; 4653116; 4718652; 5046332; 5701655; 3014717; 3080253; 3145789; 3407933; 3473469; 3604541; 3801149; 3866685; 3932221; 3997757; 4325437; 4390973; 4456509; 4522045; 4587581; 4653117; 4718653; 4849725; 4915261; 4980797; 5046333; 5111869; 5242941; 5767191; 3014718; 3080254; 3145790; 3407934; 3473470; 3604542; 3801150; 3866686; 3932222; 3997758; 4325438; 4390974; 4456510; 4522046; 4587582; 4653118; 4718654; 4849726; 4915262; 4980798; 5046334; 5111870; 5242942; 5832727; 3014719; 3080255; 3145791; 3407935; 3473471; 3604543; 3801151; 3866687; 3932223; 3997759; 4325439; 4390975; 4456511; 4522047; 4587583; 4653119; 4718655; 4849727; 4915263; 4980799; 5046335; 5111871; 5242943; 5898258; 3014720; 3080256; 3145792; 3407936; 3473472; 3801152; 3866688; 3932224; 3997760; 4390976; 4456512; 4522048; 4653120; 4718656; 4849728; 5046336; 5111872; 5242944; 5963798; 3014696; 3080232; 3145768; 3407912; 3473448; 3539009; 3801128; 3866664; 3932200; 3997736; 4325416; 4390952; 4456488; 4522024; 4587560; 4653096; 4718632; 4849704; 4980776; 5046312; 5111848; 5242920; 6160385; 3866690; 6225921; 3539011; 6291457; 3276868; 6356993; 3276869; 6553602; 3276870; 4259910; 6619137; 3604551; 6684673; 3604552; 6750209; 3801161; 6815745; 3801162; 6881283; 3801163; 4456523; 4522059; 6946824; 3080268; 3473484; 3801164; 3997772; 4456524; 4522060; 4718668; 5046348; 7012356; 3080269; 3473485; 3997773; 4718669; 7077889; 3473486; 7143426; 3473487; 4718671; 7274500; 3080272; 3473488; 3997776; 4718672; 7340036; 3080273; 3473489; 3997777; 4718673; 7405572; 3080274; 3473490; 3997778; 4718674; 7536648; 3080275; 3473491; 3801171; 3997779; 4456531; 4522067; 4718675; 5046355; 7602184; 3080276; 3473492; 3801172; 3997780; 4456532; 4522068; 4718676; 5046356; 7733256; 3080277; 3473493; 3801173; 3997781; 4456533; 4522069; 4718677; 5046357; 7798792; 3080278; 3473494; 3801174; 3997782; 4456534; 4522070; 4718678; 5046358; 7864322; 3473495; 4718679; 7995394; 3801176; 5242968; 8060930; 3801177; 5242969; 8126465; 3473498; 8192001; 3473499; 8323073; 3473500|]
let reduces = Array.zeroCreate 129
for i = 0 to 128 do
        reduces.[i] <- Array.zeroCreate 81
cur <- 0
while cur < small_reduces.Length do
    let i = small_reduces.[cur] >>> 16
    let length = small_reduces.[cur] &&& 65535
    cur <- cur + 1
    for k = 0 to length-1 do
        let j = small_reduces.[cur + k] >>> 16
        let x = small_reduces.[cur + k] &&& 65535
        reduces.[i].[j] <- lists_reduces.[x]
    cur <- cur + length
let private lists_zeroReduces = [|[|33|]; [|33; 6; 5|]; [|10|]; [|14|]; [|36; 20|]; [|40|]; [|69|]; [|36|]; [|41|]; [|45|]; [|51|]; [|67|]; [|71|]; [|53|]; [|88|]; [|49|]; [|67; 64|]; [|64|]; [|21|]; [|29|]; [|30|]; [|35|]|]
let private small_zeroReduces =
        [|11; 3080192; 3473409; 3670016; 3801088; 3997696; 4194304; 4456448; 4521984; 4718592; 5046272; 5177344; 65544; 3080194; 3473410; 3801090; 3997698; 4456450; 4521986; 4718594; 5046274; 131080; 3080194; 3473410; 3801090; 3997698; 4456450; 4521986; 4718594; 5046274; 262152; 3080194; 3473410; 3801090; 3997698; 4456450; 4521986; 4718594; 5046274; 393224; 3080194; 3473410; 3801090; 3997698; 4456450; 4521986; 4718594; 5046274; 655361; 3211267; 1572865; 3211267; 1900551; 3080196; 3473412; 3801093; 3997700; 4456453; 4521989; 4718596; 1966088; 3080198; 3473414; 3801094; 3997702; 4456454; 4521990; 4718598; 5046278; 2031623; 3080199; 3473415; 3801093; 3997703; 4456453; 4521989; 4718599; 2162689; 3801096; 2293762; 3276809; 4259849; 2359297; 3276810; 2490375; 3014667; 3407883; 3801099; 3866635; 4849675; 5111819; 5242891; 2818065; 3014668; 3080204; 3145740; 3407884; 3473420; 3801100; 3866636; 3932172; 3997708; 4456460; 4521996; 4653068; 4718604; 4849676; 5046284; 5111820; 5242892; 3538945; 3866637; 3670023; 3014667; 3407883; 3801099; 3866635; 4849675; 5111819; 5242891; 3866648; 3014670; 3080206; 3145742; 3407886; 3473422; 3604494; 3801102; 3866638; 3932174; 3997710; 4259854; 4325390; 4390926; 4456462; 4521998; 4587534; 4653070; 4718606; 4849678; 4915214; 4980750; 5046286; 5111822; 5242894; 3932183; 3014671; 3080207; 3145743; 3407887; 3473423; 3604495; 3801103; 3866639; 3932175; 3997711; 4325391; 4390927; 4456463; 4521999; 4587535; 4653071; 4718607; 4849679; 4915215; 4980751; 5046287; 5111823; 5242895; 4521991; 3014667; 3407883; 3801099; 3866635; 4849675; 5111819; 5242891; 4849671; 3014667; 3407883; 3801099; 3866635; 4849675; 5111819; 5242891; 5046288; 3014672; 3080209; 3145745; 3407883; 3473425; 3801104; 3866635; 3997713; 4456465; 4522001; 4653073; 4718609; 4849675; 5046289; 5111819; 5242891; 5111824; 3014672; 3080209; 3145745; 3407883; 3473425; 3801104; 3866635; 3997713; 4456465; 4522001; 4653073; 4718609; 4849675; 5046289; 5111819; 5242891; 5308426; 3080192; 3145728; 3473408; 3801088; 3997696; 4456448; 4521984; 4653056; 4718592; 5046272; 5963798; 3014670; 3080206; 3145742; 3407886; 3473422; 3801102; 3866638; 3932174; 3997710; 4259854; 4325390; 4390926; 4456462; 4521998; 4587534; 4653070; 4718606; 4849678; 4980750; 5046286; 5111822; 5242894; 6291457; 3276810; 7077890; 3473426; 4718610; 7143426; 3473426; 4718610; 7274504; 3080211; 3473427; 3801107; 3997715; 4456467; 4522003; 4718611; 5046291; 7340039; 3080199; 3473415; 3801093; 3997703; 4456453; 4521989; 4718599; 7536648; 3080212; 3473428; 3801108; 3997716; 4456468; 4522004; 4718612; 5046292; 7733256; 3080212; 3473428; 3801108; 3997716; 4456468; 4522004; 4718612; 5046292; 8126465; 3473429|]
let zeroReduces = Array.zeroCreate 129
for i = 0 to 128 do
        zeroReduces.[i] <- Array.zeroCreate 81
cur <- 0
while cur < small_zeroReduces.Length do
    let i = small_zeroReduces.[cur] >>> 16
    let length = small_zeroReduces.[cur] &&& 65535
    cur <- cur + 1
    for k = 0 to length-1 do
        let j = small_zeroReduces.[cur + k] >>> 16
        let x = small_zeroReduces.[cur + k] &&& 65535
        zeroReduces.[i].[j] <- lists_zeroReduces.[x]
    cur <- cur + length
let private small_acc = [128; 0]
let private accStates = Array.zeroCreate 129
for i = 0 to 128 do
        accStates.[i] <- List.exists ((=) i) small_acc
let eofIndex = 53
let private parserSource = new ParserSource<Token> (gotos, reduces, zeroReduces, accStates, rules, rulesStart, leftSide, startRule, eofIndex, tokenToNumber, acceptEmptyInput, numToString)
let buildAst : (seq<Token> -> ParseResult<Token>) =
    buildAst<Token> parserSource

let _rnglr_epsilons : Tree<Token>[] = [|new Tree<_>(null,box (new AST(new Family(41, new Nodes([||])), null)), null); new Tree<_>(null,box (new AST(new Family(33, new Nodes([||])), null)), null); null; null; null; null; new Tree<_>(null,box (new AST(new Family(5, new Nodes([|box (new AST(new Family(33, new Nodes([||])), null)); box (new AST(new Family(10, new Nodes([||])), null)); box (new AST(new Family(20, new Nodes([|box (new AST(new Family(36, new Nodes([||])), null))|])), null)); box (new AST(new Family(21, new Nodes([||])), null)); box (new AST(new Family(35, new Nodes([||])), null))|])), null)), null); new Tree<_>(null,box (new AST(new Family(35, new Nodes([||])), null)), null); null; new Tree<_>(null,box (new AST(new Family(45, new Nodes([||])), null)), null); null; null; new Tree<_>(null,box (new AST(new Family(10, new Nodes([||])), null)), null); null; null; null; new Tree<_>(null,box (new AST(new Family(88, new Nodes([||])), null)), null); null; null; null; new Tree<_>(null,box (new AST(new Family(21, new Nodes([||])), null)), null); null; new Tree<_>(null,box (new AST(new Family(67, new Nodes([||])), null)), null); new Tree<_>(null,box (new AST(new Family(30, new Nodes([||])), null)), null); new Tree<_>(null,box (new AST(new Family(29, new Nodes([||])), null)), null); null; null; null; new Tree<_>(null,box (new AST(new Family(14, new Nodes([||])), null)), null); new Tree<_>(null,box (new AST(new Family(51, new Nodes([||])), null)), null); new Tree<_>(null,box (new AST(new Family(49, new Nodes([||])), null)), null); null; new Tree<_>(null,box (new AST(new Family(71, new Nodes([||])), null)), null); null; null; new Tree<_>(null,box (new AST(new Family(36, new Nodes([||])), null)), null); new Tree<_>(null,box (new AST(new Family(69, new Nodes([||])), null)), null); null; null; new Tree<_>(null,box (new AST(new Family(64, new Nodes([||])), null)), null); new Tree<_>(null,box (new AST(new Family(40, new Nodes([||])), null)), null); null; null; new Tree<_>(null,box (new AST(new Family(20, new Nodes([|box (new AST(new Family(36, new Nodes([||])), null))|])), null)), null); new Tree<_>(null,box (new AST(new Family(53, new Nodes([||])), null)), null); new Tree<_>(null,box (new AST(new Family(6, new Nodes([|box (new AST(new Family(5, new Nodes([|box (new AST(new Family(33, new Nodes([||])), null)); box (new AST(new Family(10, new Nodes([||])), null)); box (new AST(new Family(20, new Nodes([|box (new AST(new Family(36, new Nodes([||])), null))|])), null)); box (new AST(new Family(21, new Nodes([||])), null)); box (new AST(new Family(35, new Nodes([||])), null))|])), null))|])), null)), null)|]
let _rnglr_filtered_epsilons : Tree<Token>[] = [|new Tree<_>(null,box (new AST(new Family(41, new Nodes([||])), null)), null); new Tree<_>(null,box (new AST(new Family(33, new Nodes([||])), null)), null); null; null; null; null; new Tree<_>(null,box (new AST(new Family(5, new Nodes([|box (new AST(new Family(33, new Nodes([||])), null)); box (new AST(new Family(10, new Nodes([||])), null)); box (new AST(new Family(20, new Nodes([|box (new AST(new Family(36, new Nodes([||])), null))|])), null)); box (new AST(new Family(21, new Nodes([||])), null)); box (new AST(new Family(35, new Nodes([||])), null))|])), null)), null); new Tree<_>(null,box (new AST(new Family(35, new Nodes([||])), null)), null); null; new Tree<_>(null,box (new AST(new Family(45, new Nodes([||])), null)), null); null; null; new Tree<_>(null,box (new AST(new Family(10, new Nodes([||])), null)), null); null; null; null; new Tree<_>(null,box (new AST(new Family(88, new Nodes([||])), null)), null); null; null; null; new Tree<_>(null,box (new AST(new Family(21, new Nodes([||])), null)), null); null; new Tree<_>(null,box (new AST(new Family(67, new Nodes([||])), null)), null); new Tree<_>(null,box (new AST(new Family(30, new Nodes([||])), null)), null); new Tree<_>(null,box (new AST(new Family(29, new Nodes([||])), null)), null); null; null; null; new Tree<_>(null,box (new AST(new Family(14, new Nodes([||])), null)), null); new Tree<_>(null,box (new AST(new Family(51, new Nodes([||])), null)), null); new Tree<_>(null,box (new AST(new Family(49, new Nodes([||])), null)), null); null; new Tree<_>(null,box (new AST(new Family(71, new Nodes([||])), null)), null); null; null; new Tree<_>(null,box (new AST(new Family(36, new Nodes([||])), null)), null); new Tree<_>(null,box (new AST(new Family(69, new Nodes([||])), null)), null); null; null; new Tree<_>(null,box (new AST(new Family(64, new Nodes([||])), null)), null); new Tree<_>(null,box (new AST(new Family(40, new Nodes([||])), null)), null); null; null; new Tree<_>(null,box (new AST(new Family(20, new Nodes([|box (new AST(new Family(36, new Nodes([||])), null))|])), null)), null); new Tree<_>(null,box (new AST(new Family(53, new Nodes([||])), null)), null); new Tree<_>(null,box (new AST(new Family(6, new Nodes([|box (new AST(new Family(5, new Nodes([|box (new AST(new Family(33, new Nodes([||])), null)); box (new AST(new Family(10, new Nodes([||])), null)); box (new AST(new Family(20, new Nodes([|box (new AST(new Family(36, new Nodes([||])), null))|])), null)); box (new AST(new Family(21, new Nodes([||])), null)); box (new AST(new Family(35, new Nodes([||])), null))|])), null))|])), null)), null)|]
for x in _rnglr_filtered_epsilons do if x <> null then x.ChooseSingleAst()
let _rnglr_extra_array, _rnglr_rule_, _rnglr_concats = 
  (Array.zeroCreate 0 : array<'_rnglr_type_access_modifier_opt * '_rnglr_type_action_opt * '_rnglr_type_alts * '_rnglr_type_bar_seq_nlist * '_rnglr_type_bound * '_rnglr_type_call * '_rnglr_type_file * '_rnglr_type_foot_opt * '_rnglr_type_formal_meta_list * '_rnglr_type_formal_meta_param_opt * '_rnglr_type_ident * '_rnglr_type_include_ * '_rnglr_type_includes_or_options_or_tokens * '_rnglr_type_kw * '_rnglr_type_lbl_seq * '_rnglr_type_meta_param * '_rnglr_type_meta_param_opt * '_rnglr_type_meta_params * '_rnglr_type_module_ * '_rnglr_type_module_header * '_rnglr_type_modules * '_rnglr_type_no_lbl_seq * '_rnglr_type_omit_opt * '_rnglr_type_open_list * '_rnglr_type_openings * '_rnglr_type_option * '_rnglr_type_option_block * '_rnglr_type_option_value * '_rnglr_type_opts * '_rnglr_type_param_list * '_rnglr_type_param_opt * '_rnglr_type_patt * '_rnglr_type_predicate_opt * '_rnglr_type_prim * '_rnglr_type_rule * '_rnglr_type_rule_nlist * '_rnglr_type_semi_opt * '_rnglr_type_seq * '_rnglr_type_seq_elem * '_rnglr_type_seq_elem_list * '_rnglr_type_start_rule_sign_opt * '_rnglr_type_tada_rule * '_rnglr_type_tokens_block * '_rnglr_type_unnamed_module_opt * '_rnglr_type_weight_opt * '_rnglr_type_yard_start_rule>), 
  [|
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            (match ((unbox _rnglr_children.[0]) : Token) with PRIVATE _rnglr_val -> [_rnglr_val] | a -> failwith "PRIVATE expected, but %A found" a )
             |> List.iter (fun (_S1) -> 
              _rnglr_cycle_res := (
                
# 110 "Parser.fsy"
                                                                                     _S1
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 110 "Parser.fsy"
               : '_rnglr_type_kw) 
# 338 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            (match ((unbox _rnglr_children.[0]) : Token) with PUBLIC _rnglr_val -> [_rnglr_val] | a -> failwith "PUBLIC expected, but %A found" a )
             |> List.iter (fun (_S1) -> 
              _rnglr_cycle_res := (
                
# 110 "Parser.fsy"
                                                                      _S1
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 110 "Parser.fsy"
               : '_rnglr_type_kw) 
# 358 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            (match ((unbox _rnglr_children.[0]) : Token) with OPEN _rnglr_val -> [_rnglr_val] | a -> failwith "OPEN expected, but %A found" a )
             |> List.iter (fun (_S1) -> 
              _rnglr_cycle_res := (
                
# 110 "Parser.fsy"
                                                        _S1
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 110 "Parser.fsy"
               : '_rnglr_type_kw) 
# 378 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            (match ((unbox _rnglr_children.[0]) : Token) with INCLUDE _rnglr_val -> [_rnglr_val] | a -> failwith "INCLUDE expected, but %A found" a )
             |> List.iter (fun (_S1) -> 
              _rnglr_cycle_res := (
                
# 110 "Parser.fsy"
                                            _S1
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 110 "Parser.fsy"
               : '_rnglr_type_kw) 
# 398 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            (match ((unbox _rnglr_children.[0]) : Token) with MODULE _rnglr_val -> [_rnglr_val] | a -> failwith "MODULE expected, but %A found" a )
             |> List.iter (fun (_S1) -> 
              _rnglr_cycle_res := (
                
# 110 "Parser.fsy"
                             _S1
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 110 "Parser.fsy"
               : '_rnglr_type_kw) 
# 418 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            ((unbox _rnglr_children.[0]) : '_rnglr_type_action_opt) 
             |> List.iter (fun (_S1) -> 
              ((unbox _rnglr_children.[1]) : '_rnglr_type_includes_or_options_or_tokens) 
               |> List.iter (fun (_S2) -> 
                ((unbox _rnglr_children.[2]) : '_rnglr_type_unnamed_module_opt) 
                 |> List.iter (fun (_S3) -> 
                  ((unbox _rnglr_children.[3]) : '_rnglr_type_modules) 
                   |> List.iter (fun (_S4) -> 
                    ((unbox _rnglr_children.[4]) : '_rnglr_type_foot_opt) 
                     |> List.iter (fun (_S5) -> 
                      _rnglr_cycle_res := (
                        
# 117 "Parser.fsy"
                                
                                {
                                    info = { fileName = !currentFilename }
                                    head = _S1
                                    grammar = fst_ _S2 @ _S3 @ _S4
                                    foot = _S5
                                    options = snd_ _S2
                                    tokens = trd_ _S2
                                }
                              
                          )::!_rnglr_cycle_res ) ) ) ) )
            !_rnglr_cycle_res
          )
            )
# 112 "Parser.fsy"
               : '_rnglr_type_file) 
# 455 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          ((unbox _rnglr_children.[0]) : '_rnglr_type_file) 
            )
# 112 "Parser.fsy"
               : '_rnglr_type_yard_start_rule) 
# 465 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            ((unbox _rnglr_children.[0]) : '_rnglr_type_tokens_block) 
             |> List.iter (fun (_S1) -> 
              ((unbox _rnglr_children.[1]) : '_rnglr_type_includes_or_options_or_tokens) 
               |> List.iter (fun (_S2) -> 
                _rnglr_cycle_res := (
                  
# 133 "Parser.fsy"
                                                                   fst_ _S2, snd_ _S2, joinMaps _S1 (trd_ _S2)
                    )::!_rnglr_cycle_res ) )
            !_rnglr_cycle_res
          )
            )
# 128 "Parser.fsy"
               : '_rnglr_type_includes_or_options_or_tokens) 
# 487 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            ((unbox _rnglr_children.[0]) : '_rnglr_type_option_block) 
             |> List.iter (fun (_S1) -> 
              ((unbox _rnglr_children.[1]) : '_rnglr_type_includes_or_options_or_tokens) 
               |> List.iter (fun (_S2) -> 
                _rnglr_cycle_res := (
                  
# 132 "Parser.fsy"
                                                                     fst_ _S2, joinMaps _S1 (snd_ _S2), trd_ _S2
                    )::!_rnglr_cycle_res ) )
            !_rnglr_cycle_res
          )
            )
# 128 "Parser.fsy"
               : '_rnglr_type_includes_or_options_or_tokens) 
# 509 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            ((unbox _rnglr_children.[0]) : '_rnglr_type_include_) 
             |> List.iter (fun (_S1) -> 
              ((unbox _rnglr_children.[1]) : '_rnglr_type_includes_or_options_or_tokens) 
               |> List.iter (fun (_S2) -> 
                _rnglr_cycle_res := (
                  
# 131 "Parser.fsy"
                                                                     (_S1 @ fst_ _S2), snd_ _S2, trd_ _S2
                    )::!_rnglr_cycle_res ) )
            !_rnglr_cycle_res
          )
            )
# 128 "Parser.fsy"
               : '_rnglr_type_includes_or_options_or_tokens) 
# 531 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            _rnglr_cycle_res := (
              
# 130 "Parser.fsy"
                     [],    Map.empty, Map.empty 
                )::!_rnglr_cycle_res
            !_rnglr_cycle_res
          )
            )
# 128 "Parser.fsy"
               : '_rnglr_type_includes_or_options_or_tokens) 
# 549 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            (match ((unbox _rnglr_children.[0]) : Token) with TOKENS_BLOCK _rnglr_val -> [_rnglr_val] | a -> failwith "TOKENS_BLOCK expected, but %A found" a )
             |> List.iter (fun (_S1) -> 
              _rnglr_cycle_res := (
                
# 136 "Parser.fsy"
                   
                    let block = _S1.text
                    let inner = block.[block.IndexOf '{' + 1 .. block.LastIndexOf '}' - 1]
                    inner.Split [|'|'|]
                    |> Array.map (fun s -> s.Trim())
                    |> Array.filter ((<>) "")
                    |> Array.map (fun s ->
                        let genError msg = raise <| ParseError (new Source.t(s, fst parserRange, snd parserRange, !currentFilename),
                                                                "Error in tokens block: " + msg)
                        if Regex.IsMatch(s, @"^(_|[A-Z][A-Za-z0-9_]*)$") then s, None
                        else
                            let m = Regex.Match(s, @"^(_|[A-Z][A-Za-z0-9_]*)\s*of\s*(.*)$")
                            if m.Success then
                                m.Groups.[1].Value, Some m.Groups.[2].Value
                            else
                                if not (System.Char.IsUpper s.[0]) && not (s.[0] = '_' && s.Length > 1 && System.Char.IsWhiteSpace s.[1])
                                then genError "Terminal must start from upper letter"
                                else genError "Token type description is incorrect"
                            
                    ) |> Map.ofArray
                  
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 135 "Parser.fsy"
               : '_rnglr_type_tokens_block) 
# 589 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            (match ((unbox _rnglr_children.[0]) : Token) with INCLUDE _rnglr_val -> [_rnglr_val] | a -> failwith "INCLUDE expected, but %A found" a )
             |> List.iter (fun (_) -> 
              (match ((unbox _rnglr_children.[1]) : Token) with STRING _rnglr_val -> [_rnglr_val] | a -> failwith "STRING expected, but %A found" a )
               |> List.iter (fun (_S2) -> 
                _rnglr_cycle_res := (
                  
# 160 "Parser.fsy"
                       
                          let grammar = (parseRules _S2.text).grammar
                          if grammar |> List.exists (fun m -> m.name.IsNone) then
                              eprintfn "Error %s: Grammar in included files must be inside modules" _S2.text
                          grammar
                      
                    )::!_rnglr_cycle_res ) )
            !_rnglr_cycle_res
          )
            )
# 158 "Parser.fsy"
               : '_rnglr_type_include_) 
# 616 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            (match ((unbox _rnglr_children.[0]) : Token) with OPTIONS_START _rnglr_val -> [_rnglr_val] | a -> failwith "OPTIONS_START expected, but %A found" a )
             |> List.iter (fun (_) -> 
              ((unbox _rnglr_children.[1]) : '_rnglr_type_opts) 
               |> List.iter (fun (_S2) -> 
                (match ((unbox _rnglr_children.[2]) : Token) with BLOCK_END _rnglr_val -> [_rnglr_val] | a -> failwith "BLOCK_END expected, but %A found" a )
                 |> List.iter (fun (_) -> 
                  _rnglr_cycle_res := (
                    
# 167 "Parser.fsy"
                                                                 Map.ofList _S2 : Map<_,_>
                      )::!_rnglr_cycle_res ) ) )
            !_rnglr_cycle_res
          )
            )
# 167 "Parser.fsy"
               : '_rnglr_type_option_block) 
# 640 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            _rnglr_cycle_res := (
              
# 169 "Parser.fsy"
                                               [] 
                )::!_rnglr_cycle_res
            !_rnglr_cycle_res
          )
            )
# 169 "Parser.fsy"
               : '_rnglr_type_opts) 
# 658 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            ((unbox _rnglr_children.[0]) : '_rnglr_type_option) 
             |> List.iter (fun (_S1) -> 
              ((unbox _rnglr_children.[1]) : '_rnglr_type_opts) 
               |> List.iter (fun (_S2) -> 
                _rnglr_cycle_res := (
                  
# 169 "Parser.fsy"
                                      _S1::_S2 
                    )::!_rnglr_cycle_res ) )
            !_rnglr_cycle_res
          )
            )
# 169 "Parser.fsy"
               : '_rnglr_type_opts) 
# 680 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            ((unbox _rnglr_children.[0]) : '_rnglr_type_ident) 
             |> List.iter (fun (_S1) -> 
              (match ((unbox _rnglr_children.[1]) : Token) with EQUAL _rnglr_val -> [_rnglr_val] | a -> failwith "EQUAL expected, but %A found" a )
               |> List.iter (fun (_) -> 
                ((unbox _rnglr_children.[2]) : '_rnglr_type_option_value) 
                 |> List.iter (fun (_S3) -> 
                  _rnglr_cycle_res := (
                    
# 171 "Parser.fsy"
                                                       (_S1 : Source.t).text, (_S3 : Source.t).text 
                      )::!_rnglr_cycle_res ) ) )
            !_rnglr_cycle_res
          )
            )
# 171 "Parser.fsy"
               : '_rnglr_type_option) 
# 704 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            ((unbox _rnglr_children.[0]) : '_rnglr_type_kw) 
             |> List.iter (fun (_S1) -> 
              _rnglr_cycle_res := (
                
# 173 "Parser.fsy"
                                                                  _S1 
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 173 "Parser.fsy"
               : '_rnglr_type_option_value) 
# 724 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            (match ((unbox _rnglr_children.[0]) : Token) with STRING _rnglr_val -> [_rnglr_val] | a -> failwith "STRING expected, but %A found" a )
             |> List.iter (fun (_S1) -> 
              _rnglr_cycle_res := (
                
# 173 "Parser.fsy"
                                                      _S1 
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 173 "Parser.fsy"
               : '_rnglr_type_option_value) 
# 744 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            ((unbox _rnglr_children.[0]) : '_rnglr_type_ident) 
             |> List.iter (fun (_S1) -> 
              _rnglr_cycle_res := (
                
# 173 "Parser.fsy"
                                      _S1 
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 173 "Parser.fsy"
               : '_rnglr_type_option_value) 
# 764 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            ((unbox _rnglr_children.[0]) : '_rnglr_type_rule_nlist) 
             |> List.iter (fun (_S1) -> 
              _rnglr_cycle_res := (
                
# 176 "Parser.fsy"
                     
                        match _S1 with
                        | [] -> []
                        | x ->  defaultModules x
                    
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 175 "Parser.fsy"
               : '_rnglr_type_unnamed_module_opt) 
# 788 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            _rnglr_cycle_res := (
              
# 182 "Parser.fsy"
                                                         [] 
                )::!_rnglr_cycle_res
            !_rnglr_cycle_res
          )
            )
# 182 "Parser.fsy"
               : '_rnglr_type_modules) 
# 806 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            ((unbox _rnglr_children.[0]) : '_rnglr_type_module_) 
             |> List.iter (fun (_S1) -> 
              ((unbox _rnglr_children.[1]) : '_rnglr_type_modules) 
               |> List.iter (fun (_S2) -> 
                _rnglr_cycle_res := (
                  
# 182 "Parser.fsy"
                                              _S1 :: _S2 
                    )::!_rnglr_cycle_res ) )
            !_rnglr_cycle_res
          )
            )
# 182 "Parser.fsy"
               : '_rnglr_type_modules) 
# 828 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            ((unbox _rnglr_children.[0]) : '_rnglr_type_module_header) 
             |> List.iter (fun (_S1) -> 
              ((unbox _rnglr_children.[1]) : '_rnglr_type_ident) 
               |> List.iter (fun (_S2) -> 
                ((unbox _rnglr_children.[2]) : '_rnglr_type_openings) 
                 |> List.iter (fun (_S3) -> 
                  ((unbox _rnglr_children.[3]) : '_rnglr_type_rule_nlist) 
                   |> List.iter (fun (_S4) -> 
                    _rnglr_cycle_res := (
                      
# 185 "Parser.fsy"
                           
                              {
                                  allPublic = _S1
                                  name = Some _S2
                                  openings = _S3
                                  rules = _S4
                              }
                          
                        )::!_rnglr_cycle_res ) ) ) )
            !_rnglr_cycle_res
          )
            )
# 184 "Parser.fsy"
               : '_rnglr_type_module_) 
# 861 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            (match ((unbox _rnglr_children.[0]) : Token) with LIDENT _rnglr_val -> [_rnglr_val] | a -> failwith "LIDENT expected, but %A found" a )
             |> List.iter (fun (_S1) -> 
              _rnglr_cycle_res := (
                
# 194 "Parser.fsy"
                                                 _S1 
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 194 "Parser.fsy"
               : '_rnglr_type_ident) 
# 881 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            (match ((unbox _rnglr_children.[0]) : Token) with UIDENT _rnglr_val -> [_rnglr_val] | a -> failwith "UIDENT expected, but %A found" a )
             |> List.iter (fun (_S1) -> 
              _rnglr_cycle_res := (
                
# 194 "Parser.fsy"
                                 _S1 
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 194 "Parser.fsy"
               : '_rnglr_type_ident) 
# 901 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            (match ((unbox _rnglr_children.[0]) : Token) with MODULE _rnglr_val -> [_rnglr_val] | a -> failwith "MODULE expected, but %A found" a )
             |> List.iter (fun (_) -> 
              _rnglr_cycle_res := (
                
# 201 "Parser.fsy"
                                         allPublic := false; false 
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 197 "Parser.fsy"
               : '_rnglr_type_module_header) 
# 921 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            (match ((unbox _rnglr_children.[0]) : Token) with ALL_PUBLIC _rnglr_val -> [_rnglr_val] | a -> failwith "ALL_PUBLIC expected, but %A found" a )
             |> List.iter (fun (_) -> 
              (match ((unbox _rnglr_children.[1]) : Token) with MODULE _rnglr_val -> [_rnglr_val] | a -> failwith "MODULE expected, but %A found" a )
               |> List.iter (fun (_) -> 
                _rnglr_cycle_res := (
                  
# 197 "Parser.fsy"
                                                     
                                    (* It's important the word "module" is here. It guaranties, that it won't be an epsilon-tree, so allPublic will be evaluated before rules *)
                                    allPublic := true; true
                                  
                    )::!_rnglr_cycle_res ) )
            !_rnglr_cycle_res
          )
            )
# 197 "Parser.fsy"
               : '_rnglr_type_module_header) 
# 946 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            (match ((unbox _rnglr_children.[0]) : Token) with OPEN _rnglr_val -> [_rnglr_val] | a -> failwith "OPEN expected, but %A found" a )
             |> List.iter (fun (_) -> 
              ((unbox _rnglr_children.[1]) : '_rnglr_type_ident) 
               |> List.iter (fun (_S2) -> 
                ((unbox _rnglr_children.[2]) : '_rnglr_type_open_list) 
                 |> List.iter (fun (_S3) -> 
                  _rnglr_cycle_res := (
                    
# 203 "Parser.fsy"
                                                                _S2::_S3 
                      )::!_rnglr_cycle_res ) ) )
            !_rnglr_cycle_res
          )
            )
# 203 "Parser.fsy"
               : '_rnglr_type_openings) 
# 970 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            _rnglr_cycle_res := (
              
# 203 "Parser.fsy"
                            [] 
                )::!_rnglr_cycle_res
            !_rnglr_cycle_res
          )
            )
# 203 "Parser.fsy"
               : '_rnglr_type_openings) 
# 988 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            _rnglr_cycle_res := (
              
# 205 "Parser.fsy"
                                                               [] 
                )::!_rnglr_cycle_res
            !_rnglr_cycle_res
          )
            )
# 205 "Parser.fsy"
               : '_rnglr_type_open_list) 
# 1006 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            (match ((unbox _rnglr_children.[0]) : Token) with COMMA _rnglr_val -> [_rnglr_val] | a -> failwith "COMMA expected, but %A found" a )
             |> List.iter (fun (_) -> 
              ((unbox _rnglr_children.[1]) : '_rnglr_type_ident) 
               |> List.iter (fun (_S2) -> 
                ((unbox _rnglr_children.[2]) : '_rnglr_type_open_list) 
                 |> List.iter (fun (_S3) -> 
                  _rnglr_cycle_res := (
                    
# 205 "Parser.fsy"
                                                        _S2::_S3 
                      )::!_rnglr_cycle_res ) ) )
            !_rnglr_cycle_res
          )
            )
# 205 "Parser.fsy"
               : '_rnglr_type_open_list) 
# 1030 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            (match ((unbox _rnglr_children.[0]) : Token) with ACTION _rnglr_val -> [_rnglr_val] | a -> failwith "ACTION expected, but %A found" a )
             |> List.iter (fun (_S1) -> 
              _rnglr_cycle_res := (
                
# 207 "Parser.fsy"
                                                Some _S1 
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 207 "Parser.fsy"
               : '_rnglr_type_action_opt) 
# 1050 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            _rnglr_cycle_res := (
              
# 207 "Parser.fsy"
                            None 
                )::!_rnglr_cycle_res
            !_rnglr_cycle_res
          )
            )
# 207 "Parser.fsy"
               : '_rnglr_type_action_opt) 
# 1068 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            (match ((unbox _rnglr_children.[0]) : Token) with SEMICOLON _rnglr_val -> [_rnglr_val] | a -> failwith "SEMICOLON expected, but %A found" a )
             |> List.iter (fun (_) -> 
              (match ((unbox _rnglr_children.[1]) : Token) with ACTION _rnglr_val -> [_rnglr_val] | a -> failwith "ACTION expected, but %A found" a )
               |> List.iter (fun (_S2) -> 
                _rnglr_cycle_res := (
                  
# 209 "Parser.fsy"
                                                          Some _S2 
                    )::!_rnglr_cycle_res ) )
            !_rnglr_cycle_res
          )
            )
# 209 "Parser.fsy"
               : '_rnglr_type_foot_opt) 
# 1090 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            _rnglr_cycle_res := (
              
# 209 "Parser.fsy"
                          None 
                )::!_rnglr_cycle_res
            !_rnglr_cycle_res
          )
            )
# 209 "Parser.fsy"
               : '_rnglr_type_foot_opt) 
# 1108 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            _rnglr_cycle_res := (
              
# 213 "Parser.fsy"
                      [] 
                )::!_rnglr_cycle_res
            !_rnglr_cycle_res
          )
            )
# 211 "Parser.fsy"
               : '_rnglr_type_rule_nlist) 
# 1126 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            ((unbox _rnglr_children.[0]) : '_rnglr_type_rule) 
             |> List.iter (fun (_S1) -> 
              ((unbox _rnglr_children.[1]) : '_rnglr_type_semi_opt) 
               |> List.iter (fun (_) -> 
                ((unbox _rnglr_children.[2]) : '_rnglr_type_rule_nlist) 
                 |> List.iter (fun (_S3) -> 
                  _rnglr_cycle_res := (
                    
# 212 "Parser.fsy"
                            _S1::_S3 
                      )::!_rnglr_cycle_res ) ) )
            !_rnglr_cycle_res
          )
            )
# 211 "Parser.fsy"
               : '_rnglr_type_rule_nlist) 
# 1150 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            ((unbox _rnglr_children.[0]) : '_rnglr_type_start_rule_sign_opt) 
             |> List.iter (fun (_S1) -> 
              ((unbox _rnglr_children.[1]) : '_rnglr_type_access_modifier_opt) 
               |> List.iter (fun (_S2) -> 
                (match ((unbox _rnglr_children.[2]) : Token) with LIDENT _rnglr_val -> [_rnglr_val] | a -> failwith "LIDENT expected, but %A found" a )
                 |> List.iter (fun (_S3) -> 
                  ((unbox _rnglr_children.[3]) : '_rnglr_type_formal_meta_param_opt) 
                   |> List.iter (fun (_S4) -> 
                    ((unbox _rnglr_children.[4]) : '_rnglr_type_param_list) 
                     |> List.iter (fun (_S5) -> 
                      (match ((unbox _rnglr_children.[5]) : Token) with COLON _rnglr_val -> [_rnglr_val] | a -> failwith "COLON expected, but %A found" a )
                       |> List.iter (fun (_) -> 
                        ((unbox _rnglr_children.[6]) : '_rnglr_type_alts) 
                         |> List.iter (fun (_S7) -> 
                          _rnglr_cycle_res := (
                            
# 216 "Parser.fsy"
                                  
                                    {
                                        Rule.isStart = _S1
                                        Rule.isPublic = _S2
                                        Rule.name = _S3
                                        Rule.metaArgs = getList _S4
                                        Rule.body = _S7
                                        Rule.args = _S5
                                    }
                                
                              )::!_rnglr_cycle_res ) ) ) ) ) ) )
            !_rnglr_cycle_res
          )
            )
# 215 "Parser.fsy"
               : '_rnglr_type_rule) 
# 1191 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            (match ((unbox _rnglr_children.[0]) : Token) with START_RULE_SIGN _rnglr_val -> [_rnglr_val] | a -> failwith "START_RULE_SIGN expected, but %A found" a )
             |> List.iter (fun (_) -> 
              _rnglr_cycle_res := (
                
# 227 "Parser.fsy"
                                                                true
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 227 "Parser.fsy"
               : '_rnglr_type_start_rule_sign_opt) 
# 1211 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            _rnglr_cycle_res := (
              
# 227 "Parser.fsy"
                                    false
                )::!_rnglr_cycle_res
            !_rnglr_cycle_res
          )
            )
# 227 "Parser.fsy"
               : '_rnglr_type_start_rule_sign_opt) 
# 1229 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            _rnglr_cycle_res := (
              
# 229 "Parser.fsy"
                                                                           !allPublic 
                )::!_rnglr_cycle_res
            !_rnglr_cycle_res
          )
            )
# 229 "Parser.fsy"
               : '_rnglr_type_access_modifier_opt) 
# 1247 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            (match ((unbox _rnglr_children.[0]) : Token) with PRIVATE _rnglr_val -> [_rnglr_val] | a -> failwith "PRIVATE expected, but %A found" a )
             |> List.iter (fun (_) -> 
              _rnglr_cycle_res := (
                
# 229 "Parser.fsy"
                                                                 false 
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 229 "Parser.fsy"
               : '_rnglr_type_access_modifier_opt) 
# 1267 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            (match ((unbox _rnglr_children.[0]) : Token) with PUBLIC _rnglr_val -> [_rnglr_val] | a -> failwith "PUBLIC expected, but %A found" a )
             |> List.iter (fun (_) -> 
              _rnglr_cycle_res := (
                
# 229 "Parser.fsy"
                                              true 
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 229 "Parser.fsy"
               : '_rnglr_type_access_modifier_opt) 
# 1287 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            (match ((unbox _rnglr_children.[0]) : Token) with LESS _rnglr_val -> [_rnglr_val] | a -> failwith "LESS expected, but %A found" a )
             |> List.iter (fun (_) -> 
              ((unbox _rnglr_children.[1]) : '_rnglr_type_formal_meta_list) 
               |> List.iter (fun (_S2) -> 
                (match ((unbox _rnglr_children.[2]) : Token) with GREAT _rnglr_val -> [_rnglr_val] | a -> failwith "GREAT expected, but %A found" a )
                 |> List.iter (fun (_) -> 
                  _rnglr_cycle_res := (
                    
# 231 "Parser.fsy"
                                                                                   Some _S2
                      )::!_rnglr_cycle_res ) ) )
            !_rnglr_cycle_res
          )
            )
# 231 "Parser.fsy"
               : '_rnglr_type_formal_meta_param_opt) 
# 1311 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            _rnglr_cycle_res := (
              
# 231 "Parser.fsy"
                                       None 
                )::!_rnglr_cycle_res
            !_rnglr_cycle_res
          )
            )
# 231 "Parser.fsy"
               : '_rnglr_type_formal_meta_param_opt) 
# 1329 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            (match ((unbox _rnglr_children.[0]) : Token) with LIDENT _rnglr_val -> [_rnglr_val] | a -> failwith "LIDENT expected, but %A found" a )
             |> List.iter (fun (_S1) -> 
              ((unbox _rnglr_children.[1]) : '_rnglr_type_formal_meta_list) 
               |> List.iter (fun (_S2) -> 
                _rnglr_cycle_res := (
                  
# 234 "Parser.fsy"
                                                             _S1::_S2
                    )::!_rnglr_cycle_res ) )
            !_rnglr_cycle_res
          )
            )
# 233 "Parser.fsy"
               : '_rnglr_type_formal_meta_list) 
# 1351 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            (match ((unbox _rnglr_children.[0]) : Token) with LIDENT _rnglr_val -> [_rnglr_val] | a -> failwith "LIDENT expected, but %A found" a )
             |> List.iter (fun (_S1) -> 
              _rnglr_cycle_res := (
                
# 233 "Parser.fsy"
                                          [_S1]
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 233 "Parser.fsy"
               : '_rnglr_type_formal_meta_list) 
# 1371 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            (match ((unbox _rnglr_children.[0]) : Token) with PARAM _rnglr_val -> [_rnglr_val] | a -> failwith "PARAM expected, but %A found" a )
             |> List.iter (fun (_S1) -> 
              _rnglr_cycle_res := (
                
# 236 "Parser.fsy"
                                             Some _S1
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 236 "Parser.fsy"
               : '_rnglr_type_param_opt) 
# 1391 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            _rnglr_cycle_res := (
              
# 236 "Parser.fsy"
                           None 
                )::!_rnglr_cycle_res
            !_rnglr_cycle_res
          )
            )
# 236 "Parser.fsy"
               : '_rnglr_type_param_opt) 
# 1409 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            (match ((unbox _rnglr_children.[0]) : Token) with PARAM _rnglr_val -> [_rnglr_val] | a -> failwith "PARAM expected, but %A found" a )
             |> List.iter (fun (_S1) -> 
              ((unbox _rnglr_children.[1]) : '_rnglr_type_param_list) 
               |> List.iter (fun (_S2) -> 
                _rnglr_cycle_res := (
                  
# 238 "Parser.fsy"
                                                         _S1::_S2
                    )::!_rnglr_cycle_res ) )
            !_rnglr_cycle_res
          )
            )
# 238 "Parser.fsy"
               : '_rnglr_type_param_list) 
# 1431 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            _rnglr_cycle_res := (
              
# 238 "Parser.fsy"
                            [] 
                )::!_rnglr_cycle_res
            !_rnglr_cycle_res
          )
            )
# 238 "Parser.fsy"
               : '_rnglr_type_param_list) 
# 1449 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            (match ((unbox _rnglr_children.[0]) : Token) with SQR_LBR _rnglr_val -> [_rnglr_val] | a -> failwith "SQR_LBR expected, but %A found" a )
             |> List.iter (fun (_) -> 
              (match ((unbox _rnglr_children.[1]) : Token) with NUMBER _rnglr_val -> [_rnglr_val] | a -> failwith "NUMBER expected, but %A found" a )
               |> List.iter (fun (_S2) -> 
                (match ((unbox _rnglr_children.[2]) : Token) with SQR_RBR _rnglr_val -> [_rnglr_val] | a -> failwith "SQR_RBR expected, but %A found" a )
                 |> List.iter (fun (_) -> 
                  _rnglr_cycle_res := (
                    
# 240 "Parser.fsy"
                                                                    Some _S2
                      )::!_rnglr_cycle_res ) ) )
            !_rnglr_cycle_res
          )
            )
# 240 "Parser.fsy"
               : '_rnglr_type_weight_opt) 
# 1473 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            _rnglr_cycle_res := (
              
# 240 "Parser.fsy"
                            None 
                )::!_rnglr_cycle_res
            !_rnglr_cycle_res
          )
            )
# 240 "Parser.fsy"
               : '_rnglr_type_weight_opt) 
# 1491 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            ((unbox _rnglr_children.[0]) : '_rnglr_type_seq) 
             |> List.iter (fun (_S1) -> 
              ((unbox _rnglr_children.[1]) : '_rnglr_type_bar_seq_nlist) 
               |> List.iter (fun (_S2) -> 
                _rnglr_cycle_res := (
                  
# 242 "Parser.fsy"
                                                        PAlt (_S1,_S2)
                    )::!_rnglr_cycle_res ) )
            !_rnglr_cycle_res
          )
            )
# 242 "Parser.fsy"
               : '_rnglr_type_alts) 
# 1513 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            ((unbox _rnglr_children.[0]) : '_rnglr_type_seq) 
             |> List.iter (fun (_S1) -> 
              _rnglr_cycle_res := (
                
# 242 "Parser.fsy"
                            _S1 
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 242 "Parser.fsy"
               : '_rnglr_type_alts) 
# 1533 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            (match ((unbox _rnglr_children.[0]) : Token) with BAR _rnglr_val -> [_rnglr_val] | a -> failwith "BAR expected, but %A found" a )
             |> List.iter (fun (_) -> 
              ((unbox _rnglr_children.[1]) : '_rnglr_type_seq) 
               |> List.iter (fun (_S2) -> 
                _rnglr_cycle_res := (
                  
# 245 "Parser.fsy"
                                           _S2
                    )::!_rnglr_cycle_res ) )
            !_rnglr_cycle_res
          )
            )
# 244 "Parser.fsy"
               : '_rnglr_type_bar_seq_nlist) 
# 1555 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            (match ((unbox _rnglr_children.[0]) : Token) with BAR _rnglr_val -> [_rnglr_val] | a -> failwith "BAR expected, but %A found" a )
             |> List.iter (fun (_) -> 
              ((unbox _rnglr_children.[1]) : '_rnglr_type_seq) 
               |> List.iter (fun (_S2) -> 
                ((unbox _rnglr_children.[2]) : '_rnglr_type_bar_seq_nlist) 
                 |> List.iter (fun (_S3) -> 
                  _rnglr_cycle_res := (
                    
# 244 "Parser.fsy"
                                                           PAlt(_S2,_S3) 
                      )::!_rnglr_cycle_res ) ) )
            !_rnglr_cycle_res
          )
            )
# 244 "Parser.fsy"
               : '_rnglr_type_bar_seq_nlist) 
# 1579 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            ((unbox _rnglr_children.[0]) : '_rnglr_type_lbl_seq) 
             |> List.iter (fun (_S1) -> 
              _rnglr_cycle_res := (
                
# 247 "Parser.fsy"
                                                _S1
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 247 "Parser.fsy"
               : '_rnglr_type_seq) 
# 1599 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            ((unbox _rnglr_children.[0]) : '_rnglr_type_no_lbl_seq) 
             |> List.iter (fun (_S1) -> 
              _rnglr_cycle_res := (
                
# 247 "Parser.fsy"
                                 _S1
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 247 "Parser.fsy"
               : '_rnglr_type_seq) 
# 1619 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            (match ((unbox _rnglr_children.[0]) : Token) with ACTION _rnglr_val -> [_rnglr_val] | a -> failwith "ACTION expected, but %A found" a )
             |> List.iter (fun (_S1) -> 
              _rnglr_cycle_res := (
                
# 250 "Parser.fsy"
                                     PSeq([], Some _S1, None) 
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 249 "Parser.fsy"
               : '_rnglr_type_no_lbl_seq) 
# 1639 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            ((unbox _rnglr_children.[0]) : '_rnglr_type_seq_elem) 
             |> List.iter (fun (_S1) -> 
              ((unbox _rnglr_children.[1]) : '_rnglr_type_seq_elem_list) 
               |> List.iter (fun (_S2) -> 
                ((unbox _rnglr_children.[2]) : '_rnglr_type_action_opt) 
                 |> List.iter (fun (_S3) -> 
                  _rnglr_cycle_res := (
                    
# 249 "Parser.fsy"
                                                                    PSeq (_S1::_S2, _S3, None)
                      )::!_rnglr_cycle_res ) ) )
            !_rnglr_cycle_res
          )
            )
# 249 "Parser.fsy"
               : '_rnglr_type_no_lbl_seq) 
# 1663 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            (match ((unbox _rnglr_children.[0]) : Token) with DLABEL _rnglr_val -> [_rnglr_val] | a -> failwith "DLABEL expected, but %A found" a )
             |> List.iter (fun (_S1) -> 
              ((unbox _rnglr_children.[1]) : '_rnglr_type_weight_opt) 
               |> List.iter (fun (_S2) -> 
                (match ((unbox _rnglr_children.[2]) : Token) with LPAREN _rnglr_val -> [_rnglr_val] | a -> failwith "LPAREN expected, but %A found" a )
                 |> List.iter (fun (_) -> 
                  ((unbox _rnglr_children.[3]) : '_rnglr_type_no_lbl_seq) 
                   |> List.iter (fun (_S4) -> 
                    (match ((unbox _rnglr_children.[4]) : Token) with RPAREN _rnglr_val -> [_rnglr_val] | a -> failwith "RPAREN expected, but %A found" a )
                     |> List.iter (fun (_) -> 
                      _rnglr_cycle_res := (
                        
# 252 "Parser.fsy"
                                                                             makeNewSeq _S4 _S1 _S2
                          )::!_rnglr_cycle_res ) ) ) ) )
            !_rnglr_cycle_res
          )
            )
# 252 "Parser.fsy"
               : '_rnglr_type_lbl_seq) 
# 1691 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            ((unbox _rnglr_children.[0]) : '_rnglr_type_seq_elem) 
             |> List.iter (fun (_S1) -> 
              ((unbox _rnglr_children.[1]) : '_rnglr_type_seq_elem_list) 
               |> List.iter (fun (_S2) -> 
                _rnglr_cycle_res := (
                  
# 254 "Parser.fsy"
                                                                  _S1::_S2
                    )::!_rnglr_cycle_res ) )
            !_rnglr_cycle_res
          )
            )
# 254 "Parser.fsy"
               : '_rnglr_type_seq_elem_list) 
# 1713 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            _rnglr_cycle_res := (
              
# 254 "Parser.fsy"
                               [] 
                )::!_rnglr_cycle_res
            !_rnglr_cycle_res
          )
            )
# 254 "Parser.fsy"
               : '_rnglr_type_seq_elem_list) 
# 1731 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            ((unbox _rnglr_children.[0]) : '_rnglr_type_omit_opt) 
             |> List.iter (fun (_S1) -> 
              ((unbox _rnglr_children.[1]) : '_rnglr_type_bound) 
               |> List.iter (fun (_S2) -> 
                ((unbox _rnglr_children.[2]) : '_rnglr_type_predicate_opt) 
                 |> List.iter (fun (_S3) -> 
                  _rnglr_cycle_res := (
                    
# 256 "Parser.fsy"
                                                            {_S2 with checker = _S3; omit = _S1 }
                      )::!_rnglr_cycle_res ) ) )
            !_rnglr_cycle_res
          )
            )
# 256 "Parser.fsy"
               : '_rnglr_type_seq_elem) 
# 1755 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            (match ((unbox _rnglr_children.[0]) : Token) with MINUS _rnglr_val -> [_rnglr_val] | a -> failwith "MINUS expected, but %A found" a )
             |> List.iter (fun (_) -> 
              _rnglr_cycle_res := (
                
# 258 "Parser.fsy"
                                              true 
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 258 "Parser.fsy"
               : '_rnglr_type_omit_opt) 
# 1775 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            _rnglr_cycle_res := (
              
# 258 "Parser.fsy"
                          false 
                )::!_rnglr_cycle_res
            !_rnglr_cycle_res
          )
            )
# 258 "Parser.fsy"
               : '_rnglr_type_omit_opt) 
# 1793 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            (match ((unbox _rnglr_children.[0]) : Token) with SEMICOLON _rnglr_val -> [_rnglr_val] | a -> failwith "SEMICOLON expected, but %A found" a )
             |> List.iter (fun (_) -> 
              _rnglr_cycle_res := (
                
# 260 "Parser.fsy"
                                                  true
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 260 "Parser.fsy"
               : '_rnglr_type_semi_opt) 
# 1813 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            _rnglr_cycle_res := (
              
# 260 "Parser.fsy"
                           false 
                )::!_rnglr_cycle_res
            !_rnglr_cycle_res
          )
            )
# 260 "Parser.fsy"
               : '_rnglr_type_semi_opt) 
# 1831 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            (match ((unbox _rnglr_children.[0]) : Token) with PREDICATE _rnglr_val -> [_rnglr_val] | a -> failwith "PREDICATE expected, but %A found" a )
             |> List.iter (fun (_S1) -> 
              _rnglr_cycle_res := (
                
# 262 "Parser.fsy"
                                                      Some _S1 
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 262 "Parser.fsy"
               : '_rnglr_type_predicate_opt) 
# 1851 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            _rnglr_cycle_res := (
              
# 262 "Parser.fsy"
                               None 
                )::!_rnglr_cycle_res
            !_rnglr_cycle_res
          )
            )
# 262 "Parser.fsy"
               : '_rnglr_type_predicate_opt) 
# 1869 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            ((unbox _rnglr_children.[0]) : '_rnglr_type_prim) 
             |> List.iter (fun (_S1) -> 
              _rnglr_cycle_res := (
                
# 265 "Parser.fsy"
                                         createSeqElem None false _S1 None      
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 264 "Parser.fsy"
               : '_rnglr_type_bound) 
# 1889 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            ((unbox _rnglr_children.[0]) : '_rnglr_type_patt) 
             |> List.iter (fun (_S1) -> 
              (match ((unbox _rnglr_children.[1]) : Token) with EQUAL _rnglr_val -> [_rnglr_val] | a -> failwith "EQUAL expected, but %A found" a )
               |> List.iter (fun (_) -> 
                ((unbox _rnglr_children.[2]) : '_rnglr_type_prim) 
                 |> List.iter (fun (_S3) -> 
                  _rnglr_cycle_res := (
                    
# 264 "Parser.fsy"
                                             createSeqElem (Some _S1) false _S3 None 
                      )::!_rnglr_cycle_res ) ) )
            !_rnglr_cycle_res
          )
            )
# 264 "Parser.fsy"
               : '_rnglr_type_bound) 
# 1913 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            (match ((unbox _rnglr_children.[0]) : Token) with ACTION _rnglr_val -> [_rnglr_val] | a -> failwith "ACTION expected, but %A found" a )
             |> List.iter (fun (_S1) -> 
              _rnglr_cycle_res := (
                
# 267 "Parser.fsy"
                                            _S1
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 267 "Parser.fsy"
               : '_rnglr_type_patt) 
# 1933 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            (match ((unbox _rnglr_children.[0]) : Token) with LIDENT _rnglr_val -> [_rnglr_val] | a -> failwith "LIDENT expected, but %A found" a )
             |> List.iter (fun (_S1) -> 
              _rnglr_cycle_res := (
                
# 267 "Parser.fsy"
                              _S1
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 267 "Parser.fsy"
               : '_rnglr_type_patt) 
# 1953 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            (match ((unbox _rnglr_children.[0]) : Token) with STRING _rnglr_val -> [_rnglr_val] | a -> failwith "STRING expected, but %A found" a )
             |> List.iter (fun (_S1) -> 
              _rnglr_cycle_res := (
                
# 276 "Parser.fsy"
                                            PLiteral _S1
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 269 "Parser.fsy"
               : '_rnglr_type_prim) 
# 1973 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            ((unbox _rnglr_children.[0]) : '_rnglr_type_lbl_seq) 
             |> List.iter (fun (_S1) -> 
              _rnglr_cycle_res := (
                
# 275 "Parser.fsy"
                                            _S1
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 269 "Parser.fsy"
               : '_rnglr_type_prim) 
# 1993 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            ((unbox _rnglr_children.[0]) : '_rnglr_type_call) 
             |> List.iter (fun (_S1) -> 
              _rnglr_cycle_res := (
                
# 274 "Parser.fsy"
                                            _S1
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 269 "Parser.fsy"
               : '_rnglr_type_prim) 
# 2013 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            (match ((unbox _rnglr_children.[0]) : Token) with LPAREN _rnglr_val -> [_rnglr_val] | a -> failwith "LPAREN expected, but %A found" a )
             |> List.iter (fun (_) -> 
              ((unbox _rnglr_children.[1]) : '_rnglr_type_alts) 
               |> List.iter (fun (_S2) -> 
                (match ((unbox _rnglr_children.[2]) : Token) with RPAREN _rnglr_val -> [_rnglr_val] | a -> failwith "RPAREN expected, but %A found" a )
                 |> List.iter (fun (_) -> 
                  _rnglr_cycle_res := (
                    
# 273 "Parser.fsy"
                                                _S2
                      )::!_rnglr_cycle_res ) ) )
            !_rnglr_cycle_res
          )
            )
# 269 "Parser.fsy"
               : '_rnglr_type_prim) 
# 2037 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            (match ((unbox _rnglr_children.[0]) : Token) with SQR_LBR _rnglr_val -> [_rnglr_val] | a -> failwith "SQR_LBR expected, but %A found" a )
             |> List.iter (fun (_) -> 
              ((unbox _rnglr_children.[1]) : '_rnglr_type_prim) 
               |> List.iter (fun (_S2) -> 
                (match ((unbox _rnglr_children.[2]) : Token) with SQR_RBR _rnglr_val -> [_rnglr_val] | a -> failwith "SQR_RBR expected, but %A found" a )
                 |> List.iter (fun (_) -> 
                  _rnglr_cycle_res := (
                    
# 272 "Parser.fsy"
                                                POpt _S2
                      )::!_rnglr_cycle_res ) ) )
            !_rnglr_cycle_res
          )
            )
# 269 "Parser.fsy"
               : '_rnglr_type_prim) 
# 2061 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            ((unbox _rnglr_children.[0]) : '_rnglr_type_prim) 
             |> List.iter (fun (_S1) -> 
              (match ((unbox _rnglr_children.[1]) : Token) with QUESTION _rnglr_val -> [_rnglr_val] | a -> failwith "QUESTION expected, but %A found" a )
               |> List.iter (fun (_) -> 
                _rnglr_cycle_res := (
                  
# 271 "Parser.fsy"
                                              POpt _S1
                    )::!_rnglr_cycle_res ) )
            !_rnglr_cycle_res
          )
            )
# 269 "Parser.fsy"
               : '_rnglr_type_prim) 
# 2083 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            ((unbox _rnglr_children.[0]) : '_rnglr_type_prim) 
             |> List.iter (fun (_S1) -> 
              (match ((unbox _rnglr_children.[1]) : Token) with PLUS _rnglr_val -> [_rnglr_val] | a -> failwith "PLUS expected, but %A found" a )
               |> List.iter (fun (_) -> 
                _rnglr_cycle_res := (
                  
# 270 "Parser.fsy"
                                              PSome _S1
                    )::!_rnglr_cycle_res ) )
            !_rnglr_cycle_res
          )
            )
# 269 "Parser.fsy"
               : '_rnglr_type_prim) 
# 2105 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            ((unbox _rnglr_children.[0]) : '_rnglr_type_prim) 
             |> List.iter (fun (_S1) -> 
              (match ((unbox _rnglr_children.[1]) : Token) with STAR _rnglr_val -> [_rnglr_val] | a -> failwith "STAR expected, but %A found" a )
               |> List.iter (fun (_) -> 
                _rnglr_cycle_res := (
                  
# 269 "Parser.fsy"
                                              PMany _S1
                    )::!_rnglr_cycle_res ) )
            !_rnglr_cycle_res
          )
            )
# 269 "Parser.fsy"
               : '_rnglr_type_prim) 
# 2127 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            ((unbox _rnglr_children.[0]) : '_rnglr_type_prim) 
             |> List.iter (fun (_S1) -> 
              _rnglr_cycle_res := (
                
# 278 "Parser.fsy"
                                  _S1
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 278 "Parser.fsy"
               : '_rnglr_type_meta_param) 
# 2147 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            ((unbox _rnglr_children.[0]) : '_rnglr_type_meta_param) 
             |> List.iter (fun (_S1) -> 
              ((unbox _rnglr_children.[1]) : '_rnglr_type_meta_params) 
               |> List.iter (fun (_S2) -> 
                _rnglr_cycle_res := (
                  
# 281 "Parser.fsy"
                                                       _S1 :: _S2
                    )::!_rnglr_cycle_res ) )
            !_rnglr_cycle_res
          )
            )
# 280 "Parser.fsy"
               : '_rnglr_type_meta_params) 
# 2169 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            ((unbox _rnglr_children.[0]) : '_rnglr_type_meta_param) 
             |> List.iter (fun (_S1) -> 
              _rnglr_cycle_res := (
                
# 280 "Parser.fsy"
                                         [_S1]
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 280 "Parser.fsy"
               : '_rnglr_type_meta_params) 
# 2189 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            (match ((unbox _rnglr_children.[0]) : Token) with LESS _rnglr_val -> [_rnglr_val] | a -> failwith "LESS expected, but %A found" a )
             |> List.iter (fun (_) -> 
              ((unbox _rnglr_children.[1]) : '_rnglr_type_meta_params) 
               |> List.iter (fun (_S2) -> 
                (match ((unbox _rnglr_children.[2]) : Token) with GREAT _rnglr_val -> [_rnglr_val] | a -> failwith "GREAT expected, but %A found" a )
                 |> List.iter (fun (_) -> 
                  _rnglr_cycle_res := (
                    
# 283 "Parser.fsy"
                                                                       Some _S2
                      )::!_rnglr_cycle_res ) ) )
            !_rnglr_cycle_res
          )
            )
# 283 "Parser.fsy"
               : '_rnglr_type_meta_param_opt) 
# 2213 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            _rnglr_cycle_res := (
              
# 283 "Parser.fsy"
                                None 
                )::!_rnglr_cycle_res
            !_rnglr_cycle_res
          )
            )
# 283 "Parser.fsy"
               : '_rnglr_type_meta_param_opt) 
# 2231 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            (match ((unbox _rnglr_children.[0]) : Token) with LIDENT _rnglr_val -> [_rnglr_val] | a -> failwith "LIDENT expected, but %A found" a )
             |> List.iter (fun (_S1) -> 
              ((unbox _rnglr_children.[1]) : '_rnglr_type_meta_param_opt) 
               |> List.iter (fun (_S2) -> 
                ((unbox _rnglr_children.[2]) : '_rnglr_type_param_opt) 
                 |> List.iter (fun (_S3) -> 
                  _rnglr_cycle_res := (
                    
# 287 "Parser.fsy"
                            match _S2 with
                            | None -> PRef  (_S1, _S3)
                            | Some x -> PMetaRef (_S1,_S3,x)
                          
                      )::!_rnglr_cycle_res ) ) )
            !_rnglr_cycle_res
          )
            )
# 285 "Parser.fsy"
               : '_rnglr_type_call) 
# 2258 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            (match ((unbox _rnglr_children.[0]) : Token) with UIDENT _rnglr_val -> [_rnglr_val] | a -> failwith "UIDENT expected, but %A found" a )
             |> List.iter (fun (_S1) -> 
              _rnglr_cycle_res := (
                
# 285 "Parser.fsy"
                              PToken _S1
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 285 "Parser.fsy"
               : '_rnglr_type_call) 
# 2278 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            (match ((unbox _rnglr_children.[0]) : Token) with EOF _rnglr_val -> [_rnglr_val] | a -> failwith "EOF expected, but %A found" a )
             |> List.iter (fun (_) -> 
              _rnglr_cycle_res := (
                
# 292 "Parser.fsy"
                                                
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 292 "Parser.fsy"
               : '_rnglr_type_tada_rule) 
# 2298 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            (match ((unbox _rnglr_children.[0]) : Token) with SHARPLINE _rnglr_val -> [_rnglr_val] | a -> failwith "SHARPLINE expected, but %A found" a )
             |> List.iter (fun (_) -> 
              _rnglr_cycle_res := (
                
# 292 "Parser.fsy"
                                       
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 292 "Parser.fsy"
               : '_rnglr_type_tada_rule) 
# 2318 "Parser.fs"
      );
  |] , [|
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_access_modifier_opt)   ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_action_opt)   ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_alts)   ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_bar_seq_nlist)   ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_bound)   ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_call)   ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_file)   ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_foot_opt)   ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_formal_meta_list)   ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_formal_meta_param_opt)   ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_ident)   ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_include_)   ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_includes_or_options_or_tokens)   ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_kw)   ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_lbl_seq)   ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_meta_param)   ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_meta_param_opt)   ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_meta_params)   ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_module_)   ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_module_header)   ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_modules)   ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_no_lbl_seq)   ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_omit_opt)   ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_open_list)   ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_openings)   ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_option)   ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_option_block)   ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_option_value)   ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_opts)   ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_param_list)   ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_param_opt)   ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_patt)   ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_predicate_opt)   ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_prim)   ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_rule)   ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_rule_nlist)   ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_semi_opt)   ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_seq)   ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_seq_elem)   ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_seq_elem_list)   ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_start_rule_sign_opt)   ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_tada_rule)   ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_tokens_block)   ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_unnamed_module_opt)   ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_weight_opt)   ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_yard_start_rule)   ) |> List.concat));
  |] 
let translate (args : TranslateArguments<_,_>) (tree : Tree<_>) : '_rnglr_type_yard_start_rule = 
  unbox (tree.Translate _rnglr_rule_  leftSide _rnglr_concats (if args.filterEpsilons then _rnglr_filtered_epsilons else _rnglr_epsilons) args.tokenToRange args.zeroPosition args.clearAST) : '_rnglr_type_yard_start_rule
