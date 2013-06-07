
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
    | RNGLR_EOF of Source.t
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
    | 6 -> "error"
    | 7 -> "file"
    | 8 -> "foot_opt"
    | 9 -> "formal_meta_list"
    | 10 -> "formal_meta_param_opt"
    | 11 -> "ident"
    | 12 -> "include_"
    | 13 -> "includes_or_options_or_tokens"
    | 14 -> "kw"
    | 15 -> "lbl_seq"
    | 16 -> "meta_param"
    | 17 -> "meta_param_opt"
    | 18 -> "meta_params"
    | 19 -> "module_"
    | 20 -> "module_header"
    | 21 -> "modules"
    | 22 -> "no_lbl_seq"
    | 23 -> "omit_opt"
    | 24 -> "open_list"
    | 25 -> "openings"
    | 26 -> "option"
    | 27 -> "option_block"
    | 28 -> "option_value"
    | 29 -> "opts"
    | 30 -> "param_list"
    | 31 -> "param_opt"
    | 32 -> "patt"
    | 33 -> "predicate_opt"
    | 34 -> "prim"
    | 35 -> "rule"
    | 36 -> "rule_nlist"
    | 37 -> "semi_opt"
    | 38 -> "seq"
    | 39 -> "seq_elem"
    | 40 -> "seq_elem_list"
    | 41 -> "start_rule_sign_opt"
    | 42 -> "tada_rule"
    | 43 -> "tokens_block"
    | 44 -> "unnamed_module_opt"
    | 45 -> "weight_opt"
    | 46 -> "yard_start_rule"
    | 47 -> "ACTION"
    | 48 -> "ALL_PUBLIC"
    | 49 -> "BAR"
    | 50 -> "BLOCK_END"
    | 51 -> "COLON"
    | 52 -> "COMMA"
    | 53 -> "DLABEL"
    | 54 -> "EOF"
    | 55 -> "EQUAL"
    | 56 -> "GREAT"
    | 57 -> "INCLUDE"
    | 58 -> "LESS"
    | 59 -> "LIDENT"
    | 60 -> "LPAREN"
    | 61 -> "MINUS"
    | 62 -> "MODULE"
    | 63 -> "NUMBER"
    | 64 -> "OPEN"
    | 65 -> "OPTIONS_START"
    | 66 -> "PARAM"
    | 67 -> "PLUS"
    | 68 -> "PREDICATE"
    | 69 -> "PRIVATE"
    | 70 -> "PUBLIC"
    | 71 -> "QUESTION"
    | 72 -> "RNGLR_EOF"
    | 73 -> "RPAREN"
    | 74 -> "SEMICOLON"
    | 75 -> "SHARPLINE"
    | 76 -> "SQR_LBR"
    | 77 -> "SQR_RBR"
    | 78 -> "STAR"
    | 79 -> "START_RULE_SIGN"
    | 80 -> "STRING"
    | 81 -> "TOKENS_BLOCK"
    | 82 -> "UIDENT"
    | _ -> ""
let tokenToNumber = function
    | ACTION _ -> 47
    | ALL_PUBLIC _ -> 48
    | BAR _ -> 49
    | BLOCK_END _ -> 50
    | COLON _ -> 51
    | COMMA _ -> 52
    | DLABEL _ -> 53
    | EOF _ -> 54
    | EQUAL _ -> 55
    | GREAT _ -> 56
    | INCLUDE _ -> 57
    | LESS _ -> 58
    | LIDENT _ -> 59
    | LPAREN _ -> 60
    | MINUS _ -> 61
    | MODULE _ -> 62
    | NUMBER _ -> 63
    | OPEN _ -> 64
    | OPTIONS_START _ -> 65
    | PARAM _ -> 66
    | PLUS _ -> 67
    | PREDICATE _ -> 68
    | PRIVATE _ -> 69
    | PUBLIC _ -> 70
    | QUESTION _ -> 71
    | RNGLR_EOF _ -> 72
    | RPAREN _ -> 73
    | SEMICOLON _ -> 74
    | SHARPLINE _ -> 75
    | SQR_LBR _ -> 76
    | SQR_RBR _ -> 77
    | STAR _ -> 78
    | START_RULE_SIGN _ -> 79
    | STRING _ -> 80
    | TOKENS_BLOCK _ -> 81
    | UIDENT _ -> 82

let mutable private cur = 0
let leftSide = [|14; 14; 14; 14; 14; 7; 46; 13; 13; 13; 13; 43; 12; 27; 29; 29; 26; 28; 28; 28; 44; 21; 21; 19; 11; 11; 20; 20; 25; 25; 24; 24; 1; 1; 8; 8; 36; 36; 35; 41; 41; 0; 0; 0; 10; 10; 9; 9; 31; 31; 30; 30; 45; 45; 2; 2; 3; 3; 38; 38; 22; 22; 15; 40; 40; 39; 23; 23; 37; 37; 33; 33; 4; 4; 32; 32; 34; 34; 34; 34; 34; 34; 34; 34; 16; 18; 18; 17; 17; 5; 5; 42; 42|]
let private rules = [|69; 70; 64; 57; 62; 1; 13; 44; 21; 8; 54; 7; 43; 13; 27; 13; 12; 13; 81; 57; 80; 65; 29; 50; 26; 29; 11; 55; 28; 14; 80; 11; 36; 19; 21; 20; 11; 25; 36; 59; 82; 62; 48; 62; 64; 11; 24; 52; 11; 24; 47; 74; 47; 35; 37; 36; 41; 0; 59; 10; 30; 51; 2; 79; 69; 70; 58; 9; 56; 59; 9; 59; 66; 66; 30; 76; 63; 77; 38; 3; 38; 49; 38; 49; 38; 3; 15; 22; 47; 39; 40; 1; 53; 45; 60; 22; 73; 39; 40; 23; 4; 33; 61; 74; 68; 34; 32; 55; 34; 47; 59; 80; 15; 5; 60; 2; 73; 76; 34; 77; 34; 71; 34; 67; 34; 78; 34; 16; 18; 16; 58; 18; 56; 59; 17; 31; 82; 54; 75|]
let private rulesStart = [|0; 1; 2; 3; 4; 5; 11; 12; 14; 16; 18; 18; 19; 21; 24; 24; 26; 29; 30; 31; 32; 33; 33; 35; 39; 40; 41; 42; 44; 47; 47; 47; 50; 51; 51; 53; 53; 53; 56; 63; 64; 64; 64; 65; 66; 69; 69; 71; 72; 73; 73; 75; 75; 78; 78; 80; 81; 83; 86; 87; 88; 89; 92; 97; 99; 99; 102; 103; 103; 104; 104; 105; 105; 106; 109; 110; 111; 112; 113; 114; 117; 120; 122; 124; 126; 127; 129; 130; 133; 133; 136; 137; 138; 139|]
let startRule = 6

let acceptEmptyInput = false

let defaultAstToDot =
    (fun (tree : Yard.Generators.RNGLR.AST.Tree<Token>) -> tree.AstToDot numToString tokenToNumber leftSide)

let private lists_gotos = [|1; 129; 83; 2; 29; 4; 6; 8; 10; 28; 3; 5; 7; 9; 11; 24; 26; 17; 23; 12; 13; 14; 15; 16; 18; 19; 20; 21; 22; 25; 27; 30; 107; 33; 108; 105; 31; 106; 32; 34; 103; 104; 35; 36; 98; 37; 96; 38; 39; 40; 41; 42; 72; 77; 84; 54; 80; 43; 46; 47; 48; 90; 95; 91; 69; 85; 88; 89; 44; 45; 49; 50; 59; 51; 52; 53; 55; 92; 56; 57; 58; 60; 63; 61; 62; 64; 67; 66; 65; 68; 70; 71; 73; 74; 75; 76; 78; 81; 79; 82; 86; 87; 93; 94; 97; 99; 101; 100; 102; 109; 110; 124; 121; 123; 120; 111; 112; 114; 113; 115; 116; 117; 118; 119; 122; 125; 127; 126; 128|]
let private small_gotos =
        [|3; 65536; 458753; 3080194; 65543; 786435; 851972; 1769477; 2818054; 3735559; 4259848; 5308425; 131079; 786435; 851978; 1769477; 2818054; 3735559; 4259848; 5308425; 262151; 786435; 851979; 1769477; 2818054; 3735559; 4259848; 5308425; 393223; 786435; 851980; 1769477; 2818054; 3735559; 4259848; 5308425; 524289; 5242893; 655365; 720910; 1703951; 1900560; 3866641; 5373970; 720897; 3604499; 786443; 720916; 917525; 1835030; 3735575; 3866641; 4063256; 4194329; 4522010; 4587547; 5242908; 5373970; 1572869; 720910; 1703951; 1900573; 3866641; 5373970; 1703937; 3276830; 1900549; 2293791; 2359328; 2687009; 2883618; 5177379; 1966082; 2424868; 4849701; 2031620; 2293791; 2359334; 2687009; 5177379; 2162691; 39; 4522024; 4587561; 2228225; 3866666; 2293762; 655403; 3801132; 2359298; 1966125; 4325422; 2424833; 3342383; 2490377; 131120; 983089; 1441842; 1507379; 2490420; 2555957; 3080246; 3473463; 3997752; 2752524; 262201; 327738; 983099; 2097212; 2228285; 3080254; 3473463; 3866687; 3932224; 4980801; 5242946; 5374019; 2818050; 2162756; 4456517; 3145729; 3604550; 3211273; 327738; 983099; 2228295; 3473463; 3866696; 3932224; 4980801; 5242946; 5374019; 3276803; 4390985; 4653130; 5111883; 3538946; 2949196; 4980813; 3604481; 3932238; 3670021; 1441871; 1507379; 2555957; 3080246; 3997752; 3735553; 4784208; 3866626; 1114193; 3801170; 3932162; 2031699; 4325460; 4128779; 327738; 983099; 1048661; 1179734; 2228311; 3473463; 3866696; 3932224; 4980801; 5242946; 5374019; 4194315; 327738; 983099; 1048661; 1179736; 2228311; 3473463; 3866696; 3932224; 4980801; 5242946; 5374019; 4325379; 4390985; 4653130; 5111883; 4390913; 3670105; 4521993; 131162; 983089; 1441842; 1507379; 2490420; 2555957; 3080246; 3473463; 3997752; 4587521; 4784219; 4718594; 196700; 3211357; 4849672; 983089; 1441842; 1507379; 2490462; 2555957; 3080246; 3473463; 3997752; 4915202; 196703; 3211357; 5046276; 1507379; 2556000; 2621537; 3997752; 5111812; 1507379; 2556000; 2621538; 3997752; 5308418; 65635; 3080194; 5570569; 327738; 983099; 2228324; 3473463; 3866696; 3932224; 4980801; 5242946; 5374019; 5636100; 4390985; 4653130; 5046373; 5111883; 5898243; 4390985; 4653130; 5111883; 5963778; 1114193; 3801170; 6029313; 4128870; 6094849; 5046375; 6291458; 1966184; 4325422; 6422530; 589929; 3866730; 6488065; 3670123; 6619138; 589932; 3866730; 7077893; 1245293; 1310830; 1376367; 3145840; 4063345; 7143429; 1245293; 1310830; 1376370; 3145840; 4063345; 7208963; 721011; 3866641; 5373970; 7274498; 1638516; 4194421; 7340036; 2293791; 2359414; 2687009; 5177379; 7471107; 721015; 3866641; 5373970; 7536642; 1572984; 3407993; 7667715; 721018; 3866641; 5373970; 7733250; 1572987; 3407993; 7929857; 4063356; 8126466; 524413; 4849790; 8192001; 3539071; 8323073; 3080320|]
let gotos = Array.zeroCreate 130
for i = 0 to 129 do
        gotos.[i] <- Array.zeroCreate 83
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
let private lists_reduces = [|[|9,1|]; [|9,2|]; [|8,1|]; [|8,2|]; [|7,1|]; [|7,2|]; [|12,2|]; [|19,1|]; [|17,1|]; [|16,3|]; [|3,1|]; [|24,1|]; [|4,1|]; [|2,1|]; [|0,1|]; [|1,1|]; [|18,1|]; [|25,1|]; [|15,1|]; [|15,2|]; [|13,3|]; [|11,1|]; [|37,1|]; [|37,2|]; [|37,3|]; [|38,7|]; [|58,1|]; [|59,1|]; [|65,2|]; [|65,3|]; [|70,1|]; [|78,1|]; [|77,1|]; [|73,3|]; [|82,2|]; [|81,2|]; [|83,2|]; [|62,5|]; [|89,1|]; [|89,2|]; [|89,3|]; [|48,1|]; [|86,1|]; [|85,2|]; [|84,1|]; [|87,3|]; [|79,3|]; [|55,1|]; [|54,2|]; [|56,2|]; [|57,3|]; [|61,1|]; [|63,1|]; [|63,2|]; [|66,1|]; [|61,2|]; [|61,3|]; [|32,1|]; [|60,1|]; [|80,3|]; [|76,1|]; [|90,1|]; [|72,1|]; [|75,1|]; [|52,3|]; [|74,1|]; [|50,1|]; [|50,2|]; [|44,3|]; [|47,1|]; [|46,2|]; [|42,1|]; [|43,1|]; [|39,1|]; [|68,1|]; [|20,1|]; [|22,1|]; [|23,2|]; [|23,3|]; [|23,4|]; [|28,2|]; [|28,3|]; [|31,2|]; [|31,3|]; [|22,2|]; [|27,2|]; [|26,1|]; [|5,6|]; [|34,2|]|]
let private small_reduces =
        [|131080; 3145728; 3538944; 3866624; 4063232; 4521984; 4587520; 4849664; 5177344; 196616; 3145729; 3538945; 3866625; 4063233; 4521985; 4587521; 4849665; 5177345; 262152; 3145730; 3538946; 3866626; 4063234; 4521986; 4587522; 4849666; 5177346; 327688; 3145731; 3538947; 3866627; 4063235; 4521987; 4587523; 4849667; 5177347; 393224; 3145732; 3538948; 3866628; 4063236; 4521988; 4587524; 4849668; 5177348; 458760; 3145733; 3538949; 3866629; 4063237; 4521989; 4587525; 4849669; 5177349; 589835; 3145734; 3538950; 3735558; 3866630; 4063238; 4259846; 4521990; 4587526; 4849670; 5177350; 5308422; 851971; 3276807; 3866631; 5373959; 917507; 3276808; 3866632; 5373960; 983043; 3276809; 3866633; 5373961; 1048579; 3276810; 3866634; 5373962; 1114125; 3145739; 3276811; 3407883; 3538955; 3604491; 3866635; 4063243; 4194315; 4521995; 4587531; 4849675; 5177355; 5373963; 1179651; 3276812; 3866636; 5373964; 1245187; 3276813; 3866637; 5373965; 1310723; 3276814; 3866638; 5373966; 1376259; 3276815; 3866639; 5373967; 1441795; 3276816; 3866640; 5373968; 1507341; 3145745; 3276817; 3407889; 3538961; 3604497; 3866641; 4063249; 4194321; 4522001; 4587537; 4849681; 5177361; 5373969; 1572865; 3276818; 1638401; 3276819; 1769483; 3145748; 3538964; 3735572; 3866644; 4063252; 4259860; 4522004; 4587540; 4849684; 5177364; 5308436; 1835019; 3145749; 3538965; 3735573; 3866645; 4063253; 4259861; 4522005; 4587541; 4849685; 5177365; 5308437; 1966084; 3145750; 3538966; 4063254; 4849686; 2031620; 3145751; 3538967; 4063255; 4849687; 2097156; 3145752; 3538968; 4063256; 4849688; 2555912; 3145753; 3538969; 3866649; 4063257; 4522009; 4587545; 4849689; 5177369; 2621450; 3145754; 3211290; 3538970; 3866650; 4063258; 4522010; 4587546; 4784154; 4849690; 5177370; 2686986; 3145755; 3211291; 3538971; 3866651; 4063259; 4522011; 4587547; 4784155; 4849691; 5177371; 2818065; 3080220; 3145756; 3211292; 3473436; 3538972; 3866652; 3932188; 3997724; 4063260; 4522012; 4587548; 4784156; 4849692; 4980764; 5177372; 5242908; 5373980; 2883601; 3080221; 3145757; 3211293; 3473437; 3538973; 3866653; 3932189; 3997725; 4063261; 4522013; 4587549; 4784157; 4849693; 4980765; 5177373; 5242909; 5373981; 2949137; 3080222; 3145758; 3211294; 3473438; 3538974; 3866654; 3932190; 3997726; 4063262; 4522014; 4587550; 4784158; 4849694; 4980766; 5177374; 5242910; 5373982; 3014679; 3080223; 3145759; 3211295; 3473439; 3538975; 3670047; 3866655; 3932191; 3997727; 4063263; 4390943; 4456479; 4522015; 4587551; 4653087; 4784159; 4849695; 4980767; 5046303; 5111839; 5177375; 5242911; 5373983; 3080215; 3080224; 3145760; 3211296; 3473440; 3538976; 3670048; 3866656; 3932192; 3997728; 4063264; 4390944; 4456480; 4522016; 4587552; 4653088; 4784160; 4849696; 4980768; 5046304; 5111840; 5177376; 5242912; 5373984; 3276818; 3080225; 3145761; 3211297; 3473441; 3538977; 3866657; 3932193; 3997729; 4063265; 4456481; 4522017; 4587553; 4784161; 4849697; 4980769; 5177377; 5242913; 5373985; 3342359; 3080226; 3145762; 3211298; 3473442; 3538978; 3670050; 3866658; 3932194; 3997730; 4063266; 4390946; 4456482; 4522018; 4587554; 4653090; 4784162; 4849698; 4980770; 5046306; 5111842; 5177378; 5242914; 5373986; 3407895; 3080227; 3145763; 3211299; 3473443; 3538979; 3670051; 3866659; 3932195; 3997731; 4063267; 4390947; 4456483; 4522019; 4587555; 4653091; 4784163; 4849699; 4980771; 5046307; 5111843; 5177379; 5242915; 5373987; 3473431; 3080228; 3145764; 3211300; 3473444; 3538980; 3670052; 3866660; 3932196; 3997732; 4063268; 4390948; 4456484; 4522020; 4587556; 4653092; 4784164; 4849700; 4980772; 5046308; 5111844; 5177380; 5242916; 5373988; 3801111; 3080229; 3145765; 3211301; 3473445; 3538981; 3670053; 3866661; 3932197; 3997733; 4063269; 4390949; 4456485; 4522021; 4587557; 4653093; 4784165; 4849701; 4980773; 5046309; 5111845; 5177381; 5242917; 5373989; 3866647; 3080230; 3145766; 3211302; 3473446; 3538982; 3670054; 3866662; 3932198; 3997734; 4063270; 4390950; 4456486; 4522022; 4587558; 4653094; 4784166; 4849702; 4980774; 5046310; 5111846; 5177382; 5242918; 5373990; 3932183; 3080231; 3145767; 3211303; 3473447; 3538983; 3670055; 3866663; 3932199; 3997735; 4063271; 4390951; 4456487; 4522023; 4587559; 4653095; 4784167; 4849703; 4980775; 5046311; 5111847; 5177383; 5242919; 5373991; 3997719; 3080232; 3145768; 3211304; 3473448; 3538984; 3670056; 3866664; 3932200; 3997736; 4063272; 4390952; 4456488; 4522024; 4587560; 4653096; 4784168; 4849704; 4980776; 5046312; 5111848; 5177384; 5242920; 5373992; 4063255; 3080233; 3145769; 3211305; 3473449; 3538985; 3670057; 3866665; 3932201; 3997737; 4063273; 4390953; 4456489; 4522025; 4587561; 4653097; 4784169; 4849705; 4980777; 5046313; 5111849; 5177385; 5242921; 5373993; 4194305; 3670058; 4259841; 3670059; 4325383; 3473452; 3670060; 3866668; 3932204; 4980780; 5242924; 5373996; 4456472; 3080237; 3145773; 3211309; 3473453; 3538989; 3670061; 3866669; 3932205; 3997741; 4063277; 4325421; 4390957; 4456493; 4522029; 4587565; 4653101; 4784173; 4849709; 4980781; 5046317; 5111853; 5177389; 5242925; 5373997; 4653079; 3080238; 3145774; 3211310; 3473454; 3538990; 3670062; 3866670; 3932206; 3997742; 4063278; 4390958; 4456494; 4522030; 4587566; 4653102; 4784174; 4849710; 4980782; 5046318; 5111854; 5177390; 5242926; 5373998; 4718601; 3145775; 3538991; 3866671; 4063279; 4522031; 4587567; 4784175; 4849711; 5177391; 4784137; 3145776; 3538992; 3866672; 4063280; 4522032; 4587568; 4784176; 4849712; 5177392; 4915209; 3145777; 3538993; 3866673; 4063281; 4522033; 4587569; 4784177; 4849713; 5177393; 4980745; 3145778; 3538994; 3866674; 4063282; 4522034; 4587570; 4784178; 4849714; 5177394; 5046282; 3145779; 3211315; 3538995; 3866675; 4063283; 4522035; 4587571; 4784179; 4849715; 5177395; 5111819; 3080244; 3145780; 3211316; 3538996; 3866676; 4063284; 4522036; 4587572; 4784180; 4849716; 5177396; 5177355; 3080245; 3145781; 3211317; 3538997; 3866677; 4063285; 4522037; 4587573; 4784181; 4849717; 5177397; 5242887; 3080246; 3473462; 3866678; 3932214; 4980790; 5242934; 5374006; 5308426; 3145783; 3211319; 3538999; 3866679; 4063287; 4522039; 4587575; 4784183; 4849719; 5177399; 5373962; 3145784; 3211320; 3539000; 3866680; 4063288; 4522040; 4587576; 4784184; 4849720; 5177400; 5439501; 3145785; 3211321; 3539001; 3735609; 3866681; 4063289; 4259897; 4522041; 4587577; 4784185; 4849721; 5177401; 5308473; 5505034; 3145786; 3211322; 3539002; 3866682; 4063290; 4522042; 4587578; 4784186; 4849722; 5177402; 5701655; 3080251; 3145787; 3211323; 3473467; 3539003; 3670075; 3866683; 3932219; 3997755; 4063291; 4390971; 4456507; 4522043; 4587579; 4653115; 4784187; 4849723; 4980795; 5046331; 5111867; 5177403; 5242939; 5374011; 5767191; 3080252; 3145788; 3211324; 3473468; 3539004; 3670076; 3866684; 3932220; 3997756; 4063292; 4390972; 4456508; 4522044; 4587580; 4653116; 4784188; 4849724; 4980796; 5046332; 5111868; 5177404; 5242940; 5374012; 5832727; 3080253; 3145789; 3211325; 3473469; 3539005; 3670077; 3866685; 3932221; 3997757; 4063293; 4390973; 4456509; 4522045; 4587581; 4653117; 4784189; 4849725; 4980797; 5046333; 5111869; 5177405; 5242941; 5374013; 5898258; 3080254; 3145790; 3211326; 3473470; 3539006; 3866686; 3932222; 3997758; 4063294; 4456510; 4522046; 4587582; 4784190; 4849726; 4980798; 5177406; 5242942; 5374014; 5963798; 3080230; 3145766; 3211302; 3473446; 3538982; 3604543; 3866662; 3932198; 3997734; 4063270; 4390950; 4456486; 4522022; 4587558; 4653094; 4784166; 4849702; 4980774; 5111846; 5177382; 5242918; 5373990; 6160385; 3932224; 6225921; 3604545; 6291457; 3342402; 6356993; 3342403; 6553602; 3342404; 4325444; 6619137; 3670085; 6684673; 3670086; 6750209; 3866695; 6815745; 3866696; 6881283; 3866697; 4522057; 4587593; 6946824; 3145802; 3539018; 3866698; 4063306; 4522058; 4587594; 4849738; 5177418; 7012356; 3145803; 3539019; 4063307; 4849739; 7143426; 3539020; 4849740; 7274500; 3145805; 3539021; 4063309; 4849741; 7340036; 3145806; 3539022; 4063310; 4849742; 7405572; 3145807; 3539023; 4063311; 4849743; 7536648; 3145808; 3539024; 3866704; 4063312; 4522064; 4587600; 4849744; 5177424; 7602184; 3145809; 3539025; 3866705; 4063313; 4522065; 4587601; 4849745; 5177425; 7733256; 3145810; 3539026; 3866706; 4063314; 4522066; 4587602; 4849746; 5177426; 7798792; 3145811; 3539027; 3866707; 4063315; 4522067; 4587603; 4849747; 5177427; 7864322; 3539028; 4849748; 7995394; 3866709; 5374037; 8060930; 3866710; 5374038; 8257537; 4718679; 8388609; 3539032|]
let reduces = Array.zeroCreate 130
for i = 0 to 129 do
        reduces.[i] <- Array.zeroCreate 83
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
let private lists_zeroReduces = [|[|33|]; [|10|]; [|14|]; [|36; 20|]; [|40|]; [|69|]; [|36|]; [|41|]; [|45|]; [|51|]; [|67|]; [|71|]; [|53|]; [|88|]; [|49|]; [|67; 64|]; [|64|]; [|21|]; [|29|]; [|30|]; [|35|]|]
let private small_zeroReduces =
        [|11; 3145728; 3538944; 3735552; 3866624; 4063232; 4259840; 4521984; 4587520; 4849664; 5177344; 5308416; 65544; 3145729; 3538945; 3866625; 4063233; 4521985; 4587521; 4849665; 5177345; 131080; 3145729; 3538945; 3866625; 4063233; 4521985; 4587521; 4849665; 5177345; 262152; 3145729; 3538945; 3866625; 4063233; 4521985; 4587521; 4849665; 5177345; 393224; 3145729; 3538945; 3866625; 4063233; 4521985; 4587521; 4849665; 5177345; 655361; 3276802; 1572865; 3276802; 1900551; 3145731; 3538947; 3866628; 4063235; 4521988; 4587524; 4849667; 1966088; 3145733; 3538949; 3866629; 4063237; 4521989; 4587525; 4849669; 5177349; 2031623; 3145734; 3538950; 3866628; 4063238; 4521988; 4587524; 4849670; 2162689; 3866631; 2293762; 3342344; 4325384; 2359297; 3342345; 2490375; 3080202; 3473418; 3866634; 3932170; 4980746; 5242890; 5373962; 2818065; 3080203; 3145739; 3211275; 3473419; 3538955; 3866635; 3932171; 3997707; 4063243; 4521995; 4587531; 4784139; 4849675; 4980747; 5177355; 5242891; 5373963; 3538945; 3932172; 3670023; 3080202; 3473418; 3866634; 3932170; 4980746; 5242890; 5373962; 3866648; 3080205; 3145741; 3211277; 3473421; 3538957; 3670029; 3866637; 3932173; 3997709; 4063245; 4325389; 4390925; 4456461; 4521997; 4587533; 4653069; 4784141; 4849677; 4980749; 5046285; 5111821; 5177357; 5242893; 5373965; 3932183; 3080206; 3145742; 3211278; 3473422; 3538958; 3670030; 3866638; 3932174; 3997710; 4063246; 4390926; 4456462; 4521998; 4587534; 4653070; 4784142; 4849678; 4980750; 5046286; 5111822; 5177358; 5242894; 5373966; 4521991; 3080202; 3473418; 3866634; 3932170; 4980746; 5242890; 5373962; 4849671; 3080202; 3473418; 3866634; 3932170; 4980746; 5242890; 5373962; 5046288; 3080207; 3145744; 3211280; 3473418; 3538960; 3866639; 3932170; 4063248; 4522000; 4587536; 4784144; 4849680; 4980746; 5177360; 5242890; 5373962; 5111824; 3080207; 3145744; 3211280; 3473418; 3538960; 3866639; 3932170; 4063248; 4522000; 4587536; 4784144; 4849680; 4980746; 5177360; 5242890; 5373962; 5308426; 3145728; 3211264; 3538944; 3866624; 4063232; 4521984; 4587520; 4784128; 4849664; 5177344; 5963798; 3080205; 3145741; 3211277; 3473421; 3538957; 3866637; 3932173; 3997709; 4063245; 4325389; 4390925; 4456461; 4521997; 4587533; 4653069; 4784141; 4849677; 4980749; 5111821; 5177357; 5242893; 5373965; 6291457; 3342345; 7077890; 3538961; 4849681; 7143426; 3538961; 4849681; 7274504; 3145746; 3538962; 3866642; 4063250; 4522002; 4587538; 4849682; 5177362; 7340039; 3145734; 3538950; 3866628; 4063238; 4521988; 4587524; 4849670; 7536648; 3145747; 3538963; 3866643; 4063251; 4522003; 4587539; 4849683; 5177363; 7733256; 3145747; 3538963; 3866643; 4063251; 4522003; 4587539; 4849683; 5177363; 8126465; 3538964|]
let zeroReduces = Array.zeroCreate 130
for i = 0 to 129 do
        zeroReduces.[i] <- Array.zeroCreate 83
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
let private small_acc = [129]
let private accStates = Array.zeroCreate 130
for i = 0 to 129 do
        accStates.[i] <- List.exists ((=) i) small_acc
let eofIndex = 72
let errorIndex = 6
let errorRulesExists = false
let private parserSource = new ParserSource<Token> (gotos, reduces, zeroReduces, accStates, rules, rulesStart, leftSide, startRule, eofIndex, tokenToNumber, acceptEmptyInput, numToString, errorIndex, errorRulesExists)
let buildAst : (seq<Token> -> ParseResult<Token>) =
    buildAst<Token> parserSource

let _rnglr_epsilons : Tree<Token>[] = [|new Tree<_>(null,box (new AST(new Family(41, new Nodes([||])), null)), null); new Tree<_>(null,box (new AST(new Family(33, new Nodes([||])), null)), null); null; null; null; null; null; null; new Tree<_>(null,box (new AST(new Family(35, new Nodes([||])), null)), null); null; new Tree<_>(null,box (new AST(new Family(45, new Nodes([||])), null)), null); null; null; new Tree<_>(null,box (new AST(new Family(10, new Nodes([||])), null)), null); null; null; null; new Tree<_>(null,box (new AST(new Family(88, new Nodes([||])), null)), null); null; null; null; new Tree<_>(null,box (new AST(new Family(21, new Nodes([||])), null)), null); null; new Tree<_>(null,box (new AST(new Family(67, new Nodes([||])), null)), null); new Tree<_>(null,box (new AST(new Family(30, new Nodes([||])), null)), null); new Tree<_>(null,box (new AST(new Family(29, new Nodes([||])), null)), null); null; null; null; new Tree<_>(null,box (new AST(new Family(14, new Nodes([||])), null)), null); new Tree<_>(null,box (new AST(new Family(51, new Nodes([||])), null)), null); new Tree<_>(null,box (new AST(new Family(49, new Nodes([||])), null)), null); null; new Tree<_>(null,box (new AST(new Family(71, new Nodes([||])), null)), null); null; null; new Tree<_>(null,box (new AST(new Family(36, new Nodes([||])), null)), null); new Tree<_>(null,box (new AST(new Family(69, new Nodes([||])), null)), null); null; null; new Tree<_>(null,box (new AST(new Family(64, new Nodes([||])), null)), null); new Tree<_>(null,box (new AST(new Family(40, new Nodes([||])), null)), null); null; null; new Tree<_>(null,box (new AST(new Family(20, new Nodes([|box (new AST(new Family(36, new Nodes([||])), null))|])), null)), null); new Tree<_>(null,box (new AST(new Family(53, new Nodes([||])), null)), null); null|]
let _rnglr_filtered_epsilons : Tree<Token>[] = [|new Tree<_>(null,box (new AST(new Family(41, new Nodes([||])), null)), null); new Tree<_>(null,box (new AST(new Family(33, new Nodes([||])), null)), null); null; null; null; null; null; null; new Tree<_>(null,box (new AST(new Family(35, new Nodes([||])), null)), null); null; new Tree<_>(null,box (new AST(new Family(45, new Nodes([||])), null)), null); null; null; new Tree<_>(null,box (new AST(new Family(10, new Nodes([||])), null)), null); null; null; null; new Tree<_>(null,box (new AST(new Family(88, new Nodes([||])), null)), null); null; null; null; new Tree<_>(null,box (new AST(new Family(21, new Nodes([||])), null)), null); null; new Tree<_>(null,box (new AST(new Family(67, new Nodes([||])), null)), null); new Tree<_>(null,box (new AST(new Family(30, new Nodes([||])), null)), null); new Tree<_>(null,box (new AST(new Family(29, new Nodes([||])), null)), null); null; null; null; new Tree<_>(null,box (new AST(new Family(14, new Nodes([||])), null)), null); new Tree<_>(null,box (new AST(new Family(51, new Nodes([||])), null)), null); new Tree<_>(null,box (new AST(new Family(49, new Nodes([||])), null)), null); null; new Tree<_>(null,box (new AST(new Family(71, new Nodes([||])), null)), null); null; null; new Tree<_>(null,box (new AST(new Family(36, new Nodes([||])), null)), null); new Tree<_>(null,box (new AST(new Family(69, new Nodes([||])), null)), null); null; null; new Tree<_>(null,box (new AST(new Family(64, new Nodes([||])), null)), null); new Tree<_>(null,box (new AST(new Family(40, new Nodes([||])), null)), null); null; null; new Tree<_>(null,box (new AST(new Family(20, new Nodes([|box (new AST(new Family(36, new Nodes([||])), null))|])), null)), null); new Tree<_>(null,box (new AST(new Family(53, new Nodes([||])), null)), null); null|]
for x in _rnglr_filtered_epsilons do if x <> null then x.ChooseSingleAst()
let _rnglr_extra_array, _rnglr_rule_, _rnglr_concats = 
  (Array.zeroCreate 0 : array<'_rnglr_type_access_modifier_opt * '_rnglr_type_action_opt * '_rnglr_type_alts * '_rnglr_type_bar_seq_nlist * '_rnglr_type_bound * '_rnglr_type_call * '_rnglr_type_error * '_rnglr_type_file * '_rnglr_type_foot_opt * '_rnglr_type_formal_meta_list * '_rnglr_type_formal_meta_param_opt * '_rnglr_type_ident * '_rnglr_type_include_ * '_rnglr_type_includes_or_options_or_tokens * '_rnglr_type_kw * '_rnglr_type_lbl_seq * '_rnglr_type_meta_param * '_rnglr_type_meta_param_opt * '_rnglr_type_meta_params * '_rnglr_type_module_ * '_rnglr_type_module_header * '_rnglr_type_modules * '_rnglr_type_no_lbl_seq * '_rnglr_type_omit_opt * '_rnglr_type_open_list * '_rnglr_type_openings * '_rnglr_type_option * '_rnglr_type_option_block * '_rnglr_type_option_value * '_rnglr_type_opts * '_rnglr_type_param_list * '_rnglr_type_param_opt * '_rnglr_type_patt * '_rnglr_type_predicate_opt * '_rnglr_type_prim * '_rnglr_type_rule * '_rnglr_type_rule_nlist * '_rnglr_type_semi_opt * '_rnglr_type_seq * '_rnglr_type_seq_elem * '_rnglr_type_seq_elem_list * '_rnglr_type_start_rule_sign_opt * '_rnglr_type_tada_rule * '_rnglr_type_tokens_block * '_rnglr_type_unnamed_module_opt * '_rnglr_type_weight_opt * '_rnglr_type_yard_start_rule>), 
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
# 344 "Parser.fs"
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
# 364 "Parser.fs"
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
# 384 "Parser.fs"
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
# 404 "Parser.fs"
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
# 424 "Parser.fs"
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
                      (match ((unbox _rnglr_children.[5]) : Token) with EOF _rnglr_val -> [_rnglr_val] | a -> failwith "EOF expected, but %A found" a )
                       |> List.iter (fun (_) -> 
                        _rnglr_cycle_res := (
                          
# 118 "Parser.fsy"
                                  
                                  {
                                      info = { fileName = !currentFilename }
                                      head = _S1
                                      grammar = fst_ _S2 @ _S3 @ _S4
                                      foot = _S5
                                      options = snd_ _S2
                                      tokens = trd_ _S2
                                  }
                                
                            )::!_rnglr_cycle_res ) ) ) ) ) )
            !_rnglr_cycle_res
          )
            )
# 112 "Parser.fsy"
               : '_rnglr_type_file) 
# 463 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          ((unbox _rnglr_children.[0]) : '_rnglr_type_file) 
            )
# 112 "Parser.fsy"
               : '_rnglr_type_yard_start_rule) 
# 473 "Parser.fs"
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
                  
# 134 "Parser.fsy"
                                                                   fst_ _S2, snd_ _S2, joinMaps _S1 (trd_ _S2)
                    )::!_rnglr_cycle_res ) )
            !_rnglr_cycle_res
          )
            )
# 129 "Parser.fsy"
               : '_rnglr_type_includes_or_options_or_tokens) 
# 495 "Parser.fs"
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
                  
# 133 "Parser.fsy"
                                                                     fst_ _S2, joinMaps _S1 (snd_ _S2), trd_ _S2
                    )::!_rnglr_cycle_res ) )
            !_rnglr_cycle_res
          )
            )
# 129 "Parser.fsy"
               : '_rnglr_type_includes_or_options_or_tokens) 
# 517 "Parser.fs"
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
                  
# 132 "Parser.fsy"
                                                                     (_S1 @ fst_ _S2), snd_ _S2, trd_ _S2
                    )::!_rnglr_cycle_res ) )
            !_rnglr_cycle_res
          )
            )
# 129 "Parser.fsy"
               : '_rnglr_type_includes_or_options_or_tokens) 
# 539 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            _rnglr_cycle_res := (
              
# 131 "Parser.fsy"
                     [],    Map.empty, Map.empty 
                )::!_rnglr_cycle_res
            !_rnglr_cycle_res
          )
            )
# 129 "Parser.fsy"
               : '_rnglr_type_includes_or_options_or_tokens) 
# 557 "Parser.fs"
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
                
# 137 "Parser.fsy"
                   
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
# 136 "Parser.fsy"
               : '_rnglr_type_tokens_block) 
# 597 "Parser.fs"
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
                  
# 161 "Parser.fsy"
                       
                          let grammar = (parseRules _S2.text).grammar
                          if grammar |> List.exists (fun m -> m.name.IsNone) then
                              eprintfn "Error %s: Grammar in included files must be inside modules" _S2.text
                          grammar
                      
                    )::!_rnglr_cycle_res ) )
            !_rnglr_cycle_res
          )
            )
# 159 "Parser.fsy"
               : '_rnglr_type_include_) 
# 624 "Parser.fs"
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
                    
# 168 "Parser.fsy"
                                                                 Map.ofList _S2 : Map<_,_>
                      )::!_rnglr_cycle_res ) ) )
            !_rnglr_cycle_res
          )
            )
# 168 "Parser.fsy"
               : '_rnglr_type_option_block) 
# 648 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            _rnglr_cycle_res := (
              
# 170 "Parser.fsy"
                                               [] 
                )::!_rnglr_cycle_res
            !_rnglr_cycle_res
          )
            )
# 170 "Parser.fsy"
               : '_rnglr_type_opts) 
# 666 "Parser.fs"
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
                  
# 170 "Parser.fsy"
                                      _S1::_S2 
                    )::!_rnglr_cycle_res ) )
            !_rnglr_cycle_res
          )
            )
# 170 "Parser.fsy"
               : '_rnglr_type_opts) 
# 688 "Parser.fs"
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
                    
# 172 "Parser.fsy"
                                                       (_S1 : Source.t).text, (_S3 : Source.t).text 
                      )::!_rnglr_cycle_res ) ) )
            !_rnglr_cycle_res
          )
            )
# 172 "Parser.fsy"
               : '_rnglr_type_option) 
# 712 "Parser.fs"
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
                
# 174 "Parser.fsy"
                                                                  _S1 
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 174 "Parser.fsy"
               : '_rnglr_type_option_value) 
# 732 "Parser.fs"
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
                
# 174 "Parser.fsy"
                                                      _S1 
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 174 "Parser.fsy"
               : '_rnglr_type_option_value) 
# 752 "Parser.fs"
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
                
# 174 "Parser.fsy"
                                      _S1 
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 174 "Parser.fsy"
               : '_rnglr_type_option_value) 
# 772 "Parser.fs"
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
                
# 177 "Parser.fsy"
                     
                        match _S1 with
                        | [] -> []
                        | x ->  defaultModules x
                    
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 176 "Parser.fsy"
               : '_rnglr_type_unnamed_module_opt) 
# 796 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            _rnglr_cycle_res := (
              
# 183 "Parser.fsy"
                                                         [] 
                )::!_rnglr_cycle_res
            !_rnglr_cycle_res
          )
            )
# 183 "Parser.fsy"
               : '_rnglr_type_modules) 
# 814 "Parser.fs"
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
                  
# 183 "Parser.fsy"
                                              _S1 :: _S2 
                    )::!_rnglr_cycle_res ) )
            !_rnglr_cycle_res
          )
            )
# 183 "Parser.fsy"
               : '_rnglr_type_modules) 
# 836 "Parser.fs"
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
                      
# 186 "Parser.fsy"
                           
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
# 185 "Parser.fsy"
               : '_rnglr_type_module_) 
# 869 "Parser.fs"
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
                
# 195 "Parser.fsy"
                                                 _S1 
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 195 "Parser.fsy"
               : '_rnglr_type_ident) 
# 889 "Parser.fs"
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
                
# 195 "Parser.fsy"
                                 _S1 
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 195 "Parser.fsy"
               : '_rnglr_type_ident) 
# 909 "Parser.fs"
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
                
# 202 "Parser.fsy"
                                         allPublic := false; false 
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 198 "Parser.fsy"
               : '_rnglr_type_module_header) 
# 929 "Parser.fs"
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
                  
# 198 "Parser.fsy"
                                                     
                                    (* It's important the word "module" is here. It guaranties, that it won't be an epsilon-tree, so allPublic will be evaluated before rules *)
                                    allPublic := true; true
                                  
                    )::!_rnglr_cycle_res ) )
            !_rnglr_cycle_res
          )
            )
# 198 "Parser.fsy"
               : '_rnglr_type_module_header) 
# 954 "Parser.fs"
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
                    
# 204 "Parser.fsy"
                                                                _S2::_S3 
                      )::!_rnglr_cycle_res ) ) )
            !_rnglr_cycle_res
          )
            )
# 204 "Parser.fsy"
               : '_rnglr_type_openings) 
# 978 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            _rnglr_cycle_res := (
              
# 204 "Parser.fsy"
                            [] 
                )::!_rnglr_cycle_res
            !_rnglr_cycle_res
          )
            )
# 204 "Parser.fsy"
               : '_rnglr_type_openings) 
# 996 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            _rnglr_cycle_res := (
              
# 206 "Parser.fsy"
                                                               [] 
                )::!_rnglr_cycle_res
            !_rnglr_cycle_res
          )
            )
# 206 "Parser.fsy"
               : '_rnglr_type_open_list) 
# 1014 "Parser.fs"
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
                    
# 206 "Parser.fsy"
                                                        _S2::_S3 
                      )::!_rnglr_cycle_res ) ) )
            !_rnglr_cycle_res
          )
            )
# 206 "Parser.fsy"
               : '_rnglr_type_open_list) 
# 1038 "Parser.fs"
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
                
# 208 "Parser.fsy"
                                                Some _S1 
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 208 "Parser.fsy"
               : '_rnglr_type_action_opt) 
# 1058 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            _rnglr_cycle_res := (
              
# 208 "Parser.fsy"
                            None 
                )::!_rnglr_cycle_res
            !_rnglr_cycle_res
          )
            )
# 208 "Parser.fsy"
               : '_rnglr_type_action_opt) 
# 1076 "Parser.fs"
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
                  
# 210 "Parser.fsy"
                                                          Some _S2 
                    )::!_rnglr_cycle_res ) )
            !_rnglr_cycle_res
          )
            )
# 210 "Parser.fsy"
               : '_rnglr_type_foot_opt) 
# 1098 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            _rnglr_cycle_res := (
              
# 210 "Parser.fsy"
                          None 
                )::!_rnglr_cycle_res
            !_rnglr_cycle_res
          )
            )
# 210 "Parser.fsy"
               : '_rnglr_type_foot_opt) 
# 1116 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            _rnglr_cycle_res := (
              
# 214 "Parser.fsy"
                      [] 
                )::!_rnglr_cycle_res
            !_rnglr_cycle_res
          )
            )
# 212 "Parser.fsy"
               : '_rnglr_type_rule_nlist) 
# 1134 "Parser.fs"
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
                    
# 213 "Parser.fsy"
                            _S1::_S3 
                      )::!_rnglr_cycle_res ) ) )
            !_rnglr_cycle_res
          )
            )
# 212 "Parser.fsy"
               : '_rnglr_type_rule_nlist) 
# 1158 "Parser.fs"
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
                            
# 217 "Parser.fsy"
                                  
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
# 216 "Parser.fsy"
               : '_rnglr_type_rule) 
# 1199 "Parser.fs"
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
                
# 228 "Parser.fsy"
                                                                true
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 228 "Parser.fsy"
               : '_rnglr_type_start_rule_sign_opt) 
# 1219 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            _rnglr_cycle_res := (
              
# 228 "Parser.fsy"
                                    false
                )::!_rnglr_cycle_res
            !_rnglr_cycle_res
          )
            )
# 228 "Parser.fsy"
               : '_rnglr_type_start_rule_sign_opt) 
# 1237 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            _rnglr_cycle_res := (
              
# 230 "Parser.fsy"
                                                                           !allPublic 
                )::!_rnglr_cycle_res
            !_rnglr_cycle_res
          )
            )
# 230 "Parser.fsy"
               : '_rnglr_type_access_modifier_opt) 
# 1255 "Parser.fs"
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
                
# 230 "Parser.fsy"
                                                                 false 
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 230 "Parser.fsy"
               : '_rnglr_type_access_modifier_opt) 
# 1275 "Parser.fs"
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
                
# 230 "Parser.fsy"
                                              true 
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 230 "Parser.fsy"
               : '_rnglr_type_access_modifier_opt) 
# 1295 "Parser.fs"
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
                    
# 232 "Parser.fsy"
                                                                                   Some _S2
                      )::!_rnglr_cycle_res ) ) )
            !_rnglr_cycle_res
          )
            )
# 232 "Parser.fsy"
               : '_rnglr_type_formal_meta_param_opt) 
# 1319 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            _rnglr_cycle_res := (
              
# 232 "Parser.fsy"
                                       None 
                )::!_rnglr_cycle_res
            !_rnglr_cycle_res
          )
            )
# 232 "Parser.fsy"
               : '_rnglr_type_formal_meta_param_opt) 
# 1337 "Parser.fs"
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
                  
# 235 "Parser.fsy"
                                                             _S1::_S2
                    )::!_rnglr_cycle_res ) )
            !_rnglr_cycle_res
          )
            )
# 234 "Parser.fsy"
               : '_rnglr_type_formal_meta_list) 
# 1359 "Parser.fs"
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
                
# 234 "Parser.fsy"
                                          [_S1]
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 234 "Parser.fsy"
               : '_rnglr_type_formal_meta_list) 
# 1379 "Parser.fs"
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
                
# 237 "Parser.fsy"
                                             Some _S1
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 237 "Parser.fsy"
               : '_rnglr_type_param_opt) 
# 1399 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            _rnglr_cycle_res := (
              
# 237 "Parser.fsy"
                           None 
                )::!_rnglr_cycle_res
            !_rnglr_cycle_res
          )
            )
# 237 "Parser.fsy"
               : '_rnglr_type_param_opt) 
# 1417 "Parser.fs"
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
                  
# 239 "Parser.fsy"
                                                         _S1::_S2
                    )::!_rnglr_cycle_res ) )
            !_rnglr_cycle_res
          )
            )
# 239 "Parser.fsy"
               : '_rnglr_type_param_list) 
# 1439 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            _rnglr_cycle_res := (
              
# 239 "Parser.fsy"
                            [] 
                )::!_rnglr_cycle_res
            !_rnglr_cycle_res
          )
            )
# 239 "Parser.fsy"
               : '_rnglr_type_param_list) 
# 1457 "Parser.fs"
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
                    
# 241 "Parser.fsy"
                                                                    Some _S2
                      )::!_rnglr_cycle_res ) ) )
            !_rnglr_cycle_res
          )
            )
# 241 "Parser.fsy"
               : '_rnglr_type_weight_opt) 
# 1481 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            _rnglr_cycle_res := (
              
# 241 "Parser.fsy"
                            None 
                )::!_rnglr_cycle_res
            !_rnglr_cycle_res
          )
            )
# 241 "Parser.fsy"
               : '_rnglr_type_weight_opt) 
# 1499 "Parser.fs"
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
                  
# 243 "Parser.fsy"
                                                        PAlt (_S1,_S2)
                    )::!_rnglr_cycle_res ) )
            !_rnglr_cycle_res
          )
            )
# 243 "Parser.fsy"
               : '_rnglr_type_alts) 
# 1521 "Parser.fs"
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
                
# 243 "Parser.fsy"
                            _S1 
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 243 "Parser.fsy"
               : '_rnglr_type_alts) 
# 1541 "Parser.fs"
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
                  
# 246 "Parser.fsy"
                                           _S2
                    )::!_rnglr_cycle_res ) )
            !_rnglr_cycle_res
          )
            )
# 245 "Parser.fsy"
               : '_rnglr_type_bar_seq_nlist) 
# 1563 "Parser.fs"
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
                    
# 245 "Parser.fsy"
                                                           PAlt(_S2,_S3) 
                      )::!_rnglr_cycle_res ) ) )
            !_rnglr_cycle_res
          )
            )
# 245 "Parser.fsy"
               : '_rnglr_type_bar_seq_nlist) 
# 1587 "Parser.fs"
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
                
# 248 "Parser.fsy"
                                                _S1
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 248 "Parser.fsy"
               : '_rnglr_type_seq) 
# 1607 "Parser.fs"
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
                
# 248 "Parser.fsy"
                                 _S1
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 248 "Parser.fsy"
               : '_rnglr_type_seq) 
# 1627 "Parser.fs"
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
                
# 251 "Parser.fsy"
                                     PSeq([], Some _S1, None) 
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 250 "Parser.fsy"
               : '_rnglr_type_no_lbl_seq) 
# 1647 "Parser.fs"
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
                    
# 250 "Parser.fsy"
                                                                    PSeq (_S1::_S2, _S3, None)
                      )::!_rnglr_cycle_res ) ) )
            !_rnglr_cycle_res
          )
            )
# 250 "Parser.fsy"
               : '_rnglr_type_no_lbl_seq) 
# 1671 "Parser.fs"
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
                        
# 253 "Parser.fsy"
                                                                             makeNewSeq _S4 _S1 _S2
                          )::!_rnglr_cycle_res ) ) ) ) )
            !_rnglr_cycle_res
          )
            )
# 253 "Parser.fsy"
               : '_rnglr_type_lbl_seq) 
# 1699 "Parser.fs"
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
                  
# 255 "Parser.fsy"
                                                                  _S1::_S2
                    )::!_rnglr_cycle_res ) )
            !_rnglr_cycle_res
          )
            )
# 255 "Parser.fsy"
               : '_rnglr_type_seq_elem_list) 
# 1721 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            _rnglr_cycle_res := (
              
# 255 "Parser.fsy"
                               [] 
                )::!_rnglr_cycle_res
            !_rnglr_cycle_res
          )
            )
# 255 "Parser.fsy"
               : '_rnglr_type_seq_elem_list) 
# 1739 "Parser.fs"
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
                    
# 257 "Parser.fsy"
                                                            {_S2 with checker = _S3; omit = _S1 }
                      )::!_rnglr_cycle_res ) ) )
            !_rnglr_cycle_res
          )
            )
# 257 "Parser.fsy"
               : '_rnglr_type_seq_elem) 
# 1763 "Parser.fs"
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
                
# 259 "Parser.fsy"
                                              true 
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 259 "Parser.fsy"
               : '_rnglr_type_omit_opt) 
# 1783 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            _rnglr_cycle_res := (
              
# 259 "Parser.fsy"
                          false 
                )::!_rnglr_cycle_res
            !_rnglr_cycle_res
          )
            )
# 259 "Parser.fsy"
               : '_rnglr_type_omit_opt) 
# 1801 "Parser.fs"
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
                
# 261 "Parser.fsy"
                                                  true
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 261 "Parser.fsy"
               : '_rnglr_type_semi_opt) 
# 1821 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            _rnglr_cycle_res := (
              
# 261 "Parser.fsy"
                           false 
                )::!_rnglr_cycle_res
            !_rnglr_cycle_res
          )
            )
# 261 "Parser.fsy"
               : '_rnglr_type_semi_opt) 
# 1839 "Parser.fs"
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
                
# 263 "Parser.fsy"
                                                      Some _S1 
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 263 "Parser.fsy"
               : '_rnglr_type_predicate_opt) 
# 1859 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            _rnglr_cycle_res := (
              
# 263 "Parser.fsy"
                               None 
                )::!_rnglr_cycle_res
            !_rnglr_cycle_res
          )
            )
# 263 "Parser.fsy"
               : '_rnglr_type_predicate_opt) 
# 1877 "Parser.fs"
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
                
# 266 "Parser.fsy"
                                         createSeqElem None false _S1 None      
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 265 "Parser.fsy"
               : '_rnglr_type_bound) 
# 1897 "Parser.fs"
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
                    
# 265 "Parser.fsy"
                                             createSeqElem (Some _S1) false _S3 None 
                      )::!_rnglr_cycle_res ) ) )
            !_rnglr_cycle_res
          )
            )
# 265 "Parser.fsy"
               : '_rnglr_type_bound) 
# 1921 "Parser.fs"
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
                
# 268 "Parser.fsy"
                                            _S1
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 268 "Parser.fsy"
               : '_rnglr_type_patt) 
# 1941 "Parser.fs"
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
                
# 268 "Parser.fsy"
                              _S1
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 268 "Parser.fsy"
               : '_rnglr_type_patt) 
# 1961 "Parser.fs"
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
                
# 277 "Parser.fsy"
                                            PLiteral _S1
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 270 "Parser.fsy"
               : '_rnglr_type_prim) 
# 1981 "Parser.fs"
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
                
# 276 "Parser.fsy"
                                            _S1
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 270 "Parser.fsy"
               : '_rnglr_type_prim) 
# 2001 "Parser.fs"
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
                
# 275 "Parser.fsy"
                                            _S1
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 270 "Parser.fsy"
               : '_rnglr_type_prim) 
# 2021 "Parser.fs"
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
                    
# 274 "Parser.fsy"
                                                _S2
                      )::!_rnglr_cycle_res ) ) )
            !_rnglr_cycle_res
          )
            )
# 270 "Parser.fsy"
               : '_rnglr_type_prim) 
# 2045 "Parser.fs"
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
                    
# 273 "Parser.fsy"
                                                POpt _S2
                      )::!_rnglr_cycle_res ) ) )
            !_rnglr_cycle_res
          )
            )
# 270 "Parser.fsy"
               : '_rnglr_type_prim) 
# 2069 "Parser.fs"
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
                  
# 272 "Parser.fsy"
                                              POpt _S1
                    )::!_rnglr_cycle_res ) )
            !_rnglr_cycle_res
          )
            )
# 270 "Parser.fsy"
               : '_rnglr_type_prim) 
# 2091 "Parser.fs"
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
                  
# 271 "Parser.fsy"
                                              PSome _S1
                    )::!_rnglr_cycle_res ) )
            !_rnglr_cycle_res
          )
            )
# 270 "Parser.fsy"
               : '_rnglr_type_prim) 
# 2113 "Parser.fs"
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
                  
# 270 "Parser.fsy"
                                              PMany _S1
                    )::!_rnglr_cycle_res ) )
            !_rnglr_cycle_res
          )
            )
# 270 "Parser.fsy"
               : '_rnglr_type_prim) 
# 2135 "Parser.fs"
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
                
# 279 "Parser.fsy"
                                  _S1
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 279 "Parser.fsy"
               : '_rnglr_type_meta_param) 
# 2155 "Parser.fs"
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
                  
# 282 "Parser.fsy"
                                                       _S1 :: _S2
                    )::!_rnglr_cycle_res ) )
            !_rnglr_cycle_res
          )
            )
# 281 "Parser.fsy"
               : '_rnglr_type_meta_params) 
# 2177 "Parser.fs"
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
                
# 281 "Parser.fsy"
                                         [_S1]
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 281 "Parser.fsy"
               : '_rnglr_type_meta_params) 
# 2197 "Parser.fs"
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
                    
# 284 "Parser.fsy"
                                                                       Some _S2
                      )::!_rnglr_cycle_res ) ) )
            !_rnglr_cycle_res
          )
            )
# 284 "Parser.fsy"
               : '_rnglr_type_meta_param_opt) 
# 2221 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            _rnglr_cycle_res := (
              
# 284 "Parser.fsy"
                                None 
                )::!_rnglr_cycle_res
            !_rnglr_cycle_res
          )
            )
# 284 "Parser.fsy"
               : '_rnglr_type_meta_param_opt) 
# 2239 "Parser.fs"
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
                    
# 288 "Parser.fsy"
                            match _S2 with
                            | None -> PRef  (_S1, _S3)
                            | Some x -> PMetaRef (_S1,_S3,x)
                          
                      )::!_rnglr_cycle_res ) ) )
            !_rnglr_cycle_res
          )
            )
# 286 "Parser.fsy"
               : '_rnglr_type_call) 
# 2266 "Parser.fs"
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
                
# 286 "Parser.fsy"
                              PToken _S1
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 286 "Parser.fsy"
               : '_rnglr_type_call) 
# 2286 "Parser.fs"
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
                
# 293 "Parser.fsy"
                                                
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 293 "Parser.fsy"
               : '_rnglr_type_tada_rule) 
# 2306 "Parser.fs"
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
                
# 293 "Parser.fsy"
                                       
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 293 "Parser.fsy"
               : '_rnglr_type_tada_rule) 
# 2326 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            _rnglr_cycle_res := (
              

              parserRange
                )::!_rnglr_cycle_res
            !_rnglr_cycle_res
          )
            )

               : '_rnglr_type_error) 
# 2344 "Parser.fs"
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
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_error)   ) |> List.concat));
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
