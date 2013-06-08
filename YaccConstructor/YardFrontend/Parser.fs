
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
    | LITERAL of Source.t
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
    | 60 -> "LITERAL"
    | 61 -> "LPAREN"
    | 62 -> "MINUS"
    | 63 -> "MODULE"
    | 64 -> "NUMBER"
    | 65 -> "OPEN"
    | 66 -> "OPTIONS_START"
    | 67 -> "PARAM"
    | 68 -> "PLUS"
    | 69 -> "PREDICATE"
    | 70 -> "PRIVATE"
    | 71 -> "PUBLIC"
    | 72 -> "QUESTION"
    | 73 -> "RNGLR_EOF"
    | 74 -> "RPAREN"
    | 75 -> "SEMICOLON"
    | 76 -> "SHARPLINE"
    | 77 -> "SQR_LBR"
    | 78 -> "SQR_RBR"
    | 79 -> "STAR"
    | 80 -> "START_RULE_SIGN"
    | 81 -> "STRING"
    | 82 -> "TOKENS_BLOCK"
    | 83 -> "UIDENT"
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
    | LITERAL _ -> 60
    | LPAREN _ -> 61
    | MINUS _ -> 62
    | MODULE _ -> 63
    | NUMBER _ -> 64
    | OPEN _ -> 65
    | OPTIONS_START _ -> 66
    | PARAM _ -> 67
    | PLUS _ -> 68
    | PREDICATE _ -> 69
    | PRIVATE _ -> 70
    | PUBLIC _ -> 71
    | QUESTION _ -> 72
    | RNGLR_EOF _ -> 73
    | RPAREN _ -> 74
    | SEMICOLON _ -> 75
    | SHARPLINE _ -> 76
    | SQR_LBR _ -> 77
    | SQR_RBR _ -> 78
    | STAR _ -> 79
    | START_RULE_SIGN _ -> 80
    | STRING _ -> 81
    | TOKENS_BLOCK _ -> 82
    | UIDENT _ -> 83

let mutable private cur = 0
let leftSide = [|14; 14; 14; 14; 14; 7; 46; 13; 13; 13; 13; 43; 12; 27; 29; 29; 26; 28; 28; 28; 44; 21; 21; 19; 11; 11; 20; 20; 25; 25; 24; 24; 1; 1; 8; 8; 36; 36; 35; 41; 41; 0; 0; 0; 10; 10; 9; 9; 31; 31; 30; 30; 45; 45; 2; 2; 3; 3; 38; 38; 22; 22; 15; 40; 40; 39; 23; 23; 37; 37; 33; 33; 4; 4; 32; 32; 34; 34; 34; 34; 34; 34; 34; 34; 16; 18; 18; 17; 17; 5; 5; 42; 42|]
let private rules = [|70; 71; 65; 57; 63; 1; 13; 44; 21; 8; 54; 7; 43; 13; 27; 13; 12; 13; 82; 57; 81; 66; 29; 50; 26; 29; 11; 55; 28; 14; 81; 11; 36; 19; 21; 20; 11; 25; 36; 59; 83; 63; 48; 63; 65; 11; 24; 52; 11; 24; 47; 75; 47; 35; 37; 36; 41; 0; 59; 10; 30; 51; 2; 80; 70; 71; 58; 9; 56; 59; 9; 59; 67; 67; 30; 77; 64; 78; 38; 3; 38; 49; 38; 49; 38; 3; 15; 22; 47; 39; 40; 1; 53; 45; 61; 22; 74; 39; 40; 23; 4; 33; 62; 75; 69; 34; 32; 55; 34; 47; 59; 60; 15; 5; 61; 2; 74; 77; 2; 78; 34; 72; 34; 68; 34; 79; 34; 16; 18; 16; 58; 18; 56; 59; 17; 31; 83; 54; 76|]
let private rulesStart = [|0; 1; 2; 3; 4; 5; 11; 12; 14; 16; 18; 18; 19; 21; 24; 24; 26; 29; 30; 31; 32; 33; 33; 35; 39; 40; 41; 42; 44; 47; 47; 47; 50; 51; 51; 53; 53; 53; 56; 63; 64; 64; 64; 65; 66; 69; 69; 71; 72; 73; 73; 75; 75; 78; 78; 80; 81; 83; 86; 87; 88; 89; 92; 97; 99; 99; 102; 103; 103; 104; 104; 105; 105; 106; 109; 110; 111; 112; 113; 114; 117; 120; 122; 124; 126; 127; 129; 130; 133; 133; 136; 137; 138; 139|]
let startRule = 6

let acceptEmptyInput = false

let defaultAstToDot =
    (fun (tree : Yard.Generators.RNGLR.AST.Tree<Token>) -> tree.AstToDot numToString tokenToNumber leftSide)

let private lists_gotos = [|1; 129; 84; 2; 29; 4; 6; 8; 10; 28; 3; 5; 7; 9; 11; 24; 26; 17; 23; 12; 13; 14; 15; 16; 18; 19; 20; 21; 22; 25; 27; 30; 107; 33; 108; 105; 31; 106; 32; 34; 103; 104; 35; 36; 98; 37; 96; 38; 39; 40; 41; 42; 73; 78; 85; 54; 81; 43; 46; 47; 48; 90; 95; 91; 69; 70; 86; 89; 44; 45; 49; 50; 59; 51; 52; 53; 55; 92; 56; 57; 58; 60; 63; 61; 62; 64; 67; 66; 65; 68; 71; 72; 74; 75; 76; 77; 79; 82; 80; 83; 87; 88; 93; 94; 97; 99; 101; 100; 102; 109; 110; 124; 121; 123; 120; 111; 112; 114; 113; 115; 116; 117; 118; 119; 122; 125; 127; 126; 128|]
let private small_gotos =
        [|3; 65536; 458753; 3080194; 65543; 786435; 851972; 1769477; 2818054; 3735559; 4325384; 5373961; 131079; 786435; 851978; 1769477; 2818054; 3735559; 4325384; 5373961; 262151; 786435; 851979; 1769477; 2818054; 3735559; 4325384; 5373961; 393223; 786435; 851980; 1769477; 2818054; 3735559; 4325384; 5373961; 524289; 5308429; 655365; 720910; 1703951; 1900560; 3866641; 5439506; 720897; 3604499; 786443; 720916; 917525; 1835030; 3735575; 3866641; 4128792; 4259865; 4587546; 4653083; 5308444; 5439506; 1572869; 720910; 1703951; 1900573; 3866641; 5439506; 1703937; 3276830; 1900549; 2293791; 2359328; 2687009; 2883618; 5242915; 1966082; 2424868; 4915237; 2031620; 2293791; 2359334; 2687009; 5242915; 2162691; 39; 4587560; 4653097; 2228225; 3866666; 2293762; 655403; 3801132; 2359298; 1966125; 4390958; 2424833; 3342383; 2490377; 131120; 983089; 1441842; 1507379; 2490420; 2555957; 3080246; 3473463; 4063288; 2752524; 262201; 327738; 983099; 2097212; 2228285; 3080254; 3473463; 3866687; 3932224; 3997761; 5046338; 5439555; 2818050; 2162756; 4522053; 3145729; 3604550; 3211273; 327738; 983099; 2228295; 3473463; 3866696; 3932224; 3997761; 5046338; 5439555; 3276803; 4456521; 4718666; 5177419; 3538946; 2949196; 5046349; 3604481; 3997774; 3670021; 1441871; 1507379; 2555957; 3080246; 4063288; 3735553; 4849744; 3866626; 1114193; 3801170; 3932162; 2031699; 4390996; 4128779; 327738; 983099; 1048661; 1179734; 2228311; 3473463; 3866696; 3932224; 3997761; 5046338; 5439555; 4194315; 327738; 983099; 1048661; 1179736; 2228311; 3473463; 3866696; 3932224; 3997761; 5046338; 5439555; 4325379; 4456521; 4718666; 5177419; 4390913; 3670105; 4587529; 131162; 983089; 1441842; 1507379; 2490420; 2555957; 3080246; 3473463; 4063288; 4653057; 4849755; 4784130; 196700; 3211357; 4915208; 983089; 1441842; 1507379; 2490462; 2555957; 3080246; 3473463; 4063288; 4980738; 196703; 3211357; 5111812; 1507379; 2556000; 2621537; 4063288; 5177348; 1507379; 2556000; 2621538; 4063288; 5373954; 65635; 3080194; 5636105; 131172; 983089; 1441842; 1507379; 2490420; 2555957; 3080246; 3473463; 4063288; 5701633; 5111909; 5898243; 4456521; 4718666; 5177419; 5963778; 1114193; 3801170; 6029313; 4194406; 6094849; 5111911; 6291458; 1966184; 4390958; 6422530; 589929; 3866730; 6488065; 3670123; 6619138; 589932; 3866730; 7077893; 1245293; 1310830; 1376367; 3145840; 4128881; 7143429; 1245293; 1310830; 1376370; 3145840; 4128881; 7208963; 721011; 3866641; 5439506; 7274498; 1638516; 4259957; 7340036; 2293791; 2359414; 2687009; 5242915; 7471107; 721015; 3866641; 5439506; 7536642; 1572984; 3407993; 7667715; 721018; 3866641; 5439506; 7733250; 1572987; 3407993; 7929857; 4128892; 8126466; 524413; 4915326; 8192001; 3539071; 8323073; 3080320|]
let gotos = Array.zeroCreate 130
for i = 0 to 129 do
        gotos.[i] <- Array.zeroCreate 84
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
let private lists_reduces = [|[|9,1|]; [|9,2|]; [|8,1|]; [|8,2|]; [|7,1|]; [|7,2|]; [|12,2|]; [|19,1|]; [|17,1|]; [|16,3|]; [|3,1|]; [|24,1|]; [|4,1|]; [|2,1|]; [|0,1|]; [|1,1|]; [|18,1|]; [|25,1|]; [|15,1|]; [|15,2|]; [|13,3|]; [|11,1|]; [|37,1|]; [|37,2|]; [|37,3|]; [|38,7|]; [|58,1|]; [|59,1|]; [|65,2|]; [|65,3|]; [|70,1|]; [|78,1|]; [|77,1|]; [|73,3|]; [|82,2|]; [|81,2|]; [|83,2|]; [|62,5|]; [|89,1|]; [|89,2|]; [|89,3|]; [|48,1|]; [|86,1|]; [|85,2|]; [|84,1|]; [|87,3|]; [|76,1|]; [|79,3|]; [|55,1|]; [|54,2|]; [|56,2|]; [|57,3|]; [|61,1|]; [|63,1|]; [|63,2|]; [|66,1|]; [|61,2|]; [|61,3|]; [|32,1|]; [|60,1|]; [|80,3|]; [|90,1|]; [|72,1|]; [|75,1|]; [|52,3|]; [|74,1|]; [|50,1|]; [|50,2|]; [|44,3|]; [|47,1|]; [|46,2|]; [|42,1|]; [|43,1|]; [|39,1|]; [|68,1|]; [|20,1|]; [|22,1|]; [|23,2|]; [|23,3|]; [|23,4|]; [|28,2|]; [|28,3|]; [|31,2|]; [|31,3|]; [|22,2|]; [|27,2|]; [|26,1|]; [|5,6|]; [|34,2|]|]
let private small_reduces =
        [|131080; 3145728; 3538944; 3866624; 4128768; 4587520; 4653056; 4915200; 5242880; 196616; 3145729; 3538945; 3866625; 4128769; 4587521; 4653057; 4915201; 5242881; 262152; 3145730; 3538946; 3866626; 4128770; 4587522; 4653058; 4915202; 5242882; 327688; 3145731; 3538947; 3866627; 4128771; 4587523; 4653059; 4915203; 5242883; 393224; 3145732; 3538948; 3866628; 4128772; 4587524; 4653060; 4915204; 5242884; 458760; 3145733; 3538949; 3866629; 4128773; 4587525; 4653061; 4915205; 5242885; 589835; 3145734; 3538950; 3735558; 3866630; 4128774; 4325382; 4587526; 4653062; 4915206; 5242886; 5373958; 851971; 3276807; 3866631; 5439495; 917507; 3276808; 3866632; 5439496; 983043; 3276809; 3866633; 5439497; 1048579; 3276810; 3866634; 5439498; 1114125; 3145739; 3276811; 3407883; 3538955; 3604491; 3866635; 4128779; 4259851; 4587531; 4653067; 4915211; 5242891; 5439499; 1179651; 3276812; 3866636; 5439500; 1245187; 3276813; 3866637; 5439501; 1310723; 3276814; 3866638; 5439502; 1376259; 3276815; 3866639; 5439503; 1441795; 3276816; 3866640; 5439504; 1507341; 3145745; 3276817; 3407889; 3538961; 3604497; 3866641; 4128785; 4259857; 4587537; 4653073; 4915217; 5242897; 5439505; 1572865; 3276818; 1638401; 3276819; 1769483; 3145748; 3538964; 3735572; 3866644; 4128788; 4325396; 4587540; 4653076; 4915220; 5242900; 5373972; 1835019; 3145749; 3538965; 3735573; 3866645; 4128789; 4325397; 4587541; 4653077; 4915221; 5242901; 5373973; 1966084; 3145750; 3538966; 4128790; 4915222; 2031620; 3145751; 3538967; 4128791; 4915223; 2097156; 3145752; 3538968; 4128792; 4915224; 2555912; 3145753; 3538969; 3866649; 4128793; 4587545; 4653081; 4915225; 5242905; 2621451; 3145754; 3211290; 3538970; 3866650; 4128794; 4587546; 4653082; 4849690; 4915226; 5111834; 5242906; 2686987; 3145755; 3211291; 3538971; 3866651; 4128795; 4587547; 4653083; 4849691; 4915227; 5111835; 5242907; 2818066; 3080220; 3145756; 3211292; 3473436; 3538972; 3866652; 3932188; 3997724; 4063260; 4128796; 4587548; 4653084; 4849692; 4915228; 5046300; 5111836; 5242908; 5439516; 2883602; 3080221; 3145757; 3211293; 3473437; 3538973; 3866653; 3932189; 3997725; 4063261; 4128797; 4587549; 4653085; 4849693; 4915229; 5046301; 5111837; 5242909; 5439517; 2949138; 3080222; 3145758; 3211294; 3473438; 3538974; 3866654; 3932190; 3997726; 4063262; 4128798; 4587550; 4653086; 4849694; 4915230; 5046302; 5111838; 5242910; 5439518; 3014679; 3080223; 3145759; 3211295; 3473439; 3538975; 3670047; 3866655; 3932191; 3997727; 4063263; 4128799; 4456479; 4522015; 4587551; 4653087; 4718623; 4849695; 4915231; 5046303; 5111839; 5177375; 5242911; 5439519; 3080215; 3080224; 3145760; 3211296; 3473440; 3538976; 3670048; 3866656; 3932192; 3997728; 4063264; 4128800; 4456480; 4522016; 4587552; 4653088; 4718624; 4849696; 4915232; 5046304; 5111840; 5177376; 5242912; 5439520; 3276819; 3080225; 3145761; 3211297; 3473441; 3538977; 3866657; 3932193; 3997729; 4063265; 4128801; 4522017; 4587553; 4653089; 4849697; 4915233; 5046305; 5111841; 5242913; 5439521; 3342359; 3080226; 3145762; 3211298; 3473442; 3538978; 3670050; 3866658; 3932194; 3997730; 4063266; 4128802; 4456482; 4522018; 4587554; 4653090; 4718626; 4849698; 4915234; 5046306; 5111842; 5177378; 5242914; 5439522; 3407895; 3080227; 3145763; 3211299; 3473443; 3538979; 3670051; 3866659; 3932195; 3997731; 4063267; 4128803; 4456483; 4522019; 4587555; 4653091; 4718627; 4849699; 4915235; 5046307; 5111843; 5177379; 5242915; 5439523; 3473431; 3080228; 3145764; 3211300; 3473444; 3538980; 3670052; 3866660; 3932196; 3997732; 4063268; 4128804; 4456484; 4522020; 4587556; 4653092; 4718628; 4849700; 4915236; 5046308; 5111844; 5177380; 5242916; 5439524; 3801111; 3080229; 3145765; 3211301; 3473445; 3538981; 3670053; 3866661; 3932197; 3997733; 4063269; 4128805; 4456485; 4522021; 4587557; 4653093; 4718629; 4849701; 4915237; 5046309; 5111845; 5177381; 5242917; 5439525; 3866647; 3080230; 3145766; 3211302; 3473446; 3538982; 3670054; 3866662; 3932198; 3997734; 4063270; 4128806; 4456486; 4522022; 4587558; 4653094; 4718630; 4849702; 4915238; 5046310; 5111846; 5177382; 5242918; 5439526; 3932183; 3080231; 3145767; 3211303; 3473447; 3538983; 3670055; 3866663; 3932199; 3997735; 4063271; 4128807; 4456487; 4522023; 4587559; 4653095; 4718631; 4849703; 4915239; 5046311; 5111847; 5177383; 5242919; 5439527; 3997719; 3080232; 3145768; 3211304; 3473448; 3538984; 3670056; 3866664; 3932200; 3997736; 4063272; 4128808; 4456488; 4522024; 4587560; 4653096; 4718632; 4849704; 4915240; 5046312; 5111848; 5177384; 5242920; 5439528; 4063255; 3080233; 3145769; 3211305; 3473449; 3538985; 3670057; 3866665; 3932201; 3997737; 4063273; 4128809; 4456489; 4522025; 4587561; 4653097; 4718633; 4849705; 4915241; 5046313; 5111849; 5177385; 5242921; 5439529; 4194305; 3670058; 4259841; 3670059; 4325383; 3473452; 3670060; 3866668; 3932204; 3997740; 5046316; 5439532; 4456472; 3080237; 3145773; 3211309; 3473453; 3538989; 3670061; 3866669; 3932205; 3997741; 4063277; 4128813; 4390957; 4456493; 4522029; 4587565; 4653101; 4718637; 4849709; 4915245; 5046317; 5111853; 5177389; 5242925; 5439533; 4522007; 3080238; 3145774; 3211310; 3473454; 3538990; 3670062; 3866670; 3932206; 3997742; 4063278; 4128814; 4456494; 4522030; 4587566; 4653102; 4718638; 4849710; 4915246; 5046318; 5111854; 5177390; 5242926; 5439534; 4718615; 3080239; 3145775; 3211311; 3473455; 3538991; 3670063; 3866671; 3932207; 3997743; 4063279; 4128815; 4456495; 4522031; 4587567; 4653103; 4718639; 4849711; 4915247; 5046319; 5111855; 5177391; 5242927; 5439535; 4784138; 3145776; 3538992; 3866672; 4128816; 4587568; 4653104; 4849712; 4915248; 5111856; 5242928; 4849674; 3145777; 3538993; 3866673; 4128817; 4587569; 4653105; 4849713; 4915249; 5111857; 5242929; 4980746; 3145778; 3538994; 3866674; 4128818; 4587570; 4653106; 4849714; 4915250; 5111858; 5242930; 5046282; 3145779; 3538995; 3866675; 4128819; 4587571; 4653107; 4849715; 4915251; 5111859; 5242931; 5111819; 3145780; 3211316; 3538996; 3866676; 4128820; 4587572; 4653108; 4849716; 4915252; 5111860; 5242932; 5177356; 3080245; 3145781; 3211317; 3538997; 3866677; 4128821; 4587573; 4653109; 4849717; 4915253; 5111861; 5242933; 5242892; 3080246; 3145782; 3211318; 3538998; 3866678; 4128822; 4587574; 4653110; 4849718; 4915254; 5111862; 5242934; 5308423; 3080247; 3473463; 3866679; 3932215; 3997751; 5046327; 5439543; 5373963; 3145784; 3211320; 3539000; 3866680; 4128824; 4587576; 4653112; 4849720; 4915256; 5111864; 5242936; 5439499; 3145785; 3211321; 3539001; 3866681; 4128825; 4587577; 4653113; 4849721; 4915257; 5111865; 5242937; 5505038; 3145786; 3211322; 3539002; 3735610; 3866682; 4128826; 4325434; 4587578; 4653114; 4849722; 4915258; 5111866; 5242938; 5374010; 5570571; 3145787; 3211323; 3539003; 3866683; 4128827; 4587579; 4653115; 4849723; 4915259; 5111867; 5242939; 5767191; 3080252; 3145788; 3211324; 3473468; 3539004; 3670076; 3866684; 3932220; 3997756; 4063292; 4128828; 4456508; 4522044; 4587580; 4653116; 4718652; 4849724; 4915260; 5046332; 5111868; 5177404; 5242940; 5439548; 5832727; 3080253; 3145789; 3211325; 3473469; 3539005; 3670077; 3866685; 3932221; 3997757; 4063293; 4128829; 4456509; 4522045; 4587581; 4653117; 4718653; 4849725; 4915261; 5046333; 5111869; 5177405; 5242941; 5439549; 5898259; 3080254; 3145790; 3211326; 3473470; 3539006; 3866686; 3932222; 3997758; 4063294; 4128830; 4522046; 4587582; 4653118; 4849726; 4915262; 5046334; 5111870; 5242942; 5439550; 5963799; 3080230; 3145766; 3211302; 3473446; 3538982; 3604543; 3866662; 3932198; 3997734; 4063270; 4128806; 4456486; 4522022; 4587558; 4653094; 4718630; 4849702; 4915238; 5046310; 5111846; 5177382; 5242918; 5439526; 6160385; 3997760; 6225921; 3604545; 6291457; 3342402; 6356993; 3342403; 6553602; 3342404; 4390980; 6619137; 3670085; 6684673; 3670086; 6750209; 3866695; 6815745; 3866696; 6881283; 3866697; 4587593; 4653129; 6946824; 3145802; 3539018; 3866698; 4128842; 4587594; 4653130; 4915274; 5242954; 7012356; 3145803; 3539019; 4128843; 4915275; 7143426; 3539020; 4915276; 7274500; 3145805; 3539021; 4128845; 4915277; 7340036; 3145806; 3539022; 4128846; 4915278; 7405572; 3145807; 3539023; 4128847; 4915279; 7536648; 3145808; 3539024; 3866704; 4128848; 4587600; 4653136; 4915280; 5242960; 7602184; 3145809; 3539025; 3866705; 4128849; 4587601; 4653137; 4915281; 5242961; 7733256; 3145810; 3539026; 3866706; 4128850; 4587602; 4653138; 4915282; 5242962; 7798792; 3145811; 3539027; 3866707; 4128851; 4587603; 4653139; 4915283; 5242963; 7864322; 3539028; 4915284; 7995394; 3866709; 5439573; 8060930; 3866710; 5439574; 8257537; 4784215; 8388609; 3539032|]
let reduces = Array.zeroCreate 130
for i = 0 to 129 do
        reduces.[i] <- Array.zeroCreate 84
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
        [|11; 3145728; 3538944; 3735552; 3866624; 4128768; 4325376; 4587520; 4653056; 4915200; 5242880; 5373952; 65544; 3145729; 3538945; 3866625; 4128769; 4587521; 4653057; 4915201; 5242881; 131080; 3145729; 3538945; 3866625; 4128769; 4587521; 4653057; 4915201; 5242881; 262152; 3145729; 3538945; 3866625; 4128769; 4587521; 4653057; 4915201; 5242881; 393224; 3145729; 3538945; 3866625; 4128769; 4587521; 4653057; 4915201; 5242881; 655361; 3276802; 1572865; 3276802; 1900551; 3145731; 3538947; 3866628; 4128771; 4587524; 4653060; 4915203; 1966088; 3145733; 3538949; 3866629; 4128773; 4587525; 4653061; 4915205; 5242885; 2031623; 3145734; 3538950; 3866628; 4128774; 4587524; 4653060; 4915206; 2162689; 3866631; 2293762; 3342344; 4390920; 2359297; 3342345; 2490375; 3080202; 3473418; 3866634; 3932170; 3997706; 5046282; 5439498; 2818066; 3080203; 3145739; 3211275; 3473419; 3538955; 3866635; 3932171; 3997707; 4063243; 4128779; 4587531; 4653067; 4849675; 4915211; 5046283; 5111819; 5242891; 5439499; 3538945; 3997708; 3670023; 3080202; 3473418; 3866634; 3932170; 3997706; 5046282; 5439498; 3866648; 3080205; 3145741; 3211277; 3473421; 3538957; 3670029; 3866637; 3932173; 3997709; 4063245; 4128781; 4390925; 4456461; 4521997; 4587533; 4653069; 4718605; 4849677; 4915213; 5046285; 5111821; 5177357; 5242893; 5439501; 3932183; 3080206; 3145742; 3211278; 3473422; 3538958; 3670030; 3866638; 3932174; 3997710; 4063246; 4128782; 4456462; 4521998; 4587534; 4653070; 4718606; 4849678; 4915214; 5046286; 5111822; 5177358; 5242894; 5439502; 4587527; 3080202; 3473418; 3866634; 3932170; 3997706; 5046282; 5439498; 4915207; 3080202; 3473418; 3866634; 3932170; 3997706; 5046282; 5439498; 5111825; 3080207; 3145744; 3211280; 3473418; 3538960; 3866639; 3932170; 3997706; 4128784; 4587536; 4653072; 4849680; 4915216; 5046282; 5111824; 5242896; 5439498; 5177361; 3080207; 3145744; 3211280; 3473418; 3538960; 3866639; 3932170; 3997706; 4128784; 4587536; 4653072; 4849680; 4915216; 5046282; 5111824; 5242896; 5439498; 5373963; 3145728; 3211264; 3538944; 3866624; 4128768; 4587520; 4653056; 4849664; 4915200; 5111808; 5242880; 5636103; 3080202; 3473418; 3866634; 3932170; 3997706; 5046282; 5439498; 5963799; 3080205; 3145741; 3211277; 3473421; 3538957; 3866637; 3932173; 3997709; 4063245; 4128781; 4390925; 4456461; 4521997; 4587533; 4653069; 4718605; 4849677; 4915213; 5046285; 5111821; 5177357; 5242893; 5439501; 6291457; 3342345; 7077890; 3538961; 4915217; 7143426; 3538961; 4915217; 7274504; 3145746; 3538962; 3866642; 4128786; 4587538; 4653074; 4915218; 5242898; 7340039; 3145734; 3538950; 3866628; 4128774; 4587524; 4653060; 4915206; 7536648; 3145747; 3538963; 3866643; 4128787; 4587539; 4653075; 4915219; 5242899; 7733256; 3145747; 3538963; 3866643; 4128787; 4587539; 4653075; 4915219; 5242899; 8126465; 3538964|]
let zeroReduces = Array.zeroCreate 130
for i = 0 to 129 do
        zeroReduces.[i] <- Array.zeroCreate 84
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
let eofIndex = 73
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
# 347 "Parser.fs"
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
# 367 "Parser.fs"
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
# 387 "Parser.fs"
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
# 407 "Parser.fs"
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
# 427 "Parser.fs"
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
# 466 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          ((unbox _rnglr_children.[0]) : '_rnglr_type_file) 
            )
# 112 "Parser.fsy"
               : '_rnglr_type_yard_start_rule) 
# 476 "Parser.fs"
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
# 498 "Parser.fs"
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
# 520 "Parser.fs"
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
# 542 "Parser.fs"
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
# 560 "Parser.fs"
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
# 600 "Parser.fs"
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
# 627 "Parser.fs"
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
# 651 "Parser.fs"
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
# 669 "Parser.fs"
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
# 691 "Parser.fs"
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
# 715 "Parser.fs"
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
# 735 "Parser.fs"
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
# 755 "Parser.fs"
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
# 775 "Parser.fs"
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
# 799 "Parser.fs"
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
# 817 "Parser.fs"
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
# 839 "Parser.fs"
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
# 872 "Parser.fs"
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
# 892 "Parser.fs"
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
# 912 "Parser.fs"
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
# 932 "Parser.fs"
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
# 957 "Parser.fs"
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
# 981 "Parser.fs"
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
# 999 "Parser.fs"
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
# 1017 "Parser.fs"
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
# 1041 "Parser.fs"
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
# 1061 "Parser.fs"
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
# 1079 "Parser.fs"
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
# 1101 "Parser.fs"
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
# 1119 "Parser.fs"
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
# 1137 "Parser.fs"
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
# 1161 "Parser.fs"
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
# 1202 "Parser.fs"
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
# 1222 "Parser.fs"
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
# 1240 "Parser.fs"
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
# 1258 "Parser.fs"
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
# 1278 "Parser.fs"
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
# 1298 "Parser.fs"
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
# 1322 "Parser.fs"
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
# 1340 "Parser.fs"
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
# 1362 "Parser.fs"
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
# 1382 "Parser.fs"
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
# 1402 "Parser.fs"
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
# 1420 "Parser.fs"
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
# 1442 "Parser.fs"
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
# 1460 "Parser.fs"
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
# 1484 "Parser.fs"
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
# 1502 "Parser.fs"
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
# 1524 "Parser.fs"
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
# 1544 "Parser.fs"
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
# 1566 "Parser.fs"
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
# 1590 "Parser.fs"
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
# 1610 "Parser.fs"
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
# 1630 "Parser.fs"
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
# 1650 "Parser.fs"
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
# 1674 "Parser.fs"
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
# 1702 "Parser.fs"
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
# 1724 "Parser.fs"
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
# 1742 "Parser.fs"
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
# 1766 "Parser.fs"
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
# 1786 "Parser.fs"
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
# 1804 "Parser.fs"
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
# 1824 "Parser.fs"
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
# 1842 "Parser.fs"
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
# 1862 "Parser.fs"
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
# 1880 "Parser.fs"
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
# 1900 "Parser.fs"
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
# 1924 "Parser.fs"
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
# 1944 "Parser.fs"
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
# 1964 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            (match ((unbox _rnglr_children.[0]) : Token) with LITERAL _rnglr_val -> [_rnglr_val] | a -> failwith "LITERAL expected, but %A found" a )
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
# 1984 "Parser.fs"
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
# 2004 "Parser.fs"
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
# 2024 "Parser.fs"
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
# 2048 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            (match ((unbox _rnglr_children.[0]) : Token) with SQR_LBR _rnglr_val -> [_rnglr_val] | a -> failwith "SQR_LBR expected, but %A found" a )
             |> List.iter (fun (_) -> 
              ((unbox _rnglr_children.[1]) : '_rnglr_type_alts) 
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
# 2072 "Parser.fs"
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
# 2094 "Parser.fs"
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
# 2116 "Parser.fs"
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
# 2138 "Parser.fs"
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
# 2158 "Parser.fs"
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
# 2180 "Parser.fs"
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
# 2200 "Parser.fs"
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
# 2224 "Parser.fs"
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
# 2242 "Parser.fs"
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
# 2269 "Parser.fs"
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
# 2289 "Parser.fs"
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
# 2309 "Parser.fs"
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
# 2329 "Parser.fs"
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
# 2347 "Parser.fs"
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
