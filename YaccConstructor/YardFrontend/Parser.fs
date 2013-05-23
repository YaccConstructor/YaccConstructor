
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
 
type Range = struct
    val Start: Lexing.Position
    val End: Lexing.Position

    new (start,end_) = {Start = start; End = end_}
end

exception Parse_error
let parseFile = ref Unchecked.defaultof<_>
let FrontendsManager = Yard.Core.FrontendsManager.FrontendsManager()  
let currentFilename = ref ""
let allPublic = ref false
let o2l = function Some x -> [x] | None -> []
let getList = function Some x -> x | None -> []

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

# 84 "Parser.fs"
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
    | 12 -> "includesOrOptions"
    | 13 -> "lbl_seq"
    | 14 -> "meta_param"
    | 15 -> "meta_param_opt"
    | 16 -> "meta_params"
    | 17 -> "module_"
    | 18 -> "module_header"
    | 19 -> "modules"
    | 20 -> "no_lbl_seq"
    | 21 -> "omit_opt"
    | 22 -> "open_list"
    | 23 -> "openings"
    | 24 -> "option"
    | 25 -> "option_block"
    | 26 -> "option_value"
    | 27 -> "opts"
    | 28 -> "param_list"
    | 29 -> "param_opt"
    | 30 -> "patt"
    | 31 -> "predicate_opt"
    | 32 -> "prim"
    | 33 -> "rule"
    | 34 -> "rule_nlist"
    | 35 -> "semi_opt"
    | 36 -> "seq"
    | 37 -> "seq_elem"
    | 38 -> "seq_elem_list"
    | 39 -> "start_rule_sign_opt"
    | 40 -> "tada_rule"
    | 41 -> "unnamed_module_opt"
    | 42 -> "weight_opt"
    | 43 -> "yard_start_rule"
    | 44 -> "ACTION"
    | 45 -> "ALL_PUBLIC"
    | 46 -> "BAR"
    | 47 -> "BLOCK_END"
    | 48 -> "COLON"
    | 49 -> "COMMA"
    | 50 -> "DLABEL"
    | 51 -> "EOF"
    | 52 -> "EQUAL"
    | 53 -> "GREAT"
    | 54 -> "INCLUDE"
    | 55 -> "LESS"
    | 56 -> "LIDENT"
    | 57 -> "LPAREN"
    | 58 -> "MINUS"
    | 59 -> "MODULE"
    | 60 -> "NUMBER"
    | 61 -> "OPEN"
    | 62 -> "OPTIONS_START"
    | 63 -> "PARAM"
    | 64 -> "PLUS"
    | 65 -> "PREDICATE"
    | 66 -> "PRIVATE"
    | 67 -> "PUBLIC"
    | 68 -> "QUESTION"
    | 69 -> "RPAREN"
    | 70 -> "SEMICOLON"
    | 71 -> "SHARPLINE"
    | 72 -> "SQR_LBR"
    | 73 -> "SQR_RBR"
    | 74 -> "STAR"
    | 75 -> "START_RULE_SIGN"
    | 76 -> "STRING"
    | 77 -> "UIDENT"
    | _ -> ""
let tokenToNumber = function
    | ACTION _ -> 44
    | ALL_PUBLIC _ -> 45
    | BAR _ -> 46
    | BLOCK_END _ -> 47
    | COLON _ -> 48
    | COMMA _ -> 49
    | DLABEL _ -> 50
    | EOF _ -> 51
    | EQUAL _ -> 52
    | GREAT _ -> 53
    | INCLUDE _ -> 54
    | LESS _ -> 55
    | LIDENT _ -> 56
    | LPAREN _ -> 57
    | MINUS _ -> 58
    | MODULE _ -> 59
    | NUMBER _ -> 60
    | OPEN _ -> 61
    | OPTIONS_START _ -> 62
    | PARAM _ -> 63
    | PLUS _ -> 64
    | PREDICATE _ -> 65
    | PRIVATE _ -> 66
    | PUBLIC _ -> 67
    | QUESTION _ -> 68
    | RPAREN _ -> 69
    | SEMICOLON _ -> 70
    | SHARPLINE _ -> 71
    | SQR_LBR _ -> 72
    | SQR_RBR _ -> 73
    | STAR _ -> 74
    | START_RULE_SIGN _ -> 75
    | STRING _ -> 76
    | UIDENT _ -> 77

let mutable private cur = 0
let leftSide = [|6; 43; 12; 12; 12; 11; 25; 27; 27; 24; 26; 26; 41; 19; 19; 17; 10; 10; 18; 18; 23; 23; 22; 22; 1; 1; 7; 7; 34; 34; 33; 39; 39; 0; 0; 0; 9; 9; 8; 8; 29; 29; 28; 28; 42; 42; 2; 2; 3; 3; 36; 36; 20; 20; 13; 38; 38; 37; 21; 21; 35; 35; 31; 31; 4; 4; 30; 30; 32; 32; 32; 32; 32; 32; 32; 32; 14; 16; 16; 15; 15; 5; 5; 40; 40|]
let private rules = [|1; 12; 41; 19; 7; 6; 25; 12; 11; 12; 54; 76; 62; 27; 47; 24; 27; 10; 52; 26; 76; 10; 34; 17; 19; 18; 10; 23; 34; 56; 77; 59; 45; 59; 61; 10; 22; 49; 10; 22; 44; 70; 44; 33; 35; 34; 39; 0; 56; 9; 28; 48; 2; 75; 66; 67; 55; 8; 53; 56; 8; 56; 63; 63; 28; 72; 60; 73; 36; 3; 36; 46; 36; 46; 36; 3; 13; 20; 44; 37; 38; 1; 50; 42; 57; 20; 69; 37; 38; 21; 4; 31; 58; 70; 65; 32; 30; 52; 32; 44; 56; 76; 13; 5; 57; 2; 69; 72; 32; 73; 32; 68; 32; 64; 32; 74; 32; 14; 16; 14; 55; 16; 53; 56; 15; 29; 77; 51; 71|]
let private rulesStart = [|0; 5; 6; 8; 10; 10; 12; 15; 15; 17; 20; 21; 22; 23; 23; 25; 29; 30; 31; 32; 34; 37; 37; 37; 40; 41; 41; 43; 43; 43; 46; 53; 54; 54; 54; 55; 56; 59; 59; 61; 62; 63; 63; 65; 65; 68; 68; 70; 71; 73; 76; 77; 78; 79; 82; 87; 89; 89; 92; 93; 93; 94; 94; 95; 95; 96; 99; 100; 101; 102; 103; 104; 107; 110; 112; 114; 116; 117; 119; 120; 123; 123; 126; 127; 128; 129|]
let startRule = 1

let acceptEmptyInput = true

let defaultAstToDot =
    (fun (tree : Yard.Generators.RNGLR.AST.Tree<Token>) -> tree.AstToDot numToString tokenToNumber leftSide)

let private lists_gotos = [|1; 119; 74; 2; 20; 4; 6; 8; 3; 5; 7; 9; 16; 18; 13; 15; 10; 11; 12; 14; 17; 19; 21; 98; 24; 99; 96; 22; 97; 23; 25; 94; 95; 26; 27; 89; 28; 87; 29; 30; 31; 32; 33; 63; 68; 75; 45; 71; 34; 37; 38; 39; 81; 86; 82; 60; 76; 79; 80; 35; 36; 40; 41; 50; 42; 43; 44; 46; 83; 47; 48; 49; 51; 54; 52; 53; 55; 58; 57; 56; 59; 61; 62; 64; 65; 66; 67; 69; 72; 70; 73; 77; 78; 84; 85; 88; 90; 92; 91; 93; 100; 101; 115; 112; 114; 111; 102; 103; 105; 104; 106; 107; 108; 109; 110; 113; 116; 117; 118|]
let private small_gotos =
        [|3; 65536; 393217; 2883586; 65541; 720899; 786436; 1638405; 3538950; 4063239; 131077; 720899; 786440; 1638405; 3538950; 4063239; 262149; 720899; 786441; 1638405; 3538950; 4063239; 393217; 4980746; 524293; 655371; 1572876; 1769485; 3670030; 5046287; 589825; 3407888; 655365; 655377; 1703954; 3670030; 4980755; 5046287; 1048581; 655371; 1572876; 1769492; 3670030; 5046287; 1179649; 3080213; 1310725; 2162710; 2228247; 2555928; 2687001; 4915226; 1376258; 2293787; 4587548; 1441796; 2162710; 2228253; 2555928; 4915226; 1572867; 30; 4325407; 4390944; 1638401; 3670049; 1703938; 589858; 3604515; 1769474; 1835044; 4128805; 1835009; 3145766; 1900553; 131111; 852008; 1310761; 1376298; 2359339; 2424876; 2883629; 3276846; 3801135; 2162700; 262192; 327729; 852018; 1966131; 2097204; 2883637; 3276846; 3670070; 3735607; 4718648; 4980793; 5046330; 2228226; 2031675; 4259900; 2555905; 3407933; 2621449; 327729; 852018; 2097214; 3276846; 3670079; 3735607; 4718648; 4980793; 5046330; 2686979; 4194368; 4456513; 4849730; 2949122; 2752579; 4718660; 3014657; 3735621; 3080197; 1310790; 1376298; 2424876; 2883629; 3801135; 3145729; 4522055; 3276802; 983112; 3604553; 3342338; 1900618; 4128843; 3538955; 327729; 852018; 917580; 1048653; 2097230; 3276846; 3670079; 3735607; 4718648; 4980793; 5046330; 3604491; 327729; 852018; 917580; 1048655; 2097230; 3276846; 3670079; 3735607; 4718648; 4980793; 5046330; 3735555; 4194368; 4456513; 4849730; 3801089; 3473488; 3932169; 131153; 852008; 1310761; 1376298; 2359339; 2424876; 2883629; 3276846; 3801135; 3997697; 4522066; 4128770; 196691; 3014740; 4259848; 852008; 1310761; 1376298; 2359381; 2424876; 2883629; 3276846; 3801135; 4325378; 196694; 3014740; 4456452; 1376298; 2424919; 2490456; 3801135; 4521988; 1376298; 2424919; 2490457; 3801135; 4718594; 65626; 2883586; 4980745; 327729; 852018; 2097243; 3276846; 3670079; 3735607; 4718648; 4980793; 5046330; 5046276; 4194368; 4456513; 4784220; 4849730; 5308419; 4194368; 4456513; 4849730; 5373954; 983112; 3604553; 5439489; 3932253; 5505025; 4784222; 5701634; 1835103; 4128805; 5832706; 524384; 3670113; 5898241; 3473506; 6029314; 524387; 3670113; 6488069; 1114212; 1179749; 1245286; 2949223; 3866728; 6553605; 1114212; 1179749; 1245289; 2949223; 3866728; 6619139; 655466; 3670030; 5046287; 6684674; 1507435; 3997804; 6750212; 2162710; 2228333; 2555928; 4915226; 6881283; 655470; 3670030; 5046287; 6946818; 1441903; 3211376; 7077891; 655473; 3670030; 5046287; 7143426; 1441906; 3211376; 7340033; 3866739; 7536642; 458868; 4587637; 7667713; 2883702|]
let gotos = Array.zeroCreate 120
for i = 0 to 119 do
        gotos.[i] <- Array.zeroCreate 78
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
let private lists_reduces = [|[|0,1|]; [|3,1|]; [|3,2|]; [|2,1|]; [|2,2|]; [|5,2|]; [|11,1|]; [|9,3|]; [|16,1|]; [|10,1|]; [|17,1|]; [|8,1|]; [|8,2|]; [|6,3|]; [|0,2|]; [|29,1|]; [|29,2|]; [|29,3|]; [|30,7|]; [|50,1|]; [|51,1|]; [|57,2|]; [|57,3|]; [|62,1|]; [|70,1|]; [|69,1|]; [|65,3|]; [|74,2|]; [|73,2|]; [|75,2|]; [|54,5|]; [|81,1|]; [|81,2|]; [|81,3|]; [|40,1|]; [|78,1|]; [|77,2|]; [|76,1|]; [|79,3|]; [|71,3|]; [|47,1|]; [|46,2|]; [|48,2|]; [|49,3|]; [|53,1|]; [|55,1|]; [|55,2|]; [|58,1|]; [|53,2|]; [|53,3|]; [|24,1|]; [|52,1|]; [|72,3|]; [|68,1|]; [|82,1|]; [|64,1|]; [|67,1|]; [|44,3|]; [|66,1|]; [|42,1|]; [|42,2|]; [|36,3|]; [|39,1|]; [|38,2|]; [|34,1|]; [|35,1|]; [|31,1|]; [|60,1|]; [|12,1|]; [|0,3|]; [|14,1|]; [|15,2|]; [|15,3|]; [|15,4|]; [|20,2|]; [|20,3|]; [|23,2|]; [|23,3|]; [|14,2|]; [|19,2|]; [|18,1|]; [|0,4|]; [|0,5|]; [|26,2|]|]
let private small_reduces =
        [|65537; 3342336; 131080; 2949121; 3342337; 3670017; 3866625; 4325377; 4390913; 4587521; 4915201; 196616; 2949122; 3342338; 3670018; 3866626; 4325378; 4390914; 4587522; 4915202; 262152; 2949123; 3342339; 3670019; 3866627; 4325379; 4390915; 4587523; 4915203; 327688; 2949124; 3342340; 3670020; 3866628; 4325380; 4390916; 4587524; 4915204; 458762; 2949125; 3342341; 3538949; 3670021; 3866629; 4063237; 4325381; 4390917; 4587525; 4915205; 720899; 3080198; 3670022; 5046278; 786435; 3080199; 3670023; 5046279; 851981; 2949128; 3080200; 3211272; 3342344; 3407880; 3670024; 3866632; 3997704; 4325384; 4390920; 4587528; 4915208; 5046280; 917507; 3080201; 3670025; 5046281; 983053; 2949130; 3080202; 3211274; 3342346; 3407882; 3670026; 3866634; 3997706; 4325386; 4390922; 4587530; 4915210; 5046282; 1048577; 3080203; 1114113; 3080204; 1245194; 2949133; 3342349; 3538957; 3670029; 3866637; 4063245; 4325389; 4390925; 4587533; 4915213; 1310721; 3342350; 1376260; 2949135; 3342351; 3866639; 4587535; 1441796; 2949136; 3342352; 3866640; 4587536; 1507332; 2949137; 3342353; 3866641; 4587537; 1966088; 2949138; 3342354; 3670034; 3866642; 4325394; 4390930; 4587538; 4915218; 2031626; 2949139; 3014675; 3342355; 3670035; 3866643; 4325395; 4390931; 4522003; 4587539; 4915219; 2097162; 2949140; 3014676; 3342356; 3670036; 3866644; 4325396; 4390932; 4522004; 4587540; 4915220; 2228241; 2883605; 2949141; 3014677; 3276821; 3342357; 3670037; 3735573; 3801109; 3866645; 4325397; 4390933; 4522005; 4587541; 4718613; 4915221; 4980757; 5046293; 2293777; 2883606; 2949142; 3014678; 3276822; 3342358; 3670038; 3735574; 3801110; 3866646; 4325398; 4390934; 4522006; 4587542; 4718614; 4915222; 4980758; 5046294; 2359313; 2883607; 2949143; 3014679; 3276823; 3342359; 3670039; 3735575; 3801111; 3866647; 4325399; 4390935; 4522007; 4587543; 4718615; 4915223; 4980759; 5046295; 2424855; 2883608; 2949144; 3014680; 3276824; 3342360; 3473432; 3670040; 3735576; 3801112; 3866648; 4194328; 4259864; 4325400; 4390936; 4456472; 4522008; 4587544; 4718616; 4784152; 4849688; 4915224; 4980760; 5046296; 2490391; 2883609; 2949145; 3014681; 3276825; 3342361; 3473433; 3670041; 3735577; 3801113; 3866649; 4194329; 4259865; 4325401; 4390937; 4456473; 4522009; 4587545; 4718617; 4784153; 4849689; 4915225; 4980761; 5046297; 2686994; 2883610; 2949146; 3014682; 3276826; 3342362; 3670042; 3735578; 3801114; 3866650; 4259866; 4325402; 4390938; 4522010; 4587546; 4718618; 4915226; 4980762; 5046298; 2752535; 2883611; 2949147; 3014683; 3276827; 3342363; 3473435; 3670043; 3735579; 3801115; 3866651; 4194331; 4259867; 4325403; 4390939; 4456475; 4522011; 4587547; 4718619; 4784155; 4849691; 4915227; 4980763; 5046299; 2818071; 2883612; 2949148; 3014684; 3276828; 3342364; 3473436; 3670044; 3735580; 3801116; 3866652; 4194332; 4259868; 4325404; 4390940; 4456476; 4522012; 4587548; 4718620; 4784156; 4849692; 4915228; 4980764; 5046300; 2883607; 2883613; 2949149; 3014685; 3276829; 3342365; 3473437; 3670045; 3735581; 3801117; 3866653; 4194333; 4259869; 4325405; 4390941; 4456477; 4522013; 4587549; 4718621; 4784157; 4849693; 4915229; 4980765; 5046301; 3211287; 2883614; 2949150; 3014686; 3276830; 3342366; 3473438; 3670046; 3735582; 3801118; 3866654; 4194334; 4259870; 4325406; 4390942; 4456478; 4522014; 4587550; 4718622; 4784158; 4849694; 4915230; 4980766; 5046302; 3276823; 2883615; 2949151; 3014687; 3276831; 3342367; 3473439; 3670047; 3735583; 3801119; 3866655; 4194335; 4259871; 4325407; 4390943; 4456479; 4522015; 4587551; 4718623; 4784159; 4849695; 4915231; 4980767; 5046303; 3342359; 2883616; 2949152; 3014688; 3276832; 3342368; 3473440; 3670048; 3735584; 3801120; 3866656; 4194336; 4259872; 4325408; 4390944; 4456480; 4522016; 4587552; 4718624; 4784160; 4849696; 4915232; 4980768; 5046304; 3407895; 2883617; 2949153; 3014689; 3276833; 3342369; 3473441; 3670049; 3735585; 3801121; 3866657; 4194337; 4259873; 4325409; 4390945; 4456481; 4522017; 4587553; 4718625; 4784161; 4849697; 4915233; 4980769; 5046305; 3473431; 2883618; 2949154; 3014690; 3276834; 3342370; 3473442; 3670050; 3735586; 3801122; 3866658; 4194338; 4259874; 4325410; 4390946; 4456482; 4522018; 4587554; 4718626; 4784162; 4849698; 4915234; 4980770; 5046306; 3604481; 3473443; 3670017; 3473444; 3735559; 3276837; 3473445; 3670053; 3735589; 4718629; 4980773; 5046309; 3866648; 2883622; 2949158; 3014694; 3276838; 3342374; 3473446; 3670054; 3735590; 3801126; 3866662; 4128806; 4194342; 4259878; 4325414; 4390950; 4456486; 4522022; 4587558; 4718630; 4784166; 4849702; 4915238; 4980774; 5046310; 4063255; 2883623; 2949159; 3014695; 3276839; 3342375; 3473447; 3670055; 3735591; 3801127; 3866663; 4194343; 4259879; 4325415; 4390951; 4456487; 4522023; 4587559; 4718631; 4784167; 4849703; 4915239; 4980775; 5046311; 4128777; 2949160; 3342376; 3670056; 3866664; 4325416; 4390952; 4522024; 4587560; 4915240; 4194313; 2949161; 3342377; 3670057; 3866665; 4325417; 4390953; 4522025; 4587561; 4915241; 4325385; 2949162; 3342378; 3670058; 3866666; 4325418; 4390954; 4522026; 4587562; 4915242; 4390921; 2949163; 3342379; 3670059; 3866667; 4325419; 4390955; 4522027; 4587563; 4915243; 4456458; 2949164; 3014700; 3342380; 3670060; 3866668; 4325420; 4390956; 4522028; 4587564; 4915244; 4521995; 2883629; 2949165; 3014701; 3342381; 3670061; 3866669; 4325421; 4390957; 4522029; 4587565; 4915245; 4587531; 2883630; 2949166; 3014702; 3342382; 3670062; 3866670; 4325422; 4390958; 4522030; 4587566; 4915246; 4653063; 2883631; 3276847; 3670063; 3735599; 4718639; 4980783; 5046319; 4718602; 2949168; 3014704; 3342384; 3670064; 3866672; 4325424; 4390960; 4522032; 4587568; 4915248; 4784138; 2949169; 3014705; 3342385; 3670065; 3866673; 4325425; 4390961; 4522033; 4587569; 4915249; 4849676; 2949170; 3014706; 3342386; 3538994; 3670066; 3866674; 4063282; 4325426; 4390962; 4522034; 4587570; 4915250; 4915210; 2949171; 3014707; 3342387; 3670067; 3866675; 4325427; 4390963; 4522035; 4587571; 4915251; 5111831; 2883636; 2949172; 3014708; 3276852; 3342388; 3473460; 3670068; 3735604; 3801140; 3866676; 4194356; 4259892; 4325428; 4390964; 4456500; 4522036; 4587572; 4718644; 4784180; 4849716; 4915252; 4980788; 5046324; 5177367; 2883637; 2949173; 3014709; 3276853; 3342389; 3473461; 3670069; 3735605; 3801141; 3866677; 4194357; 4259893; 4325429; 4390965; 4456501; 4522037; 4587573; 4718645; 4784181; 4849717; 4915253; 4980789; 5046325; 5242903; 2883638; 2949174; 3014710; 3276854; 3342390; 3473462; 3670070; 3735606; 3801142; 3866678; 4194358; 4259894; 4325430; 4390966; 4456502; 4522038; 4587574; 4718646; 4784182; 4849718; 4915254; 4980790; 5046326; 5308434; 2883639; 2949175; 3014711; 3276855; 3342391; 3670071; 3735607; 3801143; 3866679; 4259895; 4325431; 4390967; 4522039; 4587575; 4718647; 4915255; 4980791; 5046327; 5373974; 2883615; 2949151; 3014687; 3276831; 3342367; 3407928; 3670047; 3735583; 3801119; 3866655; 4194335; 4259871; 4325407; 4390943; 4456479; 4522015; 4587551; 4718623; 4849695; 4915231; 4980767; 5046303; 5570561; 3735609; 5636097; 3407930; 5701633; 3145787; 5767169; 3145788; 5963778; 3145789; 4128829; 6029313; 3473470; 6094849; 3473471; 6160385; 3670080; 6225921; 3670081; 6291459; 3670082; 4325442; 4390978; 6357000; 2949187; 3342403; 3670083; 3866691; 4325443; 4390979; 4587587; 4915267; 6422532; 2949188; 3342404; 3866692; 4587588; 6488065; 3342405; 6553602; 3342406; 4587590; 6684676; 2949191; 3342407; 3866695; 4587591; 6750212; 2949192; 3342408; 3866696; 4587592; 6815748; 2949193; 3342409; 3866697; 4587593; 6946824; 2949194; 3342410; 3670090; 3866698; 4325450; 4390986; 4587594; 4915274; 7012360; 2949195; 3342411; 3670091; 3866699; 4325451; 4390987; 4587595; 4915275; 7143432; 2949196; 3342412; 3670092; 3866700; 4325452; 4390988; 4587596; 4915276; 7208968; 2949197; 3342413; 3670093; 3866701; 4325453; 4390989; 4587597; 4915277; 7274498; 3342414; 4587598; 7405570; 3670095; 5046351; 7471106; 3670096; 5046352; 7536641; 3342417; 7602177; 3342418; 7733249; 3342419|]
let reduces = Array.zeroCreate 120
for i = 0 to 119 do
        reduces.[i] <- Array.zeroCreate 78
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
let private lists_zeroReduces = [|[|25|]; [|25; 1; 0|]; [|4|]; [|7|]; [|28; 12|]; [|32|]; [|61|]; [|28|]; [|33|]; [|37|]; [|43|]; [|59|]; [|63|]; [|45|]; [|80|]; [|41|]; [|59; 56|]; [|56|]; [|13|]; [|21|]; [|22|]; [|27|]|]
let private small_zeroReduces =
        [|10; 2949120; 3342337; 3538944; 3670016; 3866624; 4063232; 4325376; 4390912; 4587520; 4915200; 65544; 2949122; 3342338; 3670018; 3866626; 4325378; 4390914; 4587522; 4915202; 131080; 2949122; 3342338; 3670018; 3866626; 4325378; 4390914; 4587522; 4915202; 262152; 2949122; 3342338; 3670018; 3866626; 4325378; 4390914; 4587522; 4915202; 524289; 3080195; 1048577; 3080195; 1310727; 2949124; 3342340; 3670021; 3866628; 4325381; 4390917; 4587524; 1376264; 2949126; 3342342; 3670022; 3866630; 4325382; 4390918; 4587526; 4915206; 1441799; 2949127; 3342343; 3670021; 3866631; 4325381; 4390917; 4587527; 1572865; 3670024; 1703938; 3145737; 4128777; 1769473; 3145738; 1900551; 2883595; 3276811; 3670027; 3735563; 4718603; 4980747; 5046283; 2228241; 2883596; 2949132; 3014668; 3276812; 3342348; 3670028; 3735564; 3801100; 3866636; 4325388; 4390924; 4521996; 4587532; 4718604; 4915212; 4980748; 5046284; 2949121; 3735565; 3080199; 2883595; 3276811; 3670027; 3735563; 4718603; 4980747; 5046283; 3276824; 2883598; 2949134; 3014670; 3276814; 3342350; 3473422; 3670030; 3735566; 3801102; 3866638; 4128782; 4194318; 4259854; 4325390; 4390926; 4456462; 4521998; 4587534; 4718606; 4784142; 4849678; 4915214; 4980750; 5046286; 3342359; 2883599; 2949135; 3014671; 3276815; 3342351; 3473423; 3670031; 3735567; 3801103; 3866639; 4194319; 4259855; 4325391; 4390927; 4456463; 4521999; 4587535; 4718607; 4784143; 4849679; 4915215; 4980751; 5046287; 3932167; 2883595; 3276811; 3670027; 3735563; 4718603; 4980747; 5046283; 4259847; 2883595; 3276811; 3670027; 3735563; 4718603; 4980747; 5046283; 4456464; 2883600; 2949137; 3014673; 3276811; 3342353; 3670032; 3735563; 3866641; 4325393; 4390929; 4522001; 4587537; 4718603; 4915217; 4980747; 5046283; 4522000; 2883600; 2949137; 3014673; 3276811; 3342353; 3670032; 3735563; 3866641; 4325393; 4390929; 4522001; 4587537; 4718603; 4915217; 4980747; 5046283; 4718602; 2949120; 3014656; 3342336; 3670016; 3866624; 4325376; 4390912; 4521984; 4587520; 4915200; 5373974; 2883598; 2949134; 3014670; 3276814; 3342350; 3670030; 3735566; 3801102; 3866638; 4128782; 4194318; 4259854; 4325390; 4390926; 4456462; 4521998; 4587534; 4718606; 4849678; 4915214; 4980750; 5046286; 5701633; 3145738; 6488066; 3342354; 4587538; 6553602; 3342354; 4587538; 6684680; 2949139; 3342355; 3670035; 3866643; 4325395; 4390931; 4587539; 4915219; 6750215; 2949127; 3342343; 3670021; 3866631; 4325381; 4390917; 4587527; 6946824; 2949140; 3342356; 3670036; 3866644; 4325396; 4390932; 4587540; 4915220; 7143432; 2949140; 3342356; 3670036; 3866644; 4325396; 4390932; 4587540; 4915220; 7536641; 3342357|]
let zeroReduces = Array.zeroCreate 120
for i = 0 to 119 do
        zeroReduces.[i] <- Array.zeroCreate 78
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
let private small_acc = [119; 0]
let private accStates = Array.zeroCreate 120
for i = 0 to 119 do
        accStates.[i] <- List.exists ((=) i) small_acc
let eofIndex = 51
let private parserSource = new ParserSource<Token> (gotos, reduces, zeroReduces, accStates, rules, rulesStart, leftSide, startRule, eofIndex, tokenToNumber, acceptEmptyInput, numToString)
let buildAst : (seq<Token> -> ParseResult<Token>) =
    buildAst<Token> parserSource

let _rnglr_epsilons : Tree<Token>[] = [|new Tree<_>(null,box (new AST(new Family(33, new Nodes([||])), null)), null); new Tree<_>(null,box (new AST(new Family(25, new Nodes([||])), null)), null); null; null; null; null; new Tree<_>(null,box (new AST(new Family(0, new Nodes([|box (new AST(new Family(25, new Nodes([||])), null)); box (new AST(new Family(4, new Nodes([||])), null)); box (new AST(new Family(12, new Nodes([|box (new AST(new Family(28, new Nodes([||])), null))|])), null)); box (new AST(new Family(13, new Nodes([||])), null)); box (new AST(new Family(27, new Nodes([||])), null))|])), null)), null); new Tree<_>(null,box (new AST(new Family(27, new Nodes([||])), null)), null); null; new Tree<_>(null,box (new AST(new Family(37, new Nodes([||])), null)), null); null; null; new Tree<_>(null,box (new AST(new Family(4, new Nodes([||])), null)), null); null; null; new Tree<_>(null,box (new AST(new Family(80, new Nodes([||])), null)), null); null; null; null; new Tree<_>(null,box (new AST(new Family(13, new Nodes([||])), null)), null); null; new Tree<_>(null,box (new AST(new Family(59, new Nodes([||])), null)), null); new Tree<_>(null,box (new AST(new Family(22, new Nodes([||])), null)), null); new Tree<_>(null,box (new AST(new Family(21, new Nodes([||])), null)), null); null; null; null; new Tree<_>(null,box (new AST(new Family(7, new Nodes([||])), null)), null); new Tree<_>(null,box (new AST(new Family(43, new Nodes([||])), null)), null); new Tree<_>(null,box (new AST(new Family(41, new Nodes([||])), null)), null); null; new Tree<_>(null,box (new AST(new Family(63, new Nodes([||])), null)), null); null; null; new Tree<_>(null,box (new AST(new Family(28, new Nodes([||])), null)), null); new Tree<_>(null,box (new AST(new Family(61, new Nodes([||])), null)), null); null; null; new Tree<_>(null,box (new AST(new Family(56, new Nodes([||])), null)), null); new Tree<_>(null,box (new AST(new Family(32, new Nodes([||])), null)), null); null; new Tree<_>(null,box (new AST(new Family(12, new Nodes([|box (new AST(new Family(28, new Nodes([||])), null))|])), null)), null); new Tree<_>(null,box (new AST(new Family(45, new Nodes([||])), null)), null); new Tree<_>(null,box (new AST(new Family(1, new Nodes([|box (new AST(new Family(0, new Nodes([|box (new AST(new Family(25, new Nodes([||])), null)); box (new AST(new Family(4, new Nodes([||])), null)); box (new AST(new Family(12, new Nodes([|box (new AST(new Family(28, new Nodes([||])), null))|])), null)); box (new AST(new Family(13, new Nodes([||])), null)); box (new AST(new Family(27, new Nodes([||])), null))|])), null))|])), null)), null)|]
let _rnglr_filtered_epsilons : Tree<Token>[] = [|new Tree<_>(null,box (new AST(new Family(33, new Nodes([||])), null)), null); new Tree<_>(null,box (new AST(new Family(25, new Nodes([||])), null)), null); null; null; null; null; new Tree<_>(null,box (new AST(new Family(0, new Nodes([|box (new AST(new Family(25, new Nodes([||])), null)); box (new AST(new Family(4, new Nodes([||])), null)); box (new AST(new Family(12, new Nodes([|box (new AST(new Family(28, new Nodes([||])), null))|])), null)); box (new AST(new Family(13, new Nodes([||])), null)); box (new AST(new Family(27, new Nodes([||])), null))|])), null)), null); new Tree<_>(null,box (new AST(new Family(27, new Nodes([||])), null)), null); null; new Tree<_>(null,box (new AST(new Family(37, new Nodes([||])), null)), null); null; null; new Tree<_>(null,box (new AST(new Family(4, new Nodes([||])), null)), null); null; null; new Tree<_>(null,box (new AST(new Family(80, new Nodes([||])), null)), null); null; null; null; new Tree<_>(null,box (new AST(new Family(13, new Nodes([||])), null)), null); null; new Tree<_>(null,box (new AST(new Family(59, new Nodes([||])), null)), null); new Tree<_>(null,box (new AST(new Family(22, new Nodes([||])), null)), null); new Tree<_>(null,box (new AST(new Family(21, new Nodes([||])), null)), null); null; null; null; new Tree<_>(null,box (new AST(new Family(7, new Nodes([||])), null)), null); new Tree<_>(null,box (new AST(new Family(43, new Nodes([||])), null)), null); new Tree<_>(null,box (new AST(new Family(41, new Nodes([||])), null)), null); null; new Tree<_>(null,box (new AST(new Family(63, new Nodes([||])), null)), null); null; null; new Tree<_>(null,box (new AST(new Family(28, new Nodes([||])), null)), null); new Tree<_>(null,box (new AST(new Family(61, new Nodes([||])), null)), null); null; null; new Tree<_>(null,box (new AST(new Family(56, new Nodes([||])), null)), null); new Tree<_>(null,box (new AST(new Family(32, new Nodes([||])), null)), null); null; new Tree<_>(null,box (new AST(new Family(12, new Nodes([|box (new AST(new Family(28, new Nodes([||])), null))|])), null)), null); new Tree<_>(null,box (new AST(new Family(45, new Nodes([||])), null)), null); new Tree<_>(null,box (new AST(new Family(1, new Nodes([|box (new AST(new Family(0, new Nodes([|box (new AST(new Family(25, new Nodes([||])), null)); box (new AST(new Family(4, new Nodes([||])), null)); box (new AST(new Family(12, new Nodes([|box (new AST(new Family(28, new Nodes([||])), null))|])), null)); box (new AST(new Family(13, new Nodes([||])), null)); box (new AST(new Family(27, new Nodes([||])), null))|])), null))|])), null)), null)|]
for x in _rnglr_filtered_epsilons do if x <> null then x.ChooseSingleAst()
let _rnglr_extra_array, _rnglr_rule_, _rnglr_concats = 
  (Array.zeroCreate 0 : array<'_rnglr_type_access_modifier_opt * '_rnglr_type_action_opt * '_rnglr_type_alts * '_rnglr_type_bar_seq_nlist * '_rnglr_type_bound * '_rnglr_type_call * '_rnglr_type_file * '_rnglr_type_foot_opt * '_rnglr_type_formal_meta_list * '_rnglr_type_formal_meta_param_opt * '_rnglr_type_ident * '_rnglr_type_include_ * '_rnglr_type_includesOrOptions * '_rnglr_type_lbl_seq * '_rnglr_type_meta_param * '_rnglr_type_meta_param_opt * '_rnglr_type_meta_params * '_rnglr_type_module_ * '_rnglr_type_module_header * '_rnglr_type_modules * '_rnglr_type_no_lbl_seq * '_rnglr_type_omit_opt * '_rnglr_type_open_list * '_rnglr_type_openings * '_rnglr_type_option * '_rnglr_type_option_block * '_rnglr_type_option_value * '_rnglr_type_opts * '_rnglr_type_param_list * '_rnglr_type_param_opt * '_rnglr_type_patt * '_rnglr_type_predicate_opt * '_rnglr_type_prim * '_rnglr_type_rule * '_rnglr_type_rule_nlist * '_rnglr_type_semi_opt * '_rnglr_type_seq * '_rnglr_type_seq_elem * '_rnglr_type_seq_elem_list * '_rnglr_type_start_rule_sign_opt * '_rnglr_type_tada_rule * '_rnglr_type_unnamed_module_opt * '_rnglr_type_weight_opt * '_rnglr_type_yard_start_rule>), 
  [|
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            ((unbox _rnglr_children.[0]) : '_rnglr_type_action_opt) 
             |> List.iter (fun (_S1) -> 
              ((unbox _rnglr_children.[1]) : '_rnglr_type_includesOrOptions) 
               |> List.iter (fun (_S2) -> 
                ((unbox _rnglr_children.[2]) : '_rnglr_type_unnamed_module_opt) 
                 |> List.iter (fun (_S3) -> 
                  ((unbox _rnglr_children.[3]) : '_rnglr_type_modules) 
                   |> List.iter (fun (_S4) -> 
                    ((unbox _rnglr_children.[4]) : '_rnglr_type_foot_opt) 
                     |> List.iter (fun (_S5) -> 
                      _rnglr_cycle_res := (
                        
# 111 "Parser.fsy"
                                
                                {
                                    info = { fileName = !currentFilename }
                                    head = _S1
                                    grammar = fst _S2 @ _S3 @ _S4
                                    foot = _S5
                                    options = snd _S2
                                }
                              
                          )::!_rnglr_cycle_res ) ) ) ) )
            !_rnglr_cycle_res
          )
            )
# 106 "Parser.fsy"
               : '_rnglr_type_file) 
# 345 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          ((unbox _rnglr_children.[0]) : '_rnglr_type_file) 
            )
# 106 "Parser.fsy"
               : '_rnglr_type_yard_start_rule) 
# 355 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            ((unbox _rnglr_children.[0]) : '_rnglr_type_option_block) 
             |> List.iter (fun (_S1) -> 
              ((unbox _rnglr_children.[1]) : '_rnglr_type_includesOrOptions) 
               |> List.iter (fun (_S2) -> 
                _rnglr_cycle_res := (
                  
# 124 "Parser.fsy"
                                                       fst _S2, joinMaps _S1 (snd _S2)
                    )::!_rnglr_cycle_res ) )
            !_rnglr_cycle_res
          )
            )
# 121 "Parser.fsy"
               : '_rnglr_type_includesOrOptions) 
# 377 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            ((unbox _rnglr_children.[0]) : '_rnglr_type_include_) 
             |> List.iter (fun (_S1) -> 
              ((unbox _rnglr_children.[1]) : '_rnglr_type_includesOrOptions) 
               |> List.iter (fun (_S2) -> 
                _rnglr_cycle_res := (
                  
# 123 "Parser.fsy"
                                                   (_S1 @ fst _S2), snd _S2
                    )::!_rnglr_cycle_res ) )
            !_rnglr_cycle_res
          )
            )
# 121 "Parser.fsy"
               : '_rnglr_type_includesOrOptions) 
# 399 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            _rnglr_cycle_res := (
              
# 122 "Parser.fsy"
                    [], Map.empty 
                )::!_rnglr_cycle_res
            !_rnglr_cycle_res
          )
            )
# 121 "Parser.fsy"
               : '_rnglr_type_includesOrOptions) 
# 417 "Parser.fs"
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
                  
# 128 "Parser.fsy"
                       
                          let grammar = (parseRules _S2.text).grammar
                          if grammar |> List.exists (fun m -> m.name.IsNone) then
                              eprintfn "Error %s: Grammar in included files must be inside modules" _S2.text
                          grammar
                      
                    )::!_rnglr_cycle_res ) )
            !_rnglr_cycle_res
          )
            )
# 126 "Parser.fsy"
               : '_rnglr_type_include_) 
# 444 "Parser.fs"
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
                    
# 135 "Parser.fsy"
                                                                 Map.ofList _S2 : Map<_,_>
                      )::!_rnglr_cycle_res ) ) )
            !_rnglr_cycle_res
          )
            )
# 135 "Parser.fsy"
               : '_rnglr_type_option_block) 
# 468 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            _rnglr_cycle_res := (
              
# 137 "Parser.fsy"
                                               [] 
                )::!_rnglr_cycle_res
            !_rnglr_cycle_res
          )
            )
# 137 "Parser.fsy"
               : '_rnglr_type_opts) 
# 486 "Parser.fs"
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
                  
# 137 "Parser.fsy"
                                      _S1::_S2 
                    )::!_rnglr_cycle_res ) )
            !_rnglr_cycle_res
          )
            )
# 137 "Parser.fsy"
               : '_rnglr_type_opts) 
# 508 "Parser.fs"
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
                    
# 139 "Parser.fsy"
                                                       (_S1 : Source.t).text, (_S3 : Source.t).text 
                      )::!_rnglr_cycle_res ) ) )
            !_rnglr_cycle_res
          )
            )
# 139 "Parser.fsy"
               : '_rnglr_type_option) 
# 532 "Parser.fs"
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
                
# 141 "Parser.fsy"
                                                      _S1 
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 141 "Parser.fsy"
               : '_rnglr_type_option_value) 
# 552 "Parser.fs"
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
                
# 141 "Parser.fsy"
                                      _S1 
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 141 "Parser.fsy"
               : '_rnglr_type_option_value) 
# 572 "Parser.fs"
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
                
# 144 "Parser.fsy"
                     
                        match _S1 with
                        | [] -> []
                        | x ->  defaultModules x
                    
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 143 "Parser.fsy"
               : '_rnglr_type_unnamed_module_opt) 
# 596 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            _rnglr_cycle_res := (
              
# 150 "Parser.fsy"
                                                         [] 
                )::!_rnglr_cycle_res
            !_rnglr_cycle_res
          )
            )
# 150 "Parser.fsy"
               : '_rnglr_type_modules) 
# 614 "Parser.fs"
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
                  
# 150 "Parser.fsy"
                                              _S1 :: _S2 
                    )::!_rnglr_cycle_res ) )
            !_rnglr_cycle_res
          )
            )
# 150 "Parser.fsy"
               : '_rnglr_type_modules) 
# 636 "Parser.fs"
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
                      
# 153 "Parser.fsy"
                           
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
# 152 "Parser.fsy"
               : '_rnglr_type_module_) 
# 669 "Parser.fs"
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
                
# 162 "Parser.fsy"
                                                 _S1 
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 162 "Parser.fsy"
               : '_rnglr_type_ident) 
# 689 "Parser.fs"
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
                
# 162 "Parser.fsy"
                                 _S1 
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 162 "Parser.fsy"
               : '_rnglr_type_ident) 
# 709 "Parser.fs"
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
                
# 169 "Parser.fsy"
                                         allPublic := false; false 
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 165 "Parser.fsy"
               : '_rnglr_type_module_header) 
# 729 "Parser.fs"
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
                  
# 165 "Parser.fsy"
                                                     
                                    (* It's important the word "module" is here. It guaranties, that it won't be an epsilon-tree, so allPublic will be evaluated before rules *)
                                    allPublic := true; true
                                  
                    )::!_rnglr_cycle_res ) )
            !_rnglr_cycle_res
          )
            )
# 165 "Parser.fsy"
               : '_rnglr_type_module_header) 
# 754 "Parser.fs"
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
                    
# 171 "Parser.fsy"
                                                                _S2::_S3 
                      )::!_rnglr_cycle_res ) ) )
            !_rnglr_cycle_res
          )
            )
# 171 "Parser.fsy"
               : '_rnglr_type_openings) 
# 778 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            _rnglr_cycle_res := (
              
# 171 "Parser.fsy"
                            [] 
                )::!_rnglr_cycle_res
            !_rnglr_cycle_res
          )
            )
# 171 "Parser.fsy"
               : '_rnglr_type_openings) 
# 796 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            _rnglr_cycle_res := (
              
# 173 "Parser.fsy"
                                                               [] 
                )::!_rnglr_cycle_res
            !_rnglr_cycle_res
          )
            )
# 173 "Parser.fsy"
               : '_rnglr_type_open_list) 
# 814 "Parser.fs"
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
                    
# 173 "Parser.fsy"
                                                        _S2::_S3 
                      )::!_rnglr_cycle_res ) ) )
            !_rnglr_cycle_res
          )
            )
# 173 "Parser.fsy"
               : '_rnglr_type_open_list) 
# 838 "Parser.fs"
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
                
# 175 "Parser.fsy"
                                                Some _S1 
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 175 "Parser.fsy"
               : '_rnglr_type_action_opt) 
# 858 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            _rnglr_cycle_res := (
              
# 175 "Parser.fsy"
                            None 
                )::!_rnglr_cycle_res
            !_rnglr_cycle_res
          )
            )
# 175 "Parser.fsy"
               : '_rnglr_type_action_opt) 
# 876 "Parser.fs"
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
                  
# 177 "Parser.fsy"
                                                          Some _S2 
                    )::!_rnglr_cycle_res ) )
            !_rnglr_cycle_res
          )
            )
# 177 "Parser.fsy"
               : '_rnglr_type_foot_opt) 
# 898 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            _rnglr_cycle_res := (
              
# 177 "Parser.fsy"
                          None 
                )::!_rnglr_cycle_res
            !_rnglr_cycle_res
          )
            )
# 177 "Parser.fsy"
               : '_rnglr_type_foot_opt) 
# 916 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            _rnglr_cycle_res := (
              
# 181 "Parser.fsy"
                      [] 
                )::!_rnglr_cycle_res
            !_rnglr_cycle_res
          )
            )
# 179 "Parser.fsy"
               : '_rnglr_type_rule_nlist) 
# 934 "Parser.fs"
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
                    
# 180 "Parser.fsy"
                            _S1::_S3 
                      )::!_rnglr_cycle_res ) ) )
            !_rnglr_cycle_res
          )
            )
# 179 "Parser.fsy"
               : '_rnglr_type_rule_nlist) 
# 958 "Parser.fs"
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
                            
# 184 "Parser.fsy"
                                  
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
# 183 "Parser.fsy"
               : '_rnglr_type_rule) 
# 999 "Parser.fs"
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
                
# 195 "Parser.fsy"
                                                                true
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 195 "Parser.fsy"
               : '_rnglr_type_start_rule_sign_opt) 
# 1019 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            _rnglr_cycle_res := (
              
# 195 "Parser.fsy"
                                    false
                )::!_rnglr_cycle_res
            !_rnglr_cycle_res
          )
            )
# 195 "Parser.fsy"
               : '_rnglr_type_start_rule_sign_opt) 
# 1037 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            _rnglr_cycle_res := (
              
# 197 "Parser.fsy"
                                                                           !allPublic 
                )::!_rnglr_cycle_res
            !_rnglr_cycle_res
          )
            )
# 197 "Parser.fsy"
               : '_rnglr_type_access_modifier_opt) 
# 1055 "Parser.fs"
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
                
# 197 "Parser.fsy"
                                                                 false 
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 197 "Parser.fsy"
               : '_rnglr_type_access_modifier_opt) 
# 1075 "Parser.fs"
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
                
# 197 "Parser.fsy"
                                              true 
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 197 "Parser.fsy"
               : '_rnglr_type_access_modifier_opt) 
# 1095 "Parser.fs"
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
                    
# 199 "Parser.fsy"
                                                                                   Some _S2
                      )::!_rnglr_cycle_res ) ) )
            !_rnglr_cycle_res
          )
            )
# 199 "Parser.fsy"
               : '_rnglr_type_formal_meta_param_opt) 
# 1119 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            _rnglr_cycle_res := (
              
# 199 "Parser.fsy"
                                       None 
                )::!_rnglr_cycle_res
            !_rnglr_cycle_res
          )
            )
# 199 "Parser.fsy"
               : '_rnglr_type_formal_meta_param_opt) 
# 1137 "Parser.fs"
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
                  
# 202 "Parser.fsy"
                                                             _S1::_S2
                    )::!_rnglr_cycle_res ) )
            !_rnglr_cycle_res
          )
            )
# 201 "Parser.fsy"
               : '_rnglr_type_formal_meta_list) 
# 1159 "Parser.fs"
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
                
# 201 "Parser.fsy"
                                          [_S1]
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 201 "Parser.fsy"
               : '_rnglr_type_formal_meta_list) 
# 1179 "Parser.fs"
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
                
# 204 "Parser.fsy"
                                             Some _S1
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 204 "Parser.fsy"
               : '_rnglr_type_param_opt) 
# 1199 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            _rnglr_cycle_res := (
              
# 204 "Parser.fsy"
                           None 
                )::!_rnglr_cycle_res
            !_rnglr_cycle_res
          )
            )
# 204 "Parser.fsy"
               : '_rnglr_type_param_opt) 
# 1217 "Parser.fs"
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
                  
# 206 "Parser.fsy"
                                                         _S1::_S2
                    )::!_rnglr_cycle_res ) )
            !_rnglr_cycle_res
          )
            )
# 206 "Parser.fsy"
               : '_rnglr_type_param_list) 
# 1239 "Parser.fs"
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
               : '_rnglr_type_param_list) 
# 1257 "Parser.fs"
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
                    
# 208 "Parser.fsy"
                                                                    Some _S2
                      )::!_rnglr_cycle_res ) ) )
            !_rnglr_cycle_res
          )
            )
# 208 "Parser.fsy"
               : '_rnglr_type_weight_opt) 
# 1281 "Parser.fs"
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
               : '_rnglr_type_weight_opt) 
# 1299 "Parser.fs"
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
                  
# 210 "Parser.fsy"
                                                        PAlt (_S1,_S2)
                    )::!_rnglr_cycle_res ) )
            !_rnglr_cycle_res
          )
            )
# 210 "Parser.fsy"
               : '_rnglr_type_alts) 
# 1321 "Parser.fs"
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
                
# 210 "Parser.fsy"
                            _S1 
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 210 "Parser.fsy"
               : '_rnglr_type_alts) 
# 1341 "Parser.fs"
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
                  
# 213 "Parser.fsy"
                                           _S2
                    )::!_rnglr_cycle_res ) )
            !_rnglr_cycle_res
          )
            )
# 212 "Parser.fsy"
               : '_rnglr_type_bar_seq_nlist) 
# 1363 "Parser.fs"
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
                    
# 212 "Parser.fsy"
                                                           PAlt(_S2,_S3) 
                      )::!_rnglr_cycle_res ) ) )
            !_rnglr_cycle_res
          )
            )
# 212 "Parser.fsy"
               : '_rnglr_type_bar_seq_nlist) 
# 1387 "Parser.fs"
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
                
# 215 "Parser.fsy"
                                                _S1
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 215 "Parser.fsy"
               : '_rnglr_type_seq) 
# 1407 "Parser.fs"
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
                
# 215 "Parser.fsy"
                                 _S1
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 215 "Parser.fsy"
               : '_rnglr_type_seq) 
# 1427 "Parser.fs"
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
                
# 218 "Parser.fsy"
                                     PSeq([], Some _S1, None) 
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 217 "Parser.fsy"
               : '_rnglr_type_no_lbl_seq) 
# 1447 "Parser.fs"
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
                    
# 217 "Parser.fsy"
                                                                    PSeq (_S1::_S2, _S3, None)
                      )::!_rnglr_cycle_res ) ) )
            !_rnglr_cycle_res
          )
            )
# 217 "Parser.fsy"
               : '_rnglr_type_no_lbl_seq) 
# 1471 "Parser.fs"
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
                        
# 220 "Parser.fsy"
                                                                             makeNewSeq _S4 _S1 _S2
                          )::!_rnglr_cycle_res ) ) ) ) )
            !_rnglr_cycle_res
          )
            )
# 220 "Parser.fsy"
               : '_rnglr_type_lbl_seq) 
# 1499 "Parser.fs"
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
                  
# 222 "Parser.fsy"
                                                                  _S1::_S2
                    )::!_rnglr_cycle_res ) )
            !_rnglr_cycle_res
          )
            )
# 222 "Parser.fsy"
               : '_rnglr_type_seq_elem_list) 
# 1521 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            _rnglr_cycle_res := (
              
# 222 "Parser.fsy"
                               [] 
                )::!_rnglr_cycle_res
            !_rnglr_cycle_res
          )
            )
# 222 "Parser.fsy"
               : '_rnglr_type_seq_elem_list) 
# 1539 "Parser.fs"
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
                    
# 224 "Parser.fsy"
                                                            {_S2 with checker = _S3; omit = _S1 }
                      )::!_rnglr_cycle_res ) ) )
            !_rnglr_cycle_res
          )
            )
# 224 "Parser.fsy"
               : '_rnglr_type_seq_elem) 
# 1563 "Parser.fs"
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
                
# 226 "Parser.fsy"
                                              true 
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 226 "Parser.fsy"
               : '_rnglr_type_omit_opt) 
# 1583 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            _rnglr_cycle_res := (
              
# 226 "Parser.fsy"
                          false 
                )::!_rnglr_cycle_res
            !_rnglr_cycle_res
          )
            )
# 226 "Parser.fsy"
               : '_rnglr_type_omit_opt) 
# 1601 "Parser.fs"
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
                
# 228 "Parser.fsy"
                                                  true
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 228 "Parser.fsy"
               : '_rnglr_type_semi_opt) 
# 1621 "Parser.fs"
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
               : '_rnglr_type_semi_opt) 
# 1639 "Parser.fs"
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
                
# 230 "Parser.fsy"
                                                      Some _S1 
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 230 "Parser.fsy"
               : '_rnglr_type_predicate_opt) 
# 1659 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            _rnglr_cycle_res := (
              
# 230 "Parser.fsy"
                               None 
                )::!_rnglr_cycle_res
            !_rnglr_cycle_res
          )
            )
# 230 "Parser.fsy"
               : '_rnglr_type_predicate_opt) 
# 1677 "Parser.fs"
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
                
# 233 "Parser.fsy"
                                         createSeqElem None false _S1 None      
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 232 "Parser.fsy"
               : '_rnglr_type_bound) 
# 1697 "Parser.fs"
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
                    
# 232 "Parser.fsy"
                                             createSeqElem (Some _S1) false _S3 None 
                      )::!_rnglr_cycle_res ) ) )
            !_rnglr_cycle_res
          )
            )
# 232 "Parser.fsy"
               : '_rnglr_type_bound) 
# 1721 "Parser.fs"
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
                
# 235 "Parser.fsy"
                                            _S1
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 235 "Parser.fsy"
               : '_rnglr_type_patt) 
# 1741 "Parser.fs"
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
                
# 235 "Parser.fsy"
                              _S1
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 235 "Parser.fsy"
               : '_rnglr_type_patt) 
# 1761 "Parser.fs"
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
                
# 244 "Parser.fsy"
                                            PLiteral _S1
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 237 "Parser.fsy"
               : '_rnglr_type_prim) 
# 1781 "Parser.fs"
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
                
# 243 "Parser.fsy"
                                            _S1
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 237 "Parser.fsy"
               : '_rnglr_type_prim) 
# 1801 "Parser.fs"
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
                
# 242 "Parser.fsy"
                                            _S1
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 237 "Parser.fsy"
               : '_rnglr_type_prim) 
# 1821 "Parser.fs"
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
                    
# 241 "Parser.fsy"
                                                _S2
                      )::!_rnglr_cycle_res ) ) )
            !_rnglr_cycle_res
          )
            )
# 237 "Parser.fsy"
               : '_rnglr_type_prim) 
# 1845 "Parser.fs"
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
                    
# 240 "Parser.fsy"
                                                POpt _S2
                      )::!_rnglr_cycle_res ) ) )
            !_rnglr_cycle_res
          )
            )
# 237 "Parser.fsy"
               : '_rnglr_type_prim) 
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
              (match ((unbox _rnglr_children.[1]) : Token) with QUESTION _rnglr_val -> [_rnglr_val] | a -> failwith "QUESTION expected, but %A found" a )
               |> List.iter (fun (_) -> 
                _rnglr_cycle_res := (
                  
# 239 "Parser.fsy"
                                              POpt _S1
                    )::!_rnglr_cycle_res ) )
            !_rnglr_cycle_res
          )
            )
# 237 "Parser.fsy"
               : '_rnglr_type_prim) 
# 1891 "Parser.fs"
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
                  
# 238 "Parser.fsy"
                                              PSome _S1
                    )::!_rnglr_cycle_res ) )
            !_rnglr_cycle_res
          )
            )
# 237 "Parser.fsy"
               : '_rnglr_type_prim) 
# 1913 "Parser.fs"
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
                  
# 237 "Parser.fsy"
                                              PMany _S1
                    )::!_rnglr_cycle_res ) )
            !_rnglr_cycle_res
          )
            )
# 237 "Parser.fsy"
               : '_rnglr_type_prim) 
# 1935 "Parser.fs"
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
                
# 246 "Parser.fsy"
                                  _S1
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 246 "Parser.fsy"
               : '_rnglr_type_meta_param) 
# 1955 "Parser.fs"
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
                  
# 249 "Parser.fsy"
                                                       _S1 :: _S2
                    )::!_rnglr_cycle_res ) )
            !_rnglr_cycle_res
          )
            )
# 248 "Parser.fsy"
               : '_rnglr_type_meta_params) 
# 1977 "Parser.fs"
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
                
# 248 "Parser.fsy"
                                         [_S1]
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 248 "Parser.fsy"
               : '_rnglr_type_meta_params) 
# 1997 "Parser.fs"
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
                    
# 251 "Parser.fsy"
                                                                       Some _S2
                      )::!_rnglr_cycle_res ) ) )
            !_rnglr_cycle_res
          )
            )
# 251 "Parser.fsy"
               : '_rnglr_type_meta_param_opt) 
# 2021 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            _rnglr_cycle_res := (
              
# 251 "Parser.fsy"
                                None 
                )::!_rnglr_cycle_res
            !_rnglr_cycle_res
          )
            )
# 251 "Parser.fsy"
               : '_rnglr_type_meta_param_opt) 
# 2039 "Parser.fs"
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
                    
# 255 "Parser.fsy"
                            match _S2 with
                            | None -> PRef  (_S1, _S3)
                            | Some x -> PMetaRef (_S1,_S3,x)
                          
                      )::!_rnglr_cycle_res ) ) )
            !_rnglr_cycle_res
          )
            )
# 253 "Parser.fsy"
               : '_rnglr_type_call) 
# 2066 "Parser.fs"
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
                
# 253 "Parser.fsy"
                              PToken _S1
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 253 "Parser.fsy"
               : '_rnglr_type_call) 
# 2086 "Parser.fs"
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
                
# 260 "Parser.fsy"
                                                
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 260 "Parser.fsy"
               : '_rnglr_type_tada_rule) 
# 2106 "Parser.fs"
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
                
# 260 "Parser.fsy"
                                       
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 260 "Parser.fsy"
               : '_rnglr_type_tada_rule) 
# 2126 "Parser.fs"
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
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_includesOrOptions)   ) |> List.concat));
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
