
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

let concatModOpt a b = fst a :: fst b, joinMaps (snd a) (snd b)

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

# 86 "Parser.fs"
type Token =
    | ACTION of Source.t
    | ALL_PUBLIC of Source.t
    | BAR of Source.t
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
    | PARAM of Source.t
    | PLUS of Source.t
    | PREDICATE of Source.t
    | PRIVATE of Source.t
    | PUBLIC of Source.t
    | QUESTION of Source.t
    | RPAREN of Source.t
    | SEMICOLON of Source.t
    | SET of Source.t
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
    | 12 -> "includes"
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
    | 24 -> "option_opt"
    | 25 -> "option_param"
    | 26 -> "option_params"
    | 27 -> "param_list"
    | 28 -> "param_opt"
    | 29 -> "patt"
    | 30 -> "predicate_opt"
    | 31 -> "prim"
    | 32 -> "rule"
    | 33 -> "rule_nlist"
    | 34 -> "semi_opt"
    | 35 -> "seq"
    | 36 -> "seq_elem"
    | 37 -> "seq_elem_list"
    | 38 -> "start_rule_sign_opt"
    | 39 -> "tada_rule"
    | 40 -> "unnamed_module_opt"
    | 41 -> "weight_opt"
    | 42 -> "yard_start_rule"
    | 43 -> "ACTION"
    | 44 -> "ALL_PUBLIC"
    | 45 -> "BAR"
    | 46 -> "COLON"
    | 47 -> "COMMA"
    | 48 -> "DLABEL"
    | 49 -> "EOF"
    | 50 -> "EQUAL"
    | 51 -> "GREAT"
    | 52 -> "INCLUDE"
    | 53 -> "LESS"
    | 54 -> "LIDENT"
    | 55 -> "LPAREN"
    | 56 -> "MINUS"
    | 57 -> "MODULE"
    | 58 -> "NUMBER"
    | 59 -> "OPEN"
    | 60 -> "PARAM"
    | 61 -> "PLUS"
    | 62 -> "PREDICATE"
    | 63 -> "PRIVATE"
    | 64 -> "PUBLIC"
    | 65 -> "QUESTION"
    | 66 -> "RPAREN"
    | 67 -> "SEMICOLON"
    | 68 -> "SET"
    | 69 -> "SHARPLINE"
    | 70 -> "SQR_LBR"
    | 71 -> "SQR_RBR"
    | 72 -> "STAR"
    | 73 -> "START_RULE_SIGN"
    | 74 -> "STRING"
    | 75 -> "UIDENT"
    | _ -> ""
let tokenToNumber = function
    | ACTION _ -> 43
    | ALL_PUBLIC _ -> 44
    | BAR _ -> 45
    | COLON _ -> 46
    | COMMA _ -> 47
    | DLABEL _ -> 48
    | EOF _ -> 49
    | EQUAL _ -> 50
    | GREAT _ -> 51
    | INCLUDE _ -> 52
    | LESS _ -> 53
    | LIDENT _ -> 54
    | LPAREN _ -> 55
    | MINUS _ -> 56
    | MODULE _ -> 57
    | NUMBER _ -> 58
    | OPEN _ -> 59
    | PARAM _ -> 60
    | PLUS _ -> 61
    | PREDICATE _ -> 62
    | PRIVATE _ -> 63
    | PUBLIC _ -> 64
    | QUESTION _ -> 65
    | RPAREN _ -> 66
    | SEMICOLON _ -> 67
    | SET _ -> 68
    | SHARPLINE _ -> 69
    | SQR_LBR _ -> 70
    | SQR_RBR _ -> 71
    | STAR _ -> 72
    | START_RULE_SIGN _ -> 73
    | STRING _ -> 74
    | UIDENT _ -> 75

let mutable private cur = 0
let leftSide = [|6; 42; 40; 19; 19; 17; 10; 10; 18; 18; 23; 23; 22; 22; 1; 1; 7; 7; 12; 12; 11; 33; 33; 32; 38; 38; 0; 0; 0; 9; 9; 8; 8; 28; 28; 27; 27; 41; 41; 2; 2; 3; 3; 35; 35; 20; 20; 13; 37; 37; 36; 21; 21; 34; 34; 30; 30; 4; 4; 29; 29; 31; 31; 31; 31; 31; 31; 31; 31; 14; 16; 16; 15; 15; 5; 5; 24; 24; 26; 26; 25; 39; 39|]
let private rules = [|1; 12; 40; 19; 7; 6; 33; 17; 19; 18; 10; 23; 33; 54; 75; 57; 44; 57; 59; 10; 22; 47; 10; 22; 43; 67; 43; 11; 12; 52; 74; 32; 34; 33; 38; 0; 54; 9; 27; 46; 24; 2; 73; 63; 64; 53; 8; 51; 54; 8; 54; 60; 60; 27; 70; 58; 71; 35; 3; 35; 45; 35; 45; 35; 3; 13; 20; 43; 36; 37; 1; 48; 41; 55; 20; 66; 36; 37; 21; 4; 30; 56; 67; 62; 31; 29; 50; 31; 43; 54; 74; 13; 5; 55; 2; 66; 70; 31; 71; 31; 65; 31; 61; 31; 72; 31; 14; 16; 14; 53; 16; 51; 54; 15; 28; 75; 68; 26; 25; 47; 26; 25; 54; 50; 74; 49; 69|]
let private rulesStart = [|0; 5; 6; 7; 7; 9; 13; 14; 15; 16; 18; 21; 21; 21; 24; 25; 25; 27; 27; 27; 29; 31; 31; 34; 42; 43; 43; 43; 44; 45; 48; 48; 50; 51; 52; 52; 54; 54; 57; 57; 59; 60; 62; 65; 66; 67; 68; 71; 76; 78; 78; 81; 82; 82; 83; 83; 84; 84; 85; 88; 89; 90; 91; 92; 93; 96; 99; 101; 103; 105; 106; 108; 109; 112; 112; 115; 116; 118; 118; 121; 122; 125; 126; 127|]
let startRule = 1

let acceptEmptyInput = true

let defaultAstToDot =
    (fun (tree : Yard.Generators.RNGLR.AST.Tree<Token>) -> tree.AstToDot numToString tokenToNumber leftSide)

let private lists_gotos = [|1; 116; 61; 2; 6; 4; 3; 5; 7; 93; 10; 94; 91; 8; 92; 9; 11; 89; 90; 12; 13; 84; 14; 82; 15; 16; 74; 17; 18; 19; 20; 50; 55; 62; 32; 58; 21; 24; 25; 26; 68; 73; 69; 47; 63; 66; 67; 22; 23; 27; 28; 37; 29; 30; 31; 33; 70; 34; 35; 36; 38; 41; 39; 40; 42; 45; 44; 43; 46; 48; 49; 51; 52; 53; 54; 56; 59; 57; 60; 64; 65; 71; 72; 75; 81; 78; 76; 77; 79; 80; 83; 85; 87; 86; 88; 95; 96; 112; 109; 111; 108; 97; 106; 107; 98; 100; 99; 101; 102; 103; 104; 105; 110; 113; 114; 115|]
let private small_gotos =
        [|3; 65536; 393217; 2818050; 65539; 720899; 786436; 3407877; 131075; 720899; 786438; 3407877; 262145; 4849671; 393221; 2097160; 2162697; 2490378; 2621451; 4784140; 458754; 2228237; 4390926; 524292; 2097160; 2162703; 2490378; 4784140; 655363; 16; 4128785; 4194322; 720897; 3538963; 786434; 589844; 3473429; 851970; 1769494; 3932183; 917505; 3014680; 983042; 1572889; 4456474; 1048585; 131099; 851996; 1310749; 1376286; 2293791; 2359328; 2818081; 3145762; 3670051; 1310732; 262180; 327717; 852006; 1900583; 2031656; 2818089; 3145762; 3538986; 3604523; 4587564; 4849709; 4915246; 1376258; 1966127; 4063280; 1703937; 3276849; 1769481; 327717; 852006; 2031666; 3145762; 3538995; 3604523; 4587564; 4849709; 4915246; 1835011; 3997748; 4259893; 4718646; 2097154; 2687031; 4587576; 2162689; 3604537; 2228229; 1310778; 1376286; 2359328; 2818081; 3670051; 2293761; 4325435; 2424834; 983100; 3473469; 2490370; 1835070; 3932223; 2686987; 327717; 852006; 917568; 1048641; 2031682; 3145762; 3538995; 3604523; 4587564; 4849709; 4915246; 2752523; 327717; 852006; 917568; 1048643; 2031682; 3145762; 3538995; 3604523; 4587564; 4849709; 4915246; 2883587; 3997748; 4259893; 4718646; 2949121; 3342404; 3080201; 131141; 851996; 1310749; 1376286; 2293791; 2359328; 2818081; 3145762; 3670051; 3145729; 4325446; 3276802; 196679; 2949192; 3407880; 851996; 1310749; 1376286; 2293833; 2359328; 2818081; 3145762; 3670051; 3473410; 196682; 2949192; 3604484; 1376286; 2359371; 2424908; 3670051; 3670020; 1376286; 2359371; 2424909; 3670051; 3866626; 65614; 2818050; 4128777; 327717; 852006; 2031695; 3145762; 3538995; 3604523; 4587564; 4849709; 4915246; 4194308; 3997748; 4259893; 4653136; 4718646; 4456451; 3997748; 4259893; 4718646; 4521986; 983100; 3473469; 4587521; 3801169; 4653057; 4653138; 4849667; 1638483; 1704020; 3539029; 4915201; 3080278; 4980739; 1638483; 1704023; 3539029; 5111809; 3276888; 5177345; 4849753; 5373954; 1769562; 3932183; 5505026; 524379; 3539036; 5570561; 3342429; 5701634; 524382; 3539036; 6160389; 1114207; 1179744; 1245281; 2883682; 3735651; 6225925; 1114207; 1179744; 1245284; 2883682; 3735651; 6291459; 655461; 3539046; 4915303; 6356994; 1507432; 3866729; 6422532; 2097160; 2162794; 2490378; 4784140; 6553603; 655467; 3539046; 4915303; 6619138; 1441900; 3080301; 6750211; 655470; 3539046; 4915303; 6815746; 1441903; 3080301; 7143425; 3735664; 7340034; 458865; 4391026; 7471105; 2818163|]
let gotos = Array.zeroCreate 117
for i = 0 to 116 do
        gotos.[i] <- Array.zeroCreate 76
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
let private lists_reduces = [|[|0,1|]; [|19,1|]; [|19,2|]; [|20,2|]; [|0,2|]; [|22,1|]; [|22,2|]; [|22,3|]; [|23,8|]; [|43,1|]; [|44,1|]; [|50,2|]; [|50,3|]; [|55,1|]; [|63,1|]; [|62,1|]; [|58,3|]; [|67,2|]; [|66,2|]; [|68,2|]; [|47,5|]; [|74,1|]; [|74,2|]; [|74,3|]; [|33,1|]; [|71,1|]; [|70,2|]; [|69,1|]; [|72,3|]; [|64,3|]; [|40,1|]; [|39,2|]; [|41,2|]; [|42,3|]; [|46,1|]; [|48,1|]; [|48,2|]; [|51,1|]; [|46,2|]; [|46,3|]; [|14,1|]; [|45,1|]; [|65,3|]; [|61,1|]; [|75,1|]; [|57,1|]; [|60,1|]; [|37,3|]; [|59,1|]; [|79,1|]; [|78,3|]; [|80,3|]; [|76,2|]; [|35,1|]; [|35,2|]; [|29,3|]; [|32,1|]; [|31,2|]; [|27,1|]; [|28,1|]; [|24,1|]; [|53,1|]; [|2,1|]; [|0,3|]; [|4,1|]; [|5,2|]; [|5,3|]; [|5,4|]; [|10,2|]; [|10,3|]; [|13,2|]; [|13,3|]; [|6,1|]; [|7,1|]; [|4,2|]; [|9,2|]; [|8,1|]; [|0,4|]; [|0,5|]; [|16,2|]|]
let private small_reduces =
        [|65537; 3211264; 131080; 2883585; 3211265; 3538945; 3735553; 4128769; 4194305; 4390913; 4784129; 196616; 2883586; 3211266; 3538946; 3735554; 4128770; 4194306; 4390914; 4784130; 327689; 2883587; 3211267; 3407875; 3538947; 3735555; 4128771; 4194307; 4390915; 4784131; 393217; 3211268; 458756; 2883589; 3211269; 3735557; 4390917; 524292; 2883590; 3211270; 3735558; 4390918; 589828; 2883591; 3211271; 3735559; 4390919; 1114120; 2883592; 3211272; 3538952; 3735560; 4128776; 4194312; 4390920; 4784136; 1179658; 2883593; 2949129; 3211273; 3538953; 3735561; 4128777; 4194313; 4325385; 4390921; 4784137; 1245194; 2883594; 2949130; 3211274; 3538954; 3735562; 4128778; 4194314; 4325386; 4390922; 4784138; 1376273; 2818059; 2883595; 2949131; 3145739; 3211275; 3538955; 3604491; 3670027; 3735563; 4128779; 4194315; 4325387; 4390923; 4587531; 4784139; 4849675; 4915211; 1441809; 2818060; 2883596; 2949132; 3145740; 3211276; 3538956; 3604492; 3670028; 3735564; 4128780; 4194316; 4325388; 4390924; 4587532; 4784140; 4849676; 4915212; 1507345; 2818061; 2883597; 2949133; 3145741; 3211277; 3538957; 3604493; 3670029; 3735565; 4128781; 4194317; 4325389; 4390925; 4587533; 4784141; 4849677; 4915213; 1572887; 2818062; 2883598; 2949134; 3145742; 3211278; 3342350; 3538958; 3604494; 3670030; 3735566; 3997710; 4063246; 4128782; 4194318; 4259854; 4325390; 4390926; 4587534; 4653070; 4718606; 4784142; 4849678; 4915214; 1638423; 2818063; 2883599; 2949135; 3145743; 3211279; 3342351; 3538959; 3604495; 3670031; 3735567; 3997711; 4063247; 4128783; 4194319; 4259855; 4325391; 4390927; 4587535; 4653071; 4718607; 4784143; 4849679; 4915215; 1835026; 2818064; 2883600; 2949136; 3145744; 3211280; 3538960; 3604496; 3670032; 3735568; 4063248; 4128784; 4194320; 4325392; 4390928; 4587536; 4784144; 4849680; 4915216; 1900567; 2818065; 2883601; 2949137; 3145745; 3211281; 3342353; 3538961; 3604497; 3670033; 3735569; 3997713; 4063249; 4128785; 4194321; 4259857; 4325393; 4390929; 4587537; 4653073; 4718609; 4784145; 4849681; 4915217; 1966103; 2818066; 2883602; 2949138; 3145746; 3211282; 3342354; 3538962; 3604498; 3670034; 3735570; 3997714; 4063250; 4128786; 4194322; 4259858; 4325394; 4390930; 4587538; 4653074; 4718610; 4784146; 4849682; 4915218; 2031639; 2818067; 2883603; 2949139; 3145747; 3211283; 3342355; 3538963; 3604499; 3670035; 3735571; 3997715; 4063251; 4128787; 4194323; 4259859; 4325395; 4390931; 4587539; 4653075; 4718611; 4784147; 4849683; 4915219; 2359319; 2818068; 2883604; 2949140; 3145748; 3211284; 3342356; 3538964; 3604500; 3670036; 3735572; 3997716; 4063252; 4128788; 4194324; 4259860; 4325396; 4390932; 4587540; 4653076; 4718612; 4784148; 4849684; 4915220; 2424855; 2818069; 2883605; 2949141; 3145749; 3211285; 3342357; 3538965; 3604501; 3670037; 3735573; 3997717; 4063253; 4128789; 4194325; 4259861; 4325397; 4390933; 4587541; 4653077; 4718613; 4784149; 4849685; 4915221; 2490391; 2818070; 2883606; 2949142; 3145750; 3211286; 3342358; 3538966; 3604502; 3670038; 3735574; 3997718; 4063254; 4128790; 4194326; 4259862; 4325398; 4390934; 4587542; 4653078; 4718614; 4784150; 4849686; 4915222; 2555927; 2818071; 2883607; 2949143; 3145751; 3211287; 3342359; 3538967; 3604503; 3670039; 3735575; 3997719; 4063255; 4128791; 4194327; 4259863; 4325399; 4390935; 4587543; 4653079; 4718615; 4784151; 4849687; 4915223; 2621463; 2818072; 2883608; 2949144; 3145752; 3211288; 3342360; 3538968; 3604504; 3670040; 3735576; 3997720; 4063256; 4128792; 4194328; 4259864; 4325400; 4390936; 4587544; 4653080; 4718616; 4784152; 4849688; 4915224; 2752513; 3342361; 2818049; 3342362; 2883591; 3145755; 3342363; 3538971; 3604507; 4587547; 4849691; 4915227; 3014680; 2818076; 2883612; 2949148; 3145756; 3211292; 3342364; 3538972; 3604508; 3670044; 3735580; 3932188; 3997724; 4063260; 4128796; 4194332; 4259868; 4325404; 4390940; 4587548; 4653084; 4718620; 4784156; 4849692; 4915228; 3211287; 2818077; 2883613; 2949149; 3145757; 3211293; 3342365; 3538973; 3604509; 3670045; 3735581; 3997725; 4063261; 4128797; 4194333; 4259869; 4325405; 4390941; 4587549; 4653085; 4718621; 4784157; 4849693; 4915229; 3276809; 2883614; 3211294; 3538974; 3735582; 4128798; 4194334; 4325406; 4390942; 4784158; 3342345; 2883615; 3211295; 3538975; 3735583; 4128799; 4194335; 4325407; 4390943; 4784159; 3473417; 2883616; 3211296; 3538976; 3735584; 4128800; 4194336; 4325408; 4390944; 4784160; 3538953; 2883617; 3211297; 3538977; 3735585; 4128801; 4194337; 4325409; 4390945; 4784161; 3604490; 2883618; 2949154; 3211298; 3538978; 3735586; 4128802; 4194338; 4325410; 4390946; 4784162; 3670027; 2818083; 2883619; 2949155; 3211299; 3538979; 3735587; 4128803; 4194339; 4325411; 4390947; 4784163; 3735563; 2818084; 2883620; 2949156; 3211300; 3538980; 3735588; 4128804; 4194340; 4325412; 4390948; 4784164; 3801095; 2818085; 3145765; 3538981; 3604517; 4587557; 4849701; 4915237; 3866634; 2883622; 2949158; 3211302; 3538982; 3735590; 4128806; 4194342; 4325414; 4390950; 4784166; 3932170; 2883623; 2949159; 3211303; 3538983; 3735591; 4128807; 4194343; 4325415; 4390951; 4784167; 3997707; 2883624; 2949160; 3211304; 3407912; 3538984; 3735592; 4128808; 4194344; 4325416; 4390952; 4784168; 4063242; 2883625; 2949161; 3211305; 3538985; 3735593; 4128809; 4194345; 4325417; 4390953; 4784169; 4259863; 2818090; 2883626; 2949162; 3145770; 3211306; 3342378; 3538986; 3604522; 3670058; 3735594; 3997738; 4063274; 4128810; 4194346; 4259882; 4325418; 4390954; 4587562; 4653098; 4718634; 4784170; 4849706; 4915242; 4325399; 2818091; 2883627; 2949163; 3145771; 3211307; 3342379; 3538987; 3604523; 3670059; 3735595; 3997739; 4063275; 4128811; 4194347; 4259883; 4325419; 4390955; 4587563; 4653099; 4718635; 4784171; 4849707; 4915243; 4390935; 2818092; 2883628; 2949164; 3145772; 3211308; 3342380; 3538988; 3604524; 3670060; 3735596; 3997740; 4063276; 4128812; 4194348; 4259884; 4325420; 4390956; 4587564; 4653100; 4718636; 4784172; 4849708; 4915244; 4456466; 2818093; 2883629; 2949165; 3145773; 3211309; 3538989; 3604525; 3670061; 3735597; 4063277; 4128813; 4194349; 4325421; 4390957; 4587565; 4784173; 4849709; 4915245; 4522006; 2818069; 2883605; 2949141; 3145749; 3211285; 3276846; 3538965; 3604501; 3670037; 3735573; 3997717; 4063253; 4128789; 4194325; 4259861; 4325397; 4390933; 4587541; 4718613; 4784149; 4849685; 4915221; 4718593; 3604527; 4784129; 3276848; 4915208; 2818097; 3145777; 3538993; 3604529; 3670065; 4587569; 4849713; 4915249; 5046280; 2818098; 3145778; 3538994; 3604530; 3670066; 4587570; 4849714; 4915250; 5242889; 2818099; 3080243; 3145779; 3538995; 3604531; 3670067; 4587571; 4849715; 4915251; 5308424; 2818100; 3145780; 3538996; 3604532; 3670068; 4587572; 4849716; 4915252; 5373953; 3014709; 5439489; 3014710; 5636098; 3014711; 3932215; 5701633; 3342392; 5767169; 3342393; 5832705; 3539002; 5898241; 3539003; 5963779; 3539004; 4128828; 4194364; 6029320; 2883645; 3211325; 3539005; 3735613; 4128829; 4194365; 4390973; 4784189; 6094852; 2883646; 3211326; 3735614; 4390974; 6160385; 3211327; 6225922; 3211328; 4390976; 6356996; 2883649; 3211329; 3735617; 4390977; 6422532; 2883650; 3211330; 3735618; 4390978; 6488068; 2883651; 3211331; 3735619; 4390979; 6619144; 2883652; 3211332; 3539012; 3735620; 4128836; 4194372; 4390980; 4784196; 6684680; 2883653; 3211333; 3539013; 3735621; 4128837; 4194373; 4390981; 4784197; 6815752; 2883654; 3211334; 3539014; 3735622; 4128838; 4194374; 4390982; 4784198; 6881288; 2883655; 3211335; 3539015; 3735623; 4128839; 4194375; 4390983; 4784199; 6946826; 2883656; 3080264; 3211336; 3539016; 3735624; 3866696; 4128840; 4194376; 4390984; 4784200; 7012362; 2883657; 3080265; 3211337; 3539017; 3735625; 3866697; 4128841; 4194377; 4390985; 4784201; 7077890; 3211338; 4390986; 7208962; 3539019; 4915275; 7274498; 3539020; 4915276; 7340033; 3211341; 7405569; 3211342; 7536641; 3211343|]
let reduces = Array.zeroCreate 117
for i = 0 to 116 do
        reduces.[i] <- Array.zeroCreate 76
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
let private lists_zeroReduces = [|[|15|]; [|15; 1; 0|]; [|18|]; [|21; 2|]; [|25|]; [|54|]; [|21|]; [|26|]; [|30|]; [|36|]; [|77|]; [|52|]; [|56|]; [|38|]; [|73|]; [|34|]; [|52; 49|]; [|49|]; [|3|]; [|11|]; [|12|]; [|17|]|]
let private small_zeroReduces =
        [|9; 2883584; 3211265; 3407872; 3538944; 3735552; 4128768; 4194304; 4390912; 4784128; 65544; 2883586; 3211266; 3538946; 3735554; 4128770; 4194306; 4390914; 4784130; 131080; 2883586; 3211266; 3538946; 3735554; 4128770; 4194306; 4390914; 4784130; 393223; 2883587; 3211267; 3538948; 3735555; 4128772; 4194308; 4390915; 458760; 2883589; 3211269; 3538949; 3735557; 4128773; 4194309; 4390917; 4784133; 524295; 2883590; 3211270; 3538948; 3735558; 4128772; 4194308; 4390918; 655361; 3538951; 786434; 3014664; 3932168; 851969; 3014665; 983048; 2818058; 3145738; 3538954; 3604490; 3670026; 4587530; 4849674; 4915210; 1048583; 2818059; 3145739; 3538955; 3604491; 4587531; 4849675; 4915211; 1376273; 2818060; 2883596; 2949132; 3145740; 3211276; 3538956; 3604492; 3670028; 3735564; 4128780; 4194316; 4325388; 4390924; 4587532; 4784140; 4849676; 4915212; 2097153; 3604493; 2228231; 2818059; 3145739; 3538955; 3604491; 4587531; 4849675; 4915211; 2424856; 2818062; 2883598; 2949134; 3145742; 3211278; 3342350; 3538958; 3604494; 3670030; 3735566; 3932174; 3997710; 4063246; 4128782; 4194318; 4259854; 4325390; 4390926; 4587534; 4653070; 4718606; 4784142; 4849678; 4915214; 2490391; 2818063; 2883599; 2949135; 3145743; 3211279; 3342351; 3538959; 3604495; 3670031; 3735567; 3997711; 4063247; 4128783; 4194319; 4259855; 4325391; 4390927; 4587535; 4653071; 4718607; 4784143; 4849679; 4915215; 3080199; 2818059; 3145739; 3538955; 3604491; 4587531; 4849675; 4915211; 3407879; 2818059; 3145739; 3538955; 3604491; 4587531; 4849675; 4915211; 3604496; 2818064; 2883601; 2949137; 3145739; 3211281; 3538960; 3604491; 3735569; 4128785; 4194321; 4325393; 4390929; 4587531; 4784145; 4849675; 4915211; 3670032; 2818064; 2883601; 2949137; 3145739; 3211281; 3538960; 3604491; 3735569; 4128785; 4194321; 4325393; 4390929; 4587531; 4784145; 4849675; 4915211; 3866634; 2883584; 2949120; 3211264; 3538944; 3735552; 4128768; 4194304; 4325376; 4390912; 4784128; 4522006; 2818062; 2883598; 2949134; 3145742; 3211278; 3538958; 3604494; 3670030; 3735566; 3932174; 3997710; 4063246; 4128782; 4194318; 4259854; 4325390; 4390926; 4587534; 4718606; 4784142; 4849678; 4915214; 5373953; 3014665; 6160386; 3211282; 4390930; 6225922; 3211282; 4390930; 6357000; 2883603; 3211283; 3538963; 3735571; 4128787; 4194323; 4390931; 4784147; 6422535; 2883590; 3211270; 3538948; 3735558; 4128772; 4194308; 4390918; 6619144; 2883604; 3211284; 3538964; 3735572; 4128788; 4194324; 4390932; 4784148; 6815752; 2883604; 3211284; 3538964; 3735572; 4128788; 4194324; 4390932; 4784148; 7340033; 3211285|]
let zeroReduces = Array.zeroCreate 117
for i = 0 to 116 do
        zeroReduces.[i] <- Array.zeroCreate 76
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
let private small_acc = [116; 0]
let private accStates = Array.zeroCreate 117
for i = 0 to 116 do
        accStates.[i] <- List.exists ((=) i) small_acc
let eofIndex = 49
let private parserSource = new ParserSource<Token> (gotos, reduces, zeroReduces, accStates, rules, rulesStart, leftSide, startRule, eofIndex, tokenToNumber, acceptEmptyInput, numToString)
let buildAst : (seq<Token> -> ParseResult<Token>) =
    buildAst<Token> parserSource

let _rnglr_epsilons : Tree<Token>[] = [|new Tree<_>(null,box (new AST(new Family(26, new Nodes([||])), null)), null); new Tree<_>(null,box (new AST(new Family(15, new Nodes([||])), null)), null); null; null; null; null; new Tree<_>(null,box (new AST(new Family(0, new Nodes([|box (new AST(new Family(15, new Nodes([||])), null)); box (new AST(new Family(18, new Nodes([||])), null)); box (new AST(new Family(2, new Nodes([|box (new AST(new Family(21, new Nodes([||])), null))|])), null)); box (new AST(new Family(3, new Nodes([||])), null)); box (new AST(new Family(17, new Nodes([||])), null))|])), null)), null); new Tree<_>(null,box (new AST(new Family(17, new Nodes([||])), null)), null); null; new Tree<_>(null,box (new AST(new Family(30, new Nodes([||])), null)), null); null; null; new Tree<_>(null,box (new AST(new Family(18, new Nodes([||])), null)), null); null; null; new Tree<_>(null,box (new AST(new Family(73, new Nodes([||])), null)), null); null; null; null; new Tree<_>(null,box (new AST(new Family(3, new Nodes([||])), null)), null); null; new Tree<_>(null,box (new AST(new Family(52, new Nodes([||])), null)), null); new Tree<_>(null,box (new AST(new Family(12, new Nodes([||])), null)), null); new Tree<_>(null,box (new AST(new Family(11, new Nodes([||])), null)), null); new Tree<_>(null,box (new AST(new Family(77, new Nodes([||])), null)), null); null; null; new Tree<_>(null,box (new AST(new Family(36, new Nodes([||])), null)), null); new Tree<_>(null,box (new AST(new Family(34, new Nodes([||])), null)), null); null; new Tree<_>(null,box (new AST(new Family(56, new Nodes([||])), null)), null); null; null; new Tree<_>(null,box (new AST(new Family(21, new Nodes([||])), null)), null); new Tree<_>(null,box (new AST(new Family(54, new Nodes([||])), null)), null); null; null; new Tree<_>(null,box (new AST(new Family(49, new Nodes([||])), null)), null); new Tree<_>(null,box (new AST(new Family(25, new Nodes([||])), null)), null); null; new Tree<_>(null,box (new AST(new Family(2, new Nodes([|box (new AST(new Family(21, new Nodes([||])), null))|])), null)), null); new Tree<_>(null,box (new AST(new Family(38, new Nodes([||])), null)), null); new Tree<_>(null,box (new AST(new Family(1, new Nodes([|box (new AST(new Family(0, new Nodes([|box (new AST(new Family(15, new Nodes([||])), null)); box (new AST(new Family(18, new Nodes([||])), null)); box (new AST(new Family(2, new Nodes([|box (new AST(new Family(21, new Nodes([||])), null))|])), null)); box (new AST(new Family(3, new Nodes([||])), null)); box (new AST(new Family(17, new Nodes([||])), null))|])), null))|])), null)), null)|]
let _rnglr_filtered_epsilons : Tree<Token>[] = [|new Tree<_>(null,box (new AST(new Family(26, new Nodes([||])), null)), null); new Tree<_>(null,box (new AST(new Family(15, new Nodes([||])), null)), null); null; null; null; null; new Tree<_>(null,box (new AST(new Family(0, new Nodes([|box (new AST(new Family(15, new Nodes([||])), null)); box (new AST(new Family(18, new Nodes([||])), null)); box (new AST(new Family(2, new Nodes([|box (new AST(new Family(21, new Nodes([||])), null))|])), null)); box (new AST(new Family(3, new Nodes([||])), null)); box (new AST(new Family(17, new Nodes([||])), null))|])), null)), null); new Tree<_>(null,box (new AST(new Family(17, new Nodes([||])), null)), null); null; new Tree<_>(null,box (new AST(new Family(30, new Nodes([||])), null)), null); null; null; new Tree<_>(null,box (new AST(new Family(18, new Nodes([||])), null)), null); null; null; new Tree<_>(null,box (new AST(new Family(73, new Nodes([||])), null)), null); null; null; null; new Tree<_>(null,box (new AST(new Family(3, new Nodes([||])), null)), null); null; new Tree<_>(null,box (new AST(new Family(52, new Nodes([||])), null)), null); new Tree<_>(null,box (new AST(new Family(12, new Nodes([||])), null)), null); new Tree<_>(null,box (new AST(new Family(11, new Nodes([||])), null)), null); new Tree<_>(null,box (new AST(new Family(77, new Nodes([||])), null)), null); null; null; new Tree<_>(null,box (new AST(new Family(36, new Nodes([||])), null)), null); new Tree<_>(null,box (new AST(new Family(34, new Nodes([||])), null)), null); null; new Tree<_>(null,box (new AST(new Family(56, new Nodes([||])), null)), null); null; null; new Tree<_>(null,box (new AST(new Family(21, new Nodes([||])), null)), null); new Tree<_>(null,box (new AST(new Family(54, new Nodes([||])), null)), null); null; null; new Tree<_>(null,box (new AST(new Family(49, new Nodes([||])), null)), null); new Tree<_>(null,box (new AST(new Family(25, new Nodes([||])), null)), null); null; new Tree<_>(null,box (new AST(new Family(2, new Nodes([|box (new AST(new Family(21, new Nodes([||])), null))|])), null)), null); new Tree<_>(null,box (new AST(new Family(38, new Nodes([||])), null)), null); new Tree<_>(null,box (new AST(new Family(1, new Nodes([|box (new AST(new Family(0, new Nodes([|box (new AST(new Family(15, new Nodes([||])), null)); box (new AST(new Family(18, new Nodes([||])), null)); box (new AST(new Family(2, new Nodes([|box (new AST(new Family(21, new Nodes([||])), null))|])), null)); box (new AST(new Family(3, new Nodes([||])), null)); box (new AST(new Family(17, new Nodes([||])), null))|])), null))|])), null)), null)|]
for x in _rnglr_filtered_epsilons do if x <> null then x.ChooseSingleAst()
let _rnglr_extra_array, _rnglr_rule_, _rnglr_concats = 
  (Array.zeroCreate 0 : array<'_rnglr_type_access_modifier_opt * '_rnglr_type_action_opt * '_rnglr_type_alts * '_rnglr_type_bar_seq_nlist * '_rnglr_type_bound * '_rnglr_type_call * '_rnglr_type_file * '_rnglr_type_foot_opt * '_rnglr_type_formal_meta_list * '_rnglr_type_formal_meta_param_opt * '_rnglr_type_ident * '_rnglr_type_include_ * '_rnglr_type_includes * '_rnglr_type_lbl_seq * '_rnglr_type_meta_param * '_rnglr_type_meta_param_opt * '_rnglr_type_meta_params * '_rnglr_type_module_ * '_rnglr_type_module_header * '_rnglr_type_modules * '_rnglr_type_no_lbl_seq * '_rnglr_type_omit_opt * '_rnglr_type_open_list * '_rnglr_type_openings * '_rnglr_type_option_opt * '_rnglr_type_option_param * '_rnglr_type_option_params * '_rnglr_type_param_list * '_rnglr_type_param_opt * '_rnglr_type_patt * '_rnglr_type_predicate_opt * '_rnglr_type_prim * '_rnglr_type_rule * '_rnglr_type_rule_nlist * '_rnglr_type_semi_opt * '_rnglr_type_seq * '_rnglr_type_seq_elem * '_rnglr_type_seq_elem_list * '_rnglr_type_start_rule_sign_opt * '_rnglr_type_tada_rule * '_rnglr_type_unnamed_module_opt * '_rnglr_type_weight_opt * '_rnglr_type_yard_start_rule>), 
  [|
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            ((unbox _rnglr_children.[0]) : '_rnglr_type_action_opt) 
             |> List.iter (fun (_S1) -> 
              ((unbox _rnglr_children.[1]) : '_rnglr_type_includes) 
               |> List.iter (fun (_S2) -> 
                ((unbox _rnglr_children.[2]) : '_rnglr_type_unnamed_module_opt) 
                 |> List.iter (fun (_S3) -> 
                  ((unbox _rnglr_children.[3]) : '_rnglr_type_modules) 
                   |> List.iter (fun (_S4) -> 
                    ((unbox _rnglr_children.[4]) : '_rnglr_type_foot_opt) 
                     |> List.iter (fun (_S5) -> 
                      _rnglr_cycle_res := (
                        
# 113 "Parser.fsy"
                                
                                {
                                    info = { fileName = !currentFilename }
                                    head = _S1
                                    grammar = fst _S2 @ fst _S3 @ fst _S4
                                    foot = _S5
                                    options = (snd _S4) |> joinMaps (snd _S3) |> joinMaps (snd _S2)
                                }
                              
                          )::!_rnglr_cycle_res ) ) ) ) )
            !_rnglr_cycle_res
          )
            )
# 108 "Parser.fsy"
               : '_rnglr_type_file) 
# 343 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          ((unbox _rnglr_children.[0]) : '_rnglr_type_file) 
            )
# 108 "Parser.fsy"
               : '_rnglr_type_yard_start_rule) 
# 353 "Parser.fs"
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
                
# 124 "Parser.fsy"
                     
                        match fst _S1 with
                        | [] -> [], Map.empty
                        | x ->  defaultModules x, snd _S1
                    
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 123 "Parser.fsy"
               : '_rnglr_type_unnamed_module_opt) 
# 377 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            _rnglr_cycle_res := (
              
# 131 "Parser.fsy"
                          [], Map.empty 
                )::!_rnglr_cycle_res
            !_rnglr_cycle_res
          )
            )
# 130 "Parser.fsy"
               : '_rnglr_type_modules) 
# 395 "Parser.fs"
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
                  
# 130 "Parser.fsy"
                                              concatModOpt _S1 _S2 
                    )::!_rnglr_cycle_res ) )
            !_rnglr_cycle_res
          )
            )
# 130 "Parser.fsy"
               : '_rnglr_type_modules) 
# 417 "Parser.fs"
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
                      
# 134 "Parser.fsy"
                           
                              {
                                  allPublic = _S1
                                  name = Some _S2
                                  openings = _S3
                                  rules = fst _S4
                              }
                              , snd _S4
                          
                        )::!_rnglr_cycle_res ) ) ) )
            !_rnglr_cycle_res
          )
            )
# 133 "Parser.fsy"
               : '_rnglr_type_module_) 
# 451 "Parser.fs"
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
                
# 144 "Parser.fsy"
                                                 _S1 
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 144 "Parser.fsy"
               : '_rnglr_type_ident) 
# 471 "Parser.fs"
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
                
# 144 "Parser.fsy"
                                 _S1 
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 144 "Parser.fsy"
               : '_rnglr_type_ident) 
# 491 "Parser.fs"
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
                
# 151 "Parser.fsy"
                                         allPublic := false; false 
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 147 "Parser.fsy"
               : '_rnglr_type_module_header) 
# 511 "Parser.fs"
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
                  
# 147 "Parser.fsy"
                                                     
                                    (* It's important the word "module" is here. It guaranties, that it won't be an epsilon-tree, so allPublic will be evaluated before rules *)
                                    allPublic := true; true
                                  
                    )::!_rnglr_cycle_res ) )
            !_rnglr_cycle_res
          )
            )
# 147 "Parser.fsy"
               : '_rnglr_type_module_header) 
# 536 "Parser.fs"
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
                    
# 153 "Parser.fsy"
                                                                _S2::_S3 
                      )::!_rnglr_cycle_res ) ) )
            !_rnglr_cycle_res
          )
            )
# 153 "Parser.fsy"
               : '_rnglr_type_openings) 
# 560 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            _rnglr_cycle_res := (
              
# 153 "Parser.fsy"
                            [] 
                )::!_rnglr_cycle_res
            !_rnglr_cycle_res
          )
            )
# 153 "Parser.fsy"
               : '_rnglr_type_openings) 
# 578 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            _rnglr_cycle_res := (
              
# 155 "Parser.fsy"
                                                               [] 
                )::!_rnglr_cycle_res
            !_rnglr_cycle_res
          )
            )
# 155 "Parser.fsy"
               : '_rnglr_type_open_list) 
# 596 "Parser.fs"
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
                    
# 155 "Parser.fsy"
                                                        _S2::_S3 
                      )::!_rnglr_cycle_res ) ) )
            !_rnglr_cycle_res
          )
            )
# 155 "Parser.fsy"
               : '_rnglr_type_open_list) 
# 620 "Parser.fs"
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
                
# 157 "Parser.fsy"
                                                Some _S1 
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 157 "Parser.fsy"
               : '_rnglr_type_action_opt) 
# 640 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            _rnglr_cycle_res := (
              
# 157 "Parser.fsy"
                            None 
                )::!_rnglr_cycle_res
            !_rnglr_cycle_res
          )
            )
# 157 "Parser.fsy"
               : '_rnglr_type_action_opt) 
# 658 "Parser.fs"
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
                  
# 159 "Parser.fsy"
                                                          Some _S2 
                    )::!_rnglr_cycle_res ) )
            !_rnglr_cycle_res
          )
            )
# 159 "Parser.fsy"
               : '_rnglr_type_foot_opt) 
# 680 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            _rnglr_cycle_res := (
              
# 159 "Parser.fsy"
                          None 
                )::!_rnglr_cycle_res
            !_rnglr_cycle_res
          )
            )
# 159 "Parser.fsy"
               : '_rnglr_type_foot_opt) 
# 698 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            _rnglr_cycle_res := (
              
# 161 "Parser.fsy"
                                                                                               [], Map.empty 
                )::!_rnglr_cycle_res
            !_rnglr_cycle_res
          )
            )
# 161 "Parser.fsy"
               : '_rnglr_type_includes) 
# 716 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            ((unbox _rnglr_children.[0]) : '_rnglr_type_include_) 
             |> List.iter (fun (_S1) -> 
              ((unbox _rnglr_children.[1]) : '_rnglr_type_includes) 
               |> List.iter (fun (_S2) -> 
                _rnglr_cycle_res := (
                  
# 161 "Parser.fsy"
                                                 fst _S1 @ fst _S2, joinMaps (snd _S1) (snd _S2) 
                    )::!_rnglr_cycle_res ) )
            !_rnglr_cycle_res
          )
            )
# 161 "Parser.fsy"
               : '_rnglr_type_includes) 
# 738 "Parser.fs"
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
                  
# 165 "Parser.fsy"
                       
                          let def = parseRules _S2.text in
                          if def.grammar |> List.exists (fun m -> m.name.IsNone) then
                              eprintfn "Error %s: Grammar in included files must be inside modules" _S2.text
                          def.grammar, def.options
                      
                    )::!_rnglr_cycle_res ) )
            !_rnglr_cycle_res
          )
            )
# 163 "Parser.fsy"
               : '_rnglr_type_include_) 
# 765 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            _rnglr_cycle_res := (
              
# 175 "Parser.fsy"
                      [], Map.empty 
                )::!_rnglr_cycle_res
            !_rnglr_cycle_res
          )
            )
# 173 "Parser.fsy"
               : '_rnglr_type_rule_nlist) 
# 783 "Parser.fs"
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
                    
# 174 "Parser.fsy"
                            concatModOpt _S1 _S3 
                      )::!_rnglr_cycle_res ) ) )
            !_rnglr_cycle_res
          )
            )
# 173 "Parser.fsy"
               : '_rnglr_type_rule_nlist) 
# 807 "Parser.fs"
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
                        ((unbox _rnglr_children.[6]) : '_rnglr_type_option_opt) 
                         |> List.iter (fun (_S7) -> 
                          ((unbox _rnglr_children.[7]) : '_rnglr_type_alts) 
                           |> List.iter (fun (_S8) -> 
                            _rnglr_cycle_res := (
                              
# 178 "Parser.fsy"
                                      
                                      let newRule =
                                          {
                                              Rule.isStart = _S1
                                              Rule.isPublic = _S2
                                              Rule.name = _S3
                                              Rule.metaArgs = getList _S4
                                              Rule.body = _S8
                                              Rule.args = _S5
                                          }
                                      in
                                      match _S7 with
                                      | Some m -> newRule, Map.empty.Add (newRule, m)
                                      | None -> newRule, Map.empty
                                     
                                )::!_rnglr_cycle_res ) ) ) ) ) ) ) )
            !_rnglr_cycle_res
          )
            )
# 177 "Parser.fsy"
               : '_rnglr_type_rule) 
# 855 "Parser.fs"
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
                
# 194 "Parser.fsy"
                                                                true
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 194 "Parser.fsy"
               : '_rnglr_type_start_rule_sign_opt) 
# 875 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            _rnglr_cycle_res := (
              
# 194 "Parser.fsy"
                                    false
                )::!_rnglr_cycle_res
            !_rnglr_cycle_res
          )
            )
# 194 "Parser.fsy"
               : '_rnglr_type_start_rule_sign_opt) 
# 893 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            _rnglr_cycle_res := (
              
# 196 "Parser.fsy"
                                                                           !allPublic 
                )::!_rnglr_cycle_res
            !_rnglr_cycle_res
          )
            )
# 196 "Parser.fsy"
               : '_rnglr_type_access_modifier_opt) 
# 911 "Parser.fs"
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
                
# 196 "Parser.fsy"
                                                                 false 
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 196 "Parser.fsy"
               : '_rnglr_type_access_modifier_opt) 
# 931 "Parser.fs"
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
                
# 196 "Parser.fsy"
                                              true 
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 196 "Parser.fsy"
               : '_rnglr_type_access_modifier_opt) 
# 951 "Parser.fs"
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
                    
# 198 "Parser.fsy"
                                                                                   Some _S2
                      )::!_rnglr_cycle_res ) ) )
            !_rnglr_cycle_res
          )
            )
# 198 "Parser.fsy"
               : '_rnglr_type_formal_meta_param_opt) 
# 975 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            _rnglr_cycle_res := (
              
# 198 "Parser.fsy"
                                       None 
                )::!_rnglr_cycle_res
            !_rnglr_cycle_res
          )
            )
# 198 "Parser.fsy"
               : '_rnglr_type_formal_meta_param_opt) 
# 993 "Parser.fs"
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
                  
# 201 "Parser.fsy"
                                                             _S1::_S2
                    )::!_rnglr_cycle_res ) )
            !_rnglr_cycle_res
          )
            )
# 200 "Parser.fsy"
               : '_rnglr_type_formal_meta_list) 
# 1015 "Parser.fs"
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
                
# 200 "Parser.fsy"
                                          [_S1]
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 200 "Parser.fsy"
               : '_rnglr_type_formal_meta_list) 
# 1035 "Parser.fs"
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
                
# 203 "Parser.fsy"
                                             Some _S1
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 203 "Parser.fsy"
               : '_rnglr_type_param_opt) 
# 1055 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            _rnglr_cycle_res := (
              
# 203 "Parser.fsy"
                           None 
                )::!_rnglr_cycle_res
            !_rnglr_cycle_res
          )
            )
# 203 "Parser.fsy"
               : '_rnglr_type_param_opt) 
# 1073 "Parser.fs"
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
                  
# 205 "Parser.fsy"
                                                         _S1::_S2
                    )::!_rnglr_cycle_res ) )
            !_rnglr_cycle_res
          )
            )
# 205 "Parser.fsy"
               : '_rnglr_type_param_list) 
# 1095 "Parser.fs"
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
               : '_rnglr_type_param_list) 
# 1113 "Parser.fs"
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
                    
# 207 "Parser.fsy"
                                                                    Some _S2
                      )::!_rnglr_cycle_res ) ) )
            !_rnglr_cycle_res
          )
            )
# 207 "Parser.fsy"
               : '_rnglr_type_weight_opt) 
# 1137 "Parser.fs"
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
               : '_rnglr_type_weight_opt) 
# 1155 "Parser.fs"
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
                  
# 209 "Parser.fsy"
                                                        PAlt (_S1,_S2)
                    )::!_rnglr_cycle_res ) )
            !_rnglr_cycle_res
          )
            )
# 209 "Parser.fsy"
               : '_rnglr_type_alts) 
# 1177 "Parser.fs"
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
                
# 209 "Parser.fsy"
                            _S1 
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 209 "Parser.fsy"
               : '_rnglr_type_alts) 
# 1197 "Parser.fs"
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
                  
# 212 "Parser.fsy"
                                           _S2
                    )::!_rnglr_cycle_res ) )
            !_rnglr_cycle_res
          )
            )
# 211 "Parser.fsy"
               : '_rnglr_type_bar_seq_nlist) 
# 1219 "Parser.fs"
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
                    
# 211 "Parser.fsy"
                                                           PAlt(_S2,_S3) 
                      )::!_rnglr_cycle_res ) ) )
            !_rnglr_cycle_res
          )
            )
# 211 "Parser.fsy"
               : '_rnglr_type_bar_seq_nlist) 
# 1243 "Parser.fs"
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
                
# 214 "Parser.fsy"
                                                _S1
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 214 "Parser.fsy"
               : '_rnglr_type_seq) 
# 1263 "Parser.fs"
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
                
# 214 "Parser.fsy"
                                 _S1
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 214 "Parser.fsy"
               : '_rnglr_type_seq) 
# 1283 "Parser.fs"
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
                
# 217 "Parser.fsy"
                                     PSeq([], Some _S1, None) 
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 216 "Parser.fsy"
               : '_rnglr_type_no_lbl_seq) 
# 1303 "Parser.fs"
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
                    
# 216 "Parser.fsy"
                                                                    PSeq (_S1::_S2, _S3, None)
                      )::!_rnglr_cycle_res ) ) )
            !_rnglr_cycle_res
          )
            )
# 216 "Parser.fsy"
               : '_rnglr_type_no_lbl_seq) 
# 1327 "Parser.fs"
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
                        
# 219 "Parser.fsy"
                                                                             makeNewSeq _S4 _S1 _S2
                          )::!_rnglr_cycle_res ) ) ) ) )
            !_rnglr_cycle_res
          )
            )
# 219 "Parser.fsy"
               : '_rnglr_type_lbl_seq) 
# 1355 "Parser.fs"
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
                  
# 221 "Parser.fsy"
                                                                  _S1::_S2
                    )::!_rnglr_cycle_res ) )
            !_rnglr_cycle_res
          )
            )
# 221 "Parser.fsy"
               : '_rnglr_type_seq_elem_list) 
# 1377 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            _rnglr_cycle_res := (
              
# 221 "Parser.fsy"
                               [] 
                )::!_rnglr_cycle_res
            !_rnglr_cycle_res
          )
            )
# 221 "Parser.fsy"
               : '_rnglr_type_seq_elem_list) 
# 1395 "Parser.fs"
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
                    
# 223 "Parser.fsy"
                                                            {_S2 with checker = _S3; omit = _S1 }
                      )::!_rnglr_cycle_res ) ) )
            !_rnglr_cycle_res
          )
            )
# 223 "Parser.fsy"
               : '_rnglr_type_seq_elem) 
# 1419 "Parser.fs"
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
                
# 225 "Parser.fsy"
                                              true 
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 225 "Parser.fsy"
               : '_rnglr_type_omit_opt) 
# 1439 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            _rnglr_cycle_res := (
              
# 225 "Parser.fsy"
                          false 
                )::!_rnglr_cycle_res
            !_rnglr_cycle_res
          )
            )
# 225 "Parser.fsy"
               : '_rnglr_type_omit_opt) 
# 1457 "Parser.fs"
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
                
# 227 "Parser.fsy"
                                                  true
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 227 "Parser.fsy"
               : '_rnglr_type_semi_opt) 
# 1477 "Parser.fs"
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
               : '_rnglr_type_semi_opt) 
# 1495 "Parser.fs"
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
                
# 229 "Parser.fsy"
                                                      Some _S1 
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 229 "Parser.fsy"
               : '_rnglr_type_predicate_opt) 
# 1515 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            _rnglr_cycle_res := (
              
# 229 "Parser.fsy"
                               None 
                )::!_rnglr_cycle_res
            !_rnglr_cycle_res
          )
            )
# 229 "Parser.fsy"
               : '_rnglr_type_predicate_opt) 
# 1533 "Parser.fs"
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
                
# 232 "Parser.fsy"
                                         createSeqElem None false _S1 None      
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 231 "Parser.fsy"
               : '_rnglr_type_bound) 
# 1553 "Parser.fs"
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
                    
# 231 "Parser.fsy"
                                             createSeqElem (Some _S1) false _S3 None 
                      )::!_rnglr_cycle_res ) ) )
            !_rnglr_cycle_res
          )
            )
# 231 "Parser.fsy"
               : '_rnglr_type_bound) 
# 1577 "Parser.fs"
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
                
# 234 "Parser.fsy"
                                            _S1
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 234 "Parser.fsy"
               : '_rnglr_type_patt) 
# 1597 "Parser.fs"
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
                              _S1
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 234 "Parser.fsy"
               : '_rnglr_type_patt) 
# 1617 "Parser.fs"
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
                
# 243 "Parser.fsy"
                                            PLiteral _S1
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 236 "Parser.fsy"
               : '_rnglr_type_prim) 
# 1637 "Parser.fs"
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
                
# 242 "Parser.fsy"
                                            _S1
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 236 "Parser.fsy"
               : '_rnglr_type_prim) 
# 1657 "Parser.fs"
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
                
# 241 "Parser.fsy"
                                            _S1
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 236 "Parser.fsy"
               : '_rnglr_type_prim) 
# 1677 "Parser.fs"
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
                    
# 240 "Parser.fsy"
                                                _S2
                      )::!_rnglr_cycle_res ) ) )
            !_rnglr_cycle_res
          )
            )
# 236 "Parser.fsy"
               : '_rnglr_type_prim) 
# 1701 "Parser.fs"
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
                    
# 239 "Parser.fsy"
                                                POpt _S2
                      )::!_rnglr_cycle_res ) ) )
            !_rnglr_cycle_res
          )
            )
# 236 "Parser.fsy"
               : '_rnglr_type_prim) 
# 1725 "Parser.fs"
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
                  
# 238 "Parser.fsy"
                                              POpt _S1
                    )::!_rnglr_cycle_res ) )
            !_rnglr_cycle_res
          )
            )
# 236 "Parser.fsy"
               : '_rnglr_type_prim) 
# 1747 "Parser.fs"
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
                  
# 237 "Parser.fsy"
                                              PSome _S1
                    )::!_rnglr_cycle_res ) )
            !_rnglr_cycle_res
          )
            )
# 236 "Parser.fsy"
               : '_rnglr_type_prim) 
# 1769 "Parser.fs"
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
                  
# 236 "Parser.fsy"
                                              PMany _S1
                    )::!_rnglr_cycle_res ) )
            !_rnglr_cycle_res
          )
            )
# 236 "Parser.fsy"
               : '_rnglr_type_prim) 
# 1791 "Parser.fs"
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
                
# 245 "Parser.fsy"
                                  _S1
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 245 "Parser.fsy"
               : '_rnglr_type_meta_param) 
# 1811 "Parser.fs"
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
                  
# 248 "Parser.fsy"
                                                       _S1 :: _S2
                    )::!_rnglr_cycle_res ) )
            !_rnglr_cycle_res
          )
            )
# 247 "Parser.fsy"
               : '_rnglr_type_meta_params) 
# 1833 "Parser.fs"
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
                
# 247 "Parser.fsy"
                                         [_S1]
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 247 "Parser.fsy"
               : '_rnglr_type_meta_params) 
# 1853 "Parser.fs"
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
                    
# 250 "Parser.fsy"
                                                                       Some _S2
                      )::!_rnglr_cycle_res ) ) )
            !_rnglr_cycle_res
          )
            )
# 250 "Parser.fsy"
               : '_rnglr_type_meta_param_opt) 
# 1877 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            _rnglr_cycle_res := (
              
# 250 "Parser.fsy"
                                None 
                )::!_rnglr_cycle_res
            !_rnglr_cycle_res
          )
            )
# 250 "Parser.fsy"
               : '_rnglr_type_meta_param_opt) 
# 1895 "Parser.fs"
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
                    
# 254 "Parser.fsy"
                            match _S2 with
                            | None -> PRef  (_S1, _S3)
                            | Some x -> PMetaRef (_S1,_S3,x)
                          
                      )::!_rnglr_cycle_res ) ) )
            !_rnglr_cycle_res
          )
            )
# 252 "Parser.fsy"
               : '_rnglr_type_call) 
# 1922 "Parser.fs"
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
                
# 252 "Parser.fsy"
                              PToken _S1
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 252 "Parser.fsy"
               : '_rnglr_type_call) 
# 1942 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            (match ((unbox _rnglr_children.[0]) : Token) with SET _rnglr_val -> [_rnglr_val] | a -> failwith "SET expected, but %A found" a )
             |> List.iter (fun (_) -> 
              ((unbox _rnglr_children.[1]) : '_rnglr_type_option_params) 
               |> List.iter (fun (_S2) -> 
                _rnglr_cycle_res := (
                  
# 259 "Parser.fsy"
                                                             Some _S2 
                    )::!_rnglr_cycle_res ) )
            !_rnglr_cycle_res
          )
            )
# 259 "Parser.fsy"
               : '_rnglr_type_option_opt) 
# 1964 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            _rnglr_cycle_res := (
              
# 259 "Parser.fsy"
                            None 
                )::!_rnglr_cycle_res
            !_rnglr_cycle_res
          )
            )
# 259 "Parser.fsy"
               : '_rnglr_type_option_opt) 
# 1982 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            ((unbox _rnglr_children.[0]) : '_rnglr_type_option_param) 
             |> List.iter (fun (_S1) -> 
              (match ((unbox _rnglr_children.[1]) : Token) with COMMA _rnglr_val -> [_rnglr_val] | a -> failwith "COMMA expected, but %A found" a )
               |> List.iter (fun (_) -> 
                ((unbox _rnglr_children.[2]) : '_rnglr_type_option_params) 
                 |> List.iter (fun (_S3) -> 
                  _rnglr_cycle_res := (
                    
# 262 "Parser.fsy"
                                                                     (_S3).Add _S1
                      )::!_rnglr_cycle_res ) ) )
            !_rnglr_cycle_res
          )
            )
# 261 "Parser.fsy"
               : '_rnglr_type_option_params) 
# 2006 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            ((unbox _rnglr_children.[0]) : '_rnglr_type_option_param) 
             |> List.iter (fun (_S1) -> 
              _rnglr_cycle_res := (
                
# 261 "Parser.fsy"
                                              Map.empty.Add _S1
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 261 "Parser.fsy"
               : '_rnglr_type_option_params) 
# 2026 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            (match ((unbox _rnglr_children.[0]) : Token) with LIDENT _rnglr_val -> [_rnglr_val] | a -> failwith "LIDENT expected, but %A found" a )
             |> List.iter (fun (_S1) -> 
              (match ((unbox _rnglr_children.[1]) : Token) with EQUAL _rnglr_val -> [_rnglr_val] | a -> failwith "EQUAL expected, but %A found" a )
               |> List.iter (fun (_) -> 
                (match ((unbox _rnglr_children.[2]) : Token) with STRING _rnglr_val -> [_rnglr_val] | a -> failwith "STRING expected, but %A found" a )
                 |> List.iter (fun (_S3) -> 
                  _rnglr_cycle_res := (
                    
# 264 "Parser.fsy"
                                                        Source.toString _S1, Source.toString _S3 
                      )::!_rnglr_cycle_res ) ) )
            !_rnglr_cycle_res
          )
            )
# 264 "Parser.fsy"
               : '_rnglr_type_option_param) 
# 2050 "Parser.fs"
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
                
# 266 "Parser.fsy"
                                                
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 266 "Parser.fsy"
               : '_rnglr_type_tada_rule) 
# 2070 "Parser.fs"
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
                
# 266 "Parser.fsy"
                                       
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 266 "Parser.fsy"
               : '_rnglr_type_tada_rule) 
# 2090 "Parser.fs"
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
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_includes)   ) |> List.concat));
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
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_option_opt)   ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_option_param)   ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_option_params)   ) |> List.concat));
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
