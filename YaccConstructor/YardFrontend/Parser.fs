
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
open Yard.Core.IL.Production
 
type Range = struct
    val Start: Lexing.Position
    val End: Lexing.Position

    new (start,end_) = {Start = start; End = end_}
end

exception Parse_error
let FrontendsManager = Yard.Core.FrontendsManager.FrontendsManager()  
let currentFilename = ref ""
let o2l = function Some x -> [x] | None -> []
let getList = function Some x -> x | None -> []

let joinMaps (p:Map<'a,'b>) (q:Map<'a,'b>) = 
    Map(Seq.concat [ (Map.toSeq p) ; (Map.toSeq q) ])

let makeNewSeq seq (lbl:Source.t) = 
    match seq with 
    | PSeq(els,ac,_) ->PSeq (els,ac,Some {label=lbl.text; weight=None})
    | x -> x

let missing name = System.Console.WriteLine("Missing " + name)
let createSeqElem bnd omitted r check =
    { binding = bnd; omit = omitted; rule = r; checker = check }

let parseRules (filename:string) : Definition.t<Source.t, Source.t> =
    let ext = filename.Substring(filename.LastIndexOf(".") + 1)
    let frontend = FrontendsManager.GetByExtension ext
    let userDefs =
        let args = (!currentFilename).Trim().Split('%') in
        if args.Length = 2
        then args.[1]
        else ""
    let sameDirFilename = System.IO.Path.Combine(System.IO.Path.GetDirectoryName !currentFilename, filename) in

    match FrontendsManager.GetByExtension ext with
    | Some(frontend) -> 
        frontend.Value.ParseGrammar (sameDirFilename + "%" + userDefs)
    | None -> 
        failwith (sprintf "Not supported extension %s in file %s" ext filename )

# 74 "Parser.fs"
type Token =
    | ACTION of Source.t
    | BAR of Source.t
    | COLON of Source.t
    | COMMA of Source.t
    | DGREAT of Source.t
    | DLABEL of Source.t
    | DLESS of Source.t
    | EOF of Source.t
    | EQUAL of Source.t
    | ERROR of Source.t
    | INCLUDE of Source.t
    | LIDENT of Source.t
    | LPAREN of Source.t
    | MINUS of Source.t
    | PARAM of Source.t
    | PATTERN of Source.t
    | PLUS of Source.t
    | PREDICATE of Source.t
    | QUESTION of Source.t
    | RNGLR_EOF of Source.t
    | RPAREN of Source.t
    | SEMICOLON of Source.t
    | SET of Source.t
    | SHARPLINE of Source.t
    | STAR of Source.t
    | START_RULE_SIGN of Source.t
    | STRING of Source.t
    | UIDENT of Source.t

let numToString = function
    | 0 -> "action_opt"
    | 1 -> "alts"
    | 2 -> "bar_seq_nlist"
    | 3 -> "bound"
    | 4 -> "call"
    | 5 -> "file"
    | 6 -> "foot_opt"
    | 7 -> "formal_meta_list"
    | 8 -> "formal_meta_param_opt"
    | 9 -> "lbl_seq"
    | 10 -> "meta_param"
    | 11 -> "meta_param_opt"
    | 12 -> "meta_params"
    | 13 -> "no_lbl_seq"
    | 14 -> "omit_opt"
    | 15 -> "option_opt"
    | 16 -> "option_param"
    | 17 -> "option_params"
    | 18 -> "param_list"
    | 19 -> "param_opt"
    | 20 -> "patt"
    | 21 -> "predicate_opt"
    | 22 -> "prim"
    | 23 -> "rule"
    | 24 -> "rule_nlist"
    | 25 -> "semi_opt"
    | 26 -> "seq"
    | 27 -> "seq_elem"
    | 28 -> "seq_elem_list"
    | 29 -> "start_rule_sign_opt"
    | 30 -> "tada_rule"
    | 31 -> "yard_start_rule"
    | 32 -> "ACTION"
    | 33 -> "BAR"
    | 34 -> "COLON"
    | 35 -> "COMMA"
    | 36 -> "DGREAT"
    | 37 -> "DLABEL"
    | 38 -> "DLESS"
    | 39 -> "EOF"
    | 40 -> "EQUAL"
    | 41 -> "ERROR"
    | 42 -> "INCLUDE"
    | 43 -> "LIDENT"
    | 44 -> "LPAREN"
    | 45 -> "MINUS"
    | 46 -> "PARAM"
    | 47 -> "PATTERN"
    | 48 -> "PLUS"
    | 49 -> "PREDICATE"
    | 50 -> "QUESTION"
    | 51 -> "RNGLR_EOF"
    | 52 -> "RPAREN"
    | 53 -> "SEMICOLON"
    | 54 -> "SET"
    | 55 -> "SHARPLINE"
    | 56 -> "STAR"
    | 57 -> "START_RULE_SIGN"
    | 58 -> "STRING"
    | 59 -> "UIDENT"
    | 60 -> "error"
    | _ -> ""
let tokenToNumber = function
    | ACTION _ -> 32
    | BAR _ -> 33
    | COLON _ -> 34
    | COMMA _ -> 35
    | DGREAT _ -> 36
    | DLABEL _ -> 37
    | DLESS _ -> 38
    | EOF _ -> 39
    | EQUAL _ -> 40
    | ERROR _ -> 41
    | INCLUDE _ -> 42
    | LIDENT _ -> 43
    | LPAREN _ -> 44
    | MINUS _ -> 45
    | PARAM _ -> 46
    | PATTERN _ -> 47
    | PLUS _ -> 48
    | PREDICATE _ -> 49
    | QUESTION _ -> 50
    | RNGLR_EOF _ -> 51
    | RPAREN _ -> 52
    | SEMICOLON _ -> 53
    | SET _ -> 54
    | SHARPLINE _ -> 55
    | STAR _ -> 56
    | START_RULE_SIGN _ -> 57
    | STRING _ -> 58
    | UIDENT _ -> 59

let mutable private cur = 0
let leftSide = [|5; 31; 0; 0; 6; 6; 24; 24; 24; 23; 29; 29; 8; 8; 7; 7; 19; 19; 18; 18; 1; 1; 2; 2; 26; 26; 13; 13; 9; 28; 28; 27; 14; 14; 25; 25; 21; 21; 3; 3; 20; 20; 22; 22; 22; 22; 22; 22; 22; 10; 12; 12; 11; 11; 4; 4; 15; 15; 17; 17; 16; 30; 30|]
let private rules = [|0; 24; 6; 39; 5; 32; 53; 32; 42; 58; 24; 23; 25; 24; 29; 43; 8; 18; 34; 15; 1; 57; 38; 7; 36; 43; 7; 43; 46; 46; 18; 26; 2; 26; 33; 26; 33; 26; 2; 9; 13; 32; 27; 28; 0; 37; 44; 13; 52; 27; 28; 14; 3; 21; 45; 53; 49; 22; 20; 40; 22; 47; 43; 58; 9; 4; 44; 1; 52; 22; 50; 22; 48; 22; 56; 22; 10; 12; 10; 38; 12; 36; 43; 11; 19; 59; 54; 17; 16; 35; 17; 16; 43; 40; 58; 39; 55|]
let private rulesStart = [|0; 4; 5; 6; 6; 8; 8; 8; 11; 14; 21; 22; 22; 25; 25; 27; 28; 29; 29; 31; 31; 33; 34; 36; 39; 40; 41; 42; 45; 49; 51; 51; 54; 55; 55; 56; 56; 57; 57; 58; 61; 62; 63; 64; 65; 66; 69; 71; 73; 75; 76; 78; 79; 82; 82; 85; 86; 88; 88; 91; 92; 95; 96; 97|]
let startRule = 1

let acceptEmptyInput = false

let defaultAstToDot =
    (fun (tree : Yard.Generators.RNGLR.AST.Tree<Token>) -> tree.AstToDot numToString tokenToNumber leftSide)

let private lists_gotos = [|1; 86; 54; 2; 81; 5; 76; 79; 3; 80; 4; 6; 7; 71; 8; 69; 9; 10; 61; 11; 12; 13; 14; 43; 48; 55; 26; 51; 15; 18; 19; 20; 58; 59; 40; 60; 56; 57; 16; 17; 21; 22; 30; 23; 24; 25; 27; 28; 29; 31; 34; 32; 33; 35; 38; 37; 36; 39; 41; 42; 44; 45; 46; 47; 49; 52; 50; 53; 62; 68; 65; 63; 64; 66; 67; 70; 72; 74; 73; 75; 77; 78; 82; 84; 83; 85|]
let private small_gotos =
        [|3; 0; 327681; 2097154; 65541; 1507331; 1572868; 1900549; 2752518; 3735559; 131074; 1638408; 3473417; 196613; 1507331; 1572874; 1900549; 2752518; 3735559; 327681; 2818059; 393218; 524300; 2490381; 458754; 1179662; 3014671; 524289; 2228240; 589826; 983057; 3538962; 655369; 65555; 589844; 851989; 917526; 1703959; 1769496; 2097177; 2424858; 2949147; 917515; 196636; 262173; 589854; 1310751; 1441824; 2424858; 2818081; 2883618; 3080227; 3801124; 3866661; 983042; 1376294; 3211303; 1310721; 2621480; 1376264; 262173; 589854; 1441833; 2424858; 2818090; 2883618; 3801124; 3866661; 1441795; 3145771; 3276844; 3670061; 1703937; 2883630; 1769477; 852015; 917526; 1769496; 2097177; 2949147; 1835009; 3407920; 1966082; 720945; 2490418; 2031618; 1245235; 3014708; 2228234; 262173; 589854; 655413; 786486; 1441847; 2424858; 2818090; 2883618; 3801124; 3866661; 2293770; 262173; 589854; 655413; 786488; 1441847; 2424858; 2818090; 2883618; 3801124; 3866661; 2424835; 3145771; 3276844; 3670061; 2490369; 2359353; 2621449; 65594; 589844; 851989; 917526; 1703959; 1769496; 2097177; 2424858; 2949147; 2686977; 3407931; 2818050; 131132; 2162749; 2949128; 589844; 851989; 917526; 1703998; 1769496; 2097177; 2424858; 2949147; 3014658; 131135; 2162749; 3145732; 917526; 1769536; 1835073; 2949147; 3211268; 917526; 1769536; 1835074; 2949147; 3407874; 67; 2097154; 3801091; 3145771; 3276844; 3670061; 3866626; 720945; 2490418; 3997699; 1048644; 1114181; 2818118; 4063233; 2293831; 4128771; 1048644; 1114184; 2818118; 4259841; 2621513; 4325377; 3801162; 4521986; 1179723; 3014671; 4653058; 458828; 2818125; 4718593; 2359374; 4849666; 458831; 2818125; 4980737; 3801168; 5046277; 1507331; 1572945; 1900549; 2752518; 3735559; 5308418; 393298; 3473491; 5373953; 2555988; 5505025; 2097237|]
let gotos = Array.zeroCreate 87
for i = 0 to 86 do
        gotos.[i] <- Array.zeroCreate 60
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
let private lists_reduces = [|[|8,1|]; [|8,2|]; [|8,3|]; [|9,7|]; [|24,1|]; [|25,1|]; [|31,2|]; [|31,3|]; [|36,1|]; [|44,1|]; [|43,1|]; [|39,3|]; [|47,2|]; [|46,2|]; [|48,2|]; [|28,4|]; [|54,1|]; [|54,2|]; [|54,3|]; [|16,1|]; [|51,1|]; [|50,2|]; [|49,1|]; [|52,3|]; [|45,3|]; [|21,1|]; [|20,2|]; [|22,2|]; [|23,3|]; [|27,1|]; [|29,1|]; [|29,2|]; [|32,1|]; [|27,2|]; [|27,3|]; [|2,1|]; [|26,1|]; [|42,1|]; [|55,1|]; [|38,1|]; [|41,1|]; [|40,1|]; [|59,1|]; [|58,3|]; [|60,3|]; [|56,2|]; [|18,1|]; [|18,2|]; [|12,3|]; [|15,1|]; [|14,2|]; [|7,2|]; [|7,3|]; [|10,1|]; [|34,1|]; [|0,4|]; [|4,2|]|]
let private small_reduces =
        [|131074; 2555904; 3473408; 196610; 2555905; 3473409; 262146; 2555906; 3473410; 720901; 2555907; 2752515; 2818051; 3473411; 3735555; 786439; 2162692; 2555908; 2752516; 2818052; 3407876; 3473412; 3735556; 851975; 2162693; 2555909; 2752517; 2818053; 3407877; 3473413; 3735557; 983054; 2097158; 2162694; 2424838; 2555910; 2752518; 2818054; 2883590; 2949126; 3080198; 3407878; 3473414; 3735558; 3801094; 3866630; 1048590; 2097159; 2162695; 2424839; 2555911; 2752519; 2818055; 2883591; 2949127; 3080199; 3407879; 3473415; 3735559; 3801095; 3866631; 1114126; 2097160; 2162696; 2424840; 2555912; 2752520; 2818056; 2883592; 2949128; 3080200; 3407880; 3473416; 3735560; 3801096; 3866632; 1179667; 2097161; 2162697; 2359305; 2424841; 2555913; 2752521; 2818057; 2883593; 2949129; 3080201; 3145737; 3211273; 3276809; 3407881; 3473417; 3670025; 3735561; 3801097; 3866633; 1245203; 2097162; 2162698; 2359306; 2424842; 2555914; 2752522; 2818058; 2883594; 2949130; 3080202; 3145738; 3211274; 3276810; 3407882; 3473418; 3670026; 3735562; 3801098; 3866634; 1441807; 2097163; 2162699; 2424843; 2555915; 2752523; 2818059; 2883595; 2949131; 3080203; 3211275; 3407883; 3473419; 3735563; 3801099; 3866635; 1507347; 2097164; 2162700; 2359308; 2424844; 2555916; 2752524; 2818060; 2883596; 2949132; 3080204; 3145740; 3211276; 3276812; 3407884; 3473420; 3670028; 3735564; 3801100; 3866636; 1572883; 2097165; 2162701; 2359309; 2424845; 2555917; 2752525; 2818061; 2883597; 2949133; 3080205; 3145741; 3211277; 3276813; 3407885; 3473421; 3670029; 3735565; 3801101; 3866637; 1638419; 2097166; 2162702; 2359310; 2424846; 2555918; 2752526; 2818062; 2883598; 2949134; 3080206; 3145742; 3211278; 3276814; 3407886; 3473422; 3670030; 3735566; 3801102; 3866638; 1900563; 2097167; 2162703; 2359311; 2424847; 2555919; 2752527; 2818063; 2883599; 2949135; 3080207; 3145743; 3211279; 3276815; 3407887; 3473423; 3670031; 3735567; 3801103; 3866639; 1966099; 2097168; 2162704; 2359312; 2424848; 2555920; 2752528; 2818064; 2883600; 2949136; 3080208; 3145744; 3211280; 3276816; 3407888; 3473424; 3670032; 3735568; 3801104; 3866640; 2031635; 2097169; 2162705; 2359313; 2424849; 2555921; 2752529; 2818065; 2883601; 2949137; 3080209; 3145745; 3211281; 3276817; 3407889; 3473425; 3670033; 3735569; 3801105; 3866641; 2097171; 2097170; 2162706; 2359314; 2424850; 2555922; 2752530; 2818066; 2883602; 2949138; 3080210; 3145746; 3211282; 3276818; 3407890; 3473426; 3670034; 3735570; 3801106; 3866642; 2162707; 2097171; 2162707; 2359315; 2424851; 2555923; 2752531; 2818067; 2883603; 2949139; 3080211; 3145747; 3211283; 3276819; 3407891; 3473427; 3670035; 3735571; 3801107; 3866643; 2293761; 2359316; 2359297; 2359317; 2424838; 2359318; 2424854; 2818070; 2883606; 3801110; 3866646; 2555924; 2097175; 2162711; 2359319; 2424855; 2555927; 2752535; 2818071; 2883607; 2949143; 3014679; 3080215; 3145751; 3211287; 3276823; 3407895; 3473431; 3670039; 3735575; 3801111; 3866647; 2752531; 2097176; 2162712; 2359320; 2424856; 2555928; 2752536; 2818072; 2883608; 2949144; 3080216; 3145752; 3211288; 3276824; 3407896; 3473432; 3670040; 3735576; 3801112; 3866648; 2818054; 2555929; 2752537; 2818073; 3407897; 3473433; 3735577; 2883590; 2555930; 2752538; 2818074; 3407898; 3473434; 3735578; 3014662; 2555931; 2752539; 2818075; 3407899; 3473435; 3735579; 3080198; 2555932; 2752540; 2818076; 3407900; 3473436; 3735580; 3145735; 2162717; 2555933; 2752541; 2818077; 3407901; 3473437; 3735581; 3211272; 2097182; 2162718; 2555934; 2752542; 2818078; 3407902; 3473438; 3735582; 3276808; 2097183; 2162719; 2555935; 2752543; 2818079; 3407903; 3473439; 3735583; 3342342; 2424864; 2818080; 2883616; 3080224; 3801120; 3866656; 3407879; 2162721; 2555937; 2752545; 2818081; 3407905; 3473441; 3735585; 3473415; 2162722; 2555938; 2752546; 2818082; 3407906; 3473442; 3735586; 3538951; 2162723; 2555939; 2752547; 2818083; 3407907; 3473443; 3735587; 3604487; 2162724; 2555940; 2752548; 2818084; 3407908; 3473444; 3735588; 3670035; 2097189; 2162725; 2359333; 2424869; 2555941; 2752549; 2818085; 2883621; 2949157; 3080229; 3145765; 3211301; 3276837; 3407909; 3473445; 3670053; 3735589; 3801125; 3866661; 3735571; 2097190; 2162726; 2359334; 2424870; 2555942; 2752550; 2818086; 2883622; 2949158; 3080230; 3145766; 3211302; 3276838; 3407910; 3473446; 3670054; 3735590; 3801126; 3866662; 3801103; 2097191; 2162727; 2424871; 2555943; 2752551; 2818087; 2883623; 2949159; 3080231; 3211303; 3407911; 3473447; 3735591; 3801127; 3866663; 3866643; 2097168; 2162704; 2424848; 2555920; 2621480; 2752528; 2818064; 2883600; 2949136; 3080208; 3145744; 3211280; 3276816; 3407888; 3473424; 3670032; 3735568; 3801104; 3866640; 3932161; 2621481; 4063240; 2097194; 2424874; 2818090; 2883626; 2949162; 3080234; 3801130; 3866666; 4194312; 2097195; 2424875; 2818091; 2883627; 2949163; 3080235; 3801131; 3866667; 4390921; 2097196; 2293804; 2424876; 2818092; 2883628; 2949164; 3080236; 3801132; 3866668; 4456456; 2097197; 2424877; 2818093; 2883629; 2949165; 3080237; 3801133; 3866669; 4521985; 2228270; 4587521; 2228271; 4784130; 2228272; 3014704; 4849665; 2359345; 4915201; 2359346; 5046274; 2555955; 3473459; 5111810; 2555956; 3473460; 5177345; 2818101; 5242885; 2555958; 2752566; 2818102; 3473462; 3735606; 5439489; 3342391; 5570561; 2555960|]
let reduces = Array.zeroCreate 87
for i = 0 to 86 do
        reduces.[i] <- Array.zeroCreate 60
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
let private lists_zeroReduces = [|[|3|]; [|6|]; [|11|]; [|35|]; [|13|]; [|19|]; [|57|]; [|33|]; [|37|]; [|53|]; [|17|]; [|30|]; [|33; 30|]; [|5|]|]
let private small_zeroReduces =
        [|5; 2555904; 2752512; 2818048; 3473408; 3735552; 65539; 2555905; 2818050; 3473409; 131077; 2555907; 2752515; 2818051; 3473411; 3735555; 196611; 2555905; 2818050; 3473409; 393218; 2228228; 3014660; 458753; 2228229; 589832; 2097158; 2424838; 2818054; 2883590; 2949126; 3080198; 3801094; 3866630; 655366; 2424839; 2818055; 2883591; 3080199; 3801095; 3866631; 983054; 2097160; 2162696; 2424840; 2555912; 2752520; 2818056; 2883592; 2949128; 3080200; 3407880; 3473416; 3735560; 3801096; 3866632; 1769478; 2424839; 2818055; 2883591; 3080199; 3801095; 3866631; 1966100; 2097161; 2162697; 2359305; 2424841; 2555913; 2752521; 2818057; 2883593; 2949129; 3014665; 3080201; 3145737; 3211273; 3276809; 3407881; 3473417; 3670025; 3735561; 3801097; 3866633; 2031635; 2097162; 2162698; 2359306; 2424842; 2555914; 2752522; 2818058; 2883594; 2949130; 3080202; 3145738; 3211274; 3276810; 3407882; 3473418; 3670026; 3735562; 3801098; 3866634; 2621446; 2424839; 2818055; 2883591; 3080199; 3801095; 3866631; 2949126; 2424839; 2818055; 2883591; 3080199; 3801095; 3866631; 3145741; 2097163; 2162699; 2424839; 2555915; 2752523; 2818060; 2883591; 3080199; 3407883; 3473419; 3735563; 3801095; 3866631; 3211277; 2097163; 2162699; 2424839; 2555915; 2752523; 2818060; 2883591; 3080199; 3407883; 3473419; 3735563; 3801095; 3866631; 3407879; 2162688; 2555904; 2752512; 2818048; 3407872; 3473408; 3735552; 3866643; 2097161; 2162697; 2424841; 2555913; 2752521; 2818057; 2883593; 2949129; 3014665; 3080201; 3145737; 3211273; 3276809; 3407881; 3473417; 3670025; 3735561; 3801097; 3866633; 4521985; 2228229; 5046275; 2555905; 2818050; 3473409; 5308417; 2555917|]
let zeroReduces = Array.zeroCreate 87
for i = 0 to 86 do
        zeroReduces.[i] <- Array.zeroCreate 60
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
let private small_acc = [86]
let private accStates = Array.zeroCreate 87
for i = 0 to 86 do
        accStates.[i] <- List.exists ((=) i) small_acc
let eofIndex = 51
let errorNIndex = 60
let errorTIndex = 41
let private parserSource = new ParserSource<Token> (gotos, reduces, zeroReduces, accStates, rules, rulesStart, leftSide, startRule, eofIndex, tokenToNumber, acceptEmptyInput, numToString, errorNIndex, errorTIndex)
let buildAst : (seq<Token> -> ParseResult<Token>) =
    buildAst<Token> parserSource

let _rnglr_epsilons : Tree<Token>[] = [|new Tree<_>(null,box (new AST(new Family(3, new Nodes([||])), null)), null); null; null; null; null; null; new Tree<_>(null,box (new AST(new Family(5, new Nodes([||])), null)), null); null; new Tree<_>(null,box (new AST(new Family(13, new Nodes([||])), null)), null); null; null; new Tree<_>(null,box (new AST(new Family(53, new Nodes([||])), null)), null); null; null; new Tree<_>(null,box (new AST(new Family(33, new Nodes([||])), null)), null); new Tree<_>(null,box (new AST(new Family(57, new Nodes([||])), null)), null); null; null; new Tree<_>(null,box (new AST(new Family(19, new Nodes([||])), null)), null); new Tree<_>(null,box (new AST(new Family(17, new Nodes([||])), null)), null); null; new Tree<_>(null,box (new AST(new Family(37, new Nodes([||])), null)), null); null; null; new Tree<_>(null,box (new AST(new Family(6, new Nodes([||])), null)), null); new Tree<_>(null,box (new AST(new Family(35, new Nodes([||])), null)), null); null; null; new Tree<_>(null,box (new AST(new Family(30, new Nodes([||])), null)), null); new Tree<_>(null,box (new AST(new Family(11, new Nodes([||])), null)), null); null; null|]
let _rnglr_filtered_epsilons : Tree<Token>[] = [|new Tree<_>(null,box (new AST(new Family(3, new Nodes([||])), null)), null); null; null; null; null; null; new Tree<_>(null,box (new AST(new Family(5, new Nodes([||])), null)), null); null; new Tree<_>(null,box (new AST(new Family(13, new Nodes([||])), null)), null); null; null; new Tree<_>(null,box (new AST(new Family(53, new Nodes([||])), null)), null); null; null; new Tree<_>(null,box (new AST(new Family(33, new Nodes([||])), null)), null); new Tree<_>(null,box (new AST(new Family(57, new Nodes([||])), null)), null); null; null; new Tree<_>(null,box (new AST(new Family(19, new Nodes([||])), null)), null); new Tree<_>(null,box (new AST(new Family(17, new Nodes([||])), null)), null); null; new Tree<_>(null,box (new AST(new Family(37, new Nodes([||])), null)), null); null; null; new Tree<_>(null,box (new AST(new Family(6, new Nodes([||])), null)), null); new Tree<_>(null,box (new AST(new Family(35, new Nodes([||])), null)), null); null; null; new Tree<_>(null,box (new AST(new Family(30, new Nodes([||])), null)), null); new Tree<_>(null,box (new AST(new Family(11, new Nodes([||])), null)), null); null; null|]
for x in _rnglr_filtered_epsilons do if x <> null then x.ChooseSingleAst()
let _rnglr_extra_array, _rnglr_rule_, _rnglr_concats = 
  (Array.zeroCreate 0 : array<'_rnglr_type_action_opt * '_rnglr_type_alts * '_rnglr_type_bar_seq_nlist * '_rnglr_type_bound * '_rnglr_type_call * '_rnglr_type_file * '_rnglr_type_foot_opt * '_rnglr_type_formal_meta_list * '_rnglr_type_formal_meta_param_opt * '_rnglr_type_lbl_seq * '_rnglr_type_meta_param * '_rnglr_type_meta_param_opt * '_rnglr_type_meta_params * '_rnglr_type_no_lbl_seq * '_rnglr_type_omit_opt * '_rnglr_type_option_opt * '_rnglr_type_option_param * '_rnglr_type_option_params * '_rnglr_type_param_list * '_rnglr_type_param_opt * '_rnglr_type_patt * '_rnglr_type_predicate_opt * '_rnglr_type_prim * '_rnglr_type_rule * '_rnglr_type_rule_nlist * '_rnglr_type_semi_opt * '_rnglr_type_seq * '_rnglr_type_seq_elem * '_rnglr_type_seq_elem_list * '_rnglr_type_start_rule_sign_opt * '_rnglr_type_tada_rule * '_rnglr_type_yard_start_rule>), 
  [|
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            ((unbox _rnglr_children.[0]) : '_rnglr_type_action_opt) 
             |> List.iter (fun (_S1) -> 
              ((unbox _rnglr_children.[1]) : '_rnglr_type_rule_nlist) 
               |> List.iter (fun (_S2) -> 
                ((unbox _rnglr_children.[2]) : '_rnglr_type_foot_opt) 
                 |> List.iter (fun (_S3) -> 
                  (match ((unbox _rnglr_children.[3]) : Token) with EOF _rnglr_val -> [_rnglr_val] | a -> failwith "EOF expected, but %A found" a )
                   |> List.iter (fun (_) -> 
                    _rnglr_cycle_res := (
                      

                       
                              { Definition.empty with
                                  Definition.info = { fileName = !currentFilename }
                                  Definition.head = _S1
                                  Definition.grammar = (fst _S2)
                                  Definition.foot = _S3
                                  Definition.options = (snd _S2)
                              }
                            
                        )::!_rnglr_cycle_res ) ) ) )
            !_rnglr_cycle_res
          )
            )
# 1 "Parser.fsy"
               : '_rnglr_type_file) 
# 305 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          ((unbox _rnglr_children.[0]) : '_rnglr_type_file) 
            )
# 1 "Parser.fsy"
               : '_rnglr_type_yard_start_rule) 
# 315 "Parser.fs"
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
                

                 Some _S1 
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 1 "Parser.fsy"
               : '_rnglr_type_action_opt) 
# 335 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            _rnglr_cycle_res := (
              

               None 
                )::!_rnglr_cycle_res
            !_rnglr_cycle_res
          )
            )
# 1 "Parser.fsy"
               : '_rnglr_type_action_opt) 
# 353 "Parser.fs"
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
                  

                   Some _S2 
                    )::!_rnglr_cycle_res ) )
            !_rnglr_cycle_res
          )
            )
# 1 "Parser.fsy"
               : '_rnglr_type_foot_opt) 
# 375 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            _rnglr_cycle_res := (
              

               None 
                )::!_rnglr_cycle_res
            !_rnglr_cycle_res
          )
            )
# 1 "Parser.fsy"
               : '_rnglr_type_foot_opt) 
# 393 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            _rnglr_cycle_res := (
              

               [], Map.empty 
                )::!_rnglr_cycle_res
            !_rnglr_cycle_res
          )
            )
# 1 "Parser.fsy"
               : '_rnglr_type_rule_nlist) 
# 411 "Parser.fs"
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
                ((unbox _rnglr_children.[2]) : '_rnglr_type_rule_nlist) 
                 |> List.iter (fun (_S3) -> 
                  _rnglr_cycle_res := (
                    

                     let def = parseRules _S2.text in
                              (def.grammar @ (fst _S3), joinMaps def.options (snd _S3)) 
                            
                      )::!_rnglr_cycle_res ) ) )
            !_rnglr_cycle_res
          )
            )
# 1 "Parser.fsy"
               : '_rnglr_type_rule_nlist) 
# 437 "Parser.fs"
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
                    

                     (fst _S1) :: (fst _S3), joinMaps (snd _S1) (snd _S3) 
                      )::!_rnglr_cycle_res ) ) )
            !_rnglr_cycle_res
          )
            )
# 1 "Parser.fsy"
               : '_rnglr_type_rule_nlist) 
# 461 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            ((unbox _rnglr_children.[0]) : '_rnglr_type_start_rule_sign_opt) 
             |> List.iter (fun (_S1) -> 
              (match ((unbox _rnglr_children.[1]) : Token) with LIDENT _rnglr_val -> [_rnglr_val] | a -> failwith "LIDENT expected, but %A found" a )
               |> List.iter (fun (_S2) -> 
                ((unbox _rnglr_children.[2]) : '_rnglr_type_formal_meta_param_opt) 
                 |> List.iter (fun (_S3) -> 
                  ((unbox _rnglr_children.[3]) : '_rnglr_type_param_list) 
                   |> List.iter (fun (_S4) -> 
                    (match ((unbox _rnglr_children.[4]) : Token) with COLON _rnglr_val -> [_rnglr_val] | a -> failwith "COLON expected, but %A found" a )
                     |> List.iter (fun (_) -> 
                      ((unbox _rnglr_children.[5]) : '_rnglr_type_option_opt) 
                       |> List.iter (fun (_S6) -> 
                        ((unbox _rnglr_children.[6]) : '_rnglr_type_alts) 
                         |> List.iter (fun (_S7) -> 
                          _rnglr_cycle_res := (
                            

                             
                                    let newRule = { Rule._public = _S1
                                                    Rule.name = _S2
                                                    Rule.metaArgs = getList _S3
                                                    Rule.body = _S7
                                                    Rule.args = _S4
                                                  }
                                    in
                                    match _S6 with
                                    | Some m -> newRule, Map.empty.Add (newRule, m)
                                    | None -> newRule, Map.empty
                                   
                              )::!_rnglr_cycle_res ) ) ) ) ) ) )
            !_rnglr_cycle_res
          )
            )
# 1 "Parser.fsy"
               : '_rnglr_type_rule) 
# 504 "Parser.fs"
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
                

                true
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 1 "Parser.fsy"
               : '_rnglr_type_start_rule_sign_opt) 
# 524 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            _rnglr_cycle_res := (
              

              false
                )::!_rnglr_cycle_res
            !_rnglr_cycle_res
          )
            )
# 1 "Parser.fsy"
               : '_rnglr_type_start_rule_sign_opt) 
# 542 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            (match ((unbox _rnglr_children.[0]) : Token) with DLESS _rnglr_val -> [_rnglr_val] | a -> failwith "DLESS expected, but %A found" a )
             |> List.iter (fun (_) -> 
              ((unbox _rnglr_children.[1]) : '_rnglr_type_formal_meta_list) 
               |> List.iter (fun (_S2) -> 
                (match ((unbox _rnglr_children.[2]) : Token) with DGREAT _rnglr_val -> [_rnglr_val] | a -> failwith "DGREAT expected, but %A found" a )
                 |> List.iter (fun (_) -> 
                  _rnglr_cycle_res := (
                    

                    Some _S2
                      )::!_rnglr_cycle_res ) ) )
            !_rnglr_cycle_res
          )
            )
# 1 "Parser.fsy"
               : '_rnglr_type_formal_meta_param_opt) 
# 566 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            _rnglr_cycle_res := (
              

               None 
                )::!_rnglr_cycle_res
            !_rnglr_cycle_res
          )
            )
# 1 "Parser.fsy"
               : '_rnglr_type_formal_meta_param_opt) 
# 584 "Parser.fs"
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
                  

                  _S1::_S2
                    )::!_rnglr_cycle_res ) )
            !_rnglr_cycle_res
          )
            )
# 1 "Parser.fsy"
               : '_rnglr_type_formal_meta_list) 
# 606 "Parser.fs"
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
                

                [_S1]
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 1 "Parser.fsy"
               : '_rnglr_type_formal_meta_list) 
# 626 "Parser.fs"
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
                

                Some _S1
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 1 "Parser.fsy"
               : '_rnglr_type_param_opt) 
# 646 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            _rnglr_cycle_res := (
              

               None 
                )::!_rnglr_cycle_res
            !_rnglr_cycle_res
          )
            )
# 1 "Parser.fsy"
               : '_rnglr_type_param_opt) 
# 664 "Parser.fs"
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
                  

                  _S1::_S2
                    )::!_rnglr_cycle_res ) )
            !_rnglr_cycle_res
          )
            )
# 1 "Parser.fsy"
               : '_rnglr_type_param_list) 
# 686 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            _rnglr_cycle_res := (
              

               [] 
                )::!_rnglr_cycle_res
            !_rnglr_cycle_res
          )
            )
# 1 "Parser.fsy"
               : '_rnglr_type_param_list) 
# 704 "Parser.fs"
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
                  

                  PAlt (_S1,_S2)
                    )::!_rnglr_cycle_res ) )
            !_rnglr_cycle_res
          )
            )
# 1 "Parser.fsy"
               : '_rnglr_type_alts) 
# 726 "Parser.fs"
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
                

                 _S1 
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 1 "Parser.fsy"
               : '_rnglr_type_alts) 
# 746 "Parser.fs"
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
                  

                  _S2
                    )::!_rnglr_cycle_res ) )
            !_rnglr_cycle_res
          )
            )
# 1 "Parser.fsy"
               : '_rnglr_type_bar_seq_nlist) 
# 768 "Parser.fs"
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
                    

                    PAlt(_S2,_S3) 
                      )::!_rnglr_cycle_res ) ) )
            !_rnglr_cycle_res
          )
            )
# 1 "Parser.fsy"
               : '_rnglr_type_bar_seq_nlist) 
# 792 "Parser.fs"
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
                

                _S1
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 1 "Parser.fsy"
               : '_rnglr_type_seq) 
# 812 "Parser.fs"
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
                

                _S1
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 1 "Parser.fsy"
               : '_rnglr_type_seq) 
# 832 "Parser.fs"
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
                

                 PSeq([], Some _S1, None) 
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 1 "Parser.fsy"
               : '_rnglr_type_no_lbl_seq) 
# 852 "Parser.fs"
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
                    

                     PSeq (_S1::_S2, _S3, None)
                      )::!_rnglr_cycle_res ) ) )
            !_rnglr_cycle_res
          )
            )
# 1 "Parser.fsy"
               : '_rnglr_type_no_lbl_seq) 
# 876 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            (match ((unbox _rnglr_children.[0]) : Token) with DLABEL _rnglr_val -> [_rnglr_val] | a -> failwith "DLABEL expected, but %A found" a )
             |> List.iter (fun (_S1) -> 
              (match ((unbox _rnglr_children.[1]) : Token) with LPAREN _rnglr_val -> [_rnglr_val] | a -> failwith "LPAREN expected, but %A found" a )
               |> List.iter (fun (_) -> 
                ((unbox _rnglr_children.[2]) : '_rnglr_type_no_lbl_seq) 
                 |> List.iter (fun (_S3) -> 
                  (match ((unbox _rnglr_children.[3]) : Token) with RPAREN _rnglr_val -> [_rnglr_val] | a -> failwith "RPAREN expected, but %A found" a )
                   |> List.iter (fun (_) -> 
                    _rnglr_cycle_res := (
                      

                      makeNewSeq _S3 _S1
                        )::!_rnglr_cycle_res ) ) ) )
            !_rnglr_cycle_res
          )
            )
# 1 "Parser.fsy"
               : '_rnglr_type_lbl_seq) 
# 902 "Parser.fs"
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
                  

                  _S1::_S2
                    )::!_rnglr_cycle_res ) )
            !_rnglr_cycle_res
          )
            )
# 1 "Parser.fsy"
               : '_rnglr_type_seq_elem_list) 
# 924 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            _rnglr_cycle_res := (
              

               [] 
                )::!_rnglr_cycle_res
            !_rnglr_cycle_res
          )
            )
# 1 "Parser.fsy"
               : '_rnglr_type_seq_elem_list) 
# 942 "Parser.fs"
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
                    

                    {_S2 with checker = _S3; omit = _S1 }
                      )::!_rnglr_cycle_res ) ) )
            !_rnglr_cycle_res
          )
            )
# 1 "Parser.fsy"
               : '_rnglr_type_seq_elem) 
# 966 "Parser.fs"
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
                

                 true 
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 1 "Parser.fsy"
               : '_rnglr_type_omit_opt) 
# 986 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            _rnglr_cycle_res := (
              

               false 
                )::!_rnglr_cycle_res
            !_rnglr_cycle_res
          )
            )
# 1 "Parser.fsy"
               : '_rnglr_type_omit_opt) 
# 1004 "Parser.fs"
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
                

                true
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 1 "Parser.fsy"
               : '_rnglr_type_semi_opt) 
# 1024 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            _rnglr_cycle_res := (
              

               false 
                )::!_rnglr_cycle_res
            !_rnglr_cycle_res
          )
            )
# 1 "Parser.fsy"
               : '_rnglr_type_semi_opt) 
# 1042 "Parser.fs"
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
                

                 Some _S1 
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 1 "Parser.fsy"
               : '_rnglr_type_predicate_opt) 
# 1062 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            _rnglr_cycle_res := (
              

               None 
                )::!_rnglr_cycle_res
            !_rnglr_cycle_res
          )
            )
# 1 "Parser.fsy"
               : '_rnglr_type_predicate_opt) 
# 1080 "Parser.fs"
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
                

                 createSeqElem None false _S1 None      
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 1 "Parser.fsy"
               : '_rnglr_type_bound) 
# 1100 "Parser.fs"
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
                    

                     createSeqElem (Some _S1) false _S3 None 
                      )::!_rnglr_cycle_res ) ) )
            !_rnglr_cycle_res
          )
            )
# 1 "Parser.fsy"
               : '_rnglr_type_bound) 
# 1124 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            (match ((unbox _rnglr_children.[0]) : Token) with PATTERN _rnglr_val -> [_rnglr_val] | a -> failwith "PATTERN expected, but %A found" a )
             |> List.iter (fun (_S1) -> 
              _rnglr_cycle_res := (
                

                _S1
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 1 "Parser.fsy"
               : '_rnglr_type_patt) 
# 1144 "Parser.fs"
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
                

                _S1
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 1 "Parser.fsy"
               : '_rnglr_type_patt) 
# 1164 "Parser.fs"
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
                

                PLiteral _S1
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 1 "Parser.fsy"
               : '_rnglr_type_prim) 
# 1184 "Parser.fs"
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
                

                _S1
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 1 "Parser.fsy"
               : '_rnglr_type_prim) 
# 1204 "Parser.fs"
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
                

                _S1
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 1 "Parser.fsy"
               : '_rnglr_type_prim) 
# 1224 "Parser.fs"
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
                    

                    _S2
                      )::!_rnglr_cycle_res ) ) )
            !_rnglr_cycle_res
          )
            )
# 1 "Parser.fsy"
               : '_rnglr_type_prim) 
# 1248 "Parser.fs"
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
                  

                  POpt _S1
                    )::!_rnglr_cycle_res ) )
            !_rnglr_cycle_res
          )
            )
# 1 "Parser.fsy"
               : '_rnglr_type_prim) 
# 1270 "Parser.fs"
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
                  

                  PSome _S1
                    )::!_rnglr_cycle_res ) )
            !_rnglr_cycle_res
          )
            )
# 1 "Parser.fsy"
               : '_rnglr_type_prim) 
# 1292 "Parser.fs"
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
                  

                  PMany _S1
                    )::!_rnglr_cycle_res ) )
            !_rnglr_cycle_res
          )
            )
# 1 "Parser.fsy"
               : '_rnglr_type_prim) 
# 1314 "Parser.fs"
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
                

                _S1
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 1 "Parser.fsy"
               : '_rnglr_type_meta_param) 
# 1334 "Parser.fs"
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
                  

                  _S1 :: _S2
                    )::!_rnglr_cycle_res ) )
            !_rnglr_cycle_res
          )
            )
# 1 "Parser.fsy"
               : '_rnglr_type_meta_params) 
# 1356 "Parser.fs"
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
                

                [_S1]
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 1 "Parser.fsy"
               : '_rnglr_type_meta_params) 
# 1376 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            (match ((unbox _rnglr_children.[0]) : Token) with DLESS _rnglr_val -> [_rnglr_val] | a -> failwith "DLESS expected, but %A found" a )
             |> List.iter (fun (_) -> 
              ((unbox _rnglr_children.[1]) : '_rnglr_type_meta_params) 
               |> List.iter (fun (_S2) -> 
                (match ((unbox _rnglr_children.[2]) : Token) with DGREAT _rnglr_val -> [_rnglr_val] | a -> failwith "DGREAT expected, but %A found" a )
                 |> List.iter (fun (_) -> 
                  _rnglr_cycle_res := (
                    

                    Some _S2
                      )::!_rnglr_cycle_res ) ) )
            !_rnglr_cycle_res
          )
            )
# 1 "Parser.fsy"
               : '_rnglr_type_meta_param_opt) 
# 1400 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            _rnglr_cycle_res := (
              

               None 
                )::!_rnglr_cycle_res
            !_rnglr_cycle_res
          )
            )
# 1 "Parser.fsy"
               : '_rnglr_type_meta_param_opt) 
# 1418 "Parser.fs"
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
                    

                     match _S2 with
                            | None -> PRef  (_S1, _S3)
                            | Some x -> PMetaRef (_S1,_S3,x)
                          
                      )::!_rnglr_cycle_res ) ) )
            !_rnglr_cycle_res
          )
            )
# 1 "Parser.fsy"
               : '_rnglr_type_call) 
# 1445 "Parser.fs"
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
                

                PToken _S1
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 1 "Parser.fsy"
               : '_rnglr_type_call) 
# 1465 "Parser.fs"
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
                  

                   Some _S2 
                    )::!_rnglr_cycle_res ) )
            !_rnglr_cycle_res
          )
            )
# 1 "Parser.fsy"
               : '_rnglr_type_option_opt) 
# 1487 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          (
            let _rnglr_cycle_res = ref []
            _rnglr_cycle_res := (
              

               None 
                )::!_rnglr_cycle_res
            !_rnglr_cycle_res
          )
            )
# 1 "Parser.fsy"
               : '_rnglr_type_option_opt) 
# 1505 "Parser.fs"
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
                    

                    (_S3).Add _S1
                      )::!_rnglr_cycle_res ) ) )
            !_rnglr_cycle_res
          )
            )
# 1 "Parser.fsy"
               : '_rnglr_type_option_params) 
# 1529 "Parser.fs"
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
                

                 Map.empty.Add _S1
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 1 "Parser.fsy"
               : '_rnglr_type_option_params) 
# 1549 "Parser.fs"
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
                    

                     (Source.toString _S1, Source.toString _S3) 
                      )::!_rnglr_cycle_res ) ) )
            !_rnglr_cycle_res
          )
            )
# 1 "Parser.fsy"
               : '_rnglr_type_option_param) 
# 1573 "Parser.fs"
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
                

                
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 1 "Parser.fsy"
               : '_rnglr_type_tada_rule) 
# 1593 "Parser.fs"
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
                

                
                  )::!_rnglr_cycle_res )
            !_rnglr_cycle_res
          )
            )
# 1 "Parser.fsy"
               : '_rnglr_type_tada_rule) 
# 1613 "Parser.fs"
      );
  |] , [|
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
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_no_lbl_seq)   ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_omit_opt)   ) |> List.concat));
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
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_yard_start_rule)   ) |> List.concat));
  |] 
let translate (args : TranslateArguments<_,_>) (tree : Tree<_>) : '_rnglr_type_yard_start_rule = 
  unbox (tree.Translate _rnglr_rule_  leftSide _rnglr_concats (if args.filterEpsilons then _rnglr_filtered_epsilons else _rnglr_epsilons) args.tokenToRange args.zeroPosition args.clearAST) : '_rnglr_type_yard_start_rule
