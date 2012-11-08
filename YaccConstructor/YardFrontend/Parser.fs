
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

# 81 "Parser.fs"
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
    | INCLUDE of Source.t
    | LIDENT of Source.t
    | LPAREN of Source.t
    | MINUS of Source.t
    | PARAM of Source.t
    | PATTERN of Source.t
    | PLUS of Source.t
    | PREDICATE of Source.t
    | QUESTION of Source.t
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
    | 31 -> "weight_opt"
    | 32 -> "yard_start_rule"
    | 33 -> "ACTION"
    | 34 -> "BAR"
    | 35 -> "COLON"
    | 36 -> "COMMA"
    | 37 -> "DGREAT"
    | 38 -> "DLABEL"
    | 39 -> "DLESS"
    | 40 -> "EOF"
    | 41 -> "EQUAL"
    | 42 -> "INCLUDE"
    | 43 -> "LIDENT"
    | 44 -> "LPAREN"
    | 45 -> "MINUS"
    | 46 -> "PARAM"
    | 47 -> "PATTERN"
    | 48 -> "PLUS"
    | 49 -> "PREDICATE"
    | 50 -> "QUESTION"
    | 51 -> "RPAREN"
    | 52 -> "SEMICOLON"
    | 53 -> "SET"
    | 54 -> "SHARPLINE"
    | 55 -> "STAR"
    | 56 -> "START_RULE_SIGN"
    | 57 -> "STRING"
    | 58 -> "UIDENT"
    | _ -> ""
let tokenToNumber = function
    | ACTION _ -> 33
    | BAR _ -> 34
    | COLON _ -> 35
    | COMMA _ -> 36
    | DGREAT _ -> 37
    | DLABEL _ -> 38
    | DLESS _ -> 39
    | EOF _ -> 40
    | EQUAL _ -> 41
    | INCLUDE _ -> 42
    | LIDENT _ -> 43
    | LPAREN _ -> 44
    | MINUS _ -> 45
    | PARAM _ -> 46
    | PATTERN _ -> 47
    | PLUS _ -> 48
    | PREDICATE _ -> 49
    | QUESTION _ -> 50
    | RPAREN _ -> 51
    | SEMICOLON _ -> 52
    | SET _ -> 53
    | SHARPLINE _ -> 54
    | STAR _ -> 55
    | START_RULE_SIGN _ -> 56
    | STRING _ -> 57
    | UIDENT _ -> 58

let mutable private cur = 0
let leftSide = [|5; 32; 0; 0; 6; 6; 24; 24; 24; 23; 29; 29; 8; 8; 7; 7; 19; 19; 18; 18; 31; 31; 1; 1; 2; 2; 26; 26; 13; 13; 9; 28; 28; 27; 14; 14; 25; 25; 21; 21; 3; 3; 20; 20; 22; 22; 22; 22; 22; 22; 22; 10; 12; 12; 11; 11; 4; 4; 15; 15; 17; 17; 16; 30; 30|]
let private rules = [|0; 24; 6; 5; 33; 52; 33; 42; 57; 24; 23; 25; 24; 29; 43; 8; 18; 35; 15; 1; 56; 39; 7; 37; 43; 7; 43; 46; 46; 18; 46; 26; 2; 26; 34; 26; 34; 26; 2; 9; 13; 33; 27; 28; 0; 38; 31; 44; 13; 51; 27; 28; 14; 3; 21; 45; 52; 49; 22; 20; 41; 22; 47; 43; 57; 9; 4; 44; 1; 51; 22; 50; 22; 48; 22; 55; 22; 10; 12; 10; 39; 12; 37; 43; 11; 19; 58; 53; 17; 16; 36; 17; 16; 43; 41; 57; 40; 54|]
let private rulesStart = [|0; 3; 4; 5; 5; 7; 7; 7; 10; 13; 20; 21; 21; 24; 24; 26; 27; 28; 28; 30; 30; 31; 31; 33; 34; 36; 39; 40; 41; 42; 45; 50; 52; 52; 55; 56; 56; 57; 57; 58; 58; 59; 62; 63; 64; 65; 66; 67; 70; 72; 74; 76; 77; 79; 80; 83; 83; 86; 87; 89; 89; 92; 93; 96; 97; 98|]
let startRule = 1

let acceptEmptyInput = true

let defaultAstToDot =
    (fun (tree : Yard.Generators.RNGLR.AST.Tree<Token>) -> tree.AstToDot numToString tokenToNumber leftSide)

let private lists_gotos = [|1; 87; 55; 2; 83; 5; 78; 81; 3; 82; 4; 6; 7; 73; 8; 71; 9; 10; 63; 11; 12; 13; 14; 44; 49; 56; 26; 52; 15; 18; 19; 20; 59; 60; 41; 62; 57; 58; 16; 17; 21; 22; 31; 23; 24; 25; 27; 61; 28; 29; 30; 32; 35; 33; 34; 36; 39; 38; 37; 40; 42; 43; 45; 46; 47; 48; 50; 53; 51; 54; 64; 70; 67; 65; 66; 68; 69; 72; 74; 76; 75; 77; 79; 80; 84; 85; 86|]
let private small_gotos =
        [|3; 0; 327681; 2162690; 65541; 1507331; 1572868; 1900549; 2752518; 3670023; 131074; 1638408; 3407881; 196613; 1507331; 1572874; 1900549; 2752518; 3670023; 327681; 2818059; 393218; 524300; 2555917; 458754; 1179662; 3014671; 524289; 2293776; 589826; 983057; 3473426; 655369; 65555; 589844; 851989; 917526; 1703959; 1769496; 2162713; 2490394; 2949147; 917515; 196636; 262173; 589854; 1310751; 1441824; 2490394; 2818081; 2883618; 3080227; 3735588; 3801125; 983042; 1376294; 3211303; 1310721; 2687016; 1376264; 262173; 589854; 1441833; 2490394; 2818090; 2883618; 3735588; 3801125; 1441795; 3145771; 3276844; 3604525; 1703938; 2031662; 3014703; 1769473; 2883632; 1835013; 852017; 917526; 1769496; 2162713; 2949147; 1900545; 3342386; 2031618; 720947; 2555956; 2097154; 1245237; 3014710; 2293770; 262173; 589854; 655415; 786488; 1441849; 2490394; 2818090; 2883618; 3735588; 3801125; 2359306; 262173; 589854; 655415; 786490; 1441849; 2490394; 2818090; 2883618; 3735588; 3801125; 2490371; 3145771; 3276844; 3604525; 2555905; 2424891; 2686985; 65596; 589844; 851989; 917526; 1703959; 1769496; 2162713; 2490394; 2949147; 2752513; 3342397; 2883586; 131134; 2228287; 3014664; 589844; 851989; 917526; 1704000; 1769496; 2162713; 2490394; 2949147; 3080194; 131137; 2228287; 3211268; 917526; 1769538; 1835075; 2949147; 3276804; 917526; 1769538; 1835076; 2949147; 3473410; 69; 2162690; 3866627; 3145771; 3276844; 3604525; 3932162; 720947; 2555956; 4128771; 1048646; 1114183; 2818120; 4194305; 2359369; 4259843; 1048646; 1114186; 2818120; 4390913; 2687051; 4456449; 3735628; 4653058; 1179725; 3014671; 4784130; 458830; 2818127; 4849665; 2424912; 4980738; 458833; 2818127; 5111809; 3735634; 5177349; 1507331; 1572947; 1900549; 2752518; 3670023; 5439490; 393300; 3407957; 5570561; 2162774|]
let gotos = Array.zeroCreate 88
for i = 0 to 87 do
        gotos.[i] <- Array.zeroCreate 59
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
let private lists_reduces = [|[|0,1|]; [|8,1|]; [|8,2|]; [|8,3|]; [|9,7|]; [|26,1|]; [|27,1|]; [|33,2|]; [|33,3|]; [|38,1|]; [|46,1|]; [|45,1|]; [|41,3|]; [|49,2|]; [|48,2|]; [|50,2|]; [|30,5|]; [|56,1|]; [|56,2|]; [|56,3|]; [|16,1|]; [|53,1|]; [|52,2|]; [|51,1|]; [|54,3|]; [|47,3|]; [|23,1|]; [|22,2|]; [|24,2|]; [|25,3|]; [|29,1|]; [|31,1|]; [|31,2|]; [|34,1|]; [|29,2|]; [|29,3|]; [|2,1|]; [|28,1|]; [|44,1|]; [|57,1|]; [|40,1|]; [|43,1|]; [|20,1|]; [|42,1|]; [|61,1|]; [|60,3|]; [|62,3|]; [|58,2|]; [|18,1|]; [|18,2|]; [|12,3|]; [|15,1|]; [|14,2|]; [|7,2|]; [|7,3|]; [|10,1|]; [|36,1|]; [|0,2|]; [|0,3|]; [|4,2|]|]
let private small_reduces =
        [|65537; 2621440; 131074; 2621441; 3407873; 196610; 2621442; 3407874; 262146; 2621443; 3407875; 720901; 2621444; 2752516; 2818052; 3407876; 3670020; 786439; 2228229; 2621445; 2752517; 2818053; 3342341; 3407877; 3670021; 851975; 2228230; 2621446; 2752518; 2818054; 3342342; 3407878; 3670022; 983054; 2162695; 2228231; 2490375; 2621447; 2752519; 2818055; 2883591; 2949127; 3080199; 3342343; 3407879; 3670023; 3735559; 3801095; 1048590; 2162696; 2228232; 2490376; 2621448; 2752520; 2818056; 2883592; 2949128; 3080200; 3342344; 3407880; 3670024; 3735560; 3801096; 1114126; 2162697; 2228233; 2490377; 2621449; 2752521; 2818057; 2883593; 2949129; 3080201; 3342345; 3407881; 3670025; 3735561; 3801097; 1179667; 2162698; 2228234; 2424842; 2490378; 2621450; 2752522; 2818058; 2883594; 2949130; 3080202; 3145738; 3211274; 3276810; 3342346; 3407882; 3604490; 3670026; 3735562; 3801098; 1245203; 2162699; 2228235; 2424843; 2490379; 2621451; 2752523; 2818059; 2883595; 2949131; 3080203; 3145739; 3211275; 3276811; 3342347; 3407883; 3604491; 3670027; 3735563; 3801099; 1441807; 2162700; 2228236; 2490380; 2621452; 2752524; 2818060; 2883596; 2949132; 3080204; 3211276; 3342348; 3407884; 3670028; 3735564; 3801100; 1507347; 2162701; 2228237; 2424845; 2490381; 2621453; 2752525; 2818061; 2883597; 2949133; 3080205; 3145741; 3211277; 3276813; 3342349; 3407885; 3604493; 3670029; 3735565; 3801101; 1572883; 2162702; 2228238; 2424846; 2490382; 2621454; 2752526; 2818062; 2883598; 2949134; 3080206; 3145742; 3211278; 3276814; 3342350; 3407886; 3604494; 3670030; 3735566; 3801102; 1638419; 2162703; 2228239; 2424847; 2490383; 2621455; 2752527; 2818063; 2883599; 2949135; 3080207; 3145743; 3211279; 3276815; 3342351; 3407887; 3604495; 3670031; 3735567; 3801103; 1966099; 2162704; 2228240; 2424848; 2490384; 2621456; 2752528; 2818064; 2883600; 2949136; 3080208; 3145744; 3211280; 3276816; 3342352; 3407888; 3604496; 3670032; 3735568; 3801104; 2031635; 2162705; 2228241; 2424849; 2490385; 2621457; 2752529; 2818065; 2883601; 2949137; 3080209; 3145745; 3211281; 3276817; 3342353; 3407889; 3604497; 3670033; 3735569; 3801105; 2097171; 2162706; 2228242; 2424850; 2490386; 2621458; 2752530; 2818066; 2883602; 2949138; 3080210; 3145746; 3211282; 3276818; 3342354; 3407890; 3604498; 3670034; 3735570; 3801106; 2162707; 2162707; 2228243; 2424851; 2490387; 2621459; 2752531; 2818067; 2883603; 2949139; 3080211; 3145747; 3211283; 3276819; 3342355; 3407891; 3604499; 3670035; 3735571; 3801107; 2228243; 2162708; 2228244; 2424852; 2490388; 2621460; 2752532; 2818068; 2883604; 2949140; 3080212; 3145748; 3211284; 3276820; 3342356; 3407892; 3604500; 3670036; 3735572; 3801108; 2359297; 2424853; 2424833; 2424854; 2490374; 2424855; 2490391; 2818071; 2883607; 3735575; 3801111; 2621460; 2162712; 2228248; 2424856; 2490392; 2621464; 2752536; 2818072; 2883608; 2949144; 3014680; 3080216; 3145752; 3211288; 3276824; 3342360; 3407896; 3604504; 3670040; 3735576; 3801112; 2818067; 2162713; 2228249; 2424857; 2490393; 2621465; 2752537; 2818073; 2883609; 2949145; 3080217; 3145753; 3211289; 3276825; 3342361; 3407897; 3604505; 3670041; 3735577; 3801113; 2883590; 2621466; 2752538; 2818074; 3342362; 3407898; 3670042; 2949126; 2621467; 2752539; 2818075; 3342363; 3407899; 3670043; 3080198; 2621468; 2752540; 2818076; 3342364; 3407900; 3670044; 3145734; 2621469; 2752541; 2818077; 3342365; 3407901; 3670045; 3211271; 2228254; 2621470; 2752542; 2818078; 3342366; 3407902; 3670046; 3276808; 2162719; 2228255; 2621471; 2752543; 2818079; 3342367; 3407903; 3670047; 3342344; 2162720; 2228256; 2621472; 2752544; 2818080; 3342368; 3407904; 3670048; 3407878; 2490401; 2818081; 2883617; 3080225; 3735585; 3801121; 3473415; 2228258; 2621474; 2752546; 2818082; 3342370; 3407906; 3670050; 3538951; 2228259; 2621475; 2752547; 2818083; 3342371; 3407907; 3670051; 3604487; 2228260; 2621476; 2752548; 2818084; 3342372; 3407908; 3670052; 3670023; 2228261; 2621477; 2752549; 2818085; 3342373; 3407909; 3670053; 3735571; 2162726; 2228262; 2424870; 2490406; 2621478; 2752550; 2818086; 2883622; 2949158; 3080230; 3145766; 3211302; 3276838; 3342374; 3407910; 3604518; 3670054; 3735590; 3801126; 3801107; 2162727; 2228263; 2424871; 2490407; 2621479; 2752551; 2818087; 2883623; 2949159; 3080231; 3145767; 3211303; 3276839; 3342375; 3407911; 3604519; 3670055; 3735591; 3801127; 3866639; 2162728; 2228264; 2490408; 2621480; 2752552; 2818088; 2883624; 2949160; 3080232; 3211304; 3342376; 3407912; 3670056; 3735592; 3801128; 3932179; 2162705; 2228241; 2490385; 2621457; 2687017; 2752529; 2818065; 2883601; 2949137; 3080209; 3145745; 3211281; 3276817; 3342353; 3407889; 3604497; 3670033; 3735569; 3801105; 3997697; 2883626; 4063233; 2687019; 4194312; 2162732; 2490412; 2818092; 2883628; 2949164; 3080236; 3735596; 3801132; 4325384; 2162733; 2490413; 2818093; 2883629; 2949165; 3080237; 3735597; 3801133; 4521993; 2162734; 2359342; 2490414; 2818094; 2883630; 2949166; 3080238; 3735598; 3801134; 4587528; 2162735; 2490415; 2818095; 2883631; 2949167; 3080239; 3735599; 3801135; 4653057; 2293808; 4718593; 2293809; 4915202; 2293810; 3014706; 4980737; 2424883; 5046273; 2424884; 5177346; 2621493; 3407925; 5242882; 2621494; 3407926; 5308417; 2818103; 5373957; 2621496; 2752568; 2818104; 3407928; 3670072; 5439489; 2621497; 5505025; 2621498; 5636097; 2621499|]
let reduces = Array.zeroCreate 88
for i = 0 to 87 do
        reduces.[i] <- Array.zeroCreate 59
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
let private lists_zeroReduces = [|[|3; 1; 0|]; [|3|]; [|6|]; [|11|]; [|37|]; [|13|]; [|19|]; [|59|]; [|35|]; [|39|]; [|21|]; [|55|]; [|17|]; [|32|]; [|35; 32|]; [|5|]|]
let private small_zeroReduces =
        [|5; 2621440; 2752513; 2818049; 3407873; 3670017; 65539; 2621442; 2818051; 3407874; 131077; 2621444; 2752516; 2818052; 3407876; 3670020; 196611; 2621442; 2818051; 3407874; 393218; 2293765; 3014661; 458753; 2293766; 589832; 2162695; 2490375; 2818055; 2883591; 2949127; 3080199; 3735559; 3801095; 655366; 2490376; 2818056; 2883592; 3080200; 3735560; 3801096; 983054; 2162697; 2228233; 2490377; 2621449; 2752521; 2818057; 2883593; 2949129; 3080201; 3342345; 3407881; 3670025; 3735561; 3801097; 1703937; 2883594; 1835014; 2490376; 2818056; 2883592; 3080200; 3735560; 3801096; 2031636; 2162699; 2228235; 2424843; 2490379; 2621451; 2752523; 2818059; 2883595; 2949131; 3014667; 3080203; 3145739; 3211275; 3276811; 3342347; 3407883; 3604491; 3670027; 3735563; 3801099; 2097171; 2162700; 2228236; 2424844; 2490380; 2621452; 2752524; 2818060; 2883596; 2949132; 3080204; 3145740; 3211276; 3276812; 3342348; 3407884; 3604492; 3670028; 3735564; 3801100; 2686982; 2490376; 2818056; 2883592; 3080200; 3735560; 3801096; 3014662; 2490376; 2818056; 2883592; 3080200; 3735560; 3801096; 3211277; 2162701; 2228237; 2490376; 2621453; 2752525; 2818062; 2883592; 3080200; 3342349; 3407885; 3670029; 3735560; 3801096; 3276813; 2162701; 2228237; 2490376; 2621453; 2752525; 2818062; 2883592; 3080200; 3342349; 3407885; 3670029; 3735560; 3801096; 3473415; 2228225; 2621441; 2752513; 2818049; 3342337; 3407873; 3670017; 3932179; 2162699; 2228235; 2490379; 2621451; 2752523; 2818059; 2883595; 2949131; 3014667; 3080203; 3145739; 3211275; 3276811; 3342347; 3407883; 3604491; 3670027; 3735563; 3801099; 4653057; 2293766; 5177347; 2621442; 2818051; 3407874; 5439489; 2621455|]
let zeroReduces = Array.zeroCreate 88
for i = 0 to 87 do
        zeroReduces.[i] <- Array.zeroCreate 59
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
let private small_acc = [87; 0]
let private accStates = Array.zeroCreate 88
for i = 0 to 87 do
        accStates.[i] <- List.exists ((=) i) small_acc
let eofIndex = 40
let private parserSource = new ParserSource<Token> (gotos, reduces, zeroReduces, accStates, rules, rulesStart, leftSide, startRule, eofIndex, tokenToNumber, acceptEmptyInput)
let buildAst : (seq<Token> -> ParseResult<Token>) =
    buildAst<Token> parserSource

let _rnglr_epsilons : Tree<Token>[] = [|new Tree<_>(null,box (new AST(new Family(3, new Nodes([||])), null)), null); null; null; null; null; new Tree<_>(null,box (new AST(new Family(0, new Nodes([|box (new AST(new Family(3, new Nodes([||])), null)); box (new AST(new Family(6, new Nodes([||])), null)); box (new AST(new Family(5, new Nodes([||])), null))|])), null)), null); new Tree<_>(null,box (new AST(new Family(5, new Nodes([||])), null)), null); null; new Tree<_>(null,box (new AST(new Family(13, new Nodes([||])), null)), null); null; null; new Tree<_>(null,box (new AST(new Family(55, new Nodes([||])), null)), null); null; null; new Tree<_>(null,box (new AST(new Family(35, new Nodes([||])), null)), null); new Tree<_>(null,box (new AST(new Family(59, new Nodes([||])), null)), null); null; null; new Tree<_>(null,box (new AST(new Family(19, new Nodes([||])), null)), null); new Tree<_>(null,box (new AST(new Family(17, new Nodes([||])), null)), null); null; new Tree<_>(null,box (new AST(new Family(39, new Nodes([||])), null)), null); null; null; new Tree<_>(null,box (new AST(new Family(6, new Nodes([||])), null)), null); new Tree<_>(null,box (new AST(new Family(37, new Nodes([||])), null)), null); null; null; new Tree<_>(null,box (new AST(new Family(32, new Nodes([||])), null)), null); new Tree<_>(null,box (new AST(new Family(11, new Nodes([||])), null)), null); null; new Tree<_>(null,box (new AST(new Family(21, new Nodes([||])), null)), null); new Tree<_>(null,box (new AST(new Family(1, new Nodes([|box (new AST(new Family(0, new Nodes([|box (new AST(new Family(3, new Nodes([||])), null)); box (new AST(new Family(6, new Nodes([||])), null)); box (new AST(new Family(5, new Nodes([||])), null))|])), null))|])), null)), null)|]
let _rnglr_filtered_epsilons : Tree<Token>[] = [|new Tree<_>(null,box (new AST(new Family(3, new Nodes([||])), null)), null); null; null; null; null; new Tree<_>(null,box (new AST(new Family(0, new Nodes([|box (new AST(new Family(3, new Nodes([||])), null)); box (new AST(new Family(6, new Nodes([||])), null)); box (new AST(new Family(5, new Nodes([||])), null))|])), null)), null); new Tree<_>(null,box (new AST(new Family(5, new Nodes([||])), null)), null); null; new Tree<_>(null,box (new AST(new Family(13, new Nodes([||])), null)), null); null; null; new Tree<_>(null,box (new AST(new Family(55, new Nodes([||])), null)), null); null; null; new Tree<_>(null,box (new AST(new Family(35, new Nodes([||])), null)), null); new Tree<_>(null,box (new AST(new Family(59, new Nodes([||])), null)), null); null; null; new Tree<_>(null,box (new AST(new Family(19, new Nodes([||])), null)), null); new Tree<_>(null,box (new AST(new Family(17, new Nodes([||])), null)), null); null; new Tree<_>(null,box (new AST(new Family(39, new Nodes([||])), null)), null); null; null; new Tree<_>(null,box (new AST(new Family(6, new Nodes([||])), null)), null); new Tree<_>(null,box (new AST(new Family(37, new Nodes([||])), null)), null); null; null; new Tree<_>(null,box (new AST(new Family(32, new Nodes([||])), null)), null); new Tree<_>(null,box (new AST(new Family(11, new Nodes([||])), null)), null); null; new Tree<_>(null,box (new AST(new Family(21, new Nodes([||])), null)), null); new Tree<_>(null,box (new AST(new Family(1, new Nodes([|box (new AST(new Family(0, new Nodes([|box (new AST(new Family(3, new Nodes([||])), null)); box (new AST(new Family(6, new Nodes([||])), null)); box (new AST(new Family(5, new Nodes([||])), null))|])), null))|])), null)), null)|]
for x in _rnglr_filtered_epsilons do if x <> null then x.ChooseSingleAst()
let _rnglr_extra_array, _rnglr_rule_, _rnglr_concats = 
  (Array.zeroCreate 0 : array<'_rnglr_type_action_opt * '_rnglr_type_alts * '_rnglr_type_bar_seq_nlist * '_rnglr_type_bound * '_rnglr_type_call * '_rnglr_type_file * '_rnglr_type_foot_opt * '_rnglr_type_formal_meta_list * '_rnglr_type_formal_meta_param_opt * '_rnglr_type_lbl_seq * '_rnglr_type_meta_param * '_rnglr_type_meta_param_opt * '_rnglr_type_meta_params * '_rnglr_type_no_lbl_seq * '_rnglr_type_omit_opt * '_rnglr_type_option_opt * '_rnglr_type_option_param * '_rnglr_type_option_params * '_rnglr_type_param_list * '_rnglr_type_param_opt * '_rnglr_type_patt * '_rnglr_type_predicate_opt * '_rnglr_type_prim * '_rnglr_type_rule * '_rnglr_type_rule_nlist * '_rnglr_type_semi_opt * '_rnglr_type_seq * '_rnglr_type_seq_elem * '_rnglr_type_seq_elem_list * '_rnglr_type_start_rule_sign_opt * '_rnglr_type_tada_rule * '_rnglr_type_weight_opt * '_rnglr_type_yard_start_rule>), 
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
                  _rnglr_cycle_res := (
                    

                     
                            { Definition.empty with
                                Definition.info = { fileName = !currentFilename }
                                Definition.head = _S1
                                Definition.grammar = (fst _S2)
                                Definition.foot = _S3
                                Definition.options = (snd _S2)
                            }
                          
                      )::!_rnglr_cycle_res ) ) )
            !_rnglr_cycle_res
          )
            )
# 1 "Parser.fsy"
               : '_rnglr_type_file) 
# 303 "Parser.fs"
      );
  (
    fun (_rnglr_children : array<_>) (parserRange : (Source.Position * Source.Position)) -> 
      box (
        ( 
          ((unbox _rnglr_children.[0]) : '_rnglr_type_file) 
            )
# 1 "Parser.fsy"
               : '_rnglr_type_yard_start_rule) 
# 313 "Parser.fs"
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
# 333 "Parser.fs"
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
# 351 "Parser.fs"
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
# 373 "Parser.fs"
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
# 391 "Parser.fs"
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
# 409 "Parser.fs"
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
# 435 "Parser.fs"
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
# 459 "Parser.fs"
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
# 502 "Parser.fs"
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
# 522 "Parser.fs"
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
# 540 "Parser.fs"
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
# 564 "Parser.fs"
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
# 582 "Parser.fs"
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
# 604 "Parser.fs"
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
# 624 "Parser.fs"
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
# 644 "Parser.fs"
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
# 662 "Parser.fs"
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
# 684 "Parser.fs"
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
# 702 "Parser.fs"
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
               : '_rnglr_type_weight_opt) 
# 722 "Parser.fs"
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
               : '_rnglr_type_weight_opt) 
# 740 "Parser.fs"
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
# 762 "Parser.fs"
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
# 782 "Parser.fs"
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
# 804 "Parser.fs"
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
# 828 "Parser.fs"
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
# 848 "Parser.fs"
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
# 868 "Parser.fs"
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
# 888 "Parser.fs"
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
# 912 "Parser.fs"
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
                        

                        makeNewSeq _S4 _S1 _S2
                          )::!_rnglr_cycle_res ) ) ) ) )
            !_rnglr_cycle_res
          )
            )
# 1 "Parser.fsy"
               : '_rnglr_type_lbl_seq) 
# 940 "Parser.fs"
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
# 962 "Parser.fs"
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
# 980 "Parser.fs"
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
# 1004 "Parser.fs"
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
               : '_rnglr_type_omit_opt) 
# 1042 "Parser.fs"
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
# 1062 "Parser.fs"
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
# 1080 "Parser.fs"
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
# 1100 "Parser.fs"
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
# 1118 "Parser.fs"
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
# 1138 "Parser.fs"
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
# 1162 "Parser.fs"
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
# 1182 "Parser.fs"
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
# 1202 "Parser.fs"
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
# 1222 "Parser.fs"
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
# 1242 "Parser.fs"
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
# 1262 "Parser.fs"
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
# 1286 "Parser.fs"
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
# 1308 "Parser.fs"
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
# 1330 "Parser.fs"
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
# 1352 "Parser.fs"
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
# 1372 "Parser.fs"
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
# 1394 "Parser.fs"
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
# 1414 "Parser.fs"
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
# 1438 "Parser.fs"
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
# 1456 "Parser.fs"
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
# 1483 "Parser.fs"
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
# 1503 "Parser.fs"
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
# 1525 "Parser.fs"
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
# 1543 "Parser.fs"
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
# 1567 "Parser.fs"
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
# 1587 "Parser.fs"
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
# 1611 "Parser.fs"
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
# 1631 "Parser.fs"
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
# 1651 "Parser.fs"
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
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_weight_opt)   ) |> List.concat));
    (fun (_rnglr_list : list<_>) -> 
      box ( 
        _rnglr_list |> List.map (fun _rnglr_item -> ((unbox _rnglr_item) : '_rnglr_type_yard_start_rule)   ) |> List.concat));
  |] 
let translate (args : TranslateArguments<_,_>) (tree : Tree<_>) : '_rnglr_type_yard_start_rule = 
  unbox (tree.Translate _rnglr_rule_  leftSide _rnglr_concats (if args.filterEpsilons then _rnglr_filtered_epsilons else _rnglr_epsilons) args.tokenToRange args.zeroPosition args.clearAST) : '_rnglr_type_yard_start_rule
