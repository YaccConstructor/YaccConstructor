module GLLFSA.r16s.H22_H23
#nowarn "64";; // From fsyacc: turn off warnings that type variables used in production annotations are instantiated to concrete type
open Yard.Generators.GLL
open Yard.Generators.GLL.ParserCommon

type Token =
    | A of unit
    | U of unit
    | C of unit
    | G of unit

let tokenToNumber = function
    | A() -> 531
    | U() -> 532
    | C() -> 533
    | G() -> 534

let numToString = function
    | 531 -> "A"
    | 532 -> "U"
    | 533 -> "C"
    | 534 -> "G"
    | 60 -> "any"
    | 62 -> "full"
    | 0 -> "h23"
    | 64 -> "yard_rule_stem_1"
    | 1 -> "s1"
    | 65 -> "yard_rule_stem_2"
    | 2 -> "s2"
    | 66 -> "yard_rule_stem_3"
    | 3 -> "h24"
    | 67 -> "yard_rule_gstem_5"
    | 126 -> "yard_rule_stem_6"
    | 68 -> "yard_rule_stem_4"
    | 4 -> "h22"
    | 69 -> "yard_rule_stem_7"
    | 5 -> "s4"
    | 70 -> "yard_rule_stem_8"
    | 6 -> "s5"
    | 63 -> "yard_rule_stem_9"
    | 61 -> "any_1_2"
    | 124 -> "any_1_3"
    | 71 -> "any_2_3"
    | 127 -> "any_2_4"
    | 128 -> "any_3_4"
    | 73 -> "any_3_5"
    | 125 -> "any_5_7"
    | 74 -> "any_4_6"
    | 75 -> "any_6_8"
    | 129 -> "any_9_11"
    | 11 -> "h21"
    | 77 -> "yard_rule_stem_10"
    | 12 -> "s6"
    | 78 -> "yard_rule_stem_11"
    | 13 -> "s7"
    | 79 -> "yard_rule_stem_12"
    | 14 -> "s8"
    | 15 -> "yard_rule_stem_e2_13"
    | 80 -> "yard_rule_stem_17"
    | 16 -> "yard_rule_stem_e1_14"
    | 81 -> "yard_rule_stem_16"
    | 82 -> "yard_rule_stem_15"
    | 17 -> "root"
    | 7 -> "yard_rule_stem_19"
    | 76 -> "yard_rule_stem_18"
    | 18 -> "h26"
    | 83 -> "yard_rule_stem_22"
    | 84 -> "yard_rule_stem_21"
    | 85 -> "yard_rule_stem_20"
    | 20 -> "h25"
    | 21 -> "yard_rule_stem_e2_24"
    | 86 -> "yard_rule_stem_28"
    | 22 -> "yard_rule_stem_e1_25"
    | 87 -> "yard_rule_stem_27"
    | 88 -> "yard_rule_stem_26"
    | 89 -> "yard_rule_stem_23"
    | 23 -> "h27"
    | 90 -> "yard_rule_stem_29"
    | 24 -> "h19"
    | 91 -> "yard_rule_stem_30"
    | 25 -> "h8"
    | 92 -> "yard_rule_stem_31"
    | 19 -> "h9"
    | 26 -> "h10"
    | 27 -> "yard_rule_stem_e2_32"
    | 93 -> "yard_rule_stem_35"
    | 29 -> "yard_rule_stem_e1_33"
    | 94 -> "yard_rule_stem_34"
    | 30 -> "h6"
    | 31 -> "yard_rule_stem_e2_44"
    | 95 -> "yard_rule_stem_48"
    | 32 -> "yard_rule_stem_e1_45"
    | 96 -> "yard_rule_stem_47"
    | 33 -> "yard_rule_stem_46"
    | 34 -> "yard_rule_stem_e1_41"
    | 97 -> "yard_rule_stem_43"
    | 35 -> "yard_rule_stem_42"
    | 36 -> "yard_rule_stem_e2_36"
    | 98 -> "yard_rule_stem_40"
    | 37 -> "yard_rule_stem_e1_37"
    | 99 -> "yard_rule_stem_39"
    | 28 -> "yard_rule_stem_38"
    | 38 -> "h7"
    | 39 -> "yard_rule_stem_e2_50"
    | 100 -> "yard_rule_stem_54"
    | 40 -> "yard_rule_stem_e1_51"
    | 101 -> "yard_rule_stem_53"
    | 102 -> "yard_rule_stem_52"
    | 103 -> "yard_rule_stem_49"
    | 41 -> "h11"
    | 42 -> "yard_rule_stem_e1_60"
    | 104 -> "yard_rule_stem_62"
    | 105 -> "yard_rule_stem_61"
    | 43 -> "yard_rule_stem_e2_55"
    | 106 -> "yard_rule_stem_59"
    | 44 -> "yard_rule_stem_e1_56"
    | 107 -> "yard_rule_stem_58"
    | 9 -> "yard_rule_stem_57"
    | 45 -> "h12"
    | 108 -> "yard_rule_stem_63"
    | 46 -> "h13"
    | 10 -> "yard_rule_stem_64"
    | 47 -> "h14"
    | 109 -> "yard_rule_stem_65"
    | 8 -> "h5"
    | 110 -> "yard_rule_stem_67"
    | 111 -> "yard_rule_stem_66"
    | 49 -> "h15"
    | 50 -> "yard_rule_stem_e1_68"
    | 112 -> "yard_rule_stem_70"
    | 113 -> "yard_rule_stem_69"
    | 51 -> "h16"
    | 114 -> "yard_rule_stem_71"
    | 52 -> "h17"
    | 53 -> "yard_rule_stem_e2_72"
    | 115 -> "yard_rule_stem_76"
    | 54 -> "yard_rule_stem_e1_73"
    | 116 -> "yard_rule_stem_75"
    | 117 -> "yard_rule_stem_74"
    | 55 -> "h18"
    | 118 -> "yard_rule_stem_79"
    | 119 -> "yard_rule_stem_78"
    | 120 -> "yard_rule_stem_77"
    | 56 -> "h4"
    | 57 -> "yard_rule_stem_e1_80"
    | 121 -> "yard_rule_stem_82"
    | 48 -> "yard_rule_stem_81"
    | 58 -> "h3"
    | 59 -> "yard_rule_stem_e1_83"
    | 122 -> "yard_rule_stem_85"
    | 123 -> "yard_rule_stem_84"
    | 72 -> "root2"
    | _ -> ""

let numIsTerminal = function
    | 531 -> true
    | 532 -> true
    | 533 -> true
    | 534 -> true
    | _ -> false

let numIsEpsilon = function
    | 535 -> true
    | _ -> false

let statesToConvert =
  [|[|64,130|];
    [|65,130|];
    [|66,130|];
    [|67,130|];
    [|69,130|];
    [|70,130|];
    [|63,130|];
    [|531,431;532,369;533,168;534,305;128,280|];
    [|125,284|];
    [|531,461;532,399;533,199;534,335;75,130|];
    [|531,463;532,401;533,201;534,337;129,130|];
    [|77,130|];
    [|78,130|];
    [|79,130|];
    [|15,130|];
    [|80,130|];
    [|81,130|];
    [|7,130|];
    [|83,130|];
    [|84,130|];
    [|21,130|];
    [|86,130|];
    [|87,130|];
    [|90,130|];
    [|91,130|];
    [|92,130|];
    [|27,130|];
    [|93,130|];
    [|531,452;532,390;533,190;534,326;29,130|];
    [|94,130|];
    [|31,130|];
    [|95,130|];
    [|96,130|];
    [|531,447;532,385;533,185;534,321;34,130|];
    [|97,130|];
    [|531,449;532,387;533,187;534,323;36,130|];
    [|98,130|];
    [|99,130|];
    [|39,130|];
    [|100,130|];
    [|101,130|];
    [|42,130|];
    [|104,130|];
    [|106,130|];
    [|107,130|];
    [|108,130|];
    [|10,130|];
    [|109,130|];
    [|531,477;532,414;533,217;534,351;8,251|];
    [|50,130|];
    [|112,130|];
    [|114,130|];
    [|53,130|];
    [|115,130|];
    [|116,130|];
    [|118,130|];
    [|57,130|];
    [|121,130|];
    [|59,130|];
    [|122,130|];
    [|531,130;532,130;534,130;533,130|];
    [|60,132|];
    [|72,132|];
    [|531,424;532,362;533,146;534,298;0,518|];
    [|531,417;532,355;533,134;534,292;124,135|];
    [|531,418;532,356;533,136;534,293;60,493|];
    [|531,419;532,357;533,137;534,294;60,514|];
    [|531,420;532,358;533,139;534,480;124,140|];
    [|531,421;532,359;533,141;534,221|];
    [|531,422;532,360;533,143;534,296;5,229|];
    [|531,423;532,361;533,144;534,297;124,145|];
    [|60,147|];
    [|24,290|];
    [|60,153|];
    [|60,156|];
    [|60,158|];
    [|531,432;532,370;533,169;534,306;61,161|];
    [|531,425;532,363;533,162;534,299;535,233;60,233|];
    [|531,426;532,364;533,163;534,300;71,279|];
    [|531,427;532,365;533,164;534,301;60,488|];
    [|531,428;532,366;533,165;534,302;535,234;60,234|];
    [|531,429;532,367;533,166;534,303;535,235;60,235|];
    [|531,430;532,368;533,167;534,304;535,228;60,228|];
    [|531,433;532,371;533,170;534,307;61,285|];
    [|531,434;532,372;533,171;534,308;60,258|];
    [|531,435;532,373;533,172;534,309;74,130|];
    [|531,436;532,374;533,173;534,310;535,237;60,237|];
    [|531,437;532,375;533,174;534,311;535,238;60,238|];
    [|531,438;532,376;533,175;534,312;71,272|];
    [|531,439;532,377;533,176;534,313;60,527|];
    [|531,440;532,378;533,178;534,314;125,281|];
    [|531,441;532,379;533,179;534,315;125,277|];
    [|531,442;532,380;533,180;534,316;73,281|];
    [|531,443;532,381;533,181;534,317;535,239;60,239|];
    [|531,444;532,382;533,182;534,318;535,236;60,236|];
    [|531,445;532,383;533,183;534,319;535,240;60,240|];
    [|531,446;532,384;533,184;534,320;535,241;60,241|];
    [|531,448;532,386;533,186;534,322;535,242;60,242|];
    [|531,450;532,388;533,188;534,324;535,243;60,243|];
    [|531,451;532,389;533,189;534,325;535,244;60,244|];
    [|531,453;532,391;533,191;534,327;535,245;60,245|];
    [|531,454;532,392;533,192;534,328;535,246;60,246|];
    [|531,455;532,393;533,193;534,329;127,269|];
    [|531,456;532,394;533,194;534,330;61,289|];
    [|531,457;532,395;533,195;534,331;535,247;60,247|];
    [|531,458;532,396;533,196;534,332;535,282;60,282|];
    [|531,459;532,397;533,197;534,333;535,248;60,248|];
    [|531,460;532,398;533,198;534,334;535,249;60,249|];
    [|531,462;532,400;533,200;534,336;535,275;60,275|];
    [|531,464;532,223;533,202;534,338|];
    [|531,465;532,402;533,203;534,339;124,287|];
    [|531,466;532,403;533,204;534,340;125,131|];
    [|531,467;532,404;533,205;534,341;535,252;60,252|];
    [|531,468;532,405;533,206;534,342;127,236|];
    [|531,469;532,406;533,207;534,343;125,288|];
    [|531,470;532,407;533,208;534,344;535,253;60,253|];
    [|531,471;532,408;533,209;534,345;535,254;60,254|];
    [|531,472;532,409;533,210;534,346;127,283|];
    [|531,473;532,410;533,212;534,347;125,213|];
    [|531,474;532,411;533,214;534,348;74,273|];
    [|531,475;532,412;533,215;534,349;60,227|];
    [|531,476;532,413;533,216;534,350;535,255;60,255|];
    [|531,478;532,415;533,218;534,352;535,256;60,256|];
    [|531,479;532,416;533,219;534,353;535,271;60,271|];
    [|60,231|];
    [|60,74|];
    [|531,420;532,358;533,139;534,295;124,140|];
    [|60,124|];
    [|60,489|];
    [|60,529|];
    [||];
    [|38,502|];
    [|535,130;60,130|];
    [|60,530|];
    [|64,226|];
    [|1,149|];
    [|65,226|];
    [|66,226|];
    [|3,130|];
    [|126,226|];
    [|68,130|];
    [|68,226|];
    [|4,153|];
    [|69,226|];
    [|70,226|];
    [|6,149|];
    [|63,226|];
    [|61,130|];
    [|61,276|];
    [|124,130|];
    [|124,507|];
    [|71,130|];
    [|71,222|];
    [|127,130|];
    [|127,260|];
    [|128,130|];
    [|73,130|];
    [|73,281|];
    [|125,130|];
    [|74,130|];
    [|74,286|];
    [|11,263|];
    [|77,226|];
    [|78,226|];
    [|79,226|];
    [|80,226|];
    [|81,226|];
    [|82,226|];
    [|7,226|];
    [|76,226|];
    [|83,226|];
    [|84,226|];
    [|85,226|];
    [|86,226|];
    [|87,226|];
    [|88,226|];
    [|89,226|];
    [|23,130|];
    [|90,226|];
    [|91,226|];
    [|92,226|];
    [|93,226|];
    [|94,226|];
    [|95,226|];
    [|96,226|];
    [|33,226|];
    [|97,226|];
    [|35,226|];
    [|98,226|];
    [|99,226|];
    [|28,226|];
    [|100,226|];
    [|101,226|];
    [|102,226|];
    [|103,226|];
    [|104,226|];
    [|105,226|];
    [|106,226|];
    [|107,226|];
    [|9,226|];
    [|108,226|];
    [|10,226|];
    [|109,226|];
    [|110,226|];
    [|111,226|];
    [|112,226|];
    [|113,226|];
    [|114,226|];
    [|115,226|];
    [|116,226|];
    [|117,226|];
    [|55,130|];
    [|118,226|];
    [|119,130|];
    [|119,226|];
    [|120,226|];
    [|121,226|];
    [|48,226|];
    [|122,226|];
    [|123,226|];
    [|533,130;532,130;531,130|];
    [|68,220;531,481|];
    [|531,266|];
    [|109,224;531,482|];
    [|531,130;534,130|];
    [|532,130;534,130|];
    [|534,130|];
    [|534,484|];
    [|533,504|];
    [|60,130|];
    [|60,486;535,130|];
    [|60,132;535,130|];
    [|60,519;535,274|];
    [|12,132|];
    [|16,132|];
    [|82,132|];
    [|84,132|];
    [|22,132|];
    [|88,132|];
    [|29,132|];
    [|32,132|];
    [|33,132|];
    [|35,132|];
    [|37,132|];
    [|28,132|];
    [|40,132|];
    [|102,132|];
    [|105,132|];
    [|44,132|];
    [|9,132|];
    [|45,132|];
    [|49,132|];
    [|113,132|];
    [|54,132|];
    [|117,132|];
    [|48,132|];
    [|123,132|];
    [|531,132|];
    [|60,483|];
    [|2,520|];
    [|20,133|];
    [|535,138;60,138|];
    [|60,494;535,138|];
    [|127,142|];
    [|18,147|];
    [|26,147|];
    [|531,147|];
    [|46,148|];
    [|111,500|];
    [|103,149|];
    [|41,490|];
    [|56,150|];
    [|89,151|];
    [|120,152|];
    [|14,153|];
    [|84,153|];
    [|47,153|];
    [|17,154|];
    [|52,291|];
    [|13,155|];
    [|76,155|];
    [|84,156|];
    [|43,156|];
    [|9,156|];
    [|110,156|];
    [|85,157|];
    [|19,499|];
    [|30,501|];
    [|84,159|];
    [|25,160|];
    [|61,177|];
    [|127,211|];
    [|64,220|];
    [|65,220|];
    [|66,220|];
    [|126,220|];
    [|69,220|];
    [|70,220|];
    [|63,220|];
    [|77,220|];
    [|78,220|];
    [|79,220|];
    [|80,220|];
    [|81,220|];
    [|82,220|];
    [|7,220|];
    [|76,220|];
    [|83,220|];
    [|84,220|];
    [|85,220|];
    [|86,220|];
    [|87,220|];
    [|88,220|];
    [|89,220|];
    [|90,220|];
    [|91,220|];
    [|92,220|];
    [|93,220|];
    [|94,220|];
    [|95,220|];
    [|96,220|];
    [|33,220|];
    [|97,220|];
    [|35,220|];
    [|98,220|];
    [|99,220|];
    [|28,220|];
    [|100,220|];
    [|101,220|];
    [|102,220|];
    [|103,220|];
    [|104,220|];
    [|105,220|];
    [|106,220|];
    [|107,220|];
    [|9,220|];
    [|108,220|];
    [|10,220|];
    [|109,220|];
    [|110,220|];
    [|111,220|];
    [|112,220|];
    [|113,220|];
    [|114,220|];
    [|115,220|];
    [|116,220|];
    [|117,220|];
    [|118,220|];
    [|119,220|];
    [|120,220|];
    [|121,220|];
    [|48,220|];
    [|122,220|];
    [|123,220|];
    [|535,491;60,491|];
    [|64,224|];
    [|65,224|];
    [|66,224|];
    [|126,224|];
    [|68,224|];
    [|69,224|];
    [|70,224|];
    [|63,224|];
    [|77,224|];
    [|78,224|];
    [|79,224|];
    [|80,224|];
    [|81,224|];
    [|82,224|];
    [|7,224|];
    [|76,224|];
    [|83,224|];
    [|84,224|];
    [|85,224|];
    [|86,224|];
    [|87,224|];
    [|88,224|];
    [|89,224|];
    [|90,224|];
    [|91,224|];
    [|92,224|];
    [|93,224|];
    [|94,224|];
    [|95,224|];
    [|96,224|];
    [|33,224|];
    [|97,224|];
    [|35,224|];
    [|98,224|];
    [|99,224|];
    [|28,224|];
    [|100,224|];
    [|101,224|];
    [|102,224|];
    [|103,224|];
    [|104,224|];
    [|105,224|];
    [|106,224|];
    [|107,224|];
    [|9,224|];
    [|108,224|];
    [|10,224|];
    [|110,224|];
    [|111,224|];
    [|112,224|];
    [|113,224|];
    [|114,224|];
    [|115,224|];
    [|116,224|];
    [|117,224|];
    [|118,224|];
    [|119,224|];
    [|120,224|];
    [|121,224|];
    [|48,224|];
    [|122,224|];
    [|123,224|];
    [|64,225|];
    [|65,225|];
    [|66,225|];
    [|126,225|];
    [|68,225|];
    [|69,225|];
    [|70,225|];
    [|63,225|];
    [|77,225|];
    [|78,225|];
    [|79,225|];
    [|80,225|];
    [|81,225|];
    [|82,225|];
    [|7,225|];
    [|76,225|];
    [|83,225|];
    [|84,225|];
    [|85,225|];
    [|86,225|];
    [|87,225|];
    [|88,225|];
    [|89,225|];
    [|90,225|];
    [|91,225|];
    [|92,225|];
    [|93,225|];
    [|94,225|];
    [|95,225|];
    [|96,225|];
    [|33,225|];
    [|97,225|];
    [|35,225|];
    [|98,225|];
    [|99,225|];
    [|28,225|];
    [|100,225|];
    [|101,225|];
    [|102,225|];
    [|103,225|];
    [|104,225|];
    [|105,225|];
    [|106,225|];
    [|107,225|];
    [|9,225|];
    [|108,225|];
    [|10,225|];
    [|109,225|];
    [|110,225|];
    [|111,225|];
    [|112,225|];
    [|113,225|];
    [|114,225|];
    [|115,225|];
    [|116,225|];
    [|117,225|];
    [|118,225|];
    [|119,225|];
    [|120,225|];
    [|121,225|];
    [|48,225|];
    [|122,225|];
    [|123,225|];
    [|126,60|];
    [|531,226|];
    [|533,226|];
    [|60,229|];
    [|533,229|];
    [|60,230|];
    [|60,231;535,130|];
    [|535,278;60,278|];
    [|60,232|];
    [|60,61|];
    [|124,250|];
    [|531,257|];
    [|535,259;60,259|];
    [|60,509;535,259|];
    [|60,261;535,138|];
    [|60,496|];
    [|60,262;535,138|];
    [|535,264;60,264|];
    [|60,511;535,264|];
    [|73,265|];
    [|61,267|];
    [|125,268|];
    [|535,270;60,270|];
    [|535,274;60,274|];
    [|532,354|];
    [|60,485|];
    [|60,230;535,130|];
    [|51,487|];
    [|60,512|];
    [|60,492;535,259|];
    [|60,495|];
    [|60,497;535,264|];
    [|60,498|];
    [|60,503;535,274|];
    [|60,505|];
    [|60,506|];
    [|60,127|];
    [|60,508|];
    [|60,510|];
    [|60,513;535,274|];
    [|60,515|];
    [|60,516|];
    [|60,517|];
    [|60,521|];
    [|60,522|];
    [|60,523|];
    [|60,524|];
    [|60,525|];
    [|60,526|];
    [|60,527|];
    [|60,528|] |]

let states =  
    statesToConvert 
    |> Array.Parallel.map (fun x ->  
        x 
        |> Array.map (fun (x,y) -> x, y * 1<state>)) 
let startState = 62 * 1<state>
let finalState = 130 * 1<state>
let nontermCount = 130

let firstSet =
  set[|4194705;4194706;4194707;4194708;
       3932561;3932562;3932564;3932563;
       8126865;8126866;8126868;8126867;
       401;402;403;404;
       4260241;4260242;4260243;4260244;
       65937;65938;65939;65940;
       4325777;4325778;4325779;4325780;
       131473;131474;131475;131476;
       4391313;4391314;4391315;4391316;
       197009;197010;197011;197012;
       4522385;4522386;4522387;4522388;
       4587921;4587922;4587923;4587924;
       328081;328082;328083;328084;
       262545;262546;262547;262548;
       4129169;4129170;4129171;4129172;
       393617;393618;393619;393620;
       459153;459154;459155;459156;
       8389009;8389010;8389012;8389011;
       8192401;8192402;8192404;8192403;
       524689;524690;524692;524691;
       590225;590226;590227;590228;
       4915601;4915602;4915604;4915603;
       655761;655762;655763;655764;
       8454545;8454546;8454548;8454547;
       5046673;5046674;5046675;5046676;
       5112209;5112210;5112211;5112212;
       4653457;4653458;4653460;4653459;
       786833;786834;786835;786836;
       721297;721298;721299;721300;
       5177745;5177746;5177747;5177748;
       852369;852370;852371;852372;
       5243281;5243282;5243283;5243284;
       5308817;5308818;5308819;5308820;
       5374353;5374354;5374355;5374356;
       1048977;1048978;1048979;1048980;
       983441;983442;983443;983444;
       917905;917906;917907;917908;
       1114513;1114514;1114515;1114516;
       5439889;5439890;5439891;5439892;
       3998097;3998098;3998100;3998099;
       1180049;1180050;1180051;1180052;
       5505425;5505426;5505427;5505428;
       1245585;1245586;1245587;1245588;
       5636497;5636498;5636499;5636500;
       5702033;5702034;5702035;5702036;
       5767569;5767570;5767571;5767572;
       1442193;1442194;1442195;1442196;
       1376657;1376658;1376659;1376660;
       1311121;1311122;1311123;1311124;
       5898641;5898642;5898643;5898644;
       1507729;1507730;1507731;1507732;
       5964177;5964178;5964179;5964180;
       1573265;1573266;1573267;1573268;
       6029713;6029714;6029715;6029716;
       4784529;4784530;4784532;4784531;
       1638801;1638802;1638803;1638804;
       6095249;6095250;6095251;6095252;
       6160785;6160786;6160787;6160788;
       1900945;1900946;1900947;1900948;
       1769873;1769874;1769875;1769876;
       1704337;1704338;1704339;1704340;
       1835409;1835410;1835411;1835412;
       6226321;6226322;6226323;6226324;
       6291857;6291858;6291859;6291860;
       2163089;2163090;2163091;2163092;
       6357393;6357394;6357395;6357396;
       2294161;2294162;2294163;2294164;
       6422929;6422930;6422931;6422932;
       6488465;6488466;6488467;6488468;
       2425233;2425234;2425235;2425236;
       2359697;2359698;2359699;2359700;
       2228625;2228626;2228627;2228628;
       2097553;2097554;2097555;2097556;
       2032017;2032018;2032019;2032020;
       1966481;1966482;1966483;1966484;
       6554001;6554002;6554003;6554004;
       6619537;6619538;6619539;6619540;
       6685073;6685074;6685075;6685076;
       8323473;8323474;8323476;8323475;
       2621841;2621842;2621843;2621844;
       2556305;2556306;2556307;2556308;
       2490769;2490770;2490771;2490772;
       6816145;6816146;6816147;6816148;
       6881681;6881682;6881683;6881684;
       6947217;6947218;6947219;6947220;
       7012753;7012754;7012755;7012756;
       2883985;2883986;2883987;2883988;
       2818449;2818450;2818451;2818452;
       2752913;2752914;2752915;2752916;
       2687377;2687378;2687379;2687380;
       7078289;7078290;7078291;7078292;
       2949521;2949522;2949523;2949524;
       3015057;3015058;3015059;3015060;
       7143825;7143826;7143827;7143828;
       3080593;3080594;3080595;3080596;
       3146129;3146130;3146131;3146132;
       7340433;7340434;7340435;7340436;
       7405969;7405970;7405971;7405972;
       3277201;3277202;3277203;3277204;
       3211665;3211666;3211667;3211668;
       7471505;7471506;7471507;7471508;
       3342737;3342738;3342739;3342740;
       7537041;7537042;7537043;7537044;
       7602577;7602578;7602579;7602580;
       7668113;7668114;7668115;7668116;
       3539345;3539346;3539347;3539348;
       3473809;3473810;3473811;3473812;
       3408273;3408274;3408275;3408276;
       7733649;7733650;7733651;7733652;
       3604881;3604882;3604883;3604884;
       7930257;7930258;7930259;7930260;
       3735953;3735954;3735955;3735956;
       3670417;3670418;3670419;3670420;
       7995793;7995794;7995795;7995796;
       8061329;8061330;8061331;8061332;
       3867025;3867026;3867027;3867028;
       3801489;3801490;3801491;3801492;
       4718993;4718994;4718995;4718996;
       4063633;4063634;4063635;4063636;
       4456849;4456850;4456851;4456852;
       4850065;4850066;4850068;4850067;
       4981137;4981138;4981139;4981140;
       5570961;5570962;5570963;5570964;
       5833105;5833106;5833107;5833108;
       6750609;6750610;6750611;6750612;
       7209361;7209362;7209363;7209364;
       7274897;7274898;7274899;7274900;
       7799185;7799186;7799187;7799188;
       7864721;7864722;7864723;7864724;
       8257937;8257938;8257939;8257940|]

let private parserSource = new FSAParserSourceGLL (states, startState, finalState, nontermCount, numIsTerminal, numIsEpsilon, numToString, firstSet)
let buildAbstract : (AbstractAnalysis.Common.BioParserInputGraph -> ParserCommon.ParseResult<_>) =
    Yard.Generators.GLL.AbstractParserWithoutTreeFSAInput.buildAbstract parserSource
