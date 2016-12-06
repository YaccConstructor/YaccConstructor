module GLLFSA.R16S_1_18
#nowarn "64";; // From fsyacc: turn off warnings that type variables used in production annotations are instantiated to concrete type
open Yard.Generators.GLL
open Yard.Generators.GLL.ParserCommon

type Token =
    | A of unit
    | U of unit
    | C of unit
    | G of unit

let tokenToNumber = function
    | A() -> 779
    | U() -> 780
    | C() -> 781
    | G() -> 782

let stateToNontermName = function
    | 779 -> "A"
    | 780 -> "U"
    | 781 -> "C"
    | 782 -> "G"
    | 0 -> "yard_rule_stem_110"
    | 1 -> "any"
    | 2 -> "yard_rule_stem_4"
    | 3 -> "yard_rule_stem_65"
    | 4 -> "any_1_3"
    | 5 -> "yard_rule_stem_114"
    | 6 -> "h23"
    | 7 -> "s1"
    | 8 -> "s2"
    | 9 -> "h24"
    | 10 -> "h22"
    | 11 -> "s4"
    | 12 -> "s5"
    | 13 -> "yard_rule_stem_90"
    | 14 -> "yard_rule_stem_121"
    | 15 -> "yard_rule_stem_26"
    | 16 -> "yard_rule_stem_122"
    | 17 -> "yard_rule_stem_109"
    | 18 -> "yard_rule_stem_120"
    | 19 -> "yard_rule_stem_86"
    | 20 -> "yard_rule_stem_71"
    | 21 -> "yard_rule_stem_20"
    | 22 -> "yard_rule_stem_108"
    | 23 -> "yard_rule_stem_57"
    | 24 -> "yard_rule_stem_64"
    | 25 -> "h21"
    | 26 -> "s6"
    | 27 -> "s7"
    | 28 -> "s8"
    | 29 -> "yard_rule_stem_e2_13"
    | 30 -> "yard_rule_stem_e1_14"
    | 31 -> "root"
    | 32 -> "h26"
    | 33 -> "h9h36h40h45"
    | 34 -> "h25"
    | 35 -> "yard_rule_stem_e2_24"
    | 36 -> "yard_rule_stem_e1_25"
    | 37 -> "h27"
    | 38 -> "h19"
    | 39 -> "yard_rule_stem_21"
    | 40 -> "h8"
    | 41 -> "h10"
    | 42 -> "yard_rule_stem_e2_32"
    | 43 -> "yard_rule_stem_38"
    | 44 -> "yard_rule_stem_e1_33"
    | 45 -> "h6"
    | 46 -> "yard_rule_stem_e2_44"
    | 47 -> "yard_rule_stem_e1_45"
    | 48 -> "yard_rule_stem_46"
    | 49 -> "yard_rule_stem_e1_41"
    | 50 -> "yard_rule_stem_42"
    | 51 -> "yard_rule_stem_e2_36"
    | 52 -> "yard_rule_stem_e1_37"
    | 53 -> "h7"
    | 54 -> "yard_rule_stem_e2_50"
    | 55 -> "yard_rule_stem_e1_51"
    | 56 -> "h11"
    | 57 -> "yard_rule_stem_e1_60"
    | 58 -> "yard_rule_stem_e2_55"
    | 59 -> "yard_rule_stem_e1_56"
    | 60 -> "h12"
    | 61 -> "h13"
    | 62 -> "h14"
    | 63 -> "yard_rule_stem_81"
    | 64 -> "h15"
    | 65 -> "yard_rule_stem_e1_68"
    | 66 -> "h16"
    | 67 -> "h17"
    | 68 -> "yard_rule_stem_e2_72"
    | 69 -> "yard_rule_stem_e1_73"
    | 70 -> "h18"
    | 71 -> "h4"
    | 72 -> "yard_rule_stem_e1_80"
    | 73 -> "h3"
    | 74 -> "yard_rule_stem_e1_83"
    | 75 -> "folded"
    | 76 -> "h37"
    | 77 -> "h35"
    | 78 -> "h39"
    | 79 -> "h38"
    | 80 -> "h34"
    | 81 -> "yard_rule_stem_e1_97"
    | 82 -> "yard_rule_stem_e2_91"
    | 83 -> "yard_rule_stem_e1_92"
    | 84 -> "h33"
    | 85 -> "h32"
    | 86 -> "h31h43"
    | 87 -> "h30"
    | 88 -> "yard_rule_stem_e1_103"
    | 89 -> "h41"
    | 90 -> "h42"
    | 91 -> "h29"
    | 92 -> "h28_a"
    | 93 -> "h28"
    | 94 -> "yard_rule_stem_e2_112"
    | 95 -> "yard_rule_stem_e1_113"
    | 96 -> "h44"
    | 97 -> "yard_rule_stem_e1_117"
    | 98 -> "yard_rule_stem_15"
    | 99 -> "any_2_4"
    | 100 -> "yard_rule_stem_7"
    | 101 -> "full"
    | 102 -> "yard_rule_stem_10"
    | 103 -> "yard_rule_stem_17"
    | 104 -> "yard_rule_stem_16"
    | 105 -> "yard_rule_stem_34"
    | 106 -> "yard_rule_stem_28"
    | 107 -> "yard_rule_stem_27"
    | 108 -> "yard_rule_stem_35"
    | 109 -> "yard_rule_stem_48"
    | 110 -> "yard_rule_stem_47"
    | 111 -> "yard_rule_stem_43"
    | 112 -> "yard_rule_stem_40"
    | 113 -> "yard_rule_stem_39"
    | 114 -> "yard_rule_stem_54"
    | 115 -> "yard_rule_stem_53"
    | 116 -> "yard_rule_stem_62"
    | 117 -> "yard_rule_stem_59"
    | 118 -> "yard_rule_stem_58"
    | 119 -> "yard_rule_stem_70"
    | 120 -> "yard_rule_stem_76"
    | 121 -> "yard_rule_stem_75"
    | 122 -> "yard_rule_stem_82"
    | 123 -> "yard_rule_stem_85"
    | 124 -> "yard_rule_stem_99"
    | 125 -> "yard_rule_stem_95"
    | 126 -> "yard_rule_stem_94"
    | 127 -> "yard_rule_stem_105"
    | 128 -> "yard_rule_stem_116"
    | 129 -> "yard_rule_stem_115"
    | 130 -> "yard_rule_stem_119"
    | 131 -> "yard_rule_stem_102"
    | 132 -> "yard_rule_stem_9"
    | 133 -> "yard_rule_stem_1"
    | 134 -> "yard_rule_stem_2"
    | 135 -> "yard_rule_gstem_5"
    | 136 -> "yard_rule_stem_8"
    | 137 -> "any_2_3"
    | 138 -> "yard_rule_stem_84"
    | 139 -> "yard_rule_stem_87"
    | 140 -> "any_3_5"
    | 141 -> "yard_rule_stem_63"
    | 142 -> "yard_rule_stem_96"
    | 143 -> "any_4_6"
    | 144 -> "yard_rule_stem_61"
    | 145 -> "root3"
    | 146 -> "any_6_8"
    | 147 -> "root2"
    | 148 -> "full_size_root"
    | 149 -> "yard_rule_stem_18"
    | 150 -> "yard_rule_stem_118"
    | 151 -> "yard_rule_stem_88"
    | 152 -> "yard_rule_stem_79"
    | 153 -> "yard_rule_stem_78"
    | 154 -> "yard_rule_stem_98"
    | 155 -> "yard_rule_stem_3"
    | 156 -> "yard_rule_stem_11"
    | 157 -> "yard_rule_stem_12"
    | 158 -> "yard_rule_stem_19"
    | 159 -> "yard_rule_stem_22"
    | 160 -> "yard_rule_stem_23"
    | 161 -> "yard_rule_stem_29"
    | 162 -> "yard_rule_stem_30"
    | 163 -> "yard_rule_stem_31"
    | 164 -> "yard_rule_stem_52"
    | 165 -> "yard_rule_stem_49"
    | 166 -> "yard_rule_stem_67"
    | 167 -> "yard_rule_stem_66"
    | 168 -> "yard_rule_stem_69"
    | 169 -> "yard_rule_stem_74"
    | 170 -> "yard_rule_stem_77"
    | 171 -> "yard_rule_stem_89"
    | 172 -> "yard_rule_stem_93"
    | 173 -> "yard_rule_stem_100"
    | 174 -> "yard_rule_stem_101"
    | 175 -> "yard_rule_stem_104"
    | 176 -> "yard_rule_stem_107"
    | 177 -> "yard_rule_stem_106"
    | 178 -> "yard_rule_stem_111"
    | 179 -> "yard_rule_stem_123"
    | 180 -> "yard_rule_stem_6"
    | 181 -> "any_1_2"
    | 182 -> "any_5_7"
    | 183 -> "h5"
    | 184 -> "any_4"
    | 185 -> "any_3_4"
    | 186 -> "any_9_11"
    | _ -> ""

let private numOfAnyState = 1<state>

let private numIsTerminal = function
    | 779 -> true
    | 780 -> true
    | 781 -> true
    | 782 -> true
    | _ -> false

let private stateAndTokenToNewState = new System.Collections.Generic.Dictionary<int, int<state>>()
stateAndTokenToNewState.Add(0, 605<state>)
stateAndTokenToNewState.Add(1, 316<state>)
stateAndTokenToNewState.Add(2, 509<state>)
stateAndTokenToNewState.Add(3, 410<state>)
stateAndTokenToNewState.Add(65536, 187<state>)
stateAndTokenToNewState.Add(65537, 187<state>)
stateAndTokenToNewState.Add(65539, 187<state>)
stateAndTokenToNewState.Add(65538, 187<state>)
stateAndTokenToNewState.Add(131072, 526<state>)
stateAndTokenToNewState.Add(131073, 237<state>)
stateAndTokenToNewState.Add(131074, 430<state>)
stateAndTokenToNewState.Add(131075, 331<state>)
stateAndTokenToNewState.Add(196608, 569<state>)
stateAndTokenToNewState.Add(196609, 280<state>)
stateAndTokenToNewState.Add(196610, 473<state>)
stateAndTokenToNewState.Add(196611, 374<state>)
stateAndTokenToNewState.Add(327680, 609<state>)
stateAndTokenToNewState.Add(327681, 320<state>)
stateAndTokenToNewState.Add(327682, 513<state>)
stateAndTokenToNewState.Add(327683, 414<state>)
stateAndTokenToNewState.Add(851968, 589<state>)
stateAndTokenToNewState.Add(851969, 300<state>)
stateAndTokenToNewState.Add(851970, 493<state>)
stateAndTokenToNewState.Add(851971, 394<state>)
stateAndTokenToNewState.Add(917504, 612<state>)
stateAndTokenToNewState.Add(917505, 323<state>)
stateAndTokenToNewState.Add(917506, 516<state>)
stateAndTokenToNewState.Add(917507, 417<state>)
stateAndTokenToNewState.Add(983040, 543<state>)
stateAndTokenToNewState.Add(983041, 254<state>)
stateAndTokenToNewState.Add(983042, 447<state>)
stateAndTokenToNewState.Add(983043, 348<state>)
stateAndTokenToNewState.Add(1048576, 611<state>)
stateAndTokenToNewState.Add(1048577, 322<state>)
stateAndTokenToNewState.Add(1048578, 515<state>)
stateAndTokenToNewState.Add(1048579, 416<state>)
stateAndTokenToNewState.Add(1114112, 604<state>)
stateAndTokenToNewState.Add(1114113, 315<state>)
stateAndTokenToNewState.Add(1114114, 508<state>)
stateAndTokenToNewState.Add(1114115, 409<state>)
stateAndTokenToNewState.Add(1179648, 613<state>)
stateAndTokenToNewState.Add(1179649, 324<state>)
stateAndTokenToNewState.Add(1179650, 517<state>)
stateAndTokenToNewState.Add(1179651, 418<state>)
stateAndTokenToNewState.Add(1245184, 585<state>)
stateAndTokenToNewState.Add(1245185, 296<state>)
stateAndTokenToNewState.Add(1245186, 489<state>)
stateAndTokenToNewState.Add(1245187, 390<state>)
stateAndTokenToNewState.Add(1310720, 574<state>)
stateAndTokenToNewState.Add(1310721, 285<state>)
stateAndTokenToNewState.Add(1310722, 478<state>)
stateAndTokenToNewState.Add(1310723, 379<state>)
stateAndTokenToNewState.Add(1376256, 540<state>)
stateAndTokenToNewState.Add(1376257, 251<state>)
stateAndTokenToNewState.Add(1376258, 444<state>)
stateAndTokenToNewState.Add(1376259, 345<state>)
stateAndTokenToNewState.Add(1441792, 601<state>)
stateAndTokenToNewState.Add(1441793, 312<state>)
stateAndTokenToNewState.Add(1441794, 505<state>)
stateAndTokenToNewState.Add(1441795, 406<state>)
stateAndTokenToNewState.Add(1507328, 566<state>)
stateAndTokenToNewState.Add(1507329, 277<state>)
stateAndTokenToNewState.Add(1507330, 470<state>)
stateAndTokenToNewState.Add(1507331, 371<state>)
stateAndTokenToNewState.Add(1572864, 568<state>)
stateAndTokenToNewState.Add(1572865, 279<state>)
stateAndTokenToNewState.Add(1572866, 472<state>)
stateAndTokenToNewState.Add(1572867, 373<state>)
stateAndTokenToNewState.Add(2555904, 539<state>)
stateAndTokenToNewState.Add(2555905, 250<state>)
stateAndTokenToNewState.Add(2555906, 443<state>)
stateAndTokenToNewState.Add(2555907, 344<state>)
stateAndTokenToNewState.Add(2818048, 557<state>)
stateAndTokenToNewState.Add(2818049, 268<state>)
stateAndTokenToNewState.Add(2818050, 461<state>)
stateAndTokenToNewState.Add(2818051, 362<state>)
stateAndTokenToNewState.Add(3145728, 552<state>)
stateAndTokenToNewState.Add(3145729, 263<state>)
stateAndTokenToNewState.Add(3145730, 456<state>)
stateAndTokenToNewState.Add(3145731, 357<state>)
stateAndTokenToNewState.Add(3276800, 554<state>)
stateAndTokenToNewState.Add(3276801, 265<state>)
stateAndTokenToNewState.Add(3276802, 458<state>)
stateAndTokenToNewState.Add(3276803, 359<state>)
stateAndTokenToNewState.Add(4128768, 582<state>)
stateAndTokenToNewState.Add(4128769, 293<state>)
stateAndTokenToNewState.Add(4128770, 486<state>)
stateAndTokenToNewState.Add(4128771, 387<state>)
stateAndTokenToNewState.Add(6422528, 535<state>)
stateAndTokenToNewState.Add(6422529, 246<state>)
stateAndTokenToNewState.Add(6422530, 439<state>)
stateAndTokenToNewState.Add(6422531, 340<state>)
stateAndTokenToNewState.Add(6553600, 527<state>)
stateAndTokenToNewState.Add(6553601, 238<state>)
stateAndTokenToNewState.Add(6553602, 431<state>)
stateAndTokenToNewState.Add(6553603, 332<state>)
stateAndTokenToNewState.Add(6684672, 530<state>)
stateAndTokenToNewState.Add(6684673, 241<state>)
stateAndTokenToNewState.Add(6684674, 434<state>)
stateAndTokenToNewState.Add(6684675, 335<state>)
stateAndTokenToNewState.Add(6750208, 533<state>)
stateAndTokenToNewState.Add(6750209, 244<state>)
stateAndTokenToNewState.Add(6750210, 437<state>)
stateAndTokenToNewState.Add(6750211, 338<state>)
stateAndTokenToNewState.Add(6815744, 534<state>)
stateAndTokenToNewState.Add(6815745, 245<state>)
stateAndTokenToNewState.Add(6815746, 438<state>)
stateAndTokenToNewState.Add(6815747, 339<state>)
stateAndTokenToNewState.Add(6881280, 549<state>)
stateAndTokenToNewState.Add(6881281, 260<state>)
stateAndTokenToNewState.Add(6881282, 453<state>)
stateAndTokenToNewState.Add(6881283, 354<state>)
stateAndTokenToNewState.Add(6946816, 541<state>)
stateAndTokenToNewState.Add(6946817, 252<state>)
stateAndTokenToNewState.Add(6946818, 445<state>)
stateAndTokenToNewState.Add(6946819, 346<state>)
stateAndTokenToNewState.Add(7012352, 542<state>)
stateAndTokenToNewState.Add(7012353, 253<state>)
stateAndTokenToNewState.Add(7012354, 446<state>)
stateAndTokenToNewState.Add(7012355, 347<state>)
stateAndTokenToNewState.Add(7077888, 548<state>)
stateAndTokenToNewState.Add(7077889, 259<state>)
stateAndTokenToNewState.Add(7077890, 452<state>)
stateAndTokenToNewState.Add(7077891, 353<state>)
stateAndTokenToNewState.Add(7143424, 550<state>)
stateAndTokenToNewState.Add(7143425, 261<state>)
stateAndTokenToNewState.Add(7143426, 454<state>)
stateAndTokenToNewState.Add(7143427, 355<state>)
stateAndTokenToNewState.Add(7208960, 551<state>)
stateAndTokenToNewState.Add(7208961, 262<state>)
stateAndTokenToNewState.Add(7208962, 455<state>)
stateAndTokenToNewState.Add(7208963, 356<state>)
stateAndTokenToNewState.Add(7274496, 553<state>)
stateAndTokenToNewState.Add(7274497, 264<state>)
stateAndTokenToNewState.Add(7274498, 457<state>)
stateAndTokenToNewState.Add(7274499, 358<state>)
stateAndTokenToNewState.Add(7340032, 555<state>)
stateAndTokenToNewState.Add(7340033, 266<state>)
stateAndTokenToNewState.Add(7340034, 459<state>)
stateAndTokenToNewState.Add(7340035, 360<state>)
stateAndTokenToNewState.Add(7405568, 556<state>)
stateAndTokenToNewState.Add(7405569, 267<state>)
stateAndTokenToNewState.Add(7405570, 460<state>)
stateAndTokenToNewState.Add(7405571, 361<state>)
stateAndTokenToNewState.Add(7471104, 558<state>)
stateAndTokenToNewState.Add(7471105, 269<state>)
stateAndTokenToNewState.Add(7471106, 462<state>)
stateAndTokenToNewState.Add(7471107, 363<state>)
stateAndTokenToNewState.Add(7536640, 559<state>)
stateAndTokenToNewState.Add(7536641, 270<state>)
stateAndTokenToNewState.Add(7536642, 463<state>)
stateAndTokenToNewState.Add(7536643, 364<state>)
stateAndTokenToNewState.Add(7602176, 562<state>)
stateAndTokenToNewState.Add(7602177, 273<state>)
stateAndTokenToNewState.Add(7602178, 466<state>)
stateAndTokenToNewState.Add(7602179, 367<state>)
stateAndTokenToNewState.Add(7667712, 564<state>)
stateAndTokenToNewState.Add(7667713, 275<state>)
stateAndTokenToNewState.Add(7667714, 468<state>)
stateAndTokenToNewState.Add(7667715, 369<state>)
stateAndTokenToNewState.Add(7733248, 565<state>)
stateAndTokenToNewState.Add(7733249, 276<state>)
stateAndTokenToNewState.Add(7733250, 469<state>)
stateAndTokenToNewState.Add(7733251, 370<state>)
stateAndTokenToNewState.Add(7798784, 572<state>)
stateAndTokenToNewState.Add(7798785, 283<state>)
stateAndTokenToNewState.Add(7798786, 476<state>)
stateAndTokenToNewState.Add(7798787, 377<state>)
stateAndTokenToNewState.Add(7864320, 575<state>)
stateAndTokenToNewState.Add(7864321, 286<state>)
stateAndTokenToNewState.Add(7864322, 479<state>)
stateAndTokenToNewState.Add(7864323, 380<state>)
stateAndTokenToNewState.Add(7929856, 576<state>)
stateAndTokenToNewState.Add(7929857, 287<state>)
stateAndTokenToNewState.Add(7929858, 480<state>)
stateAndTokenToNewState.Add(7929859, 381<state>)
stateAndTokenToNewState.Add(7995392, 581<state>)
stateAndTokenToNewState.Add(7995393, 292<state>)
stateAndTokenToNewState.Add(7995394, 485<state>)
stateAndTokenToNewState.Add(7995395, 386<state>)
stateAndTokenToNewState.Add(8060928, 583<state>)
stateAndTokenToNewState.Add(8060929, 294<state>)
stateAndTokenToNewState.Add(8060930, 487<state>)
stateAndTokenToNewState.Add(8060931, 388<state>)
stateAndTokenToNewState.Add(8126464, 590<state>)
stateAndTokenToNewState.Add(8126465, 301<state>)
stateAndTokenToNewState.Add(8126466, 494<state>)
stateAndTokenToNewState.Add(8126467, 395<state>)
stateAndTokenToNewState.Add(8192000, 593<state>)
stateAndTokenToNewState.Add(8192001, 304<state>)
stateAndTokenToNewState.Add(8192002, 497<state>)
stateAndTokenToNewState.Add(8192003, 398<state>)
stateAndTokenToNewState.Add(8257536, 594<state>)
stateAndTokenToNewState.Add(8257537, 305<state>)
stateAndTokenToNewState.Add(8257538, 498<state>)
stateAndTokenToNewState.Add(8257539, 399<state>)
stateAndTokenToNewState.Add(8323072, 599<state>)
stateAndTokenToNewState.Add(8323073, 310<state>)
stateAndTokenToNewState.Add(8323074, 503<state>)
stateAndTokenToNewState.Add(8323075, 404<state>)
stateAndTokenToNewState.Add(8388608, 607<state>)
stateAndTokenToNewState.Add(8388609, 318<state>)
stateAndTokenToNewState.Add(8388610, 511<state>)
stateAndTokenToNewState.Add(8388611, 412<state>)
stateAndTokenToNewState.Add(8454144, 608<state>)
stateAndTokenToNewState.Add(8454145, 319<state>)
stateAndTokenToNewState.Add(8454146, 512<state>)
stateAndTokenToNewState.Add(8454147, 413<state>)
stateAndTokenToNewState.Add(8519680, 614<state>)
stateAndTokenToNewState.Add(8519681, 325<state>)
stateAndTokenToNewState.Add(8519682, 518<state>)
stateAndTokenToNewState.Add(8519683, 419<state>)
stateAndTokenToNewState.Add(8585216, 598<state>)
stateAndTokenToNewState.Add(8585217, 309<state>)
stateAndTokenToNewState.Add(8585218, 502<state>)
stateAndTokenToNewState.Add(8585219, 403<state>)
stateAndTokenToNewState.Add(8650752, 529<state>)
stateAndTokenToNewState.Add(8650753, 240<state>)
stateAndTokenToNewState.Add(8650754, 433<state>)
stateAndTokenToNewState.Add(8650755, 334<state>)
stateAndTokenToNewState.Add(8716288, 522<state>)
stateAndTokenToNewState.Add(8716289, 233<state>)
stateAndTokenToNewState.Add(8716290, 426<state>)
stateAndTokenToNewState.Add(8716291, 327<state>)
stateAndTokenToNewState.Add(8781824, 523<state>)
stateAndTokenToNewState.Add(8781825, 234<state>)
stateAndTokenToNewState.Add(8781826, 427<state>)
stateAndTokenToNewState.Add(8781827, 328<state>)
stateAndTokenToNewState.Add(8847360, 525<state>)
stateAndTokenToNewState.Add(8847361, 236<state>)
stateAndTokenToNewState.Add(8847362, 429<state>)
stateAndTokenToNewState.Add(8847363, 423<state>)
stateAndTokenToNewState.Add(8912896, 528<state>)
stateAndTokenToNewState.Add(8912897, 239<state>)
stateAndTokenToNewState.Add(8912898, 432<state>)
stateAndTokenToNewState.Add(8912899, 333<state>)
stateAndTokenToNewState.Add(9043968, 584<state>)
stateAndTokenToNewState.Add(9043969, 295<state>)
stateAndTokenToNewState.Add(9043970, 488<state>)
stateAndTokenToNewState.Add(9043971, 389<state>)
stateAndTokenToNewState.Add(9109504, 586<state>)
stateAndTokenToNewState.Add(9109505, 297<state>)
stateAndTokenToNewState.Add(9109506, 490<state>)
stateAndTokenToNewState.Add(9109507, 391<state>)
stateAndTokenToNewState.Add(9240576, 567<state>)
stateAndTokenToNewState.Add(9240577, 278<state>)
stateAndTokenToNewState.Add(9240578, 471<state>)
stateAndTokenToNewState.Add(9240579, 372<state>)
stateAndTokenToNewState.Add(9306112, 592<state>)
stateAndTokenToNewState.Add(9306113, 303<state>)
stateAndTokenToNewState.Add(9306114, 496<state>)
stateAndTokenToNewState.Add(9306115, 397<state>)
stateAndTokenToNewState.Add(9437184, 563<state>)
stateAndTokenToNewState.Add(9437185, 274<state>)
stateAndTokenToNewState.Add(9437186, 467<state>)
stateAndTokenToNewState.Add(9437187, 368<state>)
stateAndTokenToNewState.Add(9764864, 537<state>)
stateAndTokenToNewState.Add(9764865, 248<state>)
stateAndTokenToNewState.Add(9764866, 441<state>)
stateAndTokenToNewState.Add(9764867, 342<state>)
stateAndTokenToNewState.Add(9830400, 615<state>)
stateAndTokenToNewState.Add(9830401, 326<state>)
stateAndTokenToNewState.Add(9830402, 519<state>)
stateAndTokenToNewState.Add(9830403, 420<state>)
stateAndTokenToNewState.Add(9895936, 588<state>)
stateAndTokenToNewState.Add(9895937, 299<state>)
stateAndTokenToNewState.Add(9895938, 492<state>)
stateAndTokenToNewState.Add(9895939, 393<state>)
stateAndTokenToNewState.Add(9961472, 578<state>)
stateAndTokenToNewState.Add(9961473, 289<state>)
stateAndTokenToNewState.Add(9961474, 482<state>)
stateAndTokenToNewState.Add(9961475, 383<state>)
stateAndTokenToNewState.Add(10027008, 579<state>)
stateAndTokenToNewState.Add(10027009, 290<state>)
stateAndTokenToNewState.Add(10027010, 483<state>)
stateAndTokenToNewState.Add(10027011, 384<state>)
stateAndTokenToNewState.Add(10092544, 591<state>)
stateAndTokenToNewState.Add(10092545, 302<state>)
stateAndTokenToNewState.Add(10092546, 495<state>)
stateAndTokenToNewState.Add(10092547, 396<state>)
stateAndTokenToNewState.Add(10158080, 524<state>)
stateAndTokenToNewState.Add(10158081, 235<state>)
stateAndTokenToNewState.Add(10158082, 428<state>)
stateAndTokenToNewState.Add(10158083, 329<state>)
stateAndTokenToNewState.Add(10223616, 531<state>)
stateAndTokenToNewState.Add(10223617, 242<state>)
stateAndTokenToNewState.Add(10223618, 435<state>)
stateAndTokenToNewState.Add(10223619, 336<state>)
stateAndTokenToNewState.Add(10289152, 532<state>)
stateAndTokenToNewState.Add(10289153, 243<state>)
stateAndTokenToNewState.Add(10289154, 436<state>)
stateAndTokenToNewState.Add(10289155, 337<state>)
stateAndTokenToNewState.Add(10354688, 536<state>)
stateAndTokenToNewState.Add(10354689, 247<state>)
stateAndTokenToNewState.Add(10354690, 440<state>)
stateAndTokenToNewState.Add(10354691, 341<state>)
stateAndTokenToNewState.Add(10420224, 538<state>)
stateAndTokenToNewState.Add(10420225, 249<state>)
stateAndTokenToNewState.Add(10420226, 442<state>)
stateAndTokenToNewState.Add(10420227, 343<state>)
stateAndTokenToNewState.Add(10485760, 544<state>)
stateAndTokenToNewState.Add(10485761, 255<state>)
stateAndTokenToNewState.Add(10485762, 448<state>)
stateAndTokenToNewState.Add(10485763, 349<state>)
stateAndTokenToNewState.Add(10551296, 545<state>)
stateAndTokenToNewState.Add(10551297, 256<state>)
stateAndTokenToNewState.Add(10551298, 449<state>)
stateAndTokenToNewState.Add(10551299, 350<state>)
stateAndTokenToNewState.Add(10616832, 546<state>)
stateAndTokenToNewState.Add(10616833, 257<state>)
stateAndTokenToNewState.Add(10616834, 450<state>)
stateAndTokenToNewState.Add(10616835, 351<state>)
stateAndTokenToNewState.Add(10682368, 547<state>)
stateAndTokenToNewState.Add(10682369, 258<state>)
stateAndTokenToNewState.Add(10682370, 451<state>)
stateAndTokenToNewState.Add(10682371, 352<state>)
stateAndTokenToNewState.Add(10747904, 560<state>)
stateAndTokenToNewState.Add(10747905, 271<state>)
stateAndTokenToNewState.Add(10747906, 464<state>)
stateAndTokenToNewState.Add(10747907, 365<state>)
stateAndTokenToNewState.Add(10813440, 561<state>)
stateAndTokenToNewState.Add(10813441, 272<state>)
stateAndTokenToNewState.Add(10813442, 465<state>)
stateAndTokenToNewState.Add(10813443, 366<state>)
stateAndTokenToNewState.Add(10878976, 570<state>)
stateAndTokenToNewState.Add(10878977, 281<state>)
stateAndTokenToNewState.Add(10878978, 474<state>)
stateAndTokenToNewState.Add(10878979, 375<state>)
stateAndTokenToNewState.Add(10944512, 571<state>)
stateAndTokenToNewState.Add(10944513, 282<state>)
stateAndTokenToNewState.Add(10944514, 475<state>)
stateAndTokenToNewState.Add(10944515, 376<state>)
stateAndTokenToNewState.Add(11010048, 573<state>)
stateAndTokenToNewState.Add(11010049, 284<state>)
stateAndTokenToNewState.Add(11010050, 477<state>)
stateAndTokenToNewState.Add(11010051, 378<state>)
stateAndTokenToNewState.Add(11075584, 577<state>)
stateAndTokenToNewState.Add(11075585, 288<state>)
stateAndTokenToNewState.Add(11075586, 481<state>)
stateAndTokenToNewState.Add(11075587, 382<state>)
stateAndTokenToNewState.Add(11141120, 580<state>)
stateAndTokenToNewState.Add(11141121, 291<state>)
stateAndTokenToNewState.Add(11141122, 484<state>)
stateAndTokenToNewState.Add(11141123, 385<state>)
stateAndTokenToNewState.Add(11206656, 587<state>)
stateAndTokenToNewState.Add(11206657, 298<state>)
stateAndTokenToNewState.Add(11206658, 491<state>)
stateAndTokenToNewState.Add(11206659, 392<state>)
stateAndTokenToNewState.Add(11272192, 595<state>)
stateAndTokenToNewState.Add(11272193, 306<state>)
stateAndTokenToNewState.Add(11272194, 499<state>)
stateAndTokenToNewState.Add(11272195, 400<state>)
stateAndTokenToNewState.Add(11337728, 596<state>)
stateAndTokenToNewState.Add(11337729, 307<state>)
stateAndTokenToNewState.Add(11337730, 500<state>)
stateAndTokenToNewState.Add(11337731, 401<state>)
stateAndTokenToNewState.Add(11403264, 597<state>)
stateAndTokenToNewState.Add(11403265, 308<state>)
stateAndTokenToNewState.Add(11403266, 501<state>)
stateAndTokenToNewState.Add(11403267, 402<state>)
stateAndTokenToNewState.Add(11468800, 600<state>)
stateAndTokenToNewState.Add(11468801, 311<state>)
stateAndTokenToNewState.Add(11468802, 504<state>)
stateAndTokenToNewState.Add(11468803, 405<state>)
stateAndTokenToNewState.Add(11534336, 602<state>)
stateAndTokenToNewState.Add(11534337, 313<state>)
stateAndTokenToNewState.Add(11534338, 506<state>)
stateAndTokenToNewState.Add(11534339, 407<state>)
stateAndTokenToNewState.Add(11599872, 603<state>)
stateAndTokenToNewState.Add(11599873, 314<state>)
stateAndTokenToNewState.Add(11599874, 507<state>)
stateAndTokenToNewState.Add(11599875, 408<state>)
stateAndTokenToNewState.Add(11665408, 606<state>)
stateAndTokenToNewState.Add(11665409, 317<state>)
stateAndTokenToNewState.Add(11665410, 510<state>)
stateAndTokenToNewState.Add(11665411, 411<state>)
stateAndTokenToNewState.Add(11730944, 610<state>)
stateAndTokenToNewState.Add(11730945, 321<state>)
stateAndTokenToNewState.Add(11730946, 514<state>)
stateAndTokenToNewState.Add(11730947, 415<state>)
stateAndTokenToNewState.Add(11796480, 525<state>)
stateAndTokenToNewState.Add(11796481, 236<state>)
stateAndTokenToNewState.Add(11796482, 429<state>)
stateAndTokenToNewState.Add(11796483, 330<state>)
stateAndTokenToNewState.Add(12386304, 187<state>)
stateAndTokenToNewState.Add(12386307, 187<state>)
stateAndTokenToNewState.Add(12451842, 187<state>)
stateAndTokenToNewState.Add(12451841, 187<state>)
stateAndTokenToNewState.Add(12451840, 187<state>)
stateAndTokenToNewState.Add(12517376, 207<state>)
stateAndTokenToNewState.Add(12582914, 617<state>)
stateAndTokenToNewState.Add(12648451, 187<state>)
stateAndTokenToNewState.Add(12713984, 197<state>)
stateAndTokenToNewState.Add(12779521, 187<state>)
stateAndTokenToNewState.Add(12779523, 187<state>)
stateAndTokenToNewState.Add(12845057, 520<state>)
stateAndTokenToNewState.Add(18350080, 425<state>)
stateAndTokenToNewState.Add(21692416, 424<state>)
stateAndTokenToNewState.Add(27590656, 191<state>)
stateAndTokenToNewState.Add(27656195, 192<state>)
stateAndTokenToNewState.Add(27787264, 193<state>)
stateAndTokenToNewState.Add(27852802, 193<state>)
stateAndTokenToNewState.Add(28770305, 520<state>)
stateAndTokenToNewState.Add(34078720, 194<state>)
stateAndTokenToNewState.Add(34144256, 194<state>)
stateAndTokenToNewState.Add(40370178, 196<state>)

let private outNonterms =
  [|[|87<state>,214<state>|];
    [||];
    [||];
    [||];
    [|1<state>,618<state>|];
    [|1<state>,686<state>|];
    [|133<state>,187<state>|];
    [|134<state>,187<state>|];
    [|155<state>,187<state>|];
    [|135<state>,187<state>|];
    [|100<state>,187<state>|];
    [|136<state>,187<state>|];
    [|132<state>,187<state>|];
    [|181<state>,673<state>|];
    [|4<state>,668<state>|];
    [|137<state>,674<state>|];
    [|99<state>,701<state>|];
    [|185<state>,693<state>|];
    [|140<state>,687<state>|];
    [|182<state>,187<state>|];
    [|182<state>,707<state>|];
    [|143<state>,187<state>|];
    [|143<state>,709<state>|];
    [|146<state>,187<state>|];
    [|186<state>,187<state>|];
    [|102<state>,187<state>|];
    [|156<state>,187<state>|];
    [|157<state>,187<state>|];
    [|29<state>,187<state>|];
    [|103<state>,187<state>|];
    [|104<state>,187<state>|];
    [|158<state>,187<state>|];
    [|159<state>,187<state>|];
    [|39<state>,187<state>|];
    [|35<state>,187<state>|];
    [|106<state>,187<state>|];
    [|107<state>,187<state>|];
    [|161<state>,187<state>|];
    [|162<state>,187<state>|];
    [|184<state>,187<state>|];
    [|163<state>,187<state>|];
    [|42<state>,187<state>|];
    [|108<state>,187<state>|];
    [|44<state>,187<state>|];
    [|105<state>,187<state>|];
    [|46<state>,187<state>|];
    [|109<state>,187<state>|];
    [|110<state>,187<state>|];
    [|49<state>,187<state>|];
    [|111<state>,187<state>|];
    [|51<state>,187<state>|];
    [|112<state>,187<state>|];
    [|113<state>,187<state>|];
    [|54<state>,187<state>|];
    [|114<state>,187<state>|];
    [|115<state>,187<state>|];
    [|57<state>,187<state>|];
    [|116<state>,187<state>|];
    [|117<state>,187<state>|];
    [|118<state>,187<state>|];
    [|141<state>,187<state>|];
    [|24<state>,187<state>|];
    [|3<state>,187<state>|];
    [|183<state>,637<state>|];
    [|65<state>,187<state>|];
    [|119<state>,187<state>|];
    [|20<state>,187<state>|];
    [|68<state>,187<state>|];
    [|120<state>,187<state>|];
    [|121<state>,187<state>|];
    [|152<state>,187<state>|];
    [|72<state>,187<state>|];
    [|122<state>,187<state>|];
    [|74<state>,187<state>|];
    [|123<state>,187<state>|];
    [|147<state>,187<state>|];
    [|19<state>,187<state>|];
    [|139<state>,187<state>|];
    [|171<state>,187<state>|];
    [|13<state>,187<state>|];
    [|81<state>,187<state>|];
    [|124<state>,187<state>|];
    [|125<state>,187<state>|];
    [|126<state>,187<state>|];
    [|173<state>,187<state>|];
    [|174<state>,187<state>|];
    [|131<state>,187<state>|];
    [|88<state>,187<state>|];
    [|127<state>,187<state>|];
    [|22<state>,187<state>|];
    [|17<state>,187<state>|];
    [|0<state>,187<state>|];
    [|178<state>,187<state>|];
    [|94<state>,187<state>|];
    [|128<state>,187<state>|];
    [|129<state>,187<state>|];
    [|179<state>,187<state>|];
    [|130<state>,187<state>|];
    [|1<state>,616<state>|];
    [|1<state>,4<state>|];
    [|11<state>,617<state>|];
    [|75<state>,197<state>|];
    [|1<state>,619<state>;26<state>,197<state>|];
    [|1<state>,620<state>;30<state>,197<state>|];
    [|1<state>,621<state>;98<state>,197<state>|];
    [|1<state>,622<state>;39<state>,197<state>|];
    [|1<state>,623<state>;36<state>,197<state>|];
    [|1<state>,624<state>;15<state>,197<state>|];
    [|1<state>,625<state>;44<state>,197<state>|];
    [|1<state>,626<state>;47<state>,197<state>|];
    [|1<state>,627<state>;48<state>,197<state>|];
    [|1<state>,628<state>;50<state>,197<state>|];
    [|1<state>,629<state>;52<state>,197<state>|];
    [|1<state>,630<state>;43<state>,197<state>|];
    [|1<state>,631<state>;55<state>,197<state>|];
    [|1<state>,632<state>;164<state>,197<state>|];
    [|1<state>,633<state>;144<state>,197<state>|];
    [|1<state>,634<state>;59<state>,197<state>|];
    [|1<state>,635<state>;23<state>,197<state>|];
    [|1<state>,638<state>;168<state>,197<state>|];
    [|1<state>,639<state>;69<state>,197<state>|];
    [|1<state>,640<state>;169<state>,197<state>|];
    [|1<state>,641<state>;63<state>,197<state>|];
    [|1<state>,642<state>;138<state>,197<state>|];
    [|1<state>,643<state>;154<state>,197<state>|];
    [|1<state>,644<state>;83<state>,197<state>|];
    [|1<state>,645<state>;172<state>,197<state>|];
    [|1<state>,646<state>;175<state>,197<state>|];
    [|1<state>,647<state>;95<state>,197<state>|];
    [|1<state>,648<state>;5<state>,197<state>|];
    [|1<state>,649<state>;150<state>,197<state>|];
    [|1<state>,758<state>|];
    [|6<state>,750<state>|];
    [|4<state>,199<state>|];
    [|1<state>,200<state>|];
    [|4<state>,204<state>|];
    [|4<state>,206<state>|];
    [|1<state>,207<state>|];
    [|1<state>,672<state>;71<state>,210<state>|];
    [|33<state>,212<state>|];
    [|1<state>,213<state>|];
    [|1<state>,680<state>;39<state>,213<state>|];
    [|82<state>,213<state>|];
    [|1<state>,216<state>|];
    [|1<state>,695<state>;58<state>,216<state>|];
    [|93<state>,217<state>|];
    [|1<state>,218<state>|];
    [|73<state>,222<state>|];
    [|73<state>,223<state>|];
    [|181<state>,224<state>|];
    [|4<state>,33<state>|];
    [|4<state>,225<state>|];
    [|182<state>,227<state>|];
    [|143<state>,228<state>|];
    [|181<state>,229<state>|];
    [|1<state>,763<state>|];
    [|137<state>,691<state>|];
    [|1<state>,736<state>|];
    [|185<state>,692<state>|];
    [|181<state>,702<state>|];
    [|1<state>,764<state>|];
    [|182<state>,694<state>|];
    [|182<state>,688<state>|];
    [|140<state>,694<state>|];
    [|99<state>,667<state>|];
    [|181<state>,712<state>|];
    [|4<state>,705<state>|];
    [|182<state>,735<state>|];
    [|99<state>,622<state>|];
    [|99<state>,696<state>|];
    [|1<state>,422<state>|];
    [|99<state>,682<state>|];
    [|99<state>,713<state>|];
    [|4<state>,669<state>|];
    [|143<state>,665<state>|];
    [|140<state>,653<state>|];
    [|4<state>,700<state>|];
    [|99<state>,680<state>|];
    [|4<state>,714<state>|];
    [|4<state>,676<state>|];
    [|4<state>,204<state>|];
    [|1<state>,197<state>|];
    [|1<state>,143<state>|];
    [|182<state>,697<state>|];
    [|1<state>,745<state>|];
    [|1<state>,748<state>|];
    [|1<state>,769<state>|];
    [||];
    [|66<state>,689<state>|];
    [||];
    [||];
    [||];
    [||];
    [||];
    [||];
    [||];
    [||];
    [|1<state>,187<state>|];
    [|1<state>,778<state>|];
    [|7<state>,209<state>|];
    [|1<state>,726<state>;8<state>,768<state>|];
    [|8<state>,768<state>|];
    [|1<state>,751<state>;9<state>,187<state>|];
    [|9<state>,187<state>|];
    [|2<state>,187<state>|];
    [|10<state>,213<state>|];
    [|12<state>,209<state>|];
    [|181<state>,187<state>|];
    [|181<state>,699<state>|];
    [|4<state>,187<state>|];
    [|4<state>,188<state>|];
    [|137<state>,187<state>|];
    [|137<state>,675<state>|];
    [|99<state>,187<state>|];
    [|99<state>,706<state>|];
    [|185<state>,187<state>|];
    [|140<state>,187<state>|];
    [|140<state>,704<state>|];
    [|182<state>,187<state>|];
    [|182<state>,710<state>|];
    [|143<state>,187<state>|];
    [|143<state>,711<state>|];
    [|186<state>,187<state>|];
    [|186<state>,662<state>|];
    [|25<state>,657<state>|];
    [|21<state>,187<state>|];
    [|70<state>,187<state>|];
    [|153<state>,187<state>|];
    [|170<state>,719<state>|];
    [|142<state>,187<state>|];
    [|96<state>,187<state>|];
    [|1<state>,232<state>;145<state>,187<state>|];
    [|145<state>,187<state>|];
    [|133<state>,189<state>|];
    [|134<state>,189<state>|];
    [|155<state>,189<state>|];
    [|180<state>,189<state>|];
    [|2<state>,189<state>|];
    [|100<state>,189<state>|];
    [|136<state>,189<state>|];
    [|132<state>,189<state>|];
    [|102<state>,189<state>|];
    [|156<state>,189<state>|];
    [|157<state>,189<state>|];
    [|103<state>,189<state>|];
    [|104<state>,189<state>|];
    [|98<state>,189<state>|];
    [|158<state>,189<state>|];
    [|149<state>,189<state>|];
    [|159<state>,189<state>|];
    [|39<state>,189<state>|];
    [|21<state>,189<state>|];
    [|106<state>,189<state>|];
    [|107<state>,189<state>|];
    [|15<state>,189<state>|];
    [|160<state>,189<state>|];
    [|161<state>,189<state>|];
    [|162<state>,189<state>|];
    [|163<state>,189<state>|];
    [|108<state>,189<state>|];
    [|105<state>,189<state>|];
    [|109<state>,189<state>|];
    [|110<state>,189<state>|];
    [|48<state>,189<state>|];
    [|111<state>,189<state>|];
    [|50<state>,189<state>|];
    [|112<state>,189<state>|];
    [|113<state>,189<state>|];
    [|43<state>,189<state>|];
    [|114<state>,189<state>|];
    [|115<state>,189<state>|];
    [|164<state>,189<state>|];
    [|165<state>,189<state>|];
    [|116<state>,189<state>|];
    [|144<state>,189<state>|];
    [|117<state>,189<state>|];
    [|118<state>,189<state>|];
    [|23<state>,189<state>|];
    [|141<state>,189<state>|];
    [|24<state>,189<state>|];
    [|3<state>,189<state>|];
    [|166<state>,189<state>|];
    [|167<state>,189<state>|];
    [|119<state>,189<state>|];
    [|168<state>,189<state>|];
    [|20<state>,189<state>|];
    [|120<state>,189<state>|];
    [|121<state>,189<state>|];
    [|169<state>,189<state>|];
    [|152<state>,189<state>|];
    [|153<state>,189<state>|];
    [|170<state>,189<state>|];
    [|122<state>,189<state>|];
    [|63<state>,189<state>|];
    [|123<state>,189<state>|];
    [|138<state>,189<state>|];
    [|19<state>,189<state>|];
    [|139<state>,189<state>|];
    [|171<state>,189<state>|];
    [|151<state>,189<state>|];
    [|13<state>,189<state>|];
    [|124<state>,189<state>|];
    [|154<state>,189<state>|];
    [|142<state>,189<state>|];
    [|125<state>,189<state>|];
    [|126<state>,189<state>|];
    [|172<state>,189<state>|];
    [|173<state>,189<state>|];
    [|174<state>,189<state>|];
    [|131<state>,189<state>|];
    [|127<state>,189<state>|];
    [|175<state>,189<state>|];
    [|22<state>,189<state>|];
    [|176<state>,189<state>|];
    [|177<state>,189<state>|];
    [|17<state>,189<state>|];
    [|0<state>,189<state>|];
    [|178<state>,189<state>|];
    [|128<state>,189<state>|];
    [|129<state>,189<state>|];
    [|5<state>,189<state>|];
    [|179<state>,189<state>|];
    [|16<state>,189<state>|];
    [|14<state>,189<state>|];
    [|18<state>,189<state>|];
    [|130<state>,189<state>|];
    [|150<state>,189<state>|];
    [|133<state>,190<state>|];
    [|134<state>,190<state>|];
    [|155<state>,190<state>|];
    [|180<state>,190<state>|];
    [|2<state>,190<state>|];
    [|100<state>,190<state>|];
    [|136<state>,190<state>|];
    [|132<state>,190<state>|];
    [|102<state>,190<state>|];
    [|156<state>,190<state>|];
    [|157<state>,190<state>|];
    [|103<state>,190<state>|];
    [|104<state>,190<state>|];
    [|98<state>,190<state>|];
    [|158<state>,190<state>|];
    [|149<state>,190<state>|];
    [|159<state>,190<state>|];
    [|39<state>,190<state>|];
    [|21<state>,190<state>|];
    [|106<state>,190<state>|];
    [|107<state>,190<state>|];
    [|15<state>,190<state>|];
    [|160<state>,190<state>|];
    [|161<state>,190<state>|];
    [|162<state>,190<state>|];
    [|163<state>,190<state>|];
    [|108<state>,190<state>|];
    [|105<state>,190<state>|];
    [|109<state>,190<state>|];
    [|110<state>,190<state>|];
    [|48<state>,190<state>|];
    [|111<state>,190<state>|];
    [|50<state>,190<state>|];
    [|112<state>,190<state>|];
    [|113<state>,190<state>|];
    [|43<state>,190<state>|];
    [|114<state>,190<state>|];
    [|115<state>,190<state>|];
    [|164<state>,190<state>|];
    [|165<state>,190<state>|];
    [|116<state>,190<state>|];
    [|144<state>,190<state>|];
    [|117<state>,190<state>|];
    [|118<state>,190<state>|];
    [|23<state>,190<state>|];
    [|141<state>,190<state>|];
    [|24<state>,190<state>|];
    [|3<state>,190<state>|];
    [|166<state>,190<state>|];
    [|167<state>,190<state>|];
    [|119<state>,190<state>|];
    [|168<state>,190<state>|];
    [|20<state>,190<state>|];
    [|120<state>,190<state>|];
    [|121<state>,190<state>|];
    [|169<state>,190<state>|];
    [|152<state>,190<state>|];
    [|153<state>,190<state>|];
    [|170<state>,190<state>|];
    [|122<state>,190<state>|];
    [|63<state>,190<state>|];
    [|123<state>,190<state>|];
    [|138<state>,190<state>|];
    [|19<state>,190<state>|];
    [|139<state>,190<state>|];
    [|171<state>,190<state>|];
    [|151<state>,190<state>|];
    [|13<state>,190<state>|];
    [|124<state>,190<state>|];
    [|154<state>,190<state>|];
    [|142<state>,190<state>|];
    [|125<state>,190<state>|];
    [|126<state>,190<state>|];
    [|172<state>,190<state>|];
    [|173<state>,190<state>|];
    [|174<state>,190<state>|];
    [|131<state>,190<state>|];
    [|127<state>,190<state>|];
    [|175<state>,190<state>|];
    [|22<state>,190<state>|];
    [|176<state>,190<state>|];
    [|177<state>,190<state>|];
    [|17<state>,190<state>|];
    [|0<state>,190<state>|];
    [|178<state>,190<state>|];
    [|128<state>,190<state>|];
    [|129<state>,190<state>|];
    [|5<state>,190<state>|];
    [|179<state>,190<state>|];
    [|16<state>,190<state>|];
    [|14<state>,190<state>|];
    [|18<state>,190<state>|];
    [|130<state>,190<state>|];
    [|150<state>,190<state>|];
    [||];
    [||];
    [|180<state>,1<state>|];
    [||];
    [||];
    [|133<state>,193<state>|];
    [|134<state>,193<state>|];
    [|155<state>,193<state>|];
    [|180<state>,193<state>|];
    [|2<state>,193<state>|];
    [|100<state>,193<state>|];
    [|136<state>,193<state>|];
    [|132<state>,193<state>|];
    [|102<state>,193<state>|];
    [|156<state>,193<state>|];
    [|157<state>,193<state>|];
    [|103<state>,193<state>|];
    [|104<state>,193<state>|];
    [|98<state>,193<state>|];
    [|158<state>,193<state>|];
    [|149<state>,193<state>|];
    [|159<state>,193<state>|];
    [|39<state>,193<state>|];
    [|21<state>,193<state>|];
    [|106<state>,193<state>|];
    [|107<state>,193<state>|];
    [|15<state>,193<state>|];
    [|160<state>,193<state>|];
    [|161<state>,193<state>|];
    [|162<state>,193<state>|];
    [|163<state>,193<state>|];
    [|108<state>,193<state>|];
    [|105<state>,193<state>|];
    [|109<state>,193<state>|];
    [|110<state>,193<state>|];
    [|48<state>,193<state>|];
    [|111<state>,193<state>|];
    [|50<state>,193<state>|];
    [|112<state>,193<state>|];
    [|113<state>,193<state>|];
    [|43<state>,193<state>|];
    [|114<state>,193<state>|];
    [|115<state>,193<state>|];
    [|164<state>,193<state>|];
    [|165<state>,193<state>|];
    [|116<state>,193<state>|];
    [|144<state>,193<state>|];
    [|117<state>,193<state>|];
    [|118<state>,193<state>|];
    [|23<state>,193<state>|];
    [|141<state>,193<state>|];
    [|24<state>,193<state>|];
    [|3<state>,193<state>|];
    [|166<state>,193<state>|];
    [|167<state>,193<state>|];
    [|119<state>,193<state>|];
    [|168<state>,193<state>|];
    [|20<state>,193<state>|];
    [|120<state>,193<state>|];
    [|121<state>,193<state>|];
    [|169<state>,193<state>|];
    [|152<state>,193<state>|];
    [|153<state>,193<state>|];
    [|170<state>,193<state>|];
    [|122<state>,193<state>|];
    [|63<state>,193<state>|];
    [|123<state>,193<state>|];
    [|138<state>,193<state>|];
    [|19<state>,193<state>|];
    [|139<state>,193<state>|];
    [|171<state>,193<state>|];
    [|151<state>,193<state>|];
    [|13<state>,193<state>|];
    [|124<state>,193<state>|];
    [|154<state>,193<state>|];
    [|142<state>,193<state>|];
    [|125<state>,193<state>|];
    [|126<state>,193<state>|];
    [|172<state>,193<state>|];
    [|173<state>,193<state>|];
    [|174<state>,193<state>|];
    [|131<state>,193<state>|];
    [|127<state>,193<state>|];
    [|175<state>,193<state>|];
    [|22<state>,193<state>|];
    [|176<state>,193<state>|];
    [|177<state>,193<state>|];
    [|17<state>,193<state>|];
    [|0<state>,193<state>|];
    [|178<state>,193<state>|];
    [|128<state>,193<state>|];
    [|129<state>,193<state>|];
    [|5<state>,193<state>|];
    [|179<state>,193<state>|];
    [|16<state>,193<state>|];
    [|14<state>,193<state>|];
    [|18<state>,193<state>|];
    [|130<state>,193<state>|];
    [|150<state>,193<state>|];
    [|1<state>,521<state>|];
    [||];
    [|133<state>,195<state>|];
    [|134<state>,195<state>|];
    [|155<state>,195<state>|];
    [|180<state>,195<state>|];
    [|2<state>,195<state>|];
    [|100<state>,195<state>|];
    [|136<state>,195<state>|];
    [|132<state>,195<state>|];
    [|102<state>,195<state>|];
    [|156<state>,195<state>|];
    [|157<state>,195<state>|];
    [|103<state>,195<state>|];
    [|104<state>,195<state>|];
    [|98<state>,195<state>|];
    [|158<state>,195<state>|];
    [|149<state>,195<state>|];
    [|159<state>,195<state>|];
    [|39<state>,195<state>|];
    [|21<state>,195<state>|];
    [|106<state>,195<state>|];
    [|107<state>,195<state>|];
    [|15<state>,195<state>|];
    [|160<state>,195<state>|];
    [|161<state>,195<state>|];
    [|162<state>,195<state>|];
    [|163<state>,195<state>|];
    [|108<state>,195<state>|];
    [|105<state>,195<state>|];
    [|109<state>,195<state>|];
    [|110<state>,195<state>|];
    [|48<state>,195<state>|];
    [|111<state>,195<state>|];
    [|50<state>,195<state>|];
    [|112<state>,195<state>|];
    [|113<state>,195<state>|];
    [|43<state>,195<state>|];
    [|114<state>,195<state>|];
    [|115<state>,195<state>|];
    [|164<state>,195<state>|];
    [|165<state>,195<state>|];
    [|116<state>,195<state>|];
    [|144<state>,195<state>|];
    [|117<state>,195<state>|];
    [|118<state>,195<state>|];
    [|23<state>,195<state>|];
    [|141<state>,195<state>|];
    [|24<state>,195<state>|];
    [|3<state>,195<state>|];
    [|166<state>,195<state>|];
    [|167<state>,195<state>|];
    [|119<state>,195<state>|];
    [|168<state>,195<state>|];
    [|20<state>,195<state>|];
    [|120<state>,195<state>|];
    [|121<state>,195<state>|];
    [|169<state>,195<state>|];
    [|152<state>,195<state>|];
    [|153<state>,195<state>|];
    [|170<state>,195<state>|];
    [|122<state>,195<state>|];
    [|63<state>,195<state>|];
    [|123<state>,195<state>|];
    [|138<state>,195<state>|];
    [|19<state>,195<state>|];
    [|139<state>,195<state>|];
    [|171<state>,195<state>|];
    [|151<state>,195<state>|];
    [|13<state>,195<state>|];
    [|124<state>,195<state>|];
    [|154<state>,195<state>|];
    [|142<state>,195<state>|];
    [|125<state>,195<state>|];
    [|126<state>,195<state>|];
    [|172<state>,195<state>|];
    [|173<state>,195<state>|];
    [|174<state>,195<state>|];
    [|131<state>,195<state>|];
    [|127<state>,195<state>|];
    [|175<state>,195<state>|];
    [|22<state>,195<state>|];
    [|176<state>,195<state>|];
    [|177<state>,195<state>|];
    [|17<state>,195<state>|];
    [|0<state>,195<state>|];
    [|178<state>,195<state>|];
    [|128<state>,195<state>|];
    [|129<state>,195<state>|];
    [|5<state>,195<state>|];
    [|179<state>,195<state>|];
    [|16<state>,195<state>|];
    [|14<state>,195<state>|];
    [|18<state>,195<state>|];
    [|130<state>,195<state>|];
    [|150<state>,195<state>|];
    [||];
    [|1<state>,187<state>|];
    [|1<state>,197<state>|];
    [|26<state>,197<state>|];
    [|30<state>,197<state>|];
    [|98<state>,197<state>|];
    [|39<state>,197<state>|];
    [|36<state>,197<state>|];
    [|15<state>,197<state>|];
    [|44<state>,197<state>|];
    [|47<state>,197<state>|];
    [|48<state>,197<state>|];
    [|50<state>,197<state>|];
    [|52<state>,197<state>|];
    [|43<state>,197<state>|];
    [|55<state>,197<state>|];
    [|164<state>,197<state>|];
    [|144<state>,197<state>|];
    [|59<state>,197<state>|];
    [|23<state>,197<state>|];
    [|60<state>,197<state>|];
    [|64<state>,197<state>|];
    [|168<state>,197<state>|];
    [|69<state>,197<state>|];
    [|169<state>,197<state>|];
    [|63<state>,197<state>|];
    [|138<state>,197<state>|];
    [|154<state>,197<state>|];
    [|83<state>,197<state>|];
    [|172<state>,197<state>|];
    [|175<state>,197<state>|];
    [|95<state>,197<state>|];
    [|5<state>,197<state>|];
    [|150<state>,197<state>|];
    [|1<state>,99<state>|];
    [|34<state>,198<state>|];
    [|37<state>,775<state>|];
    [|86<state>,777<state>|];
    [|1<state>,201<state>;8<state>,768<state>|];
    [|1<state>,202<state>|];
    [|1<state>,203<state>;9<state>,187<state>|];
    [|99<state>,205<state>|];
    [|1<state>,659<state>;32<state>,207<state>|];
    [|1<state>,660<state>;32<state>,207<state>|];
    [|32<state>,207<state>|];
    [|41<state>,207<state>|];
    [|38<state>,725<state>|];
    [|61<state>,738<state>|];
    [|167<state>,732<state>|];
    [|84<state>,208<state>|];
    [|39<state>,209<state>|];
    [|165<state>,209<state>|];
    [|18<state>,209<state>|];
    [|39<state>,734<state>|];
    [|1<state>,671<state>;56<state>,722<state>|];
    [|56<state>,722<state>|];
    [|71<state>,210<state>|];
    [|78<state>,743<state>|];
    [|160<state>,211<state>|];
    [|76<state>,211<state>|];
    [|16<state>,211<state>|];
    [|1<state>,753<state>;28<state>,213<state>|];
    [|1<state>,679<state>;28<state>,213<state>|];
    [|28<state>,213<state>|];
    [|39<state>,213<state>|];
    [|62<state>,213<state>|];
    [|151<state>,213<state>|];
    [|1<state>,684<state>;85<state>,213<state>|];
    [|1<state>,685<state>;85<state>,213<state>|];
    [|85<state>,213<state>|];
    [|92<state>,213<state>|];
    [|97<state>,213<state>|];
    [|31<state>,724<state>|];
    [|1<state>,690<state>;67<state>,715<state>|];
    [|67<state>,715<state>|];
    [|27<state>,215<state>|];
    [|149<state>,215<state>|];
    [|131<state>,215<state>|];
    [|39<state>,216<state>|];
    [|58<state>,216<state>|];
    [|23<state>,216<state>|];
    [|166<state>,216<state>|];
    [|79<state>,216<state>|];
    [|80<state>,216<state>|];
    [|177<state>,216<state>|];
    [|14<state>,216<state>|];
    [|21<state>,740<state>|];
    [|33<state>,731<state>|];
    [|96<state>,716<state>|];
    [|45<state>,733<state>|];
    [|89<state>,219<state>|];
    [|39<state>,220<state>|];
    [|33<state>,220<state>|];
    [|176<state>,220<state>|];
    [|90<state>,220<state>|];
    [|86<state>,220<state>|];
    [|40<state>,742<state>|];
    [|77<state>,741<state>|];
    [|91<state>,221<state>|];
    [|99<state>,226<state>|];
    [|140<state>,230<state>|];
    [|1<state>,718<state>|];
    [|1<state>,231<state>;145<state>,187<state>|];
    [|137<state>,421<state>|];
    [|1<state>,617<state>|];
    [|1<state>,746<state>|];
    [|4<state>,636<state>|];
    [|1<state>,650<state>|];
    [|99<state>,651<state>|];
    [|181<state>,652<state>|];
    [|1<state>,654<state>;8<state>,768<state>|];
    [|1<state>,655<state>|];
    [|1<state>,656<state>;9<state>,187<state>|];
    [|1<state>,730<state>|];
    [|1<state>,658<state>;32<state>,207<state>|];
    [|140<state>,661<state>|];
    [|181<state>,663<state>|];
    [|182<state>,664<state>|];
    [|4<state>,666<state>|];
    [|53<state>,670<state>|];
    [|1<state>,677<state>|];
    [|1<state>,678<state>;28<state>,213<state>|];
    [|181<state>,681<state>|];
    [|1<state>,683<state>|];
    [|140<state>,694<state>|];
    [|143<state>,698<state>|];
    [|143<state>,703<state>|];
    [|4<state>,708<state>|];
    [|1<state>,717<state>|];
    [|1<state>,720<state>|];
    [|1<state>,618<state>|];
    [|1<state>,721<state>|];
    [|1<state>,181<state>|];
    [|1<state>,723<state>|];
    [|1<state>,727<state>|];
    [|1<state>,728<state>;9<state>,187<state>|];
    [|1<state>,729<state>|];
    [|1<state>,737<state>;28<state>,213<state>|];
    [|1<state>,739<state>|];
    [|1<state>,744<state>|];
    [|1<state>,721<state>|];
    [|1<state>,747<state>|];
    [|1<state>,749<state>|];
    [|1<state>,752<state>|];
    [|1<state>,754<state>|];
    [|1<state>,755<state>|];
    [|1<state>,756<state>|];
    [|1<state>,757<state>|];
    [|1<state>,758<state>|];
    [|1<state>,759<state>|];
    [|1<state>,760<state>|];
    [|1<state>,761<state>|];
    [|1<state>,762<state>|];
    [|1<state>,764<state>|];
    [|1<state>,765<state>|];
    [|1<state>,766<state>|];
    [|1<state>,767<state>|];
    [|1<state>,770<state>|];
    [|1<state>,771<state>|];
    [|1<state>,772<state>|];
    [|1<state>,773<state>|];
    [|1<state>,774<state>|];
    [|1<state>,776<state>|] |]

let private startState = 101<state>

let private finalStates =
  new System.Collections.Generic.HashSet<int<state>>(
     [|187<state>;
       197<state>;
       618<state>;
       756<state>;
       721<state>;
       746<state>|])

let private nontermCount = 191

let private parserSource = new FSAParserSourceGLL (outNonterms, startState, finalStates, nontermCount, numIsTerminal, stateToNontermName, numOfAnyState, stateAndTokenToNewState)

let buildAbstract : (AbstractAnalysis.Common.BioParserInputGraph -> ParserCommon.ParseResult<_>) =
    Yard.Generators.GLL.AbstractParserWithoutTreeFSAInput.buildAbstract parserSource


