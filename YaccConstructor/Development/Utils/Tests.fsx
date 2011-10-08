let tests = 
  [{
     FullGrammarPath =  "../../../Tests/GNESCC/claret/braces_1/test_simple_braces.yrd"
     FullInputFilesPaths = 
       [
       "../../../Tests/GNESCC/claret/braces_1/test_simple_braces_1.yrd.in";
       "../../../Tests/GNESCC/claret/braces_1/test_simple_braces_2.yrd.in";
       "../../../Tests/GNESCC/claret/braces_1/test_simple_braces_3.yrd.in" ]
     FullLexerPath =  "  "
     ActionReplacement =  ("GNESCC.Actions","GNESCC.Actions_simple_braces")
     RegexpReplacement =  ("GNESCC.Regexp","GNESCC.Regexp_simple_braces")
     TablesReplacement =  ("GNESCCGenerator.Tables","GNESCCGenerator.Tables_simple_braces") };
   {
     FullGrammarPath =  "../../../Tests/GNESCC/claret/braces_2/test_simple_braces_2.yrd"
     FullInputFilesPaths = 
       [
       "../../../Tests/GNESCC/claret/braces_2/test_simple_braces_2_1.yrd.in";
       "../../../Tests/GNESCC/claret/braces_2/test_simple_braces_2_2.yrd.in";
       "../../../Tests/GNESCC/claret/braces_2/test_simple_braces_2_3.yrd.in" ]
     FullLexerPath =  "  "
     ActionReplacement =  ("GNESCC.Actions","GNESCC.Actions_simple_braces_2")
     RegexpReplacement =  ("GNESCC.Regexp","GNESCC.Regexp_simple_braces_2")
     TablesReplacement =  ("GNESCCGenerator.Tables","GNESCCGenerator.Tables_simple_braces_2") };
   {
     FullGrammarPath =  "../../../Tests/GNESCC/recursive_rules/test_rec_rule_1/test_rec_rule_1.yrd"
     FullInputFilesPaths = 
       []
     FullLexerPath =  "  "
     ActionReplacement =  ("GNESCC.Actions","GNESCC.Actions_rec_rule_1")
     RegexpReplacement =  ("GNESCC.Regexp","GNESCC.Regexp_rec_rule_1")
     TablesReplacement =  ("GNESCCGenerator.Tables","GNESCCGenerator.Tables_rec_rule_1") };
   {
     FullGrammarPath =  "../../../Tests/GNESCC/recursive_rules/test_rec_rule_2/test_rec_rule_2.yrd"
     FullInputFilesPaths = 
       []
     FullLexerPath =  "  "
     ActionReplacement =  ("GNESCC.Actions","GNESCC.Actions_rec_rule_2")
     RegexpReplacement =  ("GNESCC.Regexp","GNESCC.Regexp_rec_rule_2")
     TablesReplacement =  ("GNESCCGenerator.Tables","GNESCCGenerator.Tables_rec_rule_2") };
   {
     FullGrammarPath =  "../../../Tests/GNESCC/recursive_rules/test_rec_rule_3/test_rec_rule_3.yrd"
     FullInputFilesPaths = 
       []
     FullLexerPath =  "  "
     ActionReplacement =  ("GNESCC.Actions","GNESCC.Actions_rec_rule_3")
     RegexpReplacement =  ("GNESCC.Regexp","GNESCC.Regexp_rec_rule_3")
     TablesReplacement =  ("GNESCCGenerator.Tables","GNESCCGenerator.Tables_rec_rule_3") };
   {
     FullGrammarPath =  "../../../Tests/GNESCC/recursive_rules/test_regexp/test_regexp.yrd"
     FullInputFilesPaths = 
       []
     FullLexerPath =  "  "
     ActionReplacement =  ("GNESCC.Actions","GNESCC.Actions_regexp")
     RegexpReplacement =  ("GNESCC.Regexp","GNESCC.Regexp_regexp")
     TablesReplacement =  ("GNESCCGenerator.Tables","GNESCCGenerator.Tables_regexp") };
   {
     FullGrammarPath =  "../../../Tests/GNESCC/test_1/test_1.yrd"
     FullInputFilesPaths = 
       [
       "../../../Tests/GNESCC/test_1/test_1_0.yrd.in";
       "../../../Tests/GNESCC/test_1/test_1_1.yrd.in";
       "../../../Tests/GNESCC/test_1/test_1_2.yrd.in";
       "../../../Tests/GNESCC/test_1/test_1_3.yrd.in" ]
     FullLexerPath =  "  "
     ActionReplacement =  ("GNESCC.Actions","GNESCC.Actions_1")
     RegexpReplacement =  ("GNESCC.Regexp","GNESCC.Regexp_1")
     TablesReplacement =  ("GNESCCGenerator.Tables","GNESCCGenerator.Tables_1") };
   {
     FullGrammarPath =  "../../../Tests/GNESCC/test_2/test_2.yrd"
     FullInputFilesPaths = 
       []
     FullLexerPath =  "  "
     ActionReplacement =  ("GNESCC.Actions","GNESCC.Actions_2")
     RegexpReplacement =  ("GNESCC.Regexp","GNESCC.Regexp_2")
     TablesReplacement =  ("GNESCCGenerator.Tables","GNESCCGenerator.Tables_2") };
   {
     FullGrammarPath =  "../../../Tests/GNESCC/test_3/test_3.yrd"
     FullInputFilesPaths = 
       [ "../../../Tests/GNESCC/test_3/test_3_1.yrd.in" ]
     FullLexerPath =  "  "
     ActionReplacement =  ("GNESCC.Actions","GNESCC.Actions_3")
     RegexpReplacement =  ("GNESCC.Regexp","GNESCC.Regexp_3")
     TablesReplacement =  ("GNESCCGenerator.Tables","GNESCCGenerator.Tables_3") };
   {
     FullGrammarPath =  "../../../Tests/GNESCC/test_4/test_4.yrd"
     FullInputFilesPaths = 
       [
       "../../../Tests/GNESCC/test_4/test_4_0.yrd.in";
       "../../../Tests/GNESCC/test_4/test_4_1.yrd.in";
       "../../../Tests/GNESCC/test_4/test_4_2.yrd.in";
       "../../../Tests/GNESCC/test_4/test_4_3.yrd.in" ]
     FullLexerPath =  "  "
     ActionReplacement =  ("GNESCC.Actions","GNESCC.Actions_4")
     RegexpReplacement =  ("GNESCC.Regexp","GNESCC.Regexp_4")
     TablesReplacement =  ("GNESCCGenerator.Tables","GNESCCGenerator.Tables_4") };
   {
     FullGrammarPath =  "../../../Tests/GNESCC/test_alt/test_alt.yrd"
     FullInputFilesPaths = 
       [
       "../../../Tests/GNESCC/test_alt/test_alt_1.yrd.in";
       "../../../Tests/GNESCC/test_alt/test_alt_2.yrd.in";
       "../../../Tests/GNESCC/test_alt/test_alt_3.yrd.in" ]
     FullLexerPath =  "  "
     ActionReplacement =  ("GNESCC.Actions","GNESCC.Actions_alt")
     RegexpReplacement =  ("GNESCC.Regexp","GNESCC.Regexp_alt")
     TablesReplacement =  ("GNESCCGenerator.Tables","GNESCCGenerator.Tables_alt") };
   {
     FullGrammarPath =  "../../../Tests/GNESCC/test_alt_in_cls/test_alt_in_cls.yrd"
     FullInputFilesPaths = 
       [
       "../../../Tests/GNESCC/test_alt_in_cls/test_alt_in_cls_1.yrd.in";
       "../../../Tests/GNESCC/test_alt_in_cls/test_alt_in_cls_2.yrd.in";
       "../../../Tests/GNESCC/test_alt_in_cls/test_alt_in_cls_3.yrd.in";
       "../../../Tests/GNESCC/test_alt_in_cls/test_alt_in_cls_4.yrd.in";
       "../../../Tests/GNESCC/test_alt_in_cls/test_alt_in_cls_5.yrd.in";
       "../../../Tests/GNESCC/test_alt_in_cls/test_alt_in_cls_6.yrd.in";
       "../../../Tests/GNESCC/test_alt_in_cls/test_alt_in_cls_7.yrd.in";
       "../../../Tests/GNESCC/test_alt_in_cls/test_alt_in_cls_8.yrd.in" ]
     FullLexerPath =  "  "
     ActionReplacement =  ("GNESCC.Actions","GNESCC.Actions_alt_in_cls")
     RegexpReplacement =  ("GNESCC.Regexp","GNESCC.Regexp_alt_in_cls")
     TablesReplacement =  ("GNESCCGenerator.Tables","GNESCCGenerator.Tables_alt_in_cls") };
   {
     FullGrammarPath =  "../../../Tests/GNESCC/test_arithm_glr/test_arithm_glr.yrd"
     FullInputFilesPaths = 
       [
       "../../../Tests/GNESCC/test_arithm_glr/test_arithm_glr_1.yrd.in";
       "../../../Tests/GNESCC/test_arithm_glr/test_arithm_glr_2.yrd.in";
       "../../../Tests/GNESCC/test_arithm_glr/test_arithm_glr_3.yrd.in";
       "../../../Tests/GNESCC/test_arithm_glr/test_arithm_glr_4.yrd.in";
       "../../../Tests/GNESCC/test_arithm_glr/test_arithm_glr_5.yrd.in";
       "../../../Tests/GNESCC/test_arithm_glr/test_arithm_glr_6.yrd.in" ]
     FullLexerPath =  "  "
     ActionReplacement =  ("GNESCC.Actions","GNESCC.Actions_arithm_glr")
     RegexpReplacement =  ("GNESCC.Regexp","GNESCC.Regexp_arithm_glr")
     TablesReplacement =  ("GNESCCGenerator.Tables","GNESCCGenerator.Tables_arithm_glr") };
   {
     FullGrammarPath =  "../../../Tests/GNESCC/test_checker_on_glr/test_checker_on_glr.yrd"
     FullInputFilesPaths = 
       [
       "../../../Tests/GNESCC/test_checker_on_glr/test_checker_on_glr_1.yrd.in";
       "../../../Tests/GNESCC/test_checker_on_glr/test_checker_on_glr_2.yrd.in";
       "../../../Tests/GNESCC/test_checker_on_glr/test_checker_on_glr_3.yrd.in";
       "../../../Tests/GNESCC/test_checker_on_glr/test_checker_on_glr_4.yrd.in" ]
     FullLexerPath =  "  "
     ActionReplacement =  ("GNESCC.Actions","GNESCC.Actions_checker_on_glr")
     RegexpReplacement =  ("GNESCC.Regexp","GNESCC.Regexp_checker_on_glr")
     TablesReplacement =  ("GNESCCGenerator.Tables","GNESCCGenerator.Tables_checker_on_glr") };
   {
     FullGrammarPath =  "../../../Tests/GNESCC/test_cls/test_cls.yrd"
     FullInputFilesPaths = 
       [
       "../../../Tests/GNESCC/test_cls/test_cls_1.yrd.in";
       "../../../Tests/GNESCC/test_cls/test_cls_2.yrd.in";
       "../../../Tests/GNESCC/test_cls/test_cls_3.yrd.in" ]
     FullLexerPath =  "  "
     ActionReplacement =  ("GNESCC.Actions","GNESCC.Actions_cls")
     RegexpReplacement =  ("GNESCC.Regexp","GNESCC.Regexp_cls")
     TablesReplacement =  ("GNESCCGenerator.Tables","GNESCCGenerator.Tables_cls") };
   {
     FullGrammarPath =  "../../../Tests/GNESCC/test_cls_with_head/test_cls_with_head.yrd"
     FullInputFilesPaths = 
       [
       "../../../Tests/GNESCC/test_cls_with_head/test_cls_with_head_1.yrd.in";
       "../../../Tests/GNESCC/test_cls_with_head/test_cls_with_head_2.yrd.in" ]
     FullLexerPath =  "  "
     ActionReplacement =  ("GNESCC.Actions","GNESCC.Actions_cls_with_head")
     RegexpReplacement =  ("GNESCC.Regexp","GNESCC.Regexp_cls_with_head")
     TablesReplacement =  ("GNESCCGenerator.Tables","GNESCCGenerator.Tables_cls_with_head") };
   {
     FullGrammarPath =  "../../../Tests/GNESCC/test_cls_with_tail/test_cls_with_tail.yrd"
     FullInputFilesPaths = 
       [
       "../../../Tests/GNESCC/test_cls_with_tail/test_cls_with_tail_1.yrd.in";
       "../../../Tests/GNESCC/test_cls_with_tail/test_cls_with_tail_2.yrd.in" ]
     FullLexerPath =  "  "
     ActionReplacement =  ("GNESCC.Actions","GNESCC.Actions_cls_with_tail")
     RegexpReplacement =  ("GNESCC.Regexp","GNESCC.Regexp_cls_with_tail")
     TablesReplacement =  ("GNESCCGenerator.Tables","GNESCCGenerator.Tables_cls_with_tail") };
   {
     FullGrammarPath =  "../../../Tests/GNESCC/test_l_attr/test_l_attr.yrd"
     FullInputFilesPaths = 
       [ "../../../Tests/GNESCC/test_l_attr/test_l_attr_1.yrd.in" ]
     FullLexerPath =  "  "
     ActionReplacement =  ("GNESCC.Actions","GNESCC.Actions_l_attr")
     RegexpReplacement =  ("GNESCC.Regexp","GNESCC.Regexp_l_attr")
     TablesReplacement =  ("GNESCCGenerator.Tables","GNESCCGenerator.Tables_l_attr") };
   {
     FullGrammarPath =  "../../../Tests/GNESCC/test_opt/test_opt.yrd"
     FullInputFilesPaths = 
       [
       "../../../Tests/GNESCC/test_opt/test_opt_1.yrd.in";
       "../../../Tests/GNESCC/test_opt/test_opt_2.yrd.in" ]
     FullLexerPath =  "  "
     ActionReplacement =  ("GNESCC.Actions","GNESCC.Actions_opt")
     RegexpReplacement =  ("GNESCC.Regexp","GNESCC.Regexp_opt")
     TablesReplacement =  ("GNESCCGenerator.Tables","GNESCCGenerator.Tables_opt") };
   {
     FullGrammarPath =  "../../../Tests/GNESCC/test_reduce_reduce/test_reduce_reduce.yrd"
     FullInputFilesPaths = 
       [ "../../../Tests/GNESCC/test_reduce_reduce/test_reduce_reduce_1.yrd.in" ]
     FullLexerPath =  "  "
     ActionReplacement =  ("GNESCC.Actions","GNESCC.Actions_reduce_reduce")
     RegexpReplacement =  ("GNESCC.Regexp","GNESCC.Regexp_reduce_reduce")
     TablesReplacement =  ("GNESCCGenerator.Tables","GNESCCGenerator.Tables_reduce_reduce") };
   {
     FullGrammarPath =  "../../../Tests/GNESCC/test_seq/test_seq.yrd"
     FullInputFilesPaths = 
       [ "../../../Tests/GNESCC/test_seq/test_seq_1.yrd.in" ]
     FullLexerPath =  "  "
     ActionReplacement =  ("GNESCC.Actions","GNESCC.Actions_seq")
     RegexpReplacement =  ("GNESCC.Regexp","GNESCC.Regexp_seq")
     TablesReplacement =  ("GNESCCGenerator.Tables","GNESCCGenerator.Tables_seq") };
   {
     FullGrammarPath =  "../../../Tests/GNESCC/test_seq_2/test_seq_2.yrd"
     FullInputFilesPaths = 
       [ "../../../Tests/GNESCC/test_seq_2/test_seq_2_1.yrd.in" ]
     FullLexerPath =  "  "
     ActionReplacement =  ("GNESCC.Actions","GNESCC.Actions_seq_2")
     RegexpReplacement =  ("GNESCC.Regexp","GNESCC.Regexp_seq_2")
     TablesReplacement =  ("GNESCCGenerator.Tables","GNESCCGenerator.Tables_seq_2") };
   {
     FullGrammarPath =  "../../../Tests/GNESCC/test_simple_checker/test_simple_checker.yrd"
     FullInputFilesPaths = 
       [
       "../../../Tests/GNESCC/test_simple_checker/test_simple_checker_1.yrd.in";
       "../../../Tests/GNESCC/test_simple_checker/test_simple_checker_2.yrd.in" ]
     FullLexerPath =  "  "
     ActionReplacement =  ("GNESCC.Actions","GNESCC.Actions_simple_checker")
     RegexpReplacement =  ("GNESCC.Regexp","GNESCC.Regexp_simple_checker")
     TablesReplacement =  ("GNESCCGenerator.Tables","GNESCCGenerator.Tables_simple_checker") };
   {
     FullGrammarPath =  "../../../Tests/GNESCC/test_summator_1/test_summator_1.yrd"
     FullInputFilesPaths = 
       [
       "../../../Tests/GNESCC/test_summator_1/test_summator_1_1.yrd.in";
       "../../../Tests/GNESCC/test_summator_1/test_summator_1_2.yrd.in";
       "../../../Tests/GNESCC/test_summator_1/test_summator_1_3.yrd.in";
       "../../../Tests/GNESCC/test_summator_1/test_summator_1_4.yrd.in" ]
     FullLexerPath =  "  "
     ActionReplacement =  ("GNESCC.Actions","GNESCC.Actions_summator_1")
     RegexpReplacement =  ("GNESCC.Regexp","GNESCC.Regexp_summator_1")
     TablesReplacement =  ("GNESCCGenerator.Tables","GNESCCGenerator.Tables_summator_1") };]
