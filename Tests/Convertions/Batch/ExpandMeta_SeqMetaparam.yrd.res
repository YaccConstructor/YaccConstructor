yard_rule_1:  NUMBER;

yard_rule_3: 
      expr_part
    | yard_rule_1;

yard_rule_expression_4:  yard_rule_3 bin_op yard_rule_3;

yard_rule_expression_extended_2:  yard_rule_expression_4;

+s:  yard_rule_expression_extended_2;
