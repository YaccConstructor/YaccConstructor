yard_expression_2:  (
      expr_part
    | NUMBER ) bin_op (
      expr_part
    | NUMBER );

yard_expression_extended_1:  yard_expression_2;

+s:  yard_expression_extended_1;