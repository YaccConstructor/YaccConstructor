exprlist
  : ( assignment_statement )* EOF!
  ;
assignment_statement
  : assignment SEMICOLON!
  ;

assignment
  : (IDENT ASSIGN )? expr
  ;
