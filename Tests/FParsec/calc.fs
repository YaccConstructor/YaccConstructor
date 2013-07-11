module calc

open FParsec.Primitives
let rec public expr   = yard_rule_binExpr_4  |>> fun (res) -> ( res )   

 and private yard_rule_binExpr_4   = term  >>= fun (l ) -> (many ( attempt(termOp  >>= fun (op ) -> (term  |>> fun (r) -> ( op,r ) )  )) |>> fun (r) -> (List.fold (fun l (op,r) -> op l r) l r ) )  

 and private termOp   = (attempt (Lexer.pPLUS |>> fun (_) -> ( (+) )   )) <|> (Lexer.pMINUS |>> fun (_) -> ( (-) )   )

 and private term   = yard_rule_binExpr_5  |>> fun (res) -> ( res )   

 and private yard_rule_binExpr_5   = factor  >>= fun (l ) -> (many ( attempt(factorOp  >>= fun (op ) -> (factor  |>> fun (r) -> ( op,r ) )  )) |>> fun (r) -> (List.fold (fun l (op,r) -> op l r) l r ) )  

 and private factorOp   = (attempt (Lexer.pMULT |>> fun (_) -> ( ( * ) )   )) <|> (Lexer.pDIV |>> fun (_) -> ( (/) )   )

 and private factor   = yard_rule_binExpr_6  |>> fun (res) -> ( res )   

 and private yard_rule_binExpr_6   = powExpr  >>= fun (l ) -> (many ( attempt(powOp  >>= fun (op ) -> (powExpr  |>> fun (r) -> ( op,r ) )  )) |>> fun (r) -> (List.fold (fun l (op,r) -> op l r) l r ) )  

 and private powOp   = Lexer.pPOW |>> fun (_) -> ( ( ** ) )   

 and private powExpr   = (attempt (Lexer.pNUMBER |>> fun (n) -> ( n )   )) <|> (Lexer.pLBRACE >>= fun (_ ) -> (expr  >>= fun (e ) -> (Lexer.pRBRACE |>> fun (_) -> ( e ) ))  )