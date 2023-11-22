
let rec parseS (tl: Token.token list) : Ast.ast * Token.token list =
  match tl with
  | LBRA::l1 -> let strings, tl1 = parseJ l1 in (
    let mll = List.length strings - 1 in
    let assoc_string = List.mapi (fun i str -> (str, mll - i)) strings in
    match tl1 with
    | RBRA::l2 -> parseE l2 assoc_string
    | _ -> failwith "Erreur de parsing S"
  )
  | _ -> failwith "Erreur de parsing S"

and parseJ (tl: Token.token list) : string list * Token.token list =
  let rec auxF (tl: Token.token list) (sl: string list) : string list * Token.token list =
    match tl with
    | (Token.VAR s)::t -> auxF t (s::sl)
    | Token.COMMA::(Token.VAR s)::t -> auxF t (s::sl)
    | Token.RBRA::_ -> (sl, tl)
    | _ -> failwith "Erreur de parsing J"
  in auxF tl []

and parseE (tl: Token.token list) (args: (string * int) list) : Ast.ast * Token.token list =
  let _ast, sl = parseT tl args in
  let ast_left = ref _ast in
  let rec auxE (tl: Token.token list) : Ast.ast * Token.token list =
    match tl with
    | Token.ADD::t -> let ast_right, sl2 = parseT t args in 
      ast_left := Ast.Add (!ast_left, ast_right); auxE sl2
    | Token.SUB::t -> let ast_right, sl2 = parseT t args in 
      ast_left := Ast.Sub (!ast_left, ast_right); auxE sl2
    | _ -> (!ast_left, tl)
  in auxE sl

and parseT (tl: Token.token list) (args: (string * int) list) : Ast.ast * Token.token list =
  let _ast, sl = parseF tl args in
  let ast_left = ref _ast in
  let rec auxT (tl: Token.token list) : Ast.ast * Token.token list =
    match tl with
    | Token.MUL::t -> let ast_right, sl2 = parseF t args in 
      ast_left := Ast.Mul (!ast_left, ast_right); auxT sl2
    | Token.DIV::t -> let ast_right, sl2 = parseF t args in 
      ast_left := Ast.Div (!ast_left, ast_right); auxT sl2
    | _ -> (!ast_left, tl)
  in auxT sl
and parseF (tl: Token.token list) (args: (string * int) list) : Ast.ast * Token.token list =
  match tl with
  | (Token.CONST n)::sl -> (Ast.Imm n, sl)
  | (Token.VAR str)::sl -> (Ast.Arg (List.assoc str args), sl)
  | Token.LPAR::sl -> let ast1, sl1 = parseE sl args in (
    match sl1 with
    | Token.RPAR::tl -> (ast1, tl)
    | _ -> failwith "parenthèse non fermée F"
  )
  | _ -> failwith "Erreur de parser F";;

  let parse (tl: Token.token list) : Ast.ast =
    let ast1, tkns = parseS tl in
    match tkns with
    | [EOF] -> ast1
    | _ -> failwith "Erreur de parsing, il reste des symboles non parsé"