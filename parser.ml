
let rec parseS (tl: Token.token list) : string list * Ast.ast * Token.token list =
  match tl with
  | RBRA::l1 -> let strings, tl1 = parseF l1 in (
    let assoc_string = List.mapi (fun i str -> (str, i)) strings in
    match tl1 with
    | LBRA::l2 -> let ast1, tl2 = parseE l2 assoc_string in (strings, ast1 ,tl2)
    | _ -> failwith "Erreur de parsing"
  )
  | _ -> failwith "Erreur de parsing"

and parseF (tl: Token.token list) : string list * Token.token list =
  let rec aux (tl: Token.token list) (sl: stringlist): string list * Token.token list =
    match tl with
    | (Token.VAR s)::t -> aux t (s::sl)
    | Token.COMMA::(Token.VAR s)::t -> aux t (s::sl)
    | Token.LBRA::t -> (sl, l)
    | _ -> failwith "Erreur de parsing"
  in aux tl []

and parseE (tl: Token.token list) (args: (string * int) list) : Ast.ast * Token.token list =
  let _ast, sl = parseT tl args in
  let ast_left = ref _ast in
  let rec aux (tl: Token.token list) : Ast.ast * Token.token list =
    match sl with
    | Token.ADD::t -> let ast_right, sl2 = parseT t args in 
      ast_left := Ast.Add (!ast_left, ast_right); aux sl2
    | Token.SUB::t -> let ast_right, sl2 = parseT t args in 
      ast_left := Ast.Sub (!ast_left, ast_right); aux sl2
    | _ -> (!ast_left, sl)

and parseT (tl: Token.token list) (args: (string * int) list) : Ast.ast * Token.token list =
  let _ast, sl = parseF tl args in
  let ast_left = ref _ast in
  let rec aux (tl: Token.token list) : Ast.ast * Token.token list =
    match sl with
    | Token.MUL::t -> let ast_right, sl2 = parseF t args in 
      ast_left := Ast.Add (!ast_left, ast_right); aux sl2
    | Token.DIV::t -> let ast_right, sl2 = parseF t args in 
      ast_left := Ast.Sub (!ast_left, ast_right); aux sl2
    | _ -> (!ast_left, sl)

and parseF (tl: Token.token list) (args: (string * int) list) : Ast.ast * Token.token list =
  match tl with
  | (Token.CONST n)::tl -> Ast.Imm (n, tl)
  | (Token.VAR str)::tl -> Ast.Arg ((List.assoc str args), tl)
  | Token.LPAR::tl -> let ast1, sl = parseE tl args in (
    match sl with
    | Token.RPAR::tl -> (ast1, tl)
    | _ -> failwith "parenthèse non fermée"
  )
  | _ -> failwith "Erreur de parser";;