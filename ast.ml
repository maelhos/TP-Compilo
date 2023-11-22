type ast =
  | Imm of int (* valeur immediate *)
  | Arg of int (* reference au nieme argument *)
  | Add of ast * ast (* addition des deux sous-arbres *)
  | Sub of ast * ast (* soustraction des deux sous-arbres *)
  | Mul of ast * ast (* multiplication des deux sous-arbres *)
  | Div of ast * ast (* division des deux sous-arbres *);;

let rec print_ast (arbre: ast) : unit =
  match arbre with
  | Imm x -> print_int x
  | Arg x -> print_string "arg["; print_int x; print_string "]"
  | Add(x, y) -> print_ast x; print_char '+'; print_ast y
  | Sub(x, y) -> begin 
    match y with
    | Imm _ | Arg _ -> print_ast x; print_char '-'; print_ast y
    | _ -> print_ast x; print_char '-'; print_char '('; print_ast y; print_char ')'
    end
  | Mul(x, y) -> begin 
    (match x with
    | Imm _ | Arg _ -> print_ast x
    | _ -> print_char '('; print_ast x; print_char ')');
    print_char '*';
    match y with
    | Imm _ | Arg _ -> print_ast y
    | _ -> print_char '('; print_ast y; print_char ')'
    end
  | Div(x, y) -> begin 
    (match x with
    | Imm _ | Arg _ -> print_ast x
    |_ -> print_char '('; print_ast x; print_char ')');
    print_char '/';
    match y with
    | Imm _ | Arg _ -> print_ast y
    |_ -> print_char '('; print_ast y; print_char ')'
    end;;
    
