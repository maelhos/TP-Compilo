type ast =
  | Imm of int (* valeur immediate *)
  | Arg of int (* reference au nieme argument *)
  | Add of (ast * ast) (* addition des deux sous-arbres *)
  | Sub of (ast * ast) (* soustraction *)
  | Mul of (ast * ast) (* multiplication *)
  | Div of (ast * ast) (* division *);;

(* exemples :
  ast de [ x y ] ( x + y ) / 2 
*)
let ast1 = Div(Add(Arg 0, Arg 1), Imm 2);;

(* 
  ast de [ x ] x + 2*5 
*)
let ast2 = Add(Arg 0, Mul(Imm 2, Imm 5));;


(* Erreur associee a la non-implementation : *)
exception CompilerError of string;;



let rec print_ast (arbre: ast) : unit =
   match arbre with
   | Imm x -> print_int x
   | Arg x -> print_string "arg["; print_int x; print_string "]"
   | Add(x,y) -> print_ast x; print_char '+'; print_ast y
   | Sub(x,y) -> begin match y with
                | Imm _ | Arg _ -> print_ast x; print_char '-'; print_ast y
                | _ -> print_ast x; print_char '-'; print_char '('; print_ast y; print_char ')'
   end
   | Mul(x,y) -> begin match x with
                | Imm _ | Arg _ -> print_ast x
                |_ -> print_char '('; print_ast x; print_char ')';
                print_char '*';
                match y with
                | Imm _ | Arg _ -> print_ast y
                |_ -> print_char '('; print_ast y; print_char ')'
   end
   | Div(x,y) -> begin match x with
                | Imm _ | Arg _ -> print_ast x
                |_ -> print_char '('; print_ast x; print_char ')';
                print_char '/';
                match y with
                | Imm _ | Arg _ -> print_ast y
                |_ -> print_char '('; print_ast y; print_char ')'
   end;;
    

print_ast ast1;;
print_newline ();;
print_ast ast2;;
print_newline ();;
