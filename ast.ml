type ast =
        | Imm of int (* valeur immediate *)
        | Arg of int (* reference au nieme argument *)
        | Add of (ast * ast) (* addition des deux sous-arbres *)
        | Sub of (ast * ast) (* soustraction *)
        | Mul of (ast * ast) (* multiplication *)
        | Div of (ast * ast) (* division *);;

(* exemples : *)
(* 
   ast de [ x y ] ( x + y ) / 2 
*)
let ast1 = Div(Add(Arg 0, Arg 1), Imm 2);;

(* 
   ast de [ x ] x + 2*5 
*)
let ast2 = Add(Arg 0, Mul(Imm 2, Imm 5));;


(* Erreur associee a la non-implementation : *)
exception CompilerError of string;;


(* --- Le lexer --- *)
let explode code =
        (* Entree :
            Une chaine de caracteres representant une fonction dans le langage.
           Sortie :
            Le tableau de caracteres associe a la chaine.
           *)
        raise (CompilerError "Question1 : todo");;


let tokenize code =
        (* Entree :
            Une chaine de caracteres representant une fonction dans le langage.
           Sortie :
            Une liste de tokens associes au code.
           *)
        raise (CompilerError "tokenizer : todo");;




let pass1 code =
        (* Entree :
            Une chaine de caracteres representant une fonction dans le langage.
           Sortie :
            L'AST associe a la fonction. 
        *)
        raise (CompilerError "pass1 : todo");;

let pass2 (tree:ast) -> ast =
        (* Entree :
            L'AST du premier pass.
           Sortie :
            L'AST simplifie selon la specification du TP.
           *)
        raise (CompilerError "pass2 : todo");;

let pass3 (tree:ast) -> string list =
        (* Entree :
            L'AST simplifie du second pass.
           Sortie :
            Une liste de directives assembleur.
           *)
        raise (CompilerError "pass3 : todo");;
