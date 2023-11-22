type token =
  | ADD | SUB | MUL | DIV | LPAR | RPAR | RBRA | LBRA
  | VAR of string
  | CONST of int
  | EOF
  | COMMA;;


let isnumber = function
  | '0' .. '9' -> true
  | _ -> false;;

let islowercasealpha = function
  | 'a' .. 'z' -> true
  | _ -> false;;

(* --- Le lexer --- *)
let rec explode (code: string) : char list =
  (* Entree :
      Une chaine de caracteres representant une fonction dans le langage.
     Sortie :
      Le tableau de caracteres associe a la chaine.
     *)
  if String.length code = 0 then []
  else
    let following = String.sub code 1 ((String.length code) - 1) in
    [code.[0]] @ explode following;;

let string_of_char = String.make 1;;

let isspecial t = List.mem t [' '; '['; ']'; '('; ')'; '+'; '-'; '*'; '/'; ','];;

let rec tokenize (code : char list) (liste_tokens : token list) : token list =
  match code with
  | [] -> EOF::liste_tokens
  | ' '::q -> tokenize q liste_tokens
  | '['::q -> tokenize q (RBRA::liste_tokens)
  | ']'::q -> tokenize q (LBRA::liste_tokens)
  | '('::q -> tokenize q (LPAR::liste_tokens)
  | ')'::q -> tokenize q (RPAR::liste_tokens)
  | '+'::q -> tokenize q (ADD::liste_tokens)
  | '-'::q -> tokenize q (SUB::liste_tokens)
  | '*'::q -> tokenize q (MUL::liste_tokens)
  | '/'::q -> tokenize q (DIV::liste_tokens)
  | ','::q -> tokenize q (COMMA::liste_tokens)
  | t::q when isnumber t -> lexZ q (string_of_char t) liste_tokens
  | t::q when islowercasealpha t -> lexW q (string_of_char t) liste_tokens
  | _ -> failwith "invalid character used"

and lexW (code : char list) (token : string) (liste_tokens : token list) : token list =
  match code with
  | [] -> EOF::(VAR token)::liste_tokens
  | t::q when islowercasealpha t -> lexW q (token ^ (string_of_char t)) liste_tokens
  | t::_ when isspecial t -> tokenize code ((VAR token)::liste_tokens)
  | _ -> failwith "invalid argument name"

and lexZ (code : char list) (token : string) (liste_tokens : token list) : token list =
  match code with
  | [] -> EOF::(CONST (int_of_string token))::liste_tokens
  | t::q when isnumber t -> lexZ q (token ^ (string_of_char t)) liste_tokens
  | t::_ when isspecial t -> tokenize code ((CONST (int_of_string token))::liste_tokens)
  | _ -> failwith "invalid number";;


let lex (code: string) : token list =
  List.rev (tokenize (explode code) []);;

(* -- fonctions d'affichage -- *)
let token_to_string (t : token) : string =
  match t with
  | ADD -> "+"
  | SUB -> "-"
  | MUL -> "*"
  | DIV -> "/"
  | LPAR -> "("
  | RPAR -> ")"
  | RBRA -> "["
  | LBRA -> "]"
  | VAR x -> x
  | CONST x -> (string_of_int x)
  | EOF -> ""
  | COMMA -> ",";;

let print_tokenlist (t : token list) : unit =
  List.iter (fun x -> print_string (token_to_string x)) t;;
