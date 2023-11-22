
let test_code1 = "[ xaxa, y ] xaxa + y * (xaxa - y)";;

(* 1. On reprint post-lexage, on doit donc retrouver la même chose aux espaces près*)
Token.print_tokenlist (Token.lex test_code1);;
print_newline ();;
Token.print_tokenlist (Token.lex "[ x ] x * 9");;
print_newline ();;








(* 2. Compilation *)
let prgm = Compiler.compile test_code1;;
Simulation.print_prgm prgm;;