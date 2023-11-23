
let test_code1 = "[ xaxa, y ] xaxa + y * (xaxa - y)";;
let test_code2 = "[ x, y, z, t ] x * y / z + 1 / t - 77*y + 17 * (2 + 5 - 7 * 2*y)";;
let test_codeez = "[ x, y ] x + y * x";;
(* 1. On reprint post-lexage, on doit donc retrouver la même chose aux espaces près*)
let tokens = Token.lex test_code1;;
let tokens2 = Token.lex test_code2;;

print_string "1. Token test :\n";;
Token.print_tokenlist tokens;;
Token.print_tokenlist tokens2;;

(* 2. ///////////////////////////////// AST ///////////////////////////////// *)
(* exemples :
  ast de [ x y ] ( x + y ) / 2 
*)
print_string "\n2. Ast test :\n";;
let ast1 = Ast.Div(Ast.Add(Ast.Arg 0, Ast.Arg 1), Ast.Imm 2);;
Ast.print_ast ast1;;
print_newline ();;
(* 
  ast de [ x ] x + 2*5 
*)
let ast2 = Ast.Add(Ast.Arg 0, Ast.Mul(Ast.Imm 2, Ast.Imm 5));;
Ast.print_ast ast2;;
print_newline ();;


(* 3. ///////////////////////////////// Parser ///////////////////////////////// *)
print_string "\n3. Parser test :\n";;
let ast3 = Parser.parse tokens;;
Ast.print_ast ast3;;
print_newline ();;
let ast4 = Parser.parse tokens2;;
Ast.print_ast ast4;;
print_newline ();;

(* 4. ///////////////////////////////// Optimisation ///////////////////////////////// *)
print_string "\n4. Assember test :\n";;

let ast_opti = Optimiser.opti_constants ast4;;
Ast.print_ast ast_opti;;
print_newline ();;

(* 5. ///////////////////////////////// Assembler ///////////////////////////////// *)
print_string "\n4. Assember test :\n";;

let asm = Assembler.assemble ast_opti;;
Instr.print_prgm asm;;
print_newline ();;

(* 6. ///////////////////////////////// Compilation ///////////////////////////////// *)
print_string "\n6. Final Compile test :\n";;
let prgm = Compiler.compile test_codeez;;
Instr.print_prgm prgm;;
print_newline ();;

(* 7. ///////////////////////////////// Simulation ///////////////////////////////// *)
print_string "\n7. Simulation :\n";;

print_int (Simulation.simulate prgm [| 5; 2; 4; 3|]);;
print_newline ();;