

let compile (code: string) : Instr.t list =
  let tokens = Token.lex code in
  let my_ast = Parser.parse tokens in
  let opti_ast = Optimiser.opti_constants my_ast in
  let asm = Assembler.assemble opti_ast in
   asm

