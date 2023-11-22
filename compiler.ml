

let compile (code: string) : Simulation.instr list =
  let tokens = Token.lex code in
  let my_ast = Parser.parse tokens in
  let asm = Assembler.assemble my_ast in
  asm

