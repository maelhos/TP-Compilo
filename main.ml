
Token.print_tokenlist (Token.lex (Token.explode "[ xaxa, y ] xaxa + y * (xaxa - y)"));;
print_newline ();;
Token.print_tokenlist (Token.lex (Token.explode "[ x ] x * 9"));;
