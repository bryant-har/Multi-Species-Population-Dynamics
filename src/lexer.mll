{
open Parser
}

let white = [' ' '\t']+
let digit = ['0'-'9']
let float =  digit*| digit+['.']digit* | digit*['.']digit+
let letter = ['a'-'z' 'A'-'Z']
let var = letter+digit*

rule read = 
  parse
  | white { read lexbuf }
  | "*" { TIMES }
  | "-" {SUB}
  | "+" { PLUS }
  | "(" { LPAREN }
  | ")" { RPAREN }
  | "^" {POW}
  | "!" {FACT}
  | "logBase" {LOGB}
  | "Sin" {SIN}
  | "Tan" {TAN}
  | "Cos" {COS}
  | "let" { LET }
  | "=" { EQUALS }
  | "in" { IN }
  | "'" {DERIV}
  | "/" {DIV}
  | var { VAR (Lexing.lexeme lexbuf) }
  | float { FLOAT (float_of_string (Lexing.lexeme lexbuf)) }
  | eof { EOF }