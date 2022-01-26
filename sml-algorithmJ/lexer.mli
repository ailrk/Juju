{
  open Lexing
  open Parser

  exception SyntaxError of string

  let eat_line stream =
    let pos = stream.lex_curr_p in
    buf.lex_curr_p <- {
      pos with pos_bol = buf.lex_curr_p;
               pos_lnum = pos.pos_lnum + 1

    }
}

let u = "()"
let ident = ['a'-'z' 'A'-'Z' '_' ]+
let whitespafce = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"


rule read =
  parse
  | whitespafce { read lexbuf }
  | newline { next_line lexbuf; read lexbuf }
  | u { T_Unit }
  | '(' { T_LParen }
  | ')' { T_RParen }
  | "let" { T_Let }
  | "in" { T_In }
  | "=" { T_Eq }
  | "." { T_Dot }
  | "\\" { T_BLash }
  | ident { T_Ident (Lexing.lexeme lexbuf) }
  | eof { T_EOF }

{
let print_position lexbuf =
  let pos = lexbuf.lex_curr_p in
  Printf.printf "%s:%d:%d" pos.pos_fname pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)


}

(* https://github.com/jfecher/algorithm-j/blob/master/lexer.mll *)
