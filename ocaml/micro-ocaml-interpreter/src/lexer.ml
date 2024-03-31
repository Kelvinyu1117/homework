open Types
open Str
(* Part 1: Lexer - IMPLEMENT YOUR CODE BELOW *)

let strip str = 
  let s = Str.replace_first (Str.regexp "^ +") "" str in
  Str.replace_first (Str.regexp " +$") "" s


let re_bool = Str.regexp "true|false"
let re_p_int = Str.regexp "[0-9]+"
let re_n_int = Str.regexp "(-[0+9]+)"
let re_str = Str.regexp "\"[^\"]*\""
let re_id = Str.regexp "[a-zA-Z][a-zA-Z0-9]*"
let re_lparen = Str.regexp "("
let re_rparen = Str.regexp ")" 
let re_lbrace = Str.regexp "{"
let re_rbrace = Str.regexp "}"
let re_dot = Str.regexp "."
let re_eq = Str.regexp "="
let re_neq = Str.regexp "<>"
let re_gt = Str.regexp ">"
let re_le = Str.regexp "<"
let re_geq = Str.regexp ">="
let re_leq = Str.regexp "<="
let re_or = Str.regexp "||"
let re_and = Str.regexp "&&"
let re_not = Str.regexp "not"
let re_if = Str.regexp "if"
let re_then = Str.regexp "then"
let re_else = Str.regexp "else"
let re_add = Str.regexp "+"
let re_sub = Str.regexp "-"
let re_mult = Str.regexp "*"
let re_div = Str.regexp "/"
let re_concat = Str.regexp "^"
let re_let = Str.regexp "let"
let re_def = Str.regexp "def"
let re_in = Str.regexp "in"
let re_rec = Str.regexp "rec"
let re_fun = Str.regexp "fun"
let re_arrow = Str.regexp "->"
let re_double_semi = Str.regexp ";;"
let re_semi = Str.regexp ";"

let tokenize input = failwith "unimplemented"

