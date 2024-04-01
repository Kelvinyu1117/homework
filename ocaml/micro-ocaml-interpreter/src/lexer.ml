open Types
open Str
(* Part 1: Lexer - IMPLEMENT YOUR CODE BELOW *)

let strip str =
  let s = Str.replace_first (Str.regexp "^ +") "" str in
  Str.replace_first (Str.regexp " +$") "" s

let re_bool = Str.regexp "true\\|false"
let re_p_int = Str.regexp "[0-9]+"
let re_n_int = Str.regexp "(-[0-9]+)"
let re_str = Str.regexp "\"[^\"]*\""
let re_id = Str.regexp "[a-zA-Z][a-zA-Z0-9]*"
let re_lparen = Str.regexp "("
let re_rparen = Str.regexp ")"
let re_lcurly = Str.regexp "{"
let re_rcurly = Str.regexp "}"
let re_dot = Str.regexp "\\."
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
let re_concat = Str.regexp "\\^"
let re_let = Str.regexp "let"
let re_def = Str.regexp "def"
let re_in = Str.regexp "in"
let re_rec = Str.regexp "rec"
let re_fun = Str.regexp "fun"
let re_arrow = Str.regexp "->"
let re_double_semi = Str.regexp ";;"
let re_semi = Str.regexp ";"

(* Helper regex *)
let re_skip = Str.regexp "[ \t\n]*"
let re_extra = Str.regexp "[a-zA-Z0-9]+"

let tokenize input =
  let rec consume_next_token s pos =
    let try_extend_token_to_id_or_default s token last_pos default_value =
      if Str.string_match re_extra s last_pos then
        let token_id = Str.matched_string s in
        Tok_ID (token ^ token_id) :: consume_next_token s (Str.match_end ())
      else default_value :: consume_next_token s last_pos
    in
    if pos >= String.length s then []
      (* Keywords*)
      (* Tok_Not*)
    else if Str.string_match re_not s pos then
      let token = Str.matched_string s in
      let new_pos = Str.match_end () in

      try_extend_token_to_id_or_default s token new_pos Tok_Not
    else if Str.string_match re_if s pos then
      let token = Str.matched_string s in
      let new_pos = Str.match_end () in

      try_extend_token_to_id_or_default s token new_pos Tok_If
    else if Str.string_match re_then s pos then
      let token = Str.matched_string s in
      let new_pos = Str.match_end () in

      try_extend_token_to_id_or_default s token new_pos Tok_Then
    else if Str.string_match re_else s pos then
      let token = Str.matched_string s in
      let new_pos = Str.match_end () in

      try_extend_token_to_id_or_default s token new_pos Tok_Else
    else if Str.string_match re_let s pos then
      let token = Str.matched_string s in
      let new_pos = Str.match_end () in

      try_extend_token_to_id_or_default s token new_pos Tok_Let
    else if Str.string_match re_def s pos then
      let token = Str.matched_string s in
      let new_pos = Str.match_end () in

      try_extend_token_to_id_or_default s token new_pos Tok_Def
    else if Str.string_match re_in s pos then
      let token = Str.matched_string s in
      let new_pos = Str.match_end () in

      try_extend_token_to_id_or_default s token new_pos Tok_In
    else if Str.string_match re_rec s pos then
      let token = Str.matched_string s in
      let new_pos = Str.match_end () in

      try_extend_token_to_id_or_default s token new_pos Tok_Rec
    else if Str.string_match re_fun s pos then
      let token = Str.matched_string s in
      let new_pos = Str.match_end () in

      try_extend_token_to_id_or_default s token new_pos Tok_Fun
    else if Str.string_match re_bool s pos then
      let token = Str.matched_string s in
      let new_pos = Str.match_end () in
      let re_bool_true = Str.regexp "true" in
      let re_bool_false = Str.regexp "false" in

      if Str.string_match re_extra s new_pos then
        let token_id = Str.matched_string s in
        Tok_ID (token ^ token_id) :: consume_next_token s (Str.match_end ())
      else if Str.string_match re_bool_true token 0 then
        Tok_Bool true :: consume_next_token s new_pos
      else Tok_Bool false :: consume_next_token s new_pos
      (* Tok_int, positive *)
    else if Str.string_match re_p_int s pos then
      let token = Str.matched_string s in
      let new_pos = Str.match_end () in
      Tok_Int (int_of_string token) :: consume_next_token s new_pos
      (* Tok_int, negative *)
    else if Str.string_match re_n_int s pos then
      let token = Str.matched_string s in
      let new_pos = Str.match_end () in
      let extract_n_int_re = Str.regexp "-[0-9]+" in

      if Str.string_match extract_n_int_re token 0 then
        Tok_Int (int_of_string token) :: consume_next_token s new_pos
      else raise (InvalidInputException ("tokenize, token: " ^ token))

      (* Tok_String *)
    else if Str.string_match re_str s pos then
      let token = Str.matched_string s in
      let new_pos = Str.match_end () in
      let sanitize_str_re = Str.regexp "\"\\([^\"]*\\)\"" in

      if Str.string_match sanitize_str_re token 0 then
        let sanitized_token = Str.matched_group 1 token in
        Tok_String sanitized_token :: consume_next_token s new_pos
      else raise (InvalidInputException ("tokenize, token: " ^ token))
      
      (* Tok_ID *)
    else if Str.string_match re_id s pos then
      let token = Str.matched_string s in
      Tok_ID token :: consume_next_token s (Str.match_end ())
    else if Str.string_match re_lparen s pos then
      Tok_LParen :: consume_next_token s (Str.match_end ())
    else if Str.string_match re_rparen s pos then
      Tok_RParen :: consume_next_token s (Str.match_end ())
    else if Str.string_match re_lcurly s pos then
      Tok_LCurly :: consume_next_token s (Str.match_end ())
    else if Str.string_match re_rcurly s pos then
      Tok_RCurly :: consume_next_token s (Str.match_end ())
    else if Str.string_match re_dot s pos then
      Tok_Dot :: consume_next_token s (Str.match_end ())
    else if Str.string_match re_arrow s pos then
      Tok_Arrow :: consume_next_token s (Str.match_end ())
    else if Str.string_match re_eq s pos then
      Tok_Equal :: consume_next_token s (Str.match_end ())
    else if Str.string_match re_neq s pos then
      Tok_NotEqual :: consume_next_token s (Str.match_end ())
    else if Str.string_match re_geq s pos then
      Tok_GreaterEqual :: consume_next_token s (Str.match_end ())
    else if Str.string_match re_leq s pos then
      Tok_LessEqual :: consume_next_token s (Str.match_end ())
    else if Str.string_match re_gt s pos then
      Tok_Greater :: consume_next_token s (Str.match_end ())
    else if Str.string_match re_le s pos then
      Tok_Less :: consume_next_token s (Str.match_end ())
    else if Str.string_match re_or s pos then
      Tok_Or :: consume_next_token s (Str.match_end ())
    else if Str.string_match re_and s pos then
      Tok_And :: consume_next_token s (Str.match_end ())
    else if Str.string_match re_add s pos then
      Tok_Add :: consume_next_token s (Str.match_end ())
    else if Str.string_match re_sub s pos then
      Tok_Sub :: consume_next_token s (Str.match_end ())
    else if Str.string_match re_mult s pos then
      Tok_Mult :: consume_next_token s (Str.match_end ())
    else if Str.string_match re_div s pos then
      Tok_Div :: consume_next_token s (Str.match_end ())
    else if Str.string_match re_concat s pos then
      Tok_Concat :: consume_next_token s (Str.match_end ())
    else if Str.string_match re_double_semi s pos then
      Tok_DoubleSemi :: consume_next_token s (Str.match_end ())
    else if Str.string_match re_semi s pos then
      Tok_Semi :: consume_next_token s (Str.match_end ())
      (* Skip whitespace, tab, newline*)
    else if Str.string_match re_skip s pos then
      consume_next_token s (Str.match_end ())
    else raise (InvalidInputException ("tokenize, string: " ^ s))
  in
  consume_next_token input 0
