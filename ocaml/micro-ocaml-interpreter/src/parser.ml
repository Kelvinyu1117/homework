open Types
open Utils

(* Provided functions - DO NOT MODIFY *)

(* Matches the next token in the list, throwing an error if it doesn't match the given token *)
let match_token (toks : token list) (tok : token) =
  match toks with
  | [] -> raise (InvalidInputException (string_of_token tok))
  | h :: t when h = tok -> t
  | h :: _ ->
      raise
        (InvalidInputException
           (Printf.sprintf "Expected %s from input %s, got %s"
              (string_of_token tok)
              (string_of_list string_of_token toks)
              (string_of_token h)))

(* Matches a sequence of tokens given as the second list in the order in which they appear, throwing an error if they don't match *)
let match_many (toks : token list) (to_match : token list) =
  List.fold_left match_token toks to_match

(* Return the next token in the token list as an option *)
let lookahead (toks : token list) =
  match toks with [] -> None | h :: t -> Some h

(* Return the token at the nth index in the token list as an option*)
let rec lookahead_many (toks : token list) (n : int) =
  match (toks, n) with
  | h :: _, 0 -> Some h
  | _ :: t, n when n > 0 -> lookahead_many t (n - 1)
  | _ -> None

(* Part 2: Parsing expressions *)

let to_string toks = string_of_list string_of_token toks

let rec parse_expr toks = 
  match lookahead toks with 
  | Some Tok_Let -> parse_let toks
  | Some Tok_If -> parse_if toks
  | Some Tok_Fun -> parse_fun toks
  | _ -> parse_or toks

  and parse_let toks =
  match toks with
  | Tok_Let :: Tok_ID x :: Tok_Equal :: t -> (
      let lst1 = match_many toks (Tok_Let :: Tok_ID x :: [ Tok_Equal ]) in
      let lst2, e1 = parse_expr lst1 in
      match lst2 with
      | Tok_In :: t ->
          let lst3 = match_token lst2 Tok_In in
          let lst4, e2 = parse_expr lst3 in
          (lst4, Let (x, false, e1, e2))
      | _ ->
          raise
            (InvalidInputException
               ("let error: no rec - Tok_In not found, lst: " ^ to_string lst2))
      )
  | Tok_Let :: Tok_Rec :: Tok_ID x :: Tok_Equal :: t -> (
      let lst1 =
        match_many toks (Tok_Let :: Tok_Rec :: Tok_ID x :: [ Tok_Equal ])
      in
      let lst2, e1 = parse_expr lst1 in
      match lst2 with
      | Tok_In :: t ->
          let lst3 = match_token lst2 Tok_In in
          let lst4, e2 = parse_expr lst3 in
          (lst4, Let (x, true, e1, e2))
      | _ ->
          raise
            (InvalidInputException
               ("let error: rec - Tok_In not found, lst: " ^ to_string lst2)))
  | _ -> parse_if toks

and parse_if toks =
  match toks with
  | Tok_If :: t -> (
      let lst1, e1 = parse_expr (match_token toks Tok_If) in
      match lst1 with
      | Tok_Then :: t -> (
          let lst2, e2 = parse_expr (match_token lst1 Tok_Then) in
          match lst2 with
          | Tok_Else :: t ->
              let lst3, e3 = parse_expr (match_token lst2 Tok_Else) in
              (lst3, If (e1, e2, e3))
          | _ ->
              raise
                (InvalidInputException
                   ("if error: Tok_Else not found, lst: " ^ to_string lst2)))
      | _ ->
          raise
            (InvalidInputException
               ("if error: Tok_Then not found, lst: " ^ to_string lst1)))
  | _ -> parse_fun toks

and parse_fun toks =
  match toks with
  | Tok_Fun :: Tok_ID x :: Tok_Arrow :: t ->
      let tmp = match_many toks (Tok_Fun :: Tok_ID x :: [ Tok_Arrow ]) in
      let lst1, e1 =
        parse_expr (match_many toks (Tok_Fun :: Tok_ID x :: [ Tok_Arrow ]))
      in
      (lst1, Fun (x, e1))
  | _ -> parse_or toks

and parse_or toks =
  let lst2, e1 = parse_and toks in
  match lst2 with
  | Tok_Or :: t ->
      let lst3 = match_token lst2 Tok_Or in
      let lst4, e2 = parse_or lst3 in
      (lst4, Binop (Or, e1, e2))
  | _ -> (lst2, e1)

and parse_and lst =
  let lst2, e1 = parse_equal lst in
  match lst2 with
  | Tok_And :: t ->
      let lst3 = match_token lst2 Tok_And in
      let lst4, e2 = parse_and lst3 in
      (lst4, Binop (And, e1, e2))
  | _ -> (lst2, e1)

and parse_equal lst =
  let lst2, e1 = parse_relational lst in
  match lst2 with
  | Tok_Equal :: t ->
      let lst3 = match_token lst2 Tok_Equal in
      let lst4, e2 = parse_equal lst3 in
      (lst4, Binop (Equal, e1, e2))
  | Tok_NotEqual :: t ->
      let lst3 = match_token lst2 Tok_NotEqual in
      let lst4, e2 = parse_equal lst3 in
      (lst4, Binop (NotEqual, e1, e2))
  | _ -> (lst2, e1)

and parse_relational lst =
  let lst2, e1 = parse_add lst in
  match lst2 with
  | Tok_Less :: t ->
      let lst3 = match_token lst2 Tok_Less in
      let lst4, e2 = parse_relational lst3 in
      (lst4, Binop (Less, e1, e2))
  | Tok_Greater :: t ->
      let lst3 = match_token lst2 Tok_Greater in
      let lst4, e2 = parse_relational lst3 in
      (lst4, Binop (Greater, e1, e2))
  | Tok_LessEqual :: t ->
      let lst3 = match_token lst2 Tok_LessEqual in
      let lst4, e2 = parse_relational lst3 in
      (lst4, Binop (LessEqual, e1, e2))
  | Tok_GreaterEqual :: t ->
      let lst3 = match_token lst2 Tok_GreaterEqual in
      let lst4, e2 = parse_relational lst3 in
      (lst4, Binop (GreaterEqual, e1, e2))
  | _ -> (lst2, e1)

and parse_add lst =
  let lst1, e1 = parse_mult lst in
  match lst1 with
  | Tok_Add :: t ->
      let lst2, e2 = parse_add (match_token lst1 Tok_Add) in
      (lst2, Binop (Add, e1, e2))
  | Tok_Sub :: t ->
      let lst2, e2 = parse_add (match_token lst1 Tok_Sub) in
      (lst2, Binop (Sub, e1, e2))
  | _ -> (lst1, e1)

(* parse_mult function *)
and parse_mult lst =
  let lst2, e1 = parse_concat lst in
  match lst2 with
  | Tok_Mult :: t ->
      let lst3 = match_token lst2 Tok_Mult in
      let lst4, e2 = parse_mult lst3 in
      (lst4, Binop (Mult, e1, e2))
  | Tok_Div :: t ->
      let lst3 = match_token lst2 Tok_Div in
      let lst4, e2 = parse_mult lst3 in
      (lst4, Binop (Div, e1, e2))
  | _ -> (lst2, e1)

and parse_concat lst =
  let lst2, e1 = parse_unary lst in
  match lst2 with
  | Tok_Concat :: t ->
      let lst3 = match_token lst2 Tok_Concat in
      let lst4, e2 = parse_concat lst3 in
      (lst4, Binop (Concat, e1, e2))
  | _ -> (lst2, e1)

and parse_unary lst =
  match lst with
  | Tok_Not :: t ->
      let lst2 = match_token lst Tok_Not in
      let lst3, e2 = parse_unary lst2 in
      (lst3, Not e2)
  | _ -> parse_app lst

and parse_app lst =
  let lst1, e1 = parse_select lst in
  let lst2, e2 = parse_primary lst1 in
  match (e1, e2) with
  | Some v1, Some v2 -> (lst2, App (v1, v2))
  | Some v, None -> (lst2, v)
  | _ -> raise (InvalidInputException "parse_app error")

and parse_select lst =
  let lst2, e1 = parse_primary lst in
  match (lst2, e1) with
  | Tok_Dot :: Tok_ID x :: t, Some v ->
      let lst3 = match_many lst2 (Tok_Dot :: [ Tok_ID x ]) in
      (lst3, Some (Select (Lab x, v)))
  | _ -> (lst2, e1)

and parse_record_body lst =
  match lst with
  | Tok_ID x :: Tok_Equal :: t -> (
      let lst1 = match_many lst (Tok_ID x :: [ Tok_Equal ]) in
      let lst2, e = parse_expr lst1 in
      match lst2 with
      | Tok_Semi :: t ->
          let lst3 = match_token lst2 Tok_Semi in
          let lst4, records = parse_record_body lst3 in
          (lst4, (Lab x, e) :: records)
      | _ -> (lst2, [ (Lab x, e) ]))
  | _ -> (lst, [])

and parse_primary lst =
  match lst with
  | Tok_Int x :: t ->
      let lst2 = match_token lst (Tok_Int x) in
      (lst2, Some (Int x))
  | Tok_Bool x :: t ->
      let lst2 = match_token lst (Tok_Bool x) in
      (lst2, Some (Bool x))
  | Tok_ID x :: t ->
      let lst2 = match_token lst (Tok_ID x) in
      (lst2, Some (ID x))
  | Tok_String x :: t ->
      let lst2 = match_token lst (Tok_String x) in
      (lst2, Some (String x))
  | Tok_LParen :: t -> (
      let lst2 = match_token lst Tok_LParen in
      let lst3, e2 = parse_expr lst2 in
      match lst3 with
      | Tok_RParen :: t -> (t, Some e2)
      | _ ->
          raise
            (InvalidInputException
               ("parse_primary error: right paren not found, lst: "
              ^ to_string lst3)))
  | Tok_LCurly :: t -> (
      let lst2 = match_token lst Tok_LCurly in
      let lst3, result = parse_record_body lst2 in
      match lst3 with
      | Tok_RCurly :: t -> (t, Some (Record result))
      | _ ->
          raise
            (InvalidInputException
               ("parse_record error: Tok_RCurly not found, lst: "
              ^ to_string lst2)))
  | _ -> (lst, None)

(* Part 3: Parsing mutop *)
let rec parse_mutop toks =
  match toks with Tok_DoubleSemi :: t -> (t, NoOp) | _ -> parse_defmutop toks

and parse_defmutop toks =
  match toks with
  | Tok_Def :: Tok_ID x :: Tok_Equal :: t -> (
      let lst1, e1 =
        parse_expr (match_many toks (Tok_Def :: Tok_ID x :: [ Tok_Equal ]))
      in
      match lst1 with
      | Tok_DoubleSemi :: t -> (t, Def (x, e1))
      | _ ->
          raise
            (InvalidInputException
               ("parse_defmutop error: Tok_DoubleSemi not found, lst: "
              ^ to_string lst1)))
  | _ -> parse_exprmutop toks

and parse_exprmutop toks =
  let lst1, e1 = parse_expr toks in
  match lst1 with
  | Tok_DoubleSemi :: t -> (t, Expr e1)
  | _ ->
      raise
        (InvalidInputException
           ("parse_exprmutop error: Tok_DoubleSemi not found" ^ to_string lst1))
