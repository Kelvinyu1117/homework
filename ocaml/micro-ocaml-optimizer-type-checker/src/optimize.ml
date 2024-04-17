open Ast
open Utils

let extend env x v = (x, v) :: env

let rec lookup env x =
  match env with
  | [] -> None
  | (var, value) :: t -> if x = var then Some value else lookup t x

let rec optimize env e = 
  match e with
  | Int x -> Int x
  | Bool x -> Bool x
  | ID x -> (
      match lookup env x with
      | Some v -> v
      | _ -> ID x)
  | Binop (op, e1, e2) -> optimize_binop env op e1 e2
  | Not e -> (
    let e1 = optimize env e in
      match e1 with
      | Bool v -> Bool (not v)
      | _ -> e1)
  | If (e1, e2, e3) -> optimize_if env e1 e2 e3
  | Let (v, e1, e2) -> optimize_let env v e1 e2
  | LetRec (v, e_type, e1, e2) -> optimize_let_rec env v e_type e1 e2
  | Fun (v, e_type, e) -> optimize_fun env v e_type e
  | App (e1, e2) -> optimize_app e1 e2
  | Record r -> Record r
  | Select (label, e) -> optimize_select env label e
  | _ -> e

and optimize_binop env op e1 e2 = 
  let reduced_e1 = optimize env e1 in
  let reduced_e2 = optimize env e2 in

  match (reduced_e1, reduced_e2) with
  | Int a, Int b -> (
    match op with
      | Add -> Int (a + b)
      | Sub -> Int (a - b)
      | Mult -> Int (a * b)
      | Div ->
          if b = 0 then
            raise DivByZeroError
          else Int (a / b)
      | Greater -> Bool (a > b)
      | Less -> Bool (a < b)
      | Equal -> Bool(a = b)
      | NotEqual -> Bool (a <> b)
      | _ -> Binop (op, reduced_e1, reduced_e2))
  | Int 0, _ -> 
    (match op with
    | Add -> reduced_e2
    | Mult -> Int(0)
    | Div -> Int(0)
    | _ -> Binop (op, reduced_e1, reduced_e2))
  | _, Int 0-> 
    (match op with
    | Add -> reduced_e1
    | Sub -> reduced_e1
    | Mult -> Int(0)
    | Div -> raise DivByZeroError
    | _ -> Binop (op, reduced_e1, reduced_e2))
  | Int 1, _ -> 
    (match op with
    | Mult -> reduced_e2
    | _ -> Binop (op, reduced_e1, reduced_e2))
  | _, Int 1-> 
    (match op with
    | Mult -> reduced_e1
    | Div -> reduced_e1
    | _ -> Binop (op, reduced_e1, reduced_e2))
  | Bool a, Bool b ->(
    match op with
    | Equal -> Bool(a = b)
    | NotEqual -> Bool (a <> b)
    | Or -> Bool (a || b)
    | And -> Bool (a && b)
    | _ -> Binop (op, reduced_e1, reduced_e2))
  | _, Bool (true) -> (
    match op with
    | Or -> Bool(true)
    | _ -> Binop (op, reduced_e1, reduced_e2))
  | Bool (true), _ -> (
    match op with
    | Or -> Bool(true)
    | _ -> Binop (op, reduced_e1, reduced_e2))
  | _, Bool (false) -> (
    match op with
    | And -> Bool(false)
    | _ -> Binop (op, reduced_e1, reduced_e2))
  | Bool (false), _ -> (
    match op with
    | And -> Bool(false)
    | _ -> Binop (op, reduced_e1, reduced_e2))
  | _ -> Binop (op, reduced_e1, reduced_e2)

and optimize_if env e1 e2 e3 =
  let reduced_e1 = optimize env e1 in
    match reduced_e1 with
    | Bool(v) -> if v then optimize env e2 else optimize env e3
    | _ -> If (reduced_e1, e2, e3)
and optimize_let env v e1 e2 = failwith "unimplemented"

and optimize_let_rec env v e_type e1 e2 = failwith "unimplemented"

and optimize_fun env v e_type e = failwith "unimplemented"

and optimize_app e1 e2 = failwith "unimplemented"

and optimize_select env label e = failwith "unimplemented"
