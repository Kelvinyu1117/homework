open Types
open Utils

(* Provided functions - DO NOT MODIFY *)

(* Adds mapping [x:v] to environment [env] *)
let extend env x v = (x, ref v) :: env

(* Returns [v] if [x:v] is a mapping in [env]; uses the
   most recent if multiple mappings for [x] are present *)
let rec lookup env x =
  match env with
  | [] -> raise (DeclareError ("Unbound variable " ^ x))
  | (var, value) :: t -> if x = var then !value else lookup t x

(* Creates a placeholder mapping for [x] in [env]; needed
   for handling recursive definitions *)
let extend_tmp env x = (x, ref (Int 0)) :: env

(* Updates the (most recent) mapping in [env] for [x] to [v] *)
let rec update env x v =
  match env with
  | [] -> raise (DeclareError ("Unbound variable " ^ x))
  | (var, value) :: t -> if x = var then value := v else update t x v

(* Part 1: Evaluating expressions *)

(* Evaluates MicroCaml expression [e] in environment [env],
   returning an expression, or throwing an exception on error *)

let string_of_op op = 
  match op with
  | Add -> "Add"
  | Sub -> "Sub"
  | Mult -> "Mult"
  | Div -> "Div"
  | Concat -> "Concat"
  | Greater -> "Greater"
  | Less -> "Less"
  | GreaterEqual -> "GreaterEqual"
  | LessEqual -> "LessEqual"
  | Equal -> "Equal"
  | NotEqual -> "NotEqual"
  | Or -> "Or"
  | And -> "And"

let rec eval_expr env e =
  match e with
  | Int v -> Int v
  | Bool v -> Bool v
  | String v -> String v
  | ID v -> lookup env v
  | Fun (v, e1) -> Closure(env, v, e1)
  | Not e1 -> eval_not env e1
  | Binop (op, e1, e2) -> eval_binop env op e1 e2
  | If (e1, e2, e3) -> eval_if env e1 e2 e3
  | App (e1, e2) -> eval_app env e1 e2
  | Let (v, b, e1, e2) -> eval_let  env v b e1 e2
  | Closure (env, v1, e1) -> Closure (env, v1, e1)
  | Record r -> Record r
  | Select (label, e) -> eval_select env label e
  | _ -> failwith "unimplemented"

and eval_not env e =
  let e1 = eval_expr env e in
  match e1 with
  | Bool v -> Bool (not v)
  | _ ->
      raise
        (TypeError ("expression is not boolean type, e:" ^ string_of_expr e1))

and eval_binop env op e1 e2 =
  let reduced_e1 = eval_expr env e1 in
  let reduced_e2 = eval_expr env e2 in

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
      | _ -> raise (TypeError ("op: " ^ (string_of_op op) ^ " is not defined for integer type, " ^ "a = " ^ (string_of_int a) ^ ", b = " ^ (string_of_int b))))
  | Bool a, Bool b ->(
    match op with
    | Equal -> Bool(a = b)
    | NotEqual -> Bool (a <> b)
    | Or -> Bool (a || b)
    | And -> Bool (a && b)
    | _ -> raise (TypeError ("op: " ^ (string_of_op op) ^ " is not defined for boolean type, " ^ "a = " ^ (string_of_bool a) ^ ", b = " ^ (string_of_bool b))))
  | String a, String b ->(
      match op with
      | Concat -> String(a ^ b)
      | Equal -> Bool(a = b)
      | NotEqual -> Bool (a <> b)
      | _ -> raise (TypeError ("op: " ^ (string_of_op op) ^ " is not defined for string type, " ^ "a = " ^ a ^ ", b = " ^ b)))
  | _ -> raise (TypeError("e1 = " ^ (string_of_expr reduced_e1) ^ ", e2 = " ^ (string_of_expr reduced_e2) ^ ", which has different types!"))

and eval_if env e1 e2 e3 = 
  let reduced_e1 = eval_expr env e1 in
  match reduced_e1 with
  | Bool(v) -> if v then eval_expr env e2 else eval_expr env e3
  | _ -> raise (TypeError ("eval_if: if e1 then e2 else e3, e1 is not boolean type, e1=" ^ (string_of_expr reduced_e1)))


and eval_let env x b e1 e2 = 
    match b with
    | true ->
      let new_env = (extend_tmp env x) in
      let reduced_e1 = eval_expr new_env e1 in
      update new_env x reduced_e1;
      eval_expr new_env e2
    | false ->
      let reduced_e1 = eval_expr env e1 in
      let new_env = extend env x reduced_e1 in
      (eval_expr new_env e2)

and eval_app env e1 e2 = 
  let reduced_e1 = eval_expr env e1 in
  let reduced_e2 = eval_expr env e2 in
  match reduced_e1 with
  | Closure(new_env, x, e) ->
    eval_expr (extend new_env x reduced_e2) e
  | _ -> raise (TypeError ("eval_app failed, app e1 e2, e1 = "^ (string_of_expr reduced_e1) ^ ", e2 = " ^ (string_of_expr reduced_e2)))

and eval_select env label e = 
  let rec find_record (label: label) records = 
    match records with
    | [] -> raise (SelectError("Failed to select record from label"))
    | (Lab(x), y)::t -> if Lab(x) = label then y else (find_record label t) 
  in
  let reduced_e = eval_expr env e in
  match reduced_e with
  | Record r -> find_record label r
  | _ -> raise (TypeError("eval_select failed, e cannot be evaluated to be Record type."))

(* Part 2: Evaluating mutop directive *)

(* Evaluates MicroCaml mutop directive [m] in environment [env],
   returning a possibly updated environment paired with
   a value option; throws an exception on error *)
let rec eval_mutop env m = 
  match m with
  | NoOp -> (env, None)
  | Def (v, e) -> (eval_def env v e)
  | Expr e -> (env, Some (eval_expr env e))

and eval_def env v e = 
  let new_env = extend_tmp env v in
  let reduced_e = eval_expr new_env e in
  update new_env v reduced_e;
  (new_env, Some(reduced_e))

