open Ast
open Utils

let extend env x v = (x, v) :: env

let rec lookup env x =
  match env with
  | [] -> raise (DeclareError ("Unbound type for " ^ x))
  | (var, value) :: t -> if x = var then value else lookup t x


let rec search arr x =
  match arr with
  | [] -> None
  | (var, value) :: t -> if x = var then Some(value) else search t x
let all_of iterable predicate =
  let rec loop = function
    | [] -> true
    | x :: xs -> if predicate x then loop xs else false
  in
  loop iterable
let has_same_type arr k v = match (search arr k) with
  | None -> false
  | Some y -> y == v

let rec is_subtype t1 t2 = 
  if t1 = t2 then true
  else 
    match (t1, t2) with
    | TRec r1, TRec r2 -> (is_deep_subtype r1 r2) || (is_width_subtype r1 r2) || (is_permutation_subtype r1 r2)
    | TArrow(a, b), TArrow(c, d) -> (is_subtype c a) && (is_subtype b d)
    | _ -> false

and is_deep_subtype r1 r2 =
  all_of r1 (fun (k, v) -> 
    match search r2 k with
    | Some (t) -> is_subtype v t
    | _ -> false)

and is_width_subtype r1 r2 =
  List.length r1 >= List.length r2 && all_of r2 (fun (k, v) -> has_same_type r1 k v)

and is_permutation_subtype  r1 r2 = 
  all_of r2 (fun (k, v) -> has_same_type r1 k v)

let rec typecheck gamma e = 
  match e with
  | Int(x) -> TInt
  | Bool(x) -> TBool
  | ID(x) -> lookup gamma x
  | Binop(op, e1, e2) -> typecheck_binop gamma op e1 e2
  | Not (x) -> (
    match typecheck gamma x with
    | TBool -> TBool
    | _ -> raise (TypeError("Not statement require bool type")))
  | If(e1, e2, e3) -> typecheck_if gamma e1 e2 e3
  | Let (v, e1, e2) -> typecheck_let gamma v e1 e2
  | LetRec (v, e_type, e1, e2) -> typecheck_letrec gamma v e_type e1 e2
  | Fun (v, e_type, e) -> TArrow(e_type, typecheck (extend gamma v e_type) e)
  | App (e1, e2) -> typecheck_app gamma e1 e2
  | Record r -> typecheck_record gamma r
  | Select (Lab v, e) -> typecheck_select gamma v e
  | _ -> failwith("unimplemented")

and typecheck_binop gamma op e1 e2 =
  let t1 = typecheck gamma e1 in 
  let t2 = typecheck gamma e2 in
  match (t1, t2) with
  | TInt, TInt -> (
    match op with
    | Add -> TInt
    | Sub -> TInt
    | Mult -> TInt
    | Div -> TInt
    | Greater -> TBool
    | Less -> TBool
    | GreaterEqual -> TBool
    | LessEqual -> TBool
    | Equal -> TBool
    | NotEqual -> TBool
    | _ -> raise (TypeError ("the operation is invalid for TInt operands")))
  | TBool, TBool -> (
    match op with
    | Or -> TBool
    | And -> TBool
    | Equal -> TBool
    | NotEqual -> TBool
    | _ -> raise (TypeError ("the operation is invalid for TBool operands")))
  | _ ->
    if t1 = t2 then
      (match op with
      | Equal -> TBool
      | NotEqual -> TBool
      | _ -> raise (TypeError ("the operation is invalid")))
    else
      raise (TypeError ("the operand types are mismatched"))
 
and typecheck_if gamma e1 e2 e3 =
  let t1 = typecheck gamma e1 in
  let t2 = typecheck gamma e2 in
  let t3 = typecheck gamma e3 in
  match t1 with
  | TBool -> (
    if t2 = t3 || is_subtype t2 t3 then t2
    else if is_subtype t3 t2 then t3
    else raise (TypeError ("the sub-expression of the If are not the same"))
  )
  | _ -> raise (TypeError ("the condition of the If is not TBool"))

and typecheck_let gamma v e1 e2 =
  typecheck (extend gamma v (typecheck gamma e1)) e2

  and typecheck_letrec gamma v e_type e1 e2 =
    let new_env = extend gamma v e_type in
    typecheck (extend new_env v (typecheck new_env e1)) e2

and typecheck_app gamma e1 e2 = 
  match typecheck gamma e1 with
  | TArrow(tau, tau') -> (
    if (typecheck gamma e2) = tau then tau'
    else raise (TypeError("The application type is wrong")))
  | _ -> raise (TypeError("The target is not applicable"))

and typecheck_record gamma r1 = 
  TRec ((List.map (fun (k, v) -> (k, (typecheck gamma v))) r1))

and typecheck_select gamma v e = 
  match typecheck gamma e with
  | TRec (r) -> (
    match search r (Lab(v)) with
    | Some (e1) -> e1
    | _ -> raise (SelectError("the key doesnt exist in the record")))
  | _ -> raise (DeclareError("The variable is not selectable"))
