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
    | TRec r1, TRec r2 -> (is_deep_subtype r1 r2) || (is_width_subtype r1 r2) 
    | TArrow(a, b), TArrow(c, d) -> (is_subtype c a) && (is_subtype b d)
    | _ -> false

and is_deep_subtype r1 r2 =
  all_of r1 (fun (k, v) -> 
    match search r2 k with
    | Some (t) -> is_subtype v t
    | _ -> false)

and is_width_subtype r1 r2 =
  List.length r1 >= List.length r2 || all_of r2 (fun (k, v) -> has_same_type r1 k v)
let rec typecheck gamma e = failwith "unimplemented"
