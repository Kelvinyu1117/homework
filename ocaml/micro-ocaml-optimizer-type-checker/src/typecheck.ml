open Ast
open Utils

let extend env x v = (x, v) :: env

let rec lookup env x =
  match env with
  | [] -> raise (DeclareError ("Unbound type for " ^ x))
  | (var, value) :: t -> if x = var then value else lookup t x

let rec is_subtype t1 t2 = failwith "unimplemented"

let rec typecheck gamma e = failwith "unimplemented"