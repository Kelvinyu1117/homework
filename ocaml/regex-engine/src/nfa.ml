open List
open Sets

(*********)
(* Types *)
(*********)

type ('q, 's) transition = 'q * 's option * 'q

type ('q, 's) nfa_t = {
  sigma: 's list;
  qs: 'q list;
  q0: 'q;
  fs: 'q list;
  delta: ('q, 's) transition list;
}

(***********)
(* Utility *)
(***********)

(* explode converts a string to a character list *)
let explode (s: string) : char list =
  let rec exp i l =
    if i < 0 then l else exp (i - 1) (s.[i] :: l)
  in
  exp (String.length s - 1) []

(****************)
(* Part 1: NFAs *)
(****************)

let rec find_transition state symbol delta =
  match delta with
  | [] -> None
  | (from, t, dest) :: tail -> 
      match (t, symbol) with
      | (Some v, Some symbol) when from = state && v = symbol -> Some dest
      | (None, None) when from = state -> Some dest
      | _ -> find_transition state symbol tail

let rec find_all_transition state symbol delta =
  match delta with
  | [] -> []
  | (from, t, dest) :: tail -> 
      match (t, symbol) with
      | (Some v, Some sym) when from = state && v = sym -> dest :: find_all_transition state symbol tail
      | (None, None) when from = state -> dest :: find_all_transition state symbol tail
      | _ -> find_all_transition state symbol tail 

let move (nfa: ('q,'s) nfa_t) (qs: 'q list) (s: 's option) : 'q list =
  let rec move_imp nfa qs s =
    match qs with
      | [] -> []
      | head :: tail ->
          let result = find_transition head s nfa.delta in
            match result with
            | Some v -> v :: (move_imp nfa tail s)
            | _ -> move_imp nfa tail s
  in
  move_imp nfa qs s

let e_closure (nfa: ('q,'s) nfa_t) (qs: 'q list) : 'q list =
  let rec e_closure_imp nfa qs visited =
    match qs with
      | [] -> []
      | head :: tail ->
        if elem head visited then e_closure_imp nfa tail visited
        else
          let new_visited = insert head visited in
            let next_states = find_all_transition head None nfa.delta in
              match next_states with
              | [] -> head :: e_closure_imp nfa tail new_visited
              | _ -> 
                let result = head :: e_closure_imp nfa next_states new_visited in
                union result (e_closure_imp nfa tail new_visited)
  in
    e_closure_imp nfa qs []
let accept (nfa: ('q,char) nfa_t) (s: string) : bool =
  let rec accept_imp (nfa: ('q, char) nfa_t) (qs: 'q list) (s_list: char list) (visited: 'q list) =
    match (qs, s_list) with
    | [], _ -> false
    | _, [] -> intersection visited nfa.fs != []
    | (q :: qs', head :: tail) ->
      let next_states = e_closure nfa (move nfa [q] (Some head)) in
      accept_imp nfa next_states tail (union visited next_states)
  in
  accept_imp nfa [nfa.q0] (explode s) [nfa.q0]

(*******************************)
(* Part 2: Subset Construction *)
(*******************************)

let new_states (nfa: ('q,'s) nfa_t) (qs: 'q list) : 'q list list =
  failwith "unimplemented"

let new_trans (nfa: ('q,'s) nfa_t) (qs: 'q list) : ('q list, 's) transition list =
  failwith "unimplemented"

let new_finals (nfa: ('q,'s) nfa_t) (qs: 'q list) : 'q list list =
  failwith "unimplemented"

let rec nfa_to_dfa_step (nfa: ('q,'s) nfa_t) (dfa: ('q list, 's) nfa_t)
    (work: 'q list list) : ('q list, 's) nfa_t =
  failwith "unimplemented"

let nfa_to_dfa (nfa: ('q,'s) nfa_t) : ('q list, 's) nfa_t =
  failwith "unimplemented"
