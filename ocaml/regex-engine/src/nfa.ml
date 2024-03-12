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
      let result = find_all_transition head s nfa.delta in
      union result (move_imp nfa tail s)
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
  let rec accept_imp current_states s_list =
    match s_list with
    | [] -> [] != intersection current_states nfa.fs
    | head :: tail ->
      let next_states = List.fold_left (fun acc state ->
          union acc (find_all_transition state (Some head) nfa.delta)
        ) [] current_states
      in
      let epsilon_closure = e_closure nfa next_states in
      accept_imp epsilon_closure tail
  in
  accept_imp (e_closure nfa [nfa.q0]) (explode s)

(*******************************)
(* Part 2: Subset Construction *)
(*******************************)

let rec follow_epsilon nfa lst c = 
  let next_states = List.fold_left (fun acc q -> union acc (find_all_transition q (Some c) nfa.delta)) [] lst in
    e_closure nfa next_states

let new_states (nfa: ('q,'s) nfa_t) (qs: 'q list) : 'q list list =
  List.map (fun c -> follow_epsilon nfa qs c) nfa.sigma


let new_trans (nfa: ('q,'s) nfa_t) (qs: 'q list) : ('q list, 's) transition list =
  List.map (fun c -> (qs, Some c, follow_epsilon nfa qs c)) nfa.sigma

let new_finals (nfa: ('q,'s) nfa_t) (qs: 'q list) : 'q list list =
  if List.exists (fun q -> elem q nfa.fs) qs then [qs]
  else [] 

let rec nfa_to_dfa_step (nfa: ('q,'s) nfa_t) (dfa: ('q list, 's) nfa_t)
    (work: 'q list list) : ('q list, 's) nfa_t =
    match work with
    | [] -> dfa
    | head :: tail ->
      let possible_states = List.filter (fun state -> state != [] && not (elem state dfa.qs)) (new_states nfa head) in
      let new_transitions = List.filter (fun (from, t, dest) -> dest != []) (new_trans nfa head) in
      let new_finals = new_finals nfa head in
      let new_dfa = {
        qs = union possible_states dfa.qs;
        sigma = dfa.sigma;
        delta = union new_transitions dfa.delta;
        q0 = dfa.q0;
        fs = union dfa.fs new_finals
      } in
        nfa_to_dfa_step nfa new_dfa (union possible_states tail)

let nfa_to_dfa (nfa: ('q,'s) nfa_t) : ('q list, 's) nfa_t =
  let q0 = e_closure nfa [nfa.q0] in
  let dfa = { qs = [q0]; sigma = nfa.sigma; delta = []; q0 = q0; fs = []} in
  let work = [q0] in
    nfa_to_dfa_step nfa dfa work
  

