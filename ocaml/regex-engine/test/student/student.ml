open TestUtils
open OUnit2
open P3.Nfa
open P3.Regexp

let assert_set_set_eq lst1 lst2 =
  let es l = List.sort_uniq compare (List.map (List.sort compare) l) in
  let sorted_lst1 = es lst1 in
  let sorted_lst2 = es lst2 in

  (* Debug printing *)
  print_endline "---- Debug Printing ----";
  print_endline "lst1:";
  List.iter (fun l -> List.iter (fun x -> print_int x; print_string " ") l; print_newline ()) sorted_lst1;
  print_endline "lst2:";
  List.iter (fun l -> List.iter (fun x -> print_int x; print_string " ") l; print_newline ()) sorted_lst2;
  print_endline "------------------------";

  assert_equal sorted_lst1 sorted_lst2
let student_test1 _ = 
  let m3 =
    { qs= [0; 1; 2; 3]
    ; sigma= ['0'; '1']
    ; q0= 0
    ; fs= [3]
    ; delta= [(0, Some '0', 0); 
              (0, None, 1); 
              (1, None, 3); 
              (1, Some '0', 2);
              (2, Some '1', 1); 
              (3, Some '0', 3) ] }
  in
  assert_nfa_closure m3 [0] [0; 1; 3] ;
  assert_nfa_closure m3 [1] [1;3];
  assert_nfa_closure m3 [2] [2] ;
  assert_nfa_closure m3 [3] [3] ;
;;
let student_test2 _ = 
let m1 =
  { qs= [0; 1; 2; 3; 4]
  ; sigma= ['a'; 'b']
  ; delta= [(0, Some 'a', 1); (0, Some 'a', 2); (2, Some 'b', 3); (2, None, 4); (4, Some 'a', 4)]
  ; q0= 0
  ; fs= [1; 3] } in
  assert_set_set_eq [[]; []] (new_states m1 []) ;
  assert_set_set_eq [[1; 2; 4]; []] (new_states m1 [0]) ;
  assert_set_set_eq [[4]; []] (new_states m1 [3; 4]) ;
  assert_set_set_eq [[1; 2; 4]; [3]] (new_states m1 [0; 2]) ;
  assert_set_set_eq [[1; 2; 4]; [3]] (new_states m1 [0; 1; 2; 3])

let student_test3 _ = 
  let m1 =
    { qs= [0; 1; 2; 3; 4]
    ; sigma= ['a'; 'b']
    ; delta= [(0, Some 'a', 1); (0, Some 'a', 2); (2, Some 'b', 3); (2, None, 4); (4, Some 'a', 4)]
    ; q0= 0
    ; fs= [1; 3] } in
    assert_trans_eq
      [([0], Some 'a', [1; 2; 4]); ([0], Some 'b', [])]
      (new_trans m1 [0]) ;
    assert_trans_eq
      [([0; 2], Some 'a', [1; 2; 4]); ([0; 2], Some 'b', [3])]
      (new_trans m1 [0; 2])

let student_test4 _ = 
  let m1 = regexp_to_nfa @@ string_to_regexp "a*b" in
  print_endline (str_of_int_fsm m1);
  assert_nfa_deny m1 "a";
  assert_nfa_accept m1 "b";
  assert_nfa_accept m1 "ab";
  assert_nfa_accept m1 "aab";
;;
let suite = "student" >::: [ 
  "student_test1" >:: student_test1;
  "student_test2" >:: student_test2;
  "student_test3" >:: student_test3;
  "student_test4" >:: student_test4;
]

let _ = run_test_tt_main suite
