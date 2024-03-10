open TestUtils
open OUnit2
open P3.Nfa
open P3.Regexp

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

let suite = "student" >::: [ 
  "student_test1" >:: student_test1 
]

let _ = run_test_tt_main suite
