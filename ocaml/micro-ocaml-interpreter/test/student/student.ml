open OUnit2
open TestUtils
open P4.Lexer
open P4.Parser
open P4.Types
open P4.Utils

let student_test1 _ =
  assert_equal (tokenize ";;") [ Tok_DoubleSemi ]
    ~msg:("wrong: " ^ string_of_token (List.hd (tokenize ";;")))

let student_test2 _ =
    let a, b = parse_expr (tokenize "let abc = fun a -> a + 1 in abc 1") in
    assert_equal (a,b) ([], Int(1))
      ~msg:("wrong: " ^ (string_of_expr b))

let suite = "student" >::: [ 
  "student_test1" >:: student_test1;
  (* "student_test2" >:: student_test2 *)
]
let _ = run_test_tt_main suite
