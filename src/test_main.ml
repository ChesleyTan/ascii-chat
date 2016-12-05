open OUnit2

let suite = "ASCII-Chat test suite" >:::
  Test_messaging.tests

(* The only place that should call [OUnit2.run_test_tt_main]. *)
let _ = run_test_tt_main suite
