open OUnit2

let suite = "ASCII-Chat test suite" >::: Test_messaging.tests

let _ = run_test_tt_main suite
