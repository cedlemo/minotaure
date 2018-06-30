open OUnit2

let () =
  run_test_tt_main
    ("Minotaure test suite" >:::
     [
       Test_utils.run;
     ]
    )
