open Test_utils
let _ = print_string (__MODULE__ ^ " Starts\n")

open Q1.ml (* open your own module to test *)
let _ =
  print_test_int_equal ~test_name:("iter1") 10 (iter (5, fun x -> 2 + x) 0);
  print_test_int_equal ~test_name:("iter2") 32 (iter (5, fun x -> 2 * x) 1);
  print_test_int_equal ~test_name:("iter3") 3 (iter (0, fun x -> 2 * x) 3);
  print_test_int_equal ~test_name:("iter4") 8 (iter (0, fun x-> 2 * x) 8);
  print_test_int_equal ~test_name:("iter5") 13 (iter (3, fun x-> x + 1) 10);
  print_test_int_equal ~test_name:("iter6") 1024 (iter (10, fun x -> x * 2) 1);
  print_test_equal ~test_name:("iter7") string_of_float 13.0 (iter (3, fun x -> x +. 1.0) 10.0);
  print_test_equal ~test_name:("iter8") string_of_bool false (iter (3, fun x -> not x) true);;

let _ = print_string (__MODULE__ ^ " Ends\n\n")
