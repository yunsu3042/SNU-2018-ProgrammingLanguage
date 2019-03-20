open Test_utils
let _ = print_string (__MODULE__ ^ " Starts\n")

open Q5.ml (* open your own module to test *)
let _ =
  let rec int_of_nat n =
    match n with
    | ZERO -> 0
    | SUCC n -> 1 + int_of_nat n
  in
  let rec nat_of_int i =
    match i with
    | 0 -> ZERO
    | i -> SUCC (nat_of_int (i - 1))
  in

  print_test_int_equal ~test_name:("nat1") 7 (int_of_nat (natadd (nat_of_int 3, nat_of_int 4)));
  print_test_int_equal ~test_name:("nat2") 0 (int_of_nat (natadd (nat_of_int 0, nat_of_int 0)));
  print_test_int_equal ~test_name:("nat3") 3 (int_of_nat (natadd (nat_of_int 0, nat_of_int 3)));
  print_test_int_equal ~test_name:("nat4") 4 (int_of_nat (natadd (nat_of_int 4, nat_of_int 0)));
  print_test_int_equal ~test_name:("nat5") 4 (int_of_nat (natadd (nat_of_int 1, nat_of_int 3)));
  print_test_int_equal ~test_name:("nat6") 1 (int_of_nat (natadd (nat_of_int 0, nat_of_int 1)));
  print_test_int_equal ~test_name:("nat7") 10 (int_of_nat (natadd (nat_of_int 10, nat_of_int 0)));
  print_test_int_equal ~test_name:("nat8") 17 (int_of_nat (natadd (nat_of_int 14, nat_of_int 3)));

  print_test_int_equal ~test_name:("nat9") 15 (int_of_nat (natmul (nat_of_int 3, nat_of_int 5)));
  print_test_int_equal ~test_name:("nat10") 12 (int_of_nat (natmul (nat_of_int 3, nat_of_int 4)));
  print_test_int_equal ~test_name:("nat11") 0 (int_of_nat (natmul (nat_of_int 0, nat_of_int 3)));
  print_test_int_equal ~test_name:("nat12") 0 (int_of_nat (natmul (nat_of_int 4, nat_of_int 0)));
  print_test_int_equal ~test_name:("nat13") 0 (int_of_nat (natmul (nat_of_int 0, nat_of_int 0)));
  print_test_int_equal ~test_name:("nat14") 3 (int_of_nat (natmul (nat_of_int 1, nat_of_int 3)));
  print_test_int_equal ~test_name:("nat15") 36 (int_of_nat (natmul (nat_of_int 4, nat_of_int 9)));
  print_test_int_equal ~test_name:("nat16") 0 (int_of_nat (natmul (nat_of_int 0, nat_of_int 0)));;

let _ = print_string (__MODULE__ ^ " Ends\n\n")
