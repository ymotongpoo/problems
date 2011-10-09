let rec multiples ret n =
  match n with
  | n when n < 3 -> ret
  | n when (n mod 3) = 0 || (n mod 5) = 0 ->
      multiples (n::ret) (n-1)
  | _ -> multiples ret (n-1)
;;

print_int (List.fold_left (+) 0 (multiples [] 999));;
