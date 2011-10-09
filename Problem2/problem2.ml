let fib_below n =
  let rec fib' ret c p = function
    | 1 -> ret
    | n -> 
        if p > n then ret
        else fib' (p::ret) (c+p) c (n-1)
  in 
  fib' [] 2 1 n
;;

let evens_below n = List.filter (fun n -> n mod 2 = 0) (fib_below n);;

print_int (List.fold_left (+) 0 (evens_below 4000000));;





