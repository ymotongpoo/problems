let max_num = 1000;;
let max_base = max_num * max_num / 10;;

let strip_to_list base x =
  let rec define_max_base base' =
    match base' with
    | b when b > x -> (b/10)
    | b when b > base -> base
    | b -> define_max_base (b*10)
  in 
  let rec strip_to_list ret base x =
    match base, x with
    | _, x when x = 0 -> ret
    | b, x -> strip_to_list ((x/b)::ret) (b/10) (x mod b)
  in strip_to_list [] (define_max_base base) x
;;

let rec list_to_num ret base = function
    | [] -> ret
    | x::xs -> list_to_num (ret+base*x) (base*10) xs
;;

let reverse_int n =
  let rev_num = List.rev (strip_to_list max_base n) in
  (list_to_num 0 1 rev_num)
;;

let search_palindromic m =
  let rec search ret x y =
    match x, y with
    | x, y when (x*y) = (reverse_int (x*y)) && (x*y) > ret 
        -> search (x*y) x (y-1)
    | 0, 0 -> ret
    | _, 0 -> search ret (x-1) m
    | _, _ -> search ret x (y-1)
  in search 0 m m
;;

Printf.printf "%d\n" (search_palindromic (max_num-1));;
