let max l =
  let rec max' ret = function
    | [] -> ret
    | x::xs -> if x > ret then max' x xs else max' ret xs
  in
  max' (List.hd l) l
;;

let primes_below n =
  let rec numbers ret = function
    | x when x=n -> n::ret
    | x -> numbers (x::ret) (x+1)
  in
  let ns = List.rev (numbers [] 2) in
  let rec primes m ret nums =
    match ret, nums with
    | _, [] -> ret
    | [], x::xs ->
        let l = List.filter (fun n -> n mod x <> 0) xs in
        primes (max l) [x] l
    | r::rs, x::xs -> 
        if (r*r) > m then (r::rs) @ (x::xs)
        else 
          let l = List.filter (fun n -> n mod x <> 0) xs in
          primes (max l) (x::r::rs) l
  in
  List.rev (primes (max ns) [] ns)
;;


let largest_prime_functor n =
  let ps = primes_below n in
  let rec find_functor n' = function
    | [] -> n'
    | x::xs when x = n' -> n'
    | x::xs ->
        if n' mod x = 0 then find_functor (n'/x) xs
        else find_functor n' xs
  in
  find_functor n ps
;;

let largest_prime_functor2 n =
  let rec largest_prime_functor2' r n =
    match r, n with
    | r, n when r < n -> n
    | r, n when r mod n = 0 -> largest_prime_functor2' (r/n) n
    | _, _ -> largest_prime_functor2' r (n+1)
  in
  largest_prime_functor2' n 2
;;


(* print_int (largest_prime_functor 13195);; *)
(* print_int (largest_prime_functor 600851475143);; *)

(* print_int (largest_prime_functor2 13195);; *)
print_int (largest_prime_functor2 600851475143);;

        

