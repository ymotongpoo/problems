let numbers n = 
  let rec numbers ret = function
    | x when x=n -> x::ret
    | x -> numbers (x::ret) (x+1)
  in
  List.rev (numbers [] 2)
;;

let primes n =
  let ns = numbers n in
  let rec primes' ret l =
    match ret, l with
    | _, [] -> ret
    | r::rs, xs when (r*r) > (List.hd (List.rev xs)) -> (List.rev (r::rs)) @ xs  
    | [], x::xs -> primes' [x] (List.filter (fun n -> (n mod x) <> 0) xs)
    | ret, x::xs -> primes' (x::ret) (List.filter (fun n -> (n mod x) <> 0) xs)
  in primes' [] ns
;;

let print_list l = List.map (fun n -> Printf.printf "%d " n) l;;

(* list of primes should be accending order *)
let factorize n =
  let ps = primes n in
  let rec factorize ret c = function
    | [] -> ret
    | x::xs when x >= c -> c::ret
    | x::xs when (c mod x) = 0 -> factorize (x::ret) (c/x) (x::xs)
    | x::xs -> factorize ret c xs
  in
  factorize [] n ps
;;

let lcm_below n =
  let factors = List.map factorize (numbers n) in
  let rec pick ret n = function
    | [] -> ret
    | x::xs when x=n -> ret @ xs
    | x::xs -> pick (x::ret) n xs
  in
  let rec pick_and_mul ret rest cur = 
    match rest, cur with
    | xs::xss, [] -> pick_and_mul ret xss xs
    | _, c::cs -> pick_and_mul (c*ret) (List.map (pick [] c) rest) cs
    | [], _ -> ret
  in 
  pick_and_mul 1 factors []
;;

Printf.printf "%d\n" (lcm_below 20);;


(* @_2F_1 solved one *)
let rec gcd m n = if m mod n = 0 then n else gcd n (m mod n)

let lcm m n = (m * n) / (gcd m n)

let lcm_one_to n =
  let rec f i p =
    if i < n then f (i+1) (lcm i p) else p
  in f 1 1;;

Printf.printf "%d\n" (lcm_one_to 20)
