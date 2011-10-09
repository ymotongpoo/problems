(****************************************************************
 * Utility Library for Algorithm and Programming Quiz
 *
 * created by Yoshifumi YAMAGUCHI, 2010
 ***************************************************************)

let ( *~ ) = Big_int.mult_big_int
let ( $~ ) = Big_int.mult_int_big_int
let (+~) = Big_int.add_big_int
let (-~) = Big_int.sub_big_int
let (/~) = Big_int.div_big_int

type 'a bintree = LF | Bt of 'a * 'a bintree * 'a bintree

(*********************************************************** 
 ***************************************** Basic operation 
 ***********************************************************)

let pow x n = 
  let rec pow_aux accu = function
    | 0 -> accu
    | k -> pow_aux (x*accu) (pred k)
  in
  match n with
  | 1 -> x
  | 2 -> x*x
  | _ -> pow_aux 1 n


let fact_aux n m =
  let r = ref Big_int.unit_big_int in
  for i = n-m+1 to n do
    r := i $~ !r
  done;
  !r

let fact n = fact_aux n n


let sqrt n = int_of_float (sqrt (float_of_int n))


(**
 * Basic list operations 
 **)

let rec_map f l = List.rev (List.rev_map f l)


let max l =
  let rec max_aux max = function
    | [] -> max
    | x::xs -> 
        if x > max then max_aux x xs
        else max_aux max xs
  in
  match l with
  | [x] -> x
  | x::xs -> max_aux x xs
  | [] -> failwith("Eular.max")


let min l =
  let rec min_aux min = function
    | [] -> min
    | x::xs -> 
        if x < min then min_aux x xs
        else min_aux min xs
  in
  match l with
  | [x] -> x
  | x::xs -> min_aux x xs
  | [] -> failwith("Eular.min")


let rec bottom = function
  | [] -> invalid_arg "Eular.bottom"
  | x::[] -> x
  | x::xs -> bottom xs


let range s l =
  let rec range_aux accu s' n =
    match n with
    | n' when n' < 0 -> invalid_arg "Eular.range_aux"
    | 0 -> List.rev accu
    | n' -> range_aux (s'::accu) (succ s') (pred n')
  in
  range_aux [] s l


(***********************************************************
 ******************************** Basic algebric functions 
 ***********************************************************)

let sift n l = List.filter (fun x -> x mod n <> 0) l


let sieve l =
  let rec sieve_aux accu l =
    match accu, l with
    | _, [] -> List.rev accu
    | [], l::ls -> sieve_aux [l] ls
    | _, x::xs when x*x > bottom l -> List.rev_append accu l
    | _, x::xs -> sieve_aux (x::accu) (sift x xs)
  in
  sieve_aux [] l


let primes n = sieve (range 2 n)


let rec gcd m n = 
  match m, n with
  | m, n when n > m -> gcd n m
  | _, 0 -> m
  | m, n when m mod n = 0 -> n
  | _, _ -> gcd n (m mod n)
;;


let lcm m n = m * n / (gcd m n)


let perm n l =
  let remove x xs =
    let rec remove ret = function
      | [] -> ret
      | y::ys when y = x -> remove ret ys
      | y::ys -> remove (y::ret) ys
    in
    remove [] xs
  in
  let rec perm m xs a b =
    if m = 0 then a::b
    else
      List.fold_right (fun x y -> perm (m-1) (remove x xs) (x::a) y) xs b
  in
  perm n l [] []
;;  
  

let print_int_list list =
  let _ = List.map (fun x -> Printf.printf "%d " x) list in
  ()
;;


let print_string_list list =
  let _ = List.map (fun x -> Printf.printf "%s " x) list in
  ()
;;
