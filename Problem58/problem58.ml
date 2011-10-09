(* -*- coding: utf-8 -*-; *)

(**
Problem 58

Starting with 1 and spiralling anticlockwise in the following way, 
a square spiral with side length 7 is formed.

37 36 35 34 33 32 31
38 17 16 15 14 13 30
39 18  5  4  3 12 29
40 19  6  1  2 11 28
41 20  7  8  9 10 27
42 21 22 23 24 25 26
43 44 45 46 47 48 49

It is interesting to note that the odd squares lie along the bottom right diagonal, 
but what is more interesting is that 8 out of the 13 numbers lying along both diagonals are prime; 
that is, a ratio of 8/13 ≈ 62%.

If one complete new layer is wrapped around the spiral above, 
a square spiral with side length 9 will be formed. 
If this process is continued, what is the side length of the square spiral 
for which the ratio of primes along both diagonals first falls below 10%?
*)


(**
   Sn+1 - Sn = 8*n
   S1 = 1
   --> Sn = 4*n*(n-1)+1
*)

module A = struct
(** non lazy version; took too long time *)

  let rec bottom = function
    | [x] -> x
    | x::xs -> bottom xs
    | [] -> raise Not_found
  ;;

  let filter_gt border xs =
    let rec filter = function
      | x::xs when border < x -> x::xs
      | x::xs -> filter xs
      | [] -> []
    in
    filter xs
  ;;

  let nums limit =
    let rec nums ret = function
      | 0 -> ret
      | n -> nums (n::ret) (pred n)
    in
    nums [] limit
  ;;

  let primes limit =
    let sift n l = List.filter (fun x -> (x mod n) <> 0) l in
    let rec sieve ret ns =
      match ret, ns with
      | r::rs, xs when (r*r) > (bottom xs) -> 
          List.rev_append ret ns
      | r, x::xs -> 
          sieve (x::r) (sift x xs)
      | r, [] -> 
          List.rev r
    in
    sieve [] (List.tl (nums limit))
  ;;

  
  let corner_num = function
    | nth when nth = 1 -> [1]
    | nth ->
        let last = 4*nth*(nth-1)+1 in
        let ofs = 2*(nth-1) in
        [last - 3*ofs; last - 2*ofs; last - ofs; last]
  ;;

  (* ps should be ascending order *)
  let count_prime_corner ps nth =
    let corners = corner_num nth in
    (* let _ = List.map (fun x -> Printf.printf "<%d>%! " x) corners in *)
    let filtered = List.filter (fun x -> List.mem x ps) corners in
    let _ = List.map (fun x -> Printf.printf "%d%! " x) filtered in
    print_endline "";
    let filtered_ps = filter_gt (bottom corners) ps in
    (List.length filtered, filtered_ps)
  ;;

  let solver ps limit =
    let edge_len nth = 2*nth+1 in
    let nth_total nth = 4*nth+1 in
    let nth_ratio r n = (float_of_int r) /. (float_of_int (nth_total n)) in
    let rec count ret ps n =
      if ret = 0 || (nth_ratio ret n) >= limit
      then 
        begin
          Printf.printf "%f " (nth_ratio ret n);
          let (num, ps') = count_prime_corner ps (n+1) in
          count (num+ret) ps' (succ n);
        end
      else edge_len n
    in
    count 0 ps 0
  ;;

  let test () =
    begin
      let prime_ceil = 240000000 in 
      let ps = primes prime_ceil in
      Printf.printf "*** primes collected%!";
      let limit = 0.10 in
      Printf.printf "%d\n" (solver ps limit);
    end
  ;;

end


module InfList : sig
  type 'a t
  val from : int -> int t
  val head : 'a t -> 'a
  val tail : 'a t -> 'a t
  val take : int -> 'a t -> 'a list
  val map  : ('a -> 'b) -> 'a t -> 'b t
  val nth  : int -> 'a t -> 'a
  val primes : int t
end = struct

  type 'a t = Cons of 'a * ('a t lazy_t);;

  (** n, n+1, n+2... *)
  let rec from n = Cons (n, lazy (from (succ n)));;
    
  let head (Cons (x, _)) = x;;
  let tail (Cons (_, xs)) = Lazy.force xs;;

  let take n s =
    let rec take' m (Cons (x, xs)) l =
      if m = 0 then List.rev l
      else take' (pred m) (Lazy.force xs) (x::l)
    in
    take' n s []
  ;;

  let rec map f (Cons (x, xs)) =
    Cons (f x, lazy (map f (Lazy.force xs)))
  ;;

  let rec nth n (Cons (x, xs)) =
    if n = 1 then x
    else nth (pred n) (Lazy.force xs)
  ;;

  let rec sift n (Cons (x, xs)) =
    if x mod n <> 0 then Cons (x, lazy (sift n (Lazy.force xs)))
    else sift n (Lazy.force xs)
  ;;

  let rec sieve (Cons (x, xs)) =
    Cons (x, lazy (sieve (sift x (Lazy.force xs))))
  ;;

  let primes = sieve (from 2);;

end


module B : sig

  val nums : int -> int list
  val int_sqrt : int -> int
  val isPrime : int -> bool
  val test : unit -> unit

end = struct

  let nums limit =
    let rec nums ret = function
      | 0 -> ret
      | n -> nums (n::ret) (pred n)
    in
    nums [] limit
  ;;


  let int_sqrt n =
    int_of_float (sqrt (float_of_int n))
  ;;


  let isPrime = function
    | n when n = 1 -> false
    | n when n mod 2 = 0 -> false
    | n -> List.exists (fun x -> n mod x <> 0) (List.tl (nums (int_sqrt n + 1)))
  ;;


  let corner_num nth =
    let last = 4*nth*(nth-1)+1 in
    let ofs = 2*nth in
    [last - 3*ofs; last - 2*ofs; last - ofs; last]
  ;;


  let solver limit =
    let edge_len nth = 2*nth+1 in
    let nth_total nth = 4*nth+1 in
    let nth_ratio r n = (float_of_int r) /. (float_of_int (nth_total n)) in
    let rec count r n =
      if nth_ratio r n >= limit
      then count (r + (List.length (List.filter isPrime (corner_num n)))) (succ n)
      else edge_len n
    in
    count 1 2
  ;;

  let test () = 
    let _ = 
      begin
        let limit = 0.10 in
        Printf.printf "%d\n" (solver limit)
      end
    in
    ()
  ;;

end


let _ = A.test ();;
