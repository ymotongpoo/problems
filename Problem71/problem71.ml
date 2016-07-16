(** -*- coding: utf-8 -*- *)
(**
   Consider the fraction, n/d, where n and d are positive integers. 
   If n<d and HCF(n,d)=1, it is called a reduced proper fraction.

   If we list the set of reduced proper fractions 
   for d <= 8 in ascending order of size, we get:

   1/8, 1/7, 1/6, 1/5, 1/4, 2/7, 1/3, 3/8, 2/5, 3/7, 
   1/2, 4/7, 3/5, 5/8, 2/3, 5/7, 3/4, 4/5, 5/6, 6/7, 7/8

   It can be seen that 2/5 is the fraction immediately to the left of 3/7.

   By listing the set of reduced proper fractions for d <= 1,000,000 
   in ascending order of size, find the numerator of the fraction 
   immediately to the left of 3/7.
*)

module A : sig

  val fraction : int -> int -> float
    (** [fraction n d] calculates float value of n / d *)

  val find_coprimes_under : int -> int list

  val calc_all_rpf : int -> ((int * int) * float) list
    (** [calc_all_rpf target limit] find all rpf value
        with denominator under [limit] *)

  val calc_nearest_rpf : float -> int -> ((int * int) * float)
    (** [calc_nearest_rpf target limit] find nearest rpf value
        to [target] with denominator under [limit] *)

  val calc_nearest_rpf_directly : int -> int -> int -> ((int * int) * float)
    (** [calc_nearest_rpf_directly n d limit] find nearest rpf value
        to [n] / [d] with denominator under [limit] directly. 
        See experiment.txt to see how I find the regularity of this
        problem.
    *)

  val test : unit -> unit

end = struct

  let max_number = 1000000

  let fraction n d = (float_of_int n) /. (float_of_int d)

  let target = fraction 3 7

  let find_coprimes_under num =
    let is_coprime m n = if Euler.gcd m n = 1 then true else false in
    let rec coprimes accu = function
      | n when n = num -> accu
      | n when is_coprime num n -> coprimes (n::accu) (n+1)
      | n -> coprimes accu (n+1)
    in
    coprimes [] 1
  ;;


  let calc_all_rpf limit =
    let create_rpf_tuple n d =
      ((n, d), (float_of_int n) /. (float_of_int d)) in
    let rec all_rpf accu = function
      | d when d = max_number -> accu
      | d -> 
          let joined = 
            List.fold_left (fun x y -> (create_rpf_tuple y d) :: x) 
              accu (find_coprimes_under d) in
          all_rpf joined (d+1)
    in
    all_rpf [] 2
  ;;


  let calc_nearest_rpf target limit =
    let rec find_rpf _n _d max = function
      | d when d > limit -> ((_n, _d), max)
      | d ->
          let coprimes = find_coprimes_under d in
          (* let _ = List.map (fun x -> Printf.printf "%d/%d " x d) coprimes in *)
          let candidates = List.filter (fun x -> fraction x d < target) coprimes in 
          if candidates = [] then find_rpf _n _d max (d+1)
          else 
            let candidate = Euler.max candidates in
            let cur_val = fraction candidate d in
            if cur_val > max then find_rpf candidate d cur_val (d+1)
            else find_rpf _n _d max (d+1)
    in
    find_rpf 0 0 0.0 2
  ;;


  let calc_nearest_rpf_directly n d limit =
    let target = fraction n d in
    let (firstn, firstd), _ = calc_nearest_rpf target (d+1) in
    let multiple = (limit - firstd) / d in
    let lastn = firstn + n * multiple
    and lastd = firstd + d * multiple in
    ((lastn, lastd), fraction lastn lastd)
  ;;

              
  let test () =
    let print_rpf_tuple n d limit = 
      let ((_n, _d), result) = calc_nearest_rpf_directly n d limit in
      Printf.printf "%d -> (%d / %d, %f)\n%!" limit _n _d result in
    let _ = print_rpf_tuple 3 7 max_number in
    ()
  ;;

end

let _ = A.test ()
