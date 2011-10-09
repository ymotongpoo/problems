(**
   phi(n) is defined as follows:
   phi(n) = n * (1 - 1/p(1)) * (1 - 1/p(2)) * ... * (1 - 1/p(d))
   where p(k) is prime factor of n.

   minimizing n/phi(n) means maximizing phi(n)/n, which correspond to that
   numbers of prime factor should be less and the prime should be as large
   as possible.
   multiples of the prime are also answer.
*)

module A = struct

  let nums limit =
    let rec nums accu = function
      | 0 -> accu
      | n -> nums (n::accu) (pred n)
    in
    nums [] limit
  ;;

  let rec bottom = function
    | [x] -> x
    | x::xs -> bottom xs
    | [] -> invalid_arg "bottom"
  ;;

  let pow x n =
    let x' = float_of_int x
    and n' = float_of_int n in
    int_of_float (x' ** n')
  ;;

  let primes limit =
    let sift n l = List.filter (fun x -> x mod n <> 0) l in
    let rec sieve accu ns =
      match accu, ns with
      | _, [] -> accu
      | r::rs, ns when (r*r) > (bottom ns) -> List.rev_append accu ns
      | accu, x::xs -> sieve (x::accu) (sift x xs)
    in
    sieve [] (List.tl (nums limit))
  ;;

  let digits_to_number ds =
    let len = (List.length ds) in
    List.fold_left2 (fun x y z -> x+y*(pow 10 (z-1))) 0 ds (List.rev (nums len))
  ;;

  let number_to_digits n =
    let len = String.length (string_of_int n) - 1 in
    let rec n2d accu rest = function
      | 0 -> List.rev (rest::accu)
      | n ->
          let d = rest / (pow 10 n) in
          n2d (d::accu) (rest - d * (pow 10 n)) (pred n)
    in
    n2d [] n len
  ;;

  let rec sweep accu = function
    | [] -> accu
    | x::xs -> sweep (x::accu) (List.filter (fun y -> y <> x) xs)
  ;;

  let permutation l =
    let len = List.length l in
    let remove x xs =
      let rec remove ret x = function
        | [] -> ret
        | y::ys -> 
            if x = y then List.rev_append ret ys
            else remove (y::ret) x ys
      in
      remove [] x xs
    in
    let rec perm n xs a b =
      if n = 0 then a::b
      else List.fold_right (fun x y -> perm (n-1) (remove x xs) (x::a) y) xs b
    in
    sweep [] (perm len l [] [])
  ;;

  let phi' n limit =
    let rec raiser accu p =
      if p*accu < limit 
      then raiser (p*accu) p
      else accu
    in
    raiser (n-1) n
  ;;

  let solver limit =
    let ps = List.rev (primes limit) in
    let rec finder = function
      | [] -> 0
      | p::ps -> 
          let p' = phi' p limit in
          if List.mem p 
            (List.rev_map digits_to_number (permutation (number_to_digits p')))
          then p
          else finder ps
    in
    finder ps
  ;;

  let phi_is_perm n phi =
    List.mem phi (List.map digits_to_number (permutation (number_to_digits n)))
  ;;

  let solver2 limit ps =
    let rec prod accu p1 = function
      | [] -> Printf.printf "...finished %d\n%!" p1; accu
      | p2::pps when p1*p2 > accu && p1*p2 < limit -> 
          if phi_is_perm (p1*p2) ((p1-1)*(p2-1)) 
          then prod (p1*p2) p1 pps
          else prod accu p1 pps
      | p2::pps -> prod accu p1 pps
    in
    let rec finder = function
      | [] -> 0
      | x::xs -> 
          let c = prod 0 x xs in
          if c <> 0 then c else finder xs
    in
    finder ps
  ;;

  let test () =
    begin
      let limit = pow 10 7 in
      let ps = List.rev (primes (limit/2)) in
      Printf.printf "*** primes collected\n%!";
      Printf.printf "%d\n%!" (solver2 limit ps);
    end
  ;;

end

let _ = A.test ();;
