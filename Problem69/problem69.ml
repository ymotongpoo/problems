(* -*- coding:utf-8; -*- *)

module A = struct

  let rec gcm a b =
    match a, b with
    | x, 0 | 0, x -> x
    | a', b' when a' = b' -> a'
    | a', b' when a' > b' -> gcm b' (a' mod b')
    | a', b' -> gcm a' (b' mod a')
  ;;

  let phi n = 
    let rec phi accu k =
      match k, gcm n k with
      | k, _ when k = n -> accu
      | 1, _ -> phi (succ accu) (succ k)
      | _, 1 -> phi (succ accu) (succ k)
      | _, _ -> phi accu (succ k)
    in
    phi 0 1
  ;;

  let ratio n = (float_of_int n) /. (float_of_int (phi n));;

  let solver limit =
    let rec solver accu n = function
      | 1 -> n
      | k ->
          if ratio k > accu
          then solver (ratio k) k (pred k)
          else solver accu n (pred k)
    in
    solver 0.0 0 limit
  ;;

  let test () =
    begin
      Printf.printf "%d\n%!" (solver 10);
      Printf.printf "%d\n%!" (solver 100);
      Printf.printf "%d\n%!" (solver 1000);
      Printf.printf "%d\n%!" (solver 10000);
      Printf.printf "%d\n%!" (solver 100000);
      Printf.printf "%d\n%!" (solver 1000000);
    end
  ;;

end


(**
   http://eli.thegreenplace.net/2009/02/28/project-euler-problem-69/
   http://en.wikipedia.org/wiki/Euler's_totient_function

   miximizing n/phi(n) means minimizing phi(n)/n, and phi(n) is
   defined as follows:
   phi(n) = n * (1 - 1/p(1)) * (1 - 1/p(2)) * ... * (1 - 1/p(d))
   where p(k) is prime factor of n.

   So, what we have to do is find maximun product of primes 
   which is below 1,000,000.
*)

module B = struct

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

  let solve ps limit =
    let rec finder accu = function
      | [] -> accu
      | p::ps ->
          if p*accu < limit 
          then finder (p*accu) ps
          else accu
    in
    finder 1 ps
  ;;

  let test () =
    begin
      let ps = primes 100000 in
      Printf.printf "%d\n%!" (solve ps 1000000);
    end
  ;;

end

let _ = B.test ();;
