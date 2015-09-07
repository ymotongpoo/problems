module A = struct
  type 'a seq = Cons of 'a * (unit -> 'a seq);;

  let rec from n = Cons (n, fun () -> from (n+1));;

  let rec sift n (Cons (x, f)) =
    if (x mod n) = 0 then sift n (f ())
    else Cons (n, fun () -> sift n (f ()))
  ;;

  let rec sieve (Cons (x, f)) = Cons (x, fun () -> sieve (sift x (f ())));;

  let primes = sieve (from 2);;

  let rec nthseq n (Cons (x, f)) =
    if n = 1 then x
    else nthseq (n-1) (f())
  ;;

  let test () =
    Printf.printf "%d\n" (nthseq 10001 primes);;
end

              
module B = struct
  let interval = 200000

  let numbers n1 n2 =
    let rec from ret = function
    | x when x=n1 -> n1::ret
    | x -> from (x::ret) (x-1)
    in from [] (n1+n2)
  ;;

  (** [l] must be accending order*)
  let rec sieve seed l =
    let sift n l = List.filter (fun x -> (x mod n) <> 0) l in
    match seed, l with
    | _, [] -> seed
    | r::rs, x::xs when (r*r) > List.hd (List.rev (x::xs)) -> (List.rev (x::xs)) @ (r::rs)
    | [], x::xs -> sieve [x] (sift x xs)
    | ret, x::xs -> sieve (x::ret) (sift x xs) 
  ;;

  let test n =
    let primes =  List.rev (sieve [] (numbers 2 interval)) in
    let rec takeN ret n l =
      match n, l with
      | 0, _ -> List.rev ret
      | n, [] -> []
      | n, x::xs -> takeN (x::ret) (n-1) xs
    in
    begin
      let _ = List.map (fun n -> Printf.printf "%d " n) (takeN [] 20 primes) in
      Printf.printf "\n";
      Printf.printf "length of prime list is %d\n" (List.length primes);
      Printf.printf "largest one is %d\n" (List.hd (List.rev primes));
      Printf.printf "%d th primes is %d\n" 2 (List.nth primes 2);
      Printf.printf "%d th primes is %d\n" n (List.nth primes (n-1));
      ();
    end;
  ;;
end


(* A.test ();; *)

let () = B.test 10001;;
