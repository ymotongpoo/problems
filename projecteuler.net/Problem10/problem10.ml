module A = struct
  let limit = 2000000;;
  
  let numbers n limit =
    let rec numbers ret = function
      | n when n=limit -> List.rev (n::ret)
      | n -> numbers (n::ret) (n+1)
    in
    numbers [] n
  ;;

  (** [l] must be acsending order *)
  let sieve l =
    let sift n l = List.filter (fun x -> (x mod n) <> 0) l in
    let rec sieve ret l =
      match ret, l with
      | _, [] -> ret
      | [], x::xs -> sieve [x] (sift x xs)
      | r::rs, l when (r*r) > (List.hd (List.rev l)) -> 
          (List.rev l) @ (r::rs)
      | ret, x::xs -> sieve (x::ret) (sift x xs)
    in
    sieve [] l
  ;;

  let primes n limit =
    let ns = numbers n limit in
    sieve ns
  ;;

  let test () =
    let result = List.fold_left (+) 0 (primes 2 limit) in
    begin
      Printf.printf "sum of primes below %d is %d\n" limit result;
      ();
    end
  ;;
end


let () = A.test ();;
