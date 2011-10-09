module A = struct 

  let limit = 100000;;
  
  let numbers n = 
    let rec numbers ret = function
      | 0 -> ret
      | n -> numbers (n::ret) (n-1)
    in
    numbers [] n
  ;;
  
  let nth_triangle n = List.fold_left (+) 0 (numbers n);;

  let primes n =
    let sift n l = List.filter (fun x -> (x mod n) <> 0) l in
    let rec sieve ret l = 
      match ret, l with
      | _, [] -> ret
      | [], x::xs -> sieve [x] (sift x xs)
      | r::rs, l when (r*r) > (List.hd (List.rev l)) 
          -> (List.rev (r::rs)) @ l
      | ret, x::xs -> sieve (x::ret) (sift x xs)
    in
    sieve [] (List.tl (numbers n))
  ;;

  let primes_n = primes limit;;

  let count_factors n =
    let rec count ret c n ps =
      match n, ps with
      | _, [] | 1, _ -> (1::ret)
      | n, p::ps ->
          if (n mod p) = 0 then count ret (c+1) (n/p) (p::ps)
          else count (c::ret) 0 n ps
    in
    count [] 0 n primes_n
  ;;

  let num_factors n =
    List.fold_left ( * ) 1 (List.map (fun x -> x+1) (count_factors n))
  ;;

  let find_factors_over n =
    let rec finder t =
      let tri = nth_triangle t in
      if n <= (num_factors tri) then tri
      else finder (t+1)
    in
    finder 1
  ;;

  let test () =
    begin
      Printf.printf "%d\n" (List.length primes_n);
      Printf.printf "%d\n" (find_factors_over 500);
    end
  ;;

end

let () = A.test ();;

    
