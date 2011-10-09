module A = struct

  let nums limit = 
    let rec nums ret = function
      | 0 -> ret
      | n -> nums (n::ret) (n-1)
    in
    nums [] limit
  ;;

  let filter_acc border xs =
    let rec find ret = function
      | [] -> List.rev ret
      | x::xs -> if x < border then find (x::ret) xs else List.rev ret
    in
    find [] xs
  ;;

  let map f xs =
    let rec map ret = function
      | [] -> List.rev ret
      | x::xs -> map ((f x)::ret) xs
    in
    map [] xs
  ;;

  let rec bottom = function
    | [x] -> x
    | x::xs -> bottom xs
    | [] -> raise Not_found
  ;;

  let primes limit = 
    let sift n l = List.filter (fun x -> x mod n <> 0) l in
    let rec sieve ret ns =
      match ret, ns with
      | r::rs, xs when (r*r) > (bottom xs) ->
          List.rev_append ret ns
      | r, x::xs -> sieve (x::r) (sift x xs)
      | _, [] -> List.rev ret
    in
    sieve [] (List.tl (nums limit))
  ;;

  let odds max_idx = map (fun n -> 2*n-1) (nums max_idx);;

  let has_sqrt n =
    let s = sqrt (float_of_int n) in
    s = float_of_int (int_of_float s)
  ;;
  
  let finder ps n =
    if List.mem n ps
    then false
    else
      let pcands = filter_acc n ps in
      let cands = List.filter (fun x -> x mod 2 = 0) (map (fun x -> n - x) pcands) in
      List.for_all (fun x -> not (has_sqrt (x/2))) cands
  ;;

  let solver ps odds =
    List.filter (fun x -> finder ps x) odds
  ;;

  let test () =
    begin
      let ps = primes 200000 in
      prerr_endline "*** primes collected";
      let os = odds 100000 in
      prerr_endline "*** odds collected";
      let result = solver ps os in
      let _ = map (fun x -> Printf.printf "%d %!" x) result in
      ();
    end
  ;;

end

let _ = A.test ();;
