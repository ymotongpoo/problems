module A = struct

  let pow x n =
    int_of_float ((float_of_int x) ** (float_of_int n))
  ;;
  
  let nums limit =
    let rec nums ret = function
      | 0 -> ret
      | n -> nums (n::ret) (n-1)
    in
    nums [] limit
  ;;

  let rec bottom = function
    | [x] -> x
    | x::xs -> bottom xs
    | [] -> raise Not_found
  ;;

  let filter f list =
    let rec filter ret = function
      | [] -> List.rev ret
      | x::xs -> if f x then filter (x::ret) xs else filter ret xs
    in
    filter [] list
  ;;

  let map f list =
    let rec map ret = function
      | [] -> List.rev ret
      | x::xs -> map ((f x)::ret) xs
    in
    map [] list
  ;;

  let primes limit =
    let rec sift n l = filter (fun x -> (x mod n) <> 0) l in
    let rec sieve ret ns =
      match ret, ns with
      | r::rs, xs when (r*r) > bottom xs ->
          List.rev_append ret ns
      | r, x::xs ->
          sieve (x::r) (sift x xs)
      | r, [] ->
          List.rev r
    in
    sieve [] (List.tl (nums limit))
  ;;

  let n2d num =
    let len = String.length (string_of_int num) - 1 in
    let rec n2d ret n rest =
      match n, rest with
      | (-1), _ -> List.rev ret
      | _, r ->
          let d = r / (pow 10 n) in
          n2d (d::ret) (n-1) (rest mod (pow 10 n))
    in
    n2d [] len num
  ;;

  let d2n ds =
    let len = List.length ds in
    let base = nums len in
    List.fold_left2 (fun x y z -> x+y*(pow 10 (z-1))) 0 ds (List.rev base)
  ;;

  let perm l =
    let remove x xs = filter (fun y -> x <> y) xs in
    let rec perm n xs a b =
      if n = 0 then a::b
      else List.fold_right (fun x y -> perm (n-1) (remove x xs) (x::a) y) xs b
    in
    perm (List.length l) l [] []
  ;;

  let takeN xs n =
    let rec takeN ret xs n =
      match xs, n with
      | _, 0 | [], _ -> List.rev ret
      | x::xs, n -> takeN (x::ret) xs (n-1) 
    in
    takeN [] xs n
  ;;

  let take3 = function
    | x1::x2::x3::xs -> [x1;x2;x3]
    | [] -> []
    | x1::[] -> []
    | x1::x2::[] -> []
  ;;

  let is_funny ps ds =
    (**
    let rec funny c = function
      | [] -> false
      | p::ps when c mod p = 0 -> true 
      | p::ps -> if c < p then false else funny c ps
    in
     *)
    let rec is_funny ps ds =
      match ps, ds with
      | [], _ | _, [] -> true
      | p::ps, d'::ds' when List.length ds > 2 ->
          let c = d2n (take3 ds) in
          if c mod p = 0 then is_funny ps ds'
          else false
      | _, d'::ds' -> is_funny ps ds'
    in
    is_funny ps ds
  ;;

  let solver ps cands =
    let rec solver ret = function
      | [] -> ret
      | c::cs -> 
          if is_funny ps c
          then solver (c::ret) cs
          else solver ret cs
    in
    solver [] cands
  ;;

  let string_of_bool b = if b then "true" else "false";;

  let test () =
    begin
      let cands = perm (0::(nums 9)) in
      prerr_endline "*** candidates collected";
      let ps = 1::(primes 17) in
      prerr_endline "*** primes collected";
      let result = solver ps cands in
      prerr_endline "*** solved";
      let result_num = map (fun x -> d2n x) result in
      Printf.printf "%d\n" (List.length result_num);
      let _ = map (fun x -> Printf.printf "%d " x) result_num in
      Printf.printf "\n%d\n%!" (List.fold_left (+) 0 result_num);
    end
  ;;

end

let _ = A.test ();;

