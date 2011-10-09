module A = struct

  let nums limit =
    let rec nums ret = function
      | 0 -> ret
      | n -> nums (n::ret) (n-1)
    in
    nums [] limit
  ;;


  let rec pow x = function
    | 0 -> 1
    | n -> x * (pow x (n-1))
  ;;


  let primes limit =
    let sift n l = List.filter (fun x -> (x mod n) <> 0) l in
    let rec sieve ret l =
      match ret, l with
      | _, [] -> List.rev ret
      | [], x::xs -> sieve [x] (sift x xs)
      | r::rs, xs when (r*r) > (List.hd (List.rev xs)) ->
          List.rev_append (r::rs) xs
      | ret, x::xs -> sieve (x::ret) (sift x xs)
    in
    sieve [] (List.tl (nums limit))
  ;;


  let d2n ds =
    let len = List.length ds in
    List.fold_left2 (fun x y z -> x+y*(pow 10 (z-1))) 0 ds (List.rev (nums len))
  ;;

  let rec sweep ret = function
    | [] -> ret
    | x::xs -> sweep (x::ret) (List.filter (fun y -> y <> x) xs)
  ;;

  let n2d num =
    let len = String.length (string_of_int num) - 1 in
    let rec n2d ret rest = function
      | 0 -> List.rev (rest::ret)
      | n ->
          let d = rest / (pow 10 n) in
          n2d (d::ret) (rest - d*(pow 10 n)) (n-1)
    in
    n2d [] num len
  ;;

  let perm n l =
    let remove x xs =
      let rec remove ret x = function
        | [] -> ret
        | y::ys -> 
            if x = y then (List.rev ret) @ ys
            else remove (y::ret) x ys
      in
      remove [] x xs
    in
    let rec perm n xs a b =
      if n = 0 then a::b
      else List.fold_right (fun x y -> perm (n-1) (remove x xs) (x::a) y) xs b
    in
    sweep [] (perm n l [] [])
  ;;

  let comb n l =
    let rec comb l c =
      if (List.length c) = n then [c] else
        match l with
        | [] -> []
        | (h::t) -> List.rev_append (comb t (h::c)) (comb t c)
    in
    comb l []
  ;;
  
  let roller ps = 
    let is_fun cands =
      let pairs = comb 2 cands in
      List.exists (fun x -> List.mem ((List.fold_left (+) 0 x)/2) cands) pairs
    in
    let rec roller ret = function
      | [] -> ret
      | x::xs ->
          let d_x = n2d x in
          let perms = List.map (fun x -> d2n x) (perm (List.length d_x) d_x) in
          let valid = List.filter (fun x -> List.mem x ps) perms in
          if List.length valid > 2 && is_fun valid
          then roller ((List.sort (fun x y -> x-y) valid) :: ret)
            (List.filter (fun x -> not (List.mem x valid)) xs)
          else roller ret xs;
    in
    roller [] ps
  ;;

  let rec print_list_list = function
    | [] -> ();
    | l::ls ->
        begin
          let _ = List.map (fun x -> Printf.printf "%d " x) l in
          Printf.printf "\n";
          print_list_list ls
        end
  ;;

  let test () =
    begin
      let four_dig_prime = List.filter (fun x -> x >= 1000) (primes 10000) in
      let result = roller four_dig_prime in
      let _ = Printf.printf "%d\n" (List.length result) in
      print_list_list result;
      ();
    end
  ;;


end

let _ = A.test ();;
