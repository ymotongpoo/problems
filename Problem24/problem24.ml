module A = struct

  (* this solution do now work to be stack overflow *)

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
    List.sort (fun x y -> x - y) (List.map d2n (perm n l [] []))
  ;;

  let test () =
    begin
      let numbers = 0::(nums 9) in
      let result = perm (List.length numbers) numbers in
      Printf.printf "%d\n" (List.nth result 1000000);
    end
  ;;

end

module B = struct

  let rec fact = function
    | 0 -> 1
    | n -> n * fact (n-1)
  ;;

  let find index labels =
    let remove_nth ls nth =
      let rec remove_nth ret ls nth =
        match ls, nth with
        | [], _ -> List.rev ret
        | l::ls, 0 -> List.rev_append ret ls
        | l::ls, n -> remove_nth (l::ret) ls (n-1)
      in
      remove_nth [] ls nth
    in
    let rec find ret rest ls =
      match rest, ls with
      | 0, _  -> List.rev_append ret ls
      | _, [] -> List.rev ret
      | _, _ ->
          let len = List.length ls - 1 in
          find ((List.nth ls (rest/(fact len)))::ret) (rest mod (fact len))
            (remove_nth ls (rest/(fact len)))
    in
    find [] index labels
  ;;

  let test () = 
    begin
      let labels = [0;1;2;3;4;5;6;7;8;9] in
      let _ = List.map (fun x -> Printf.printf "%d" x) (find 999999 labels) in
      Printf.printf "\n";
      ();
    end
  ;;

end


let _ = B.test ();;
