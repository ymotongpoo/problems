module A = struct

  let nums n = 
    let rec nums ret = function
      | 0 -> ret
      | n -> nums (n::ret) (n-1)
    in
    nums [] n
  ;;

  let numbers = nums 9;;

  let comb n l =
    let rec comb l c =
      if (List.length c) = n then [c] else
        match l with
        | [] -> []
        | (h::t) -> List.rev_append (comb t (h::c)) (comb t c)
    in
    comb l []
  ;;

  let perm n l =
    let rec perm n xs a b =
      let remove x xs = List.filter (fun y -> x <> y) xs in
      if n = 0 then a::b
      else List.fold_right (fun x y -> perm (n-1) (remove x xs) (x::a) y) xs b
    in
    perm n l [] []
  ;;

  let dig2num ds =
    let len = List.length ds in
    let rec pow x = function
      | 0 -> 1
      | n -> x * (pow x (n-1))
    in
    List.fold_left2 (fun x y z -> x + y * (pow 10 (z-1))) 0 ds (List.rev (nums len))
  ;;

  let rec takeMN retm retn ds m n =
      match m, n with
      | m, _ when m > List.length ds -> None
      | _, n when n > List.length ds -> None
      | 0, 0 -> Some (List.rev retm, List.rev retn, ds)
      | 0, n -> takeMN retm ((List.hd ds)::retn) (List.tl ds) m (n-1)
      | m, _ -> takeMN ((List.hd ds)::retm) retn (List.tl ds) (m-1) n
  ;;

  let check ds =
    let is_pand = function
      | None -> false
      | Some (a,b,c) -> (dig2num a)*(dig2num b)=(dig2num c) 
    in
    let rec find ret a b =
      let t = takeMN [] [] ds a b in
      match t, a, b with
      | _, a, _ when a > List.length ds -> ret
      | _, a, b when a+b > List.length ds -> 
          find ret (a+1) 1
      | Some (_, _, r), _, _ when is_pand t -> 
          find ((dig2num r)::ret) a (b+1)
      | _, _, _ -> 
          find ret a (b+1)
    in
    find [] 1 1
  ;;

  let print_list_list ll =
    List.map (fun x -> List.map (fun y -> Printf.printf "%d " y) x) ll
  ;;

  let roller pands =
    let rec roller ret = function
      | [] -> ret
      | x::xs -> roller (List.append (check x) ret) xs
    in
    roller [] pands
  ;;

  let sweep_dup pands =
    let rec sweep ret = function
      | [] -> ret
      | x::xs -> sweep (x::ret) (List.filter (fun y -> x <> y) xs)
    in
    sweep [] pands
  ;;

  let test () =
    begin
      let l = sweep_dup (roller (perm (List.length numbers) numbers)) in
      let _ = List.map (fun x -> Printf.printf "%d " x) l in
      Printf.printf "\n";
      Printf.printf "%d\n" (List.fold_left (+) 0 l);
      ();
    end
  ;;

end

let _ = A.test ();;
