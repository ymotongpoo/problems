module A = struct

  let tri n = n*(n+1)/2;;
  let penta n = n*(3*n-1)/2;;
  let hexa n = n*(2*n-1);;

  let nums gen limit = 
    let rec nums ret = function
      | 0 -> ret
      | n -> nums ((gen n)::ret) (n-1)
    in
    nums [] limit
  ;;

  let find_tri_pen_hexa limit =
    let tris = nums tri limit in
    let pentas = nums penta limit in
    let hexas = nums hexa limit in
    let rec find_ph ret tris =
      match tris with
      | [] -> ret
      | t::ts ->
          if (List.mem t pentas) && (List.mem t hexas)
          then find_ph (t::ret) ts
          else find_ph ret ts
    in
    find_ph [] tris
  ;;

  let test () = 
    begin
      let l = find_tri_pen_hexa 100000 in
      let _ = List.map (fun x -> Printf.printf "%d " x) l in
      ();
    end
  ;;

end


let _ = A.test ();;
