module A = struct
  
  let find_trio total =
    let is_pythagorean (a, b, c) = (a*a + b*b) = (c*c) in
    let rec find_trio ret x y =
      let z = total - x - y in
      match x, y with
      | x, _ when z <= x -> ret
      | _, y when z <= y -> find_trio ret (x+1) (x+2)
      | x, y -> 
          if is_pythagorean (x, y, z) 
          then find_trio ((x, y, z)::ret) x (y+1)
          else find_trio ret x (y+1)
    in
    find_trio [] 1 2
  ;;

  let prod_of_trio (x,y,z) = x*y*z;;
  let print_trio (x,y,z) = Printf.printf "(%d, %d, %d)\n" x y z;;

  let test () =
    let result = find_trio 1000 in
    begin
      let _ = List.map print_trio result in
      let prod_results = List.map prod_of_trio result in
      let _ = List.map (fun n -> Printf.printf "%d\n" n) prod_results in
      ();
    end

end


let () = A.test ();;
