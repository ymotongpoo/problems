module A = struct

  let solutions p =
    let rec find ret x y =
      let z = p-x-y in
      match x, y with
      | x, _ when x > z -> ret
      | _, y when y > z -> find ret (x+1) (x+1)
      | x, y -> 
          if (x*x)+(y*y)=(z*z) 
          then find ((x,y,z)::ret) x (y+1)
          else find ret x (y+1)
    in
    find [] 1 1
  ;;

  let max_solution limit =
    let rec roller p num =
      function
      | 0 -> (p, num)
      | n -> 
          let ntrios = List.length (solutions n) in
          if num < ntrios then roller n ntrios (n-1)
          else roller p num (n-1)
    in
    roller limit 0 limit
  ;;

  let test () =
    begin
      let (p, num) = max_solution 1000 in
      Printf.printf "%d -> %d\n" p num;
    end
  ;;

end

let () = A.test ();;
