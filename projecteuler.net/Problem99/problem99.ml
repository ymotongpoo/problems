module A = struct

  let filename = "base_exp.txt"

  let load_sample filename = 
    let cin = open_in filename in
    let lines = ref [] 
    and line_num = ref 0 in
    try
      while true;
      do
        lines := (!line_num, (input_line cin)) :: !lines;
        line_num := succ !line_num;
      done; []
    with
      End_of_file -> 
        close_in cin;
        List.rev !lines
  ;;


  let split c str =
    let len = String.length str in
    let rec divider_at = function
      | n when n = len -> 0
      | n ->
          if str.[n] = c then n
          else divider_at (succ n)
    in
    let split_pos = divider_at 0 in
    try
      let former = String.sub str 0 split_pos
      and latter = String.sub str (split_pos + 1) (len - split_pos - 2) in
      (former, latter)
    with
      Invalid_argument _ ->
        Printf.printf "%s -> %d\n" str split_pos;
        ("0", "0.0")
  ;;


  let log10ize base power = (float_of_int power) *. (log10 (float_of_int base))

  let solver lines =
    let rec solver max_idx max_val rest =
      match rest with
      | [] -> max_idx
      | _ ->
          let i, line = (List.hd rest) in
          let base, power = split ',' line in
          let value = log10ize (int_of_string base) (int_of_string power) in
          if value > max_val then solver i value (List.tl rest)
          else solver max_idx max_val (List.tl rest)
    in
    solver 0 0.0 lines
  ;;
      
  let test () =
    let lines = load_sample filename in
    Printf.printf "%d th is largest\n%!" ((solver lines) + 1)
  ;;

end

let _ = A.test ()


