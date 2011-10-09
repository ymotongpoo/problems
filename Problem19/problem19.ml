(** -*- coding: utf-8 -*- *)
(**
Problem 19

You are given the following information, 
but you may prefer to do some research for yourself.

    * 1 Jan 1900 was a Monday.
    * Thirty days has September,
      April, June and November.
      All the rest have thirty-one,
      Saving February alone,
      Which has twenty-eight, rain or shine.
      And on leap years, twenty-nine.
    * A leap year occurs on any year evenly divisible by 4, 
      but not on a century unless it is divisible by 400.

How many Sundays fell on the first of the month during the twentieth century 
(1 Jan 1901 to 31 Dec 2000)?
*)


module A : sig

  val is_leap : int -> bool

  val days_of_year : int -> int

  val days_of_mon : int -> int -> int

  val from_1900_1 : int -> int -> int

  val is_sunday : int -> int -> bool

  val test : unit -> unit

end = struct
       
  let is_leap = function
    | y when y mod 4 = 0 ->
        if y mod 400 = 0 then true
        else 
          if y mod 100 <> 0 then true
          else false
    | _ -> false
  ;;


  let days_of_year = function
    | year when is_leap year -> 366
    | _ -> 365
  ;;


  let days_of_mon year mon =
    match year, mon with
    | _, m when (List.mem m [1;3;5;7;8;10;12]) -> 31
    | _, m when (List.mem m [4;6;9;11]) -> 30
    | y, 2 when is_leap y -> 29
    | _, 2 -> 28
    | _, _ -> failwith("days_of_mon")
  ;;


  let from_1900_1 year mon = 
    let rec from_1st_day_of_year accu year mon =
      match mon with
      | 1 -> accu
      | m when m < 13 -> 
          from_1st_day_of_year ((days_of_mon year (m-1))+accu) year (m-1)
      | _ -> failwith("from_1900_1")
    in
    let rec from_1900_to accu = function
      | 1900 -> accu
      | y when y > 1900 -> from_1900_to ((days_of_year (y-1))+accu) (y-1)
      | _ -> failwith("from_1900_to")
    in
    (from_1900_to 0 year) + (from_1st_day_of_year 0 year mon)
  ;;


  let is_sunday year mon =
    if (from_1900_1 year mon) mod 7 = 6 then true else false
  ;;
  

  let solver () =
    let rec solver accu year mon =
      match year, mon with
      | 2001, _ -> accu
      | _, 13 -> solver accu (year+1) 1
      | y, m -> 
          if is_sunday y m then solver (accu+1) y (m+1)
          else solver accu y (m+1)
    in
    solver 0 1901 1
  ;;
    
  let test () = 
    let _ = print_endline (string_of_bool (is_leap 400)) in
    let _ = print_endline (string_of_int (days_of_year 2000)) in
    let _ = print_endline (string_of_int (days_of_mon 1901 2)) in
    let _ = print_endline (string_of_int (from_1900_1 1901 2)) in
    let _ = print_endline (string_of_int (solver ())) in
    ()
  ;;

end

let _ = A.test ()
