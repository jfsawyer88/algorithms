(** Project Euler **)
(** Maximum Path Sum I **)

(** timing function **)
(*#load "unix.cma";;*)
let time f number =
  let t = Unix.gettimeofday () in  (** get the current time **)
  let time_unit = match number with
    | 1 -> ""
    | 1000 -> "milli-"
    | 1000000 -> "micro-"
    | 1000000000 -> "nano-"
    | _ -> "1/" ^ (string_of_int number) ^ "-"
  in
  let rec loop i =
    if i = number then f ()        (** evaluate f number times **)
    else
      begin
	ignore (f ());
	loop (i + 1)
      end
  in
  let res = loop 1 in              (** keep the result to print **)
  Printf.printf "Execution time: %f %sseconds\n"
                (Unix.gettimeofday () -. t)
		time_unit;
  Printf.printf "Output: %d\n" res;
;;


let scan_max1 l =
  let rec aux a l out =
    match l with
    |    []    -> out
    | hd :: tl -> aux hd tl ((max a hd) :: out)
  in
  List.rev (aux (List.hd l) (List.tl l) [])
;;

let scan_max2 l =
  let rec aux a l =
    match l with
    |    []    -> []
    | hd :: tl -> (max a hd) :: (aux hd tl)
  in
  aux (List.hd l) (List.tl l)
;;

let rec scan_max3 = function
  |      []       -> []
  |    _ :: []    -> []
  | a :: hd :: tl -> (max a hd) :: scan_max3 (hd :: tl)
;;

(**********************************************************)
(**********************************************************)
(**********************************************************)

let scan_max l =
  let rec aux a l out =
    match l with
    |    []    -> out
    | hd :: tl -> aux hd tl ((max a hd) :: out)
  in
  aux (List.hd l) (List.tl l) []
;;


let rec expand1 v n =
  if n = 0 then []
  else v :: expand1 v (n - 1)
;;

let expand2 v n =
  let rec aux n out =
    if n = 0 then out
    else aux (n - 1) (v :: out)
  in
  aux n []
;;


let tri = List.rev [[0]; [1;2]; [3;4;5]; [6;7;8;9]];;
(*expand 0 (1 + List.length tri);;*)

let a = List.rev_map Random.int (expand2 100 1000);;
(*time (fun () -> (ignore (scan_max1 a); 10)) 1;;
time (fun () -> (ignore (scan_max2 a); 10)) 1;;
time (fun () -> (ignore (scan_max3 a); 10)) 1;;*)


let int_range a b =
  let rec int_range_rec l a b =
    if a > b then l
    else int_range_rec (b :: l) a (b - 1)
  in
  int_range_rec [] a b
;;

let rec sum1 = function
  |    []    -> 0
  | hd :: tl -> hd + sum tl
;;

let sum2 l =
  let rec aux s l = 
    match l with
    |    []    -> s
    | hd :: tl -> aux (hd + s) tl
  in
  aux 0 l
;;

let a = List.rev_map Random.int (expand2 100 1000000);;
time (fun () -> (sum1 a)) 1;;
time (fun () -> (sum2 a)) 1;;
