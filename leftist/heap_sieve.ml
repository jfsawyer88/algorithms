(** functional heap/priority queue **)
(** borrowed from http://typeocaml.com/2015/03/12/heap-leftist-tree/ **)

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


(** type **)
(** also includes incrementer **)
type 'a leftist =
  | Leaf
  | Node of 'a leftist * 'a * 'a * 'a leftist * int
;;

(** essentials **)
let singleton k v = Node (Leaf, k, v, Leaf, 1);;

let rank  = function
  | Leaf -> 0
  | Node (_,_,_,_,r) -> r
;;

(** merge **)
let rec merge t1 t2 =
  match t1,t2 with
  | Leaf, t
  | t, Leaf -> t
  | Node (l, k1, v1, r, _), Node (_, k2, v2, _, _) ->
     if k1 > k2 then merge t2 t1 (* switch merge if necessary *)
     else
       let merged = merge r t2 in (* always merge with right *)
       let rank_left = rank l and rank_right = rank merged in
       if rank_left >= rank_right then Node (l, k1, v1, merged, rank_right + 1)
       else Node (merged, k1, v1, l, rank_left + 1) (* left becomes right due
                                                       to being shorter *)
;;

(** insert, get_min, delete_min **)
let insert t k v = merge (singleton k v) t;;

let get_min_key = function
  | Leaf -> failwith "empty"
  | Node (_, k, _, _, _) -> k
;;

let get_min_val = function
  | Leaf -> failwith "empty"
  | Node (_, _, v, _, _) -> v
;;

let delete_min = function
  | Leaf -> failwith "empty"
  | Node (l, _, _, r, _) -> merge l r
;;


let sieve limit =
  let rec aux curr primes tree =
    if curr = limit then primes
    else
      let next_composite = get_min_key tree in
      if curr < next_composite then aux (succ curr)
					(curr :: primes)
					(insert tree (curr * curr) curr)
      else
	let iter = get_min_val tree in
	aux curr primes (insert (delete_min tree)
				(next_composite + iter)
				(iter))
  in
  aux 3 [2] (singleton 4 2)
;;



let sieve limit =
  let rec aux2 comp tree =
    if comp <> get_min_key tree then tree
    else
      let iter = get_min_val tree in
      aux2 comp (insert (delete_min tree)
			(comp + iter)
			(iter))
    in
  let rec aux curr primes tree =
    if curr = limit then primes
    else
      let next_composite = get_min_key tree in
      if curr < next_composite then aux (succ curr)
					(curr :: primes)
					(insert tree (curr * curr) curr)
      else
	aux (succ curr) primes (aux2 next_composite tree)
  in      
  aux 3 [2] (singleton 4 2)
;;

let sum_primes limit =
  let rec aux2 comp tree =
    if comp <> get_min_key tree then tree
    else
      let iter = get_min_val tree in
      aux2 comp (insert (delete_min tree)
			(comp + iter)
			(iter))
    in
  let rec aux curr primes tree =
    if curr = limit then primes
    else
      let next_composite = get_min_key tree in
      if curr < next_composite then aux (succ curr)
					(curr + primes)
					(insert tree (curr * curr) curr)
      else
	aux (succ curr) primes (aux2 next_composite tree)
  in      
  aux 3 2 (singleton 4 2)
;;
