(** functional heap/priority queue **)
(** borrowed from http://typeocaml.com/2015/03/12/heap-leftist-tree/ **)


(** type **)
type 'a leftist =
  | Leaf
  | Node of 'a leftist * 'a * 'a leftist * int
;;

(** essentials **)
let singleton k = Node (Leaf, k, Leaf, 1);;

let rank  = function
  | Leaf -> 0
  | Node (_,_,_,r) -> r
;;

(** merge **)
let rec merge t1 t2 =
  match t1,t2 with
  | Leaf, t
  | t, Leaf -> t
  | Node (l, k1, r, _), Node (_, k2, _, _) ->
     if k1 > k2 then merge t2 t1 (* switch merge if necessary *)
     else
       let merged = merge r t2 in (* always merge with right *)
       let rank_left = rank l and rank_right = rank merged in
       if rank_left >= rank_right then Node (l, k1, merged, rank_right + 1)
       else Node (merged, k1, l, rank_left + 1) (* left becomes right due
                                                   to being shorter *)
;;

(** insert, get_min, delete_min **)
let insert t x = merge (singleton x) t;;

let get_min = function
  | Leaf -> failwith "empty"
  | Node (_, k, _, _) -> k
;;

let delete_min = function
  | Leaf -> failwith "empty"
  | Node (l, _, r, _) -> merge l r
;;

