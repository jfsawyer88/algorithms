(** using streams **)
(** from http://www.cs.cornell.edu/courses/cs3110/2011sp/lectures/lec24-streams/streams.htm **)


type 'a stream = Nil | Cons of 'a * (unit -> 'a stream);;

(* an infinite stream of 1's *)
let rec (ones : int stream) = Cons (1, fun () -> ones);;

(* the natural numbers *)
let rec from (n : int) : int stream =
  Cons (n, fun () -> from (n + 1))
;;

let naturals = from 0;;

(* head of a stream *)
let hd (s : 'a stream) : 'a =
  match s with
    Nil -> failwith "hd"
  | Cons (x, _) -> x
;;

(* tail of a stream *)
let tl (s : 'a stream) : 'a stream =
  match s with
    Nil -> failwith "tl"
  | Cons (_, g) -> g () (* get the tail by evaluating the thunk *)
;;

(* n-th element of a stream *)
let rec nth (s : 'a stream) (n : int) : 'a =
  if n = 0 then hd s else nth (tl s) (n - 1)
;;

(* make a stream from a list *)
let from_list (l : 'a list) : 'a stream =
  List.fold_right (fun x s -> Cons (x, fun () -> s)) l Nil
;;

(* make a list from the first n elements of a stream *)
let rec take (s : 'a stream) (n : int) : 'a list =
  if n <= 0 then [] else
  match s with
    Nil -> []
  | _ -> hd s :: take (tl s) (n - 1)
;;

let rec map (f : 'a -> 'b) (s : 'a stream) : 'b stream =
  match s with Nil -> Nil
	     | _ -> Cons (f (hd s), fun () -> map f (tl s))
;;

let rec filter (f : 'a -> bool) (s : 'a stream) : 'a stream =
  match s with Nil -> Nil
	     | Cons (x, g) ->
      if f x then Cons (x, fun () -> filter f (g ()))
      else filter f (g ())
;;



(* delete multiples of p from a stream *)
let sift (p : int) : int stream -> int stream =
  filter (fun n -> n mod p <> 0)
;;

let rec sieve (s : int stream) : int stream =
  match s with Nil -> Nil
	     | Cons (p, g) -> Cons (p, fun () -> sieve (sift p (g ())))
;;

let primes = sieve (from 2);;
