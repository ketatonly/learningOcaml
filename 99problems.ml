(* Problem 1: Tail of a List
  Write a function last : 'a list -> 'a option that returns the last element of a list *)

let rec last l = 
  match l with
   [] -> None
  | [a] -> Some a
  | h::t -> last t;;

(* tests: *)
 last ["a" ; "b" ; "c" ; "d"];;  (*Some "d"*)
 last [];;  (*None*)

(* Problem 2: Last Two Elements of a List
  Find the last two (last and penultimate) elements of a list. *)

let rec last_two l = 
  match l with
   [] | [_] -> None
  | [a;b] -> Some (a, b)
  | _::t -> last_two t;;

last_two ["a"; "b"; "c"; "d"];;  (*Some ("c", "d")*)
last_two ["a"];;   (*None*)

(* Problem 3: N'th Element of a List
  Find the N'th element of a list. *)

let rec at n l =
  match l with 
   [] -> None
  | h::t -> if n=0 then Some h else at (n-1) t;;

at 2 ["a"; "b"; "c"; "d"; "e"];; (*Some "c"*)
at 2 ["a"];;  (*None*)


(* Problem 4: Length of a List
  Find the number of elements of a list.
  OCaml standard library has List.length but we ask that you reimplement it. Bonus for a tail recursive solution. *)

let length l = let rec helper l acc = 
  match l with
   [] -> acc
  | _::t -> helper t (acc+1)
in helper l 0;;

length ["a"; "b"; "c"];; (*3*)
length [];;  (*0*)


(* Problem 5 : Reverse a List
  Reverse a list.
  OCaml standard library has List.rev but we ask that you reimplement it. *)

let rev l = let rec helper l acc =
  match l with
   [] -> acc
  | h::t -> helper t (h::acc)
in helper l [];;

rev ["a"; "b"; "c"];;  (* ["c"; "b"; "a"] *)


(* Problem 6: Palindrome
  Find out whether a list is a palindrome.
  Hint: A palindrome is its own reverse. *)

let is_palindrome l = 
  match l with 
   [] | [_] -> true
  | h::t -> l = List.rev l;;

is_palindrome ["x"; "a"; "m"; "a"; "x"];; (* true *)
not (is_palindrome ["a"; "b"]);;  (* true *)


(* Problem 7 : Flatten a List
  Flatten a nested list structure. *)


