(* 8 subtasks *)
(* 1 *)
(* Write a function member, which takes a comparision function c, a
term t and a list l and returns true if l contains an element e such
that e and t are equal with respect to c. *)
let rec member c t l = match l with 
     [] -> false 
    | x::xs -> if c t x = 0 then true else member c t xs;;

member compare 3 [1; 2; 3];;  (* true *)
member compare 4 [1; 2; 3];;  (* false *)
member compare 'a' ['a'; 'b'; 'c'];;  (* true *)


let equal_second_components (_, x) (_, y) = compare x y;;

member equal_second_components ('a',5) [(1,2);(3,4); (5,6)];;  (* false *)
member equal_second_components ('a',6) [(1,2);(3,4); (5,6)];;  (* true *)
member equal_second_components (42, 6) [(1,2);(3,4); (5,6)];;  (* true *)


let evens_eq_evens_odds_eq_odds n1 n2 = compare (n1 mod 2) (n2 mod 2);;

member evens_eq_evens_odds_eq_odds 4 [1; 2; 3];; (* true *)
member evens_eq_evens_odds_eq_odds 4 [1; 3; 5];; (* false *)


(* 2 *)
let count_occurrences lst =
  let rec count x = function
    | [] -> 0
    | y :: ys -> if x = y then 1 + count x ys else count x ys
in
  let rec unique = function
    | [] -> []
    | x :: xs -> if List.mem x xs then unique xs else x :: unique xs
in
  let unique_elements = unique lst 
in
  let counted = List.map (fun x -> (x, count x lst)) unique_elements 
in
  List.sort (fun (_, a) (_, b) -> compare b a) counted;;


 count_occurrences ['a'; 'b'; 'a'; 'c'; 'c'; 'a'; 'd'];;
 count_occurrences [0; 0; 0; -2; 3; -1; -1; 3; 3; 0];;
 count_occurrences [("str1", 1); ("str1",2); ("str2",1); ("str2",1); ("str1",2)];;


(* 3 *)
let rec drop_last l = match l with 
     [] -> raise (Failure "Empty list has no last element")
    | [x] -> []
    | x::xs -> x::(drop_last xs);;

drop_last [1; 2; 3; 4];;
drop_last [1];;
(* drop_last [];; *)


(* 4 *)
let rec drop_last_opt l = match l with 
     [] -> None 
    | [x] -> Some []
    | x::xs -> (
        match drop_last_opt xs with 
         Some y -> Some (x::y)
        | None -> None 
    );;

drop_last_opt [];;
drop_last_opt [1];;
drop_last_opt [1;2;3];;


(* 5 *)
let rec zip_with f l1 l2 = match (l1, l2) with 
     (x::xs, y::ys) -> (f x y)::(zip_with f xs ys)
    | _ -> [];;

zip_with (fun x y -> [x;y]) [1;2;3] [5;6];;
zip_with (fun x y -> [x;y]) [1;2;3] [5;6;7;8];;
zip_with (fun x y -> (x,y)) [1;2;3] ['a';'b'];;
zip_with (+) [1;2;3] [5;6];;
zip_with (^) ["aa";"bb";"cc"] ["1";"2"];;


(* 6 *)
let unzip l = 
    List.fold_right (fun (a,b) (acc1, acc2) -> (a::acc1, b::acc2)) l ([],[]);;

unzip [('a',1); ('b',2)];;
unzip [('a',1); ('b',2); ('c',3)];;

let unzip1 l = let rec helper acc1 acc2 l =
    match l with 
     [] -> (List.rev acc1, List.rev acc2)
    | (a,b)::xs -> helper (a::acc1) (b::acc2) xs
in helper [] [] l;;

unzip1 [('a',1); ('b',2)];;
unzip1 [('a',1); ('b',2); ('c',3)];;


(* 6 subtasks *)
(* 1 *)
let f1 acc (a, b) = (b, a) :: acc;;

List.fold_left f1 [] [('a', 1); ('b', 2); ('c', 3)];;

(* 2 *)
let rec map f = function
 | [] -> []
 | x :: xs -> f x :: map f xs;;


let map_tr f l = let rec helper acc = function 
     [] -> List.rev acc 
    | x::xs -> helper (f x ::acc) xs 
in helper [] l;;

map_tr (fun x -> x + 1) [1;2;3];;

let rec replicate n x =
 if n < 1 then [] else x :: replicate (n-1) x;;

let replicate_tr n x = let rec helper acc n = 
    if n < 1 then List.rev acc else helper (x::acc) (n-1)
in helper [] n;;

replicate 3 "hi";;
replicate_tr 3 "hi";;


(* lazy lists *)
type 'a lazy_list = Nil | Cons of 'a * (unit -> 'a lazy_list);;

let rec from n = Cons (n, fun () -> from (n + 1));;
let head = function 
   Nil -> failwith "Empty lazy list"
  | Cons (x, _) -> x;;

let tail = function 
   Nil -> failwith "Empty lazy list"
  | Cons (_, xs) -> xs ();;

let rec from n = Cons (n, fun () -> from (n+1));;

let numbers = from 1;;
head numbers;;
tail numbers |> head;;  (* |> means pass the result of the left expression into head *)


(* 3 *)
type 'a llist = Nil | Cons of 'a * (unit -> 'a llist);;
let rec map_over_custom_llist f = function 
   Nil -> Nil 
  | Cons (x, xs) -> Cons (f x, fun () -> map_over_custom_llist f (xs ()));;


(* 4 *)
let rec merge_llists l1 l2 = match (l1, l2) with 
   (Nil, l) | (l, Nil) -> l
  | (Cons (x, xs), Cons (y, ys)) -> 
    if x <= y then Cons (x, fun () -> merge_llists (xs()) l2)
  else Cons (y, fun () -> merge_llists l1 (ys()));;


let rec from_to s e j = if s > e then Nil
else Cons (s, fun () -> from_to (s+j) e j);;

from_to 0 5 1;;


let rec string_of_llist l =
  match l with
  | Nil -> "Nil"
  | Cons (x, xf) -> string_of_int x ^ ", " ^ string_of_llist (xf ());;


string_of_llist (merge_llists (from_to 0 5 1) (from_to 0 5 1));;


(* 5 *)
let rec drop_dupl = function
  | Nil -> Nil
  | Cons (x, xf) ->
      let rec skip_same x rest =
        match rest with
        | Nil -> Nil
        | Cons (y, yf) when x = y -> skip_same x (yf ())
        | Cons (y, yf) -> Cons (y, fun () -> drop_dupl (yf ()))
      in
      Cons (x, fun () -> skip_same x (xf ()));;


string_of_llist (drop_dupl (merge_llists (from_to 0 5 1) (from_to 6 5 1)));;

let rec h n m a = if a = 0 then Nil 
else Cons (n, fun () -> h (n*m) m (a-1));;
let h2 = h 1 2;;
let h3 = h 1 3;;
let h5 = h 1 5;;

let hamming_llist =  (merge_llists (h2 10) (merge_llists (h3 10) (h5 10)));;
let final = string_of_llist (drop_dupl hamming_llist);;

(* correct version: *)
let rec scale n l = match l with
  | Nil -> Nil
  | Cons (x, xf) -> Cons (n * x, fun () -> scale n (xf ()));;


let rec hamm_llist =
  let rec h = Cons (1, fun () ->
    let h2 = scale 2 h in
    let h3 = scale 3 h in
    let h5 = scale 5 h in
    merge_llists h2 (merge_llists h3 h5))
  in h;;

let rec take n l = match n, l with
  | 0, _ | _, Nil -> []
  | n, Cons (x, xf) -> x :: take (n - 1) (xf ());;

let () =
  let first_20 = take 20 hamming_llist in
  List.iter (fun x -> Printf.printf "%d " x) first_20;
  print_newline ()  (* <-- make sure it ends with a newline *)



(* Binary search trees *)
(* 1 *)
type binary_tree = Empty | Node of int * binary_tree * binary_tree;;

(* 2 *)
let t1 = Node (9, 
  Node (6, 
    Node (1, Empty, Empty),
    Node (8, Empty, Empty)),
  Node (12,
    Empty,
    Node (42, Empty, Empty))
  );;

(* 3 *)
let rec to_list = function 
   Empty -> [] 
  | Node (x, t1, t2) -> (to_list t1) @ x::(to_list t2);;

to_list t1;;


(* 4 *)
let rec insert x = function 
   Empty -> Node (x, Empty, Empty)
  | Node (a, t1, t2) as node -> if a = x then node else 
      if x > a then Node (a, t1, insert x t2) 
      else Node (a, insert x t1, t2);;


(* 5 *)
let rec remove_max = function
  | Empty -> failwith "unreachable"
  | Node (v, l, Empty) -> v, l
  | Node (v, l, r) -> let v', r' = remove_max r in v', Node (v, l, r')


let rec remove x = function
    Empty -> Empty
  | Node (a, t1, t2) -> if x < a then Node (a, remove x t1, t2)
  else if x > a then Node (a, t1, remove x t2)
  else match t1, t2 with 
   Empty, t | t, Empty -> t
  | _ -> let x', t1' = remove_max t1 in Node (x', t1', t2);;