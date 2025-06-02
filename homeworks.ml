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