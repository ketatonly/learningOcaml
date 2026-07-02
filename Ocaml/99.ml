let x = 200 in x * x * x;;
let a = 500 in (let b = a * a in a + b);;

(* Problem 1 *)

let rec last = function
  [] -> None
| x::xs -> match xs with
            [] -> Some x 
          | _ -> last xs;; 

last ["a" ; "b" ; "c" ; "d"];;
last [];;
last [1];;


(* Problem 2*)

let rec last_two = function
  [] -> None
| x::xs -> match xs with
            [] -> None
          | [y] -> Some (x, y)
          | _ -> last_two xs;;

last_two ["a"; "b"; "c"; "d"];;
last_two ["a"];;
last_two [2; 3];;


(* Problem 3 *)

let rec at k lst = match lst with 
  [] -> None
| x::xs -> if k = 1 then Some x else at (k-1) xs;;

at 3 ["a"; "b"; "c"; "d"; "e"];;
at 3 ["a"];;


(* Problem 4 *)

let length lst = let rec help b lst =
  match lst with
  [] -> b
| x::xs -> help (b+1) xs
in help 0 lst;;

length ["a"; "b"; "c"];;
length [];;


(* Problem 5 *)

let rec rev = function
  [] -> []
| x::xs -> rev xs @ [x];;

rev ["a"; "b"; "c"];;

(* Problem 6 *)

let is_palindrome lst = 
    lst == List.rev lst;;

    is_palindrome ["x"; "a"; "m"; "a"; "x"];;
    not (is_palindrome ["a"; "b"]);;


(* Problem 7 *)

type 'a node = One of 'a | Many of 'a node list;;

let rec flatten lst = match lst with 
  [] -> []
| One a :: t -> [a] @ flatten t
| Many y::ys -> flatten y @ flatten ys;;

flatten [One "a"; Many [One "b"; Many [One "c" ;One "d"]; One "e"]];;


(* Problem 8 *)

let rec compress = function
  [] -> []
| [x] -> [x]
| x::(y::xs) -> if x=y then compress (y::xs) else [x]@compress (y::xs);;
  

compress ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"];;


(* Problem 9 *)

let pack list = let rec help lst lists = function
  [] -> []
| [x] -> (x::lst)::lists
| x::(y::xs) -> if x=y then help (lst@[x]) lists (y::xs) else help [] (lists @ [lst @ [x]]) (y::xs)
in help [] [] list
;;

pack ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "d"; "e"; "e"; "e"; "e"];;


(* Problem 14 *)

let rec duplicate = function
  [] -> []
| x::xs -> x::x:: duplicate xs;;

duplicate ["a"; "b"; "c"; "c"; "d"];;


(* Problem 15 *)

let replicate lst n = let rec help list lst num = match lst with
  [] -> list
| x::xs -> if num = 0 then help list xs n else help (x::list) (x::xs) (num-1)
in help [] (List.rev lst) n
;;

replicate ["a"; "b"; "c"] 3;;


(* Problem 16 *)

let drop lst n = let rec help list lst num = match lst with
  [] -> list
| x::xs -> if num = 1 then help list xs n else help (list@[x]) xs (num-1) 
in help [] lst n
;;

drop ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j"] 3;;


(* Problem 17 *)


let split lst n = let rec help tuple list num = match list with
    [] -> (List.rev tuple, [])
  | x::xs as t -> if num = 0 then (List.rev tuple, t) else help (x::tuple) xs (num-1)
in help [] lst n
;;
    
split ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j"] 3;;


(* Problem 18 *)

let slice list n m = let rec help acc lst k = match lst with 
  [] -> List.rev acc
| x::xs -> if n <= k && k <= m then help (x::acc) xs (k + 1) else help acc xs (k + 1)
in help [] list 0
;;


slice ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j"] 2 6;;


(* Problem 19 *)

let rotate list n = let rec help acc lst k = match lst with 
  [] -> List.rev acc
| x::xs as t -> if k = 0 then t@(List.rev acc) else help (x::acc) xs (k - 1)
in let m = if n > 0 then n else List.length list + n
in help [] list m
;;

rotate ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"] 3;;
rotate ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"] (-2);;


(* Problem 20 *)

let remove_at n list = let rec help acc k = function
  [] -> List.rev acc
| x::xs -> if k = n then (List.rev acc)@xs  else help (x::acc) (k + 1) xs
in help [] 0 list
;;

(* or:
   
let rec remove_at n = function
    | [] -> []
    | h :: t -> if n = 0 then t else h :: remove_at (n - 1) t;;

*)

remove_at 1 ["a"; "b"; "c"; "d"];;


(* Problem 21 *)

let rec insert_at y n = function
  [] -> [y]
| x::xs -> if n = 1 then x::(y::xs) else insert_at y (n-1) xs
;;

insert_at "alfa" 1 ["a"; "b"; "c"; "d"];;


(* Problem 22 *)


let range n m = let rec help x y lst = match (x, y) with
  (x, y) when x < y -> help (x+1) y (x::lst)
| (x, y) when x > y -> help (x-1) y (x::lst)
| (x, y) when x = y -> List.rev (x::lst)
in help n m [];;

range 4 9;;
range 9 4;;


(* Problem 23*)


let rand_select lst n = 
    let rec extracted z = function
      [] -> []
    | x::xs -> if z = 0 then xs else x::extracted (z-1) xs
in
    let rec help acc n list = match list with 
    [] -> acc
  | x::xs -> 
    if n = 0 then acc 
    else 
      let randomint = Random.int (List.length list) in
      help (acc@[List.nth list randomint]) (n-1) (extracted (randomint) list) 
    in help [] n lst;;
  

    rand_select ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"] 3;;



   (* Problem 24 *)


let lotto_select n m = let rec help acc n m = 
  if n = 0 then acc else help (acc@[Random.int m]) (n-1) m
  in help [] n m;;

  lotto_select 6 49;;

  (* Problem 25 *)


  let permutation list = 
    let rec extracted z = function
      [] -> []
    | x::xs -> if z = 0 then xs else x::extracted (z-1) xs
in
    let rec help acc lst = match lst with 
    [] -> acc
  | x::xs -> 
      let randomnum = Random.int (List.length lst) in
      help (acc@[List.nth lst randomnum]) (extracted randomnum lst)
  in help [] list;;

  permutation ["a"; "b"; "c"; "d"; "e"; "f"];;



  (* Problem 26 *)






