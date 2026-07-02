
(* HW *)

(* OCamlification blatt05 *)

let foo x y b =
  let rec whileloop a z k = if a < z then 
    if k then whileloop (a+1) z (not k) 
    else whileloop a (z-1) (not k) else a
  in
  if x > y then whileloop y x b else whileloop x y b
  ;;



  let result = foo 3 10 true;;



(* More Students blatt05 *)


type student = {first_name : string; last_name : string; id : int; semester : int; grades : (int * float) list}

type database = student list

let insert s db = s::db

let rec find_by_id id db = match db with [] -> []
| x::xs -> if x.id = id then [x] else find_by_id id xs

let rec find_by_last_name name db = match db with [] -> []
| x::xs -> if x.last_name = name
  then x::find_by_last_name name xs
  else find_by_last_name name xs


let rec remove_by_id i = function
  [] -> []
| x::xs -> if x.id = i then remove_by_id i xs else x::remove_by_id i xs

let count_in_semester n db = let rec help acc n = function
  [] -> acc
| x::xs -> if x.semester = n then help (acc + 1) n xs else help acc n xs
in help 0 n db;;
  

let student_avg_grade i db = let rec help id = function
  [] -> 0.0
| x::xs -> if x.id = id then 
  let gradelist = List.map (fun (x, y) -> y) (x.grades) in
  let subnum = float_of_int (List.length x.grades) in
  let grade = List.fold_left (+.) 0.0 gradelist in
  grade /. subnum 
else help id xs
in help i db;;



let courses db = List.map (fun (x, _) -> x) (db.grades);;
let gradelst db = List.map (fun (_, x) -> x) (db.grades);;

let course_avg_grade c db = let rec help n = function
[] -> 0.0
| x::xs -> let rec crs lst n =
match lst with
    [] -> 0.0
  | y::ys -> if n = y then 
    let ourgrades = List.map (fun (a, b) -> if n = a then b else 0.0) x.grades in
    let sum = List.fold_left (+.) 0.0 ourgrades in
    let num1 = float_of_int (List.length ourgrades) in
    sum /. num1
    else crs ys n
  in crs (courses x) n 
  in help c db
;;


(* Peano Arithmetic *)


type nat = Zero | Succ of nat;;

let rec int_to_nat i = if i <= 0 then Zero else 
  Succ (int_to_nat (i - 1));;

let nat_zero = int_to_nat 0    (* Should be Zero *)
let nat_one = int_to_nat 1     (* Should be Succ Zero *)
let nat_three = int_to_nat 3  


let rec nat_to_int = function
Zero -> 0
| Succ xs -> 1 + nat_to_int xs;;

let int_zero = nat_to_int Zero                    (* Should be 0 *)
let int_one = nat_to_int (Succ Zero)              (* Should be 1 *)
let int_three = nat_to_int (Succ (Succ (Succ Zero)))


let add n m = 
let intn = nat_to_int n in
let intm = nat_to_int m in
let num = intn + intm in
int_to_nat num;;

let two = Succ (Succ Zero)
let three = Succ (Succ (Succ Zero))
let five = add two three


let mul n m = 
let intn = nat_to_int n in
let intm = nat_to_int m in
let num = intn * intm in
int_to_nat num;;

let two = Succ (Succ Zero)
let three = Succ (Succ (Succ Zero))
let five = mul two three


let pow n m =
let intn = nat_to_int n in
let intm = nat_to_int m in
let rec help acc b p = if p = 0 then acc else help (acc*b) b (p - 1) 
in 
let num = help 1 intn intm in
int_to_nat num
;;

let two = Succ (Succ Zero)
let three = Succ (Succ (Succ Zero))
let eight = pow two three


(* 6 subtask *)

let f1 acc (a, b) = List.rev ((b, a)::acc);; 

let rec fold_left f acc = function
[] -> acc
| x::xs -> fold_left f (acc@(f x)) xs
;;

let pairs = [(1, "one"); (2, "two"); (3, "three")];;
let reversed_pairs = List.fold_left f1 [] pairs;;

let rec g acc n = function
[] -> acc
| x::xs -> 
let fa (a, b) n = if n = a then acc@[b] else acc in
g (fa x n) n xs;;


(* HW3 *)

let compare t e = if t = e then 0 else 1;;

let rec member c t = function
[] -> false 
| x::xs -> if c t x = 0 then true else member c t xs;;

member compare 3 [1; 2; 3];;
member compare 4 [1; 2; 3];;
member compare 'a' ['a'; 'b'; 'c'];;

let equal_second_components (_, x) (_, y) = compare x y;;
member equal_second_components ('a',5) [(1,2); (3,4); (5,6)];;

let evens_eq_evens_odds_eq_odds n1 n2 = compare (n1 mod 2) (n2 mod 2);;
member evens_eq_evens_odds_eq_odds 4 [1; 2; 3];;



let rec tuplelist x acc = match acc with
[] -> (x, 1)::acc
| (a, b)::xs -> if x = a then (a, b + 1)::xs else (a, b)::tuplelist x xs;;

let count_occurrences lst = let rec help acc = function
[] -> acc
| x::xs -> help (tuplelist x acc) xs
in help [] lst;;

let test1 = count_occurrences ['a'; 'b'; 'a'; 'c'; 'c'; 'a'; 'd'];;


(* Longest Twins *)





let rec count_occurrences lst x = 
List.fold_left (fun acc elem -> if elem = x then acc + 1 else acc) 0 lst

let rec longest_sequence acc cur seq = match cur, seq with
| [], _ | _, [] -> acc
| _, x :: xs ->
  let new_cur = x :: cur in
  let new_acc = 
    if count_occurrences seq x > 1 && List.length new_cur > List.length acc then
      new_cur 
    else 
      acc 
  in
  longest_sequence new_acc new_cur xs

let lt_seq seq = longest_sequence [] [] seq


let test1 = lt_seq [1; 2; 3; 4; 5; 1; 2; 3; 4; 5; 6]  (* Output: [1; 2; 3; 4; 5] *)
let test2 = lt_seq [1; 2; 3; 4; 5; 6; 7; 8; 9]  (* Output: [] *)


(* Mappings *)

let is_empty = function
  [] -> true 
| _ -> false;;

let rec get x = function
  [] -> None
| (a, b)::xs -> if x = a then Some b else get x xs;;

let rec put a b list = match list with
  [] -> (a, b)::[]
| (c, d)::xs as t -> if a = c then t else (c, d) :: put a b xs;;

let rec contains_key k list = match list with
  [] -> false
| (a, b)::xs -> if k = a then true else contains_key k xs;;


(* 8 subtask 6 *)

let unzip list = let rec help acc1 acc2 = function
  [] -> acc1, acc2
| (a, b)::xs -> help (acc1@[a]) (acc2@[b]) xs
in help [] [] list;;

unzip [('a',1); ('b',2)] 



(* Lazy Lists *)

type 'a llist = Cons of 'a * (unit -> 'a llist)

let rec inat x = Cons (x, (fun() -> inat (x + 1) ));;

let rec fib () = let rec help a b = 
Cons (a, fun() -> help b (a + b) )
in help 0 1;;


let itake x llist = let rec help x acc = function
| Cons (a, t) -> if x = 0 then acc else help (x-1) (acc@[a]) (t())
in help x [] llist;;


