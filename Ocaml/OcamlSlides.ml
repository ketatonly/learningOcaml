(*
3+4;;

"so "^"it "^"goes";;

let seven = 3 + 4;;

(3, 4);;
*)



(*

type person = {name:string; sur:string; age:int};;

let paul = {name="Paul"; sur="Meier"; age=24};;
let hans = { sur="Meier"; age=24; name="Paul"};;

hans = paul;;

paul.name;;
hans.age;;

let {name = x; sur = y; age = z} = paul;;
let {age = c; _} = paul;;  (*or let {age = c} = paul;;*)
   
*)


(*

let mt = [];;
let l1 = 1::[];;

let l2 = [1;2;3];;

let l = 1::2::3::[];;

let a = 4::l2;;
let b = [];;
let c = b @ [1];;
let d = [3] @ [4];;
let e = [4] @ [3];;
let f = 4::e;;
let g = d@e;; 
let h = [0]@[1;2;3];;
let i = h@[4];;

*)



(*

let l = [3;2];;

match l 
with [] -> -1 
    | x::xs -> x;;

*)


(*
   
let double x = 2*x;;
(double 3, double(double 1));;

let double = fun x -> 2*x;;

*)



(*
   
let rec fac n = if n < 2 then 1 else n * fac(n-1);;

let rec fib = fun x -> if x <= 1 then 1 else fib (x-1) + fib (x-2);;


let rec even n = if n=0 then "even" else odd (n-1)
    and odd n = if n=0 then "odd" else even (n-1);;

*)


(*

let rec length = fun l -> match l with [] -> 0 | x::xs -> 1 + length xs;;

let myList = [1; 2; 3; 4; 5];;
let result = length myList;; 

*)


(*
   
let rec app l y = match l with
   | [] -> y
   | x::xs -> x :: app xs y ;;

app [1;2] [3;4;5];;

let rec app = function 
        | [] -> fun y -> y
        | x::xs -> fun y -> x::app xs y;;

        app [1;2] [3;4;5];;

let add x = 
     fun y -> x+y;;
let n = add 3;;
let m = n 5;;

*)



(*
   
let x = 5
    in let sq = x * x
    in sq + sq;;

(*or:*)
let n = 3;;

let x = n
    in let sq = x * x
    in sq + sq;;


let facit n = let rec   
    iter m yet = if m > n then yet  
    else iter (m+1) (m*yet)
in iter 2 1;;

*)

(* local binding: *)
(* let x = 5 in x + 1;; *)


(*
   
type color = Diamonds | Hearts | Gras | Clubs;;
type value = Seven | Eight | Nine | Jack | Queen | King | Ten | Ace;;

let gras_jack = (Gras, Jack);;
let jora = (Clubs, Nine);;

Clubs < Diamonds;;

let is_trump = function 
    | (Hearts, _) -> true
    | (_, Jack) -> true
    | (_, Queen) -> true
    | (_,_) -> false;;

is_trump (Gras, Jack);;
is_trump (Clubs, Nine);;

let stringOfColor = function    
    | (Diamonds,_) -> "Diamonds"
    | (Hearts,_) -> "Hearts"
    | (Gras,_) -> "Gras"
    | (Clubs,_) -> "Clubs";;

stringOfColor (gras_jack);;

let string_of_color = function
| Diamonds -> "Diamonds"
| Hearts -> "Hearts"
| Gras -> "Gras"
| Clubs -> "Clubs";;

let cardColor = Diamonds;;
let colorString = string_of_color cardColor;;

*)


(*
   
let takes c1 c2 = match (c1,c2) with
| ((f1,Queen),(f2,Queen)) -> f1 > f2
| ((_,Queen),_) -> true
| (_,(_,Queen)) -> false
| ((f1,Jack),(f2,Jack)) -> f1 > f2
| ((_,Jack),_) -> true
| (_,(_,Jack)) -> false
| ((Hearts,w1),(Hearts,w2)) -> w1 > w2
| ((Hearts,_),_) -> true
| (_,(Hearts,_)) -> false
| ((f1,w1),(f2,w2)) -> if f1=f2 then w1 > w2
else false;;


let card1 = (Diamonds, Eight);;
let card2 = (Hearts, Jack);;

let card1_wins = takes card1 card2;;

let take card1 card2 =
    if takes card1 card2  then card1 else card2;;

let winner = take card1 card2;;

let trick card1 card2 card3 card4 =
    take card4 (take card3 (take card2 card1));;
    
let tricker =  trick (Gras,Ace) (Gras,Nine) (Hearts,Ten) (Clubs,Jack);;

*)



(*
   
type 'a option = None | Some of 'a 
let is_some x = match x with 
| Some _ -> true 
| None -> false;;
let get x = match x with
| Some y -> y
let value x a = match x with
| Some y -> y
| None -> a
let map f x = match x with
| Some y -> Some (f y)
| None -> None
let join a = match a with
| Some a' -> a'
| None -> None

let inc a = a+1;;
let num = Some 5;;
let result = map inc num;;

let nest = Some (Some 3);;
let flat = join nest;;

*)

(*

type unis = Con of string;;
let myUni = "KIU";;

type ('a, 'b) pair = { first : 'a; second : 'b };;
let pair1 = {first=10; second="Ten"};;
let pair2 = {first="Hello"; second="world"};;

*)



   
(*
   
let rec get_value a l = match l with
| [] -> None
| (b, z)::rest -> if a = b then Some z
else get_value a rest

type order = First | Second | Third;;

let list = [(First, 3); (Second, 6); (Third, 4)];;

let rslt = get_value Second list;;


type subj = Math | OS | Calc;;

let gpa = [(Math, 3.0); (OS, 2.54); (Calc, 3.6)];;
let myGPA = get_value Calc gpa;;

*)


(*
   
type sequence = End | Next of (int * sequence);;
Next (1, Next (2, End));;

type 'a sequence2 = End | Next of ('a * 'a sequence2);;
Next ("Hello", Next ("World", End) );; 

*)


(*
   
let rec nth n s = match (n, s) with
| (_, End) -> None
| (0, Next (x, _)) -> Some x
| (n, Next (_, rest)) -> nth (n-1) rest;;

nth 3 (Next (1, Next(2, Next (5, Next (17, End)))));;


let rec down = function
    | 0 -> End
    | n -> Next (n, down (n-1));;

down 3;;


let rec up n = match n with
    | 0 -> End
    | _ -> Next (1, inc 2 n)
   and inc a b =  if a > b then End else Next (a, inc (a+1) b);;

up 3;;

*)


(*
   

(*Tail calls:*)
let f x = x + 5;;

let g y = let z = 7
in if y > 5 then f (-y)   (*f (-y) is a tail call the second call is not because it adds something to the f output*)
else z + f y;;

(*Tail recursives:*)

let fac x = let rec facit n acc =
    if n <= 1 then acc
    else facit (n - 1) (n * acc)
    in facit x 1;;    (* this is tail recursive*)

let rec loop x = if x < 2 then x
    else if x mod 2 = 0 then loop (x / 2)
    else loop (3 * x + 1);;  (*this is not*)

    loop 3;;

*)


(*
   
let rev list =
    let rec r2 a l =
    match l
    with [] -> a
    | x::xs -> r2 (x::a) xs
    in r2 [] list;;

    let result = rev [1; 2; 3; 4; 5];;

*)
    


(*
   
(*currying:*)

let f (a, b) = a + b + 1;;  (*uncurried function*)
let g a b = a + b + 1;;   (*curried function, higher order function*)


f (3, 5);;
let g1 = g 3;;
g1 5;;

g 3 5;;

let curry f a b = f (a,b);;
let plus (x,y) = x+y;;
curry plus;;

let plus2 = curry plus 2;;
let plus3 = curry plus 3;;
plus2 (plus3 4);;

*)


(*
   
(* Some list functions: *)

let rec map f = function
[] -> []
| x::xs -> f x :: map f xs

(*  or:
   
let rec map f lst =
  match lst with
  | [] -> []
  | x::xs -> (f x) :: (map f xs);;

*)


let double x = x * 2;;
let numbers = [1; 2; 3; 4; 5];;
let doubled_numbers = map double numbers;;
numbers;; 
doubled_numbers;; 



let rec cdcd_left f a = function
[] -> a
| x::xs -> fold_left f (f a x) xs;;

let rec fold_right f = function
[] -> fun b -> b
| x::xs -> fun b -> f x (fold_right f xs b);;

let add x y = x + y;;
let numbers1 = [1; 2; 3; 4; 5];;
let sum_left = fold_left add 0 numbers1;;
let sum_right = fold_right add numbers1 0;;



let rec find_opt f = function
[] -> None
| x::xs -> if f x then Some x
else find_opt f xs;;

let is_even x = x mod 2 == 0;;
let numbers = [1; 3; 5; 4; 6; 7];;
let result = find_opt is_even numbers;;
let result2 = find_opt (fun x -> x > 10) numbers;;


let rec find f = function
[] -> []
| x::xs -> if f x then x ::find f xs
else find f xs;;

let iseven x = x mod 2 == 0;;
let numbers1 = [1; 3; 5; 4; 6; 7];;
let result1 = find iseven numbers;;

*)



(*
   
(*  Polymorphic Functions: *)

let cons_r xs x = x::xs;;
let rev l = fold_left cons_r [] l;;
rev [1; 2; 3];;
rev [true; false; false];;

let compose f g x = f (g x);;
let twice f x = f (f x);;
let rec iter f g x = if g x then x else iter f g (f x);;


let plus2 x = x + 2;; 


compose not not;;
compose not not true;;
compose Char.chr plus2 65;; (* converts an int to the corresponding ASCII value which is C in this case*)


*)


(*
   
type 'a tree = Leaf of 'a | Node of ('a tree * 'a tree);;

Leaf 1;;
Node (Leaf ('a', true), Leaf ('b', false));;



let rec size = function
| Leaf _ -> 1
| Node(t,t') -> size t + size t';;

let rec flatten = function
| Leaf x -> [x]
| Node(t,t') -> flatten t @ flatten t';;

let flatten1 t = let rec doit t xs = match t with
| Leaf x -> x :: xs
| Node(t,t') -> let xs = doit t' xs
in doit t xs
in doit t [];;


let t = Node (Node (Leaf 1, Leaf 5), Leaf 3);;

*)



(*
   
(* Queues: *)

(* queues represented by list:*)
type 'a queue = 'a list;;

let dequeue = function
[] -> (None, [])
| x::xs -> (Some x, xs);;

let enqueue x xs = xs @ [x];;


(* Representing the queue with two lists: *)

type 'a queue = Queue of 'a list * 'a list;;

let is_empty = function
Queue ([],[]) -> true
| _ -> false;;

let queue_of_list list = Queue (list,[]);;

let list_of_queue = function
Queue (first,[]) -> first
| Queue (first,last) ->
first @ List.rev last;;


let rec rev1 = function
    [] -> []
    | x::xs -> rev1 xs @ [x];;


let enqueue x (Queue (first, last)) = Queue (first, x::last);;

let dequeue = function
    Queue ([],last) -> (match List.rev last
                with [] -> (None, Queue ([],[]))
                | x::xs -> (Some x, Queue (xs,[])))
    | Queue (x::xs,last) -> (Some x, Queue (xs,last));;

*)




(*
   
(* Anonymous Functions: *)


fun x y z -> x + y + z;;  (*we can se functions without naming them*)

function None -> 0
| Some x -> x * x + 1;;

let test = ( function None -> 0
   | Some x -> x * x + 1) None ;;

let test1 = ( function None -> 0
   | Some x -> x * x + 1) (Some 3) ;;



let rec map f lst =
    match lst with
    | [] -> []
    | x::xs -> (f x) :: (map f xs);;

map (fun x -> x * x) [1; 2; 3];;


let make_undefined () = fun x -> None;;

let def_one (x,y) = fun x' -> if x = x' then Some y else None;;


let undefined = make_undefined ();;
let result1 = undefined 10;;


let lookup_one = def_one (3, "three");;
let result1 = lookup_one 3;;  (* result1 will be Some "three" *)
let result2 = lookup_one 4;;  (* result2 will be None *)

*)



(*
   
(*Exceptions: *)

(*  1 / 0;; *)
(* Exception: Division_by_zero *)



Division_by_zero;;
(* - : exn = Division_by_zero *)

Failure "complete nonsense!";;
(* - : exn = Failure "complete nonsense!" *)

(*Own exceptions: *)

exception Hell;;
Hell;;

exception Hell of string;;
Hell "damn!";;


let divide (n,m) = try Some (n / m)
with Division_by_zero -> None;;

divide (10,3);;
divide (10,0);;

Failure "complete nonsense!";;


let rec member x l = try if x = List.hd l then true
else member x (List.tl l)
with Failure _ -> false;;

member 2 [1;2;3];;
member 4 [1;2;3];;


(* 1 + raise Division_by_zero;; *)


exception MyError of string;;

let my_function x =
  if x < 0 then raise (MyError "Negative value not allowed")
  else x + 1;;


  (*my_function (-5);;*)



let f (x, y) = x / (y - 1);;

let g (x, y) = 
    try let n = 
        try f (x,y)
        with Division_by_zero -> raise (Failure "Division by zero")
        in string_of_int (n * n)
    with Failure str -> "Error: "^str;;
  

g (6, 1);;
g (6, 3);;

*)


(*
   
(* Input and Output *)

print_string "Hello World!\n";;

(*read_line ();; *)

let infile = open_in "c:/Users/Admin/Desktop/readfrom.txt.txt";;

input_line infile;;
input_line infile;;

close_in infile;;


let infile2 = open_in "c:/Users/Admin/Desktop/readfrom.txt.txt";;

input_char infile2;;
input_char infile2;;
input_char infile2;;
close_in infile2;;

let length = in_channel_length (open_in "c:/Users/Admin/Desktop/readfrom.txt.txt");;


let write = open_out "c:/Users/Admin/Desktop/readfrom.txt.txt";;

output_string write "Hello";;
output_string write "World";;

close_out write;;

let infile3 = open_in "c:/Users/Admin/Desktop/readfrom.txt.txt";;
input_line infile3;;
close_in infile3;;

*)




(*

(* Sequences: *)

print_string "Hello";
print_string " ";
print_string "world!\n";;

let rec iter f = function
[] -> ()
| x::[] -> f x
| x::xs -> f x; iter f xs;;

iter print_string ["Hello "; "world";"!\n"];;

*)


(*


(* Modules: *)


module Pairs =
struct
type 'a pair = 'a * 'a
let pair (a,b) = (a,b)
let first (a,b) = a
let second (a,b) = b
end;;

Pairs.first;;


module Triples =
struct
type 'a triple = Triple of 'a * 'a * 'a
let first (Triple (a, _, _)) = a
let second (Triple (_, b, _)) = b
let third (Triple (_, _, c)) = c
end;;

Triples.first;;


module Pairs2 =
struct
type 'a pair = bool -> 'a
let pair (a,b) = fun x -> if x then a else b
let first ab = ab true
let second ab = ab false
end;;

open Pairs2;;

pair;;
pair (4,3) true;;


module A = struct let x = 1 end;;

module B =
    struct
        open A  (* we can access A here*)
        let y = 2
    end;;

module C =
    struct
        include A  (*Directly copies A and B*)
        include B
    end;;


module Quads = struct
    module Pairs =
    struct
        type 'a pair = 'a * 'a
        let pair (a,b) = (a,b)
        let first (a,_) = a
        let second (_,b) = b
    end

    type 'a quad = 'a Pairs.pair Pairs.pair
    let quad (a,b,c,d) =
    Pairs.pair (Pairs.pair (a,b), Pairs.pair (c,d))

    let first q = Pairs.first (Pairs.first q)
    let second q = Pairs.second (Pairs.first q)
    let third q = Pairs.first (Pairs.second q)
    let fourth q = Pairs.second (Pairs.second q)
end

(*

 (*Simplified version:*)
module Quads = struct
    type a' quad = 'a*'a*'a*'a 
    let quad (a, b, c, d) = (a, b, c, d)
    let first (a,_, _, _) = a
    let second (_,b. _, _) = b
    let third (_,_, c, _) = c
    let fourth (_, _, _, d) = d
end;;

*)


let my_quad = Quads.quad (1, 2, 3, 4);;
Quads.Pairs.first;;

*)


(*
   
let rec map f = function
[] -> []
| x::xs -> f x :: map f xs;;



module Sort = struct
    let single lst = map (fun x -> [x]) lst

    let rec merge l1 l2 = match (l1, l2)
        with ([],_) -> l2
        | (_,[]) -> l1
        | (x::xs,y::ys) -> if x < y then x :: merge xs l2
        else y :: merge l1 ys

    let rec merge_lists = function
        [] -> [] | [l] -> [l]
        | l1::l2::ll -> merge l1 l2 :: merge_lists ll

    let sort lst = let lst = single lst
        in let rec doit = function
        [] -> [] | [l] -> l
        | l -> doit (merge_lists l)
        in doit lst
    end;;


    Sort.single [1;2;3];;
    Sort.merge [4; 8; 7] [1; 3; 9];;
    Sort.sort [7; 3; 6; 9; 4];;

    module type SortType = sig
        val merge : 'a list -> 'a list -> 'a list
        val sort : 'a list -> 'a list
    end

    module MySort : SortType = Sort;;

    (*MySort.single;;    this won't work*)

    let sorted_list = MySort.sort [4; 2; 7; 1; 5];;



module type A1 = sig
    val f : 'a -> 'b -> 'b
    end

module type A2 = sig
    val f : int -> char -> int
    end

module A = struct
    let f x y = x
end

(*module A1 : A1 = A;;   singature mismatch*)
module A2 : A2 = A;;
A2.f;;


*)



(*
   
(* Information hiding: *)

module ListQueue = struct
    type 'a queue = 'a list
    let empty_queue () = []
    let is_empty = function
    [] -> true | _ -> false
    let enqueue xs y = xs @ [y]
    let dequeue (x::xs) = (x,xs)
end

module type Queue = sig
    type 'a queue
    val empty_queue : unit -> 'a queue
    val is_empty : 'a queue -> bool
    val enqueue : 'a queue -> 'a -> 'a queue
    val dequeue : 'a queue -> 'a * 'a queue
end

module Queue : Queue = ListQueue;;

open Queue;;
(* is_empty [];; (* this is wrong because it only works on queues not lists*) *)



module type Fact = sig
    val fact : int -> int
end

module RecursiveFact : Fact = struct
    let rec fact n =
        if n = 0 then 1 else
            n * fact (n-1)
        end



module type Queue = sig
    type 'a queue = Queue of ('a list * 'a list)
    val empty_queue : unit -> 'a queue
    val is_empty : 'a queue -> bool
    val enqueue : 'a -> 'a queue -> 'a queue
    val dequeue : 'a queue -> 'a option * 'a queue
end

*)



(* Functors: *)

module type Decons = sig
    type 'a t
    val decons : 'a t -> ('a * 'a t) option
    end

module type GenFold = functor (X : Decons) -> sig
    val fold_left : ('b -> 'a -> 'b) -> 'b -> 'a X.t -> 'b
    val fold_right : ('a -> 'b -> 'b) -> 'a X.t -> 'b -> 'b
    val size : 'a X.t -> int
    val list_of : 'a X.t -> 'a list
    val iter : ('a -> unit) -> 'a X.t -> unit
end


module Fold : GenFold = functor (X : Decons) ->
    struct
        let rec fold_left f b t =
            match X.decons t with None -> b |
                                  Some (x, t) -> fold_left f (f b x) t

        let rec fold_right f t b =
            match X.decons t with None -> b |
                                  Some (x, t) -> f x (fold_right f t b)

        let size t = fold_left (fun a x -> a + 1) 0 t

        let list_of t = fold_right (fun x xs -> x :: xs) t []

        let iter f t = fold_left (fun () x -> f x) () t
end



module MyQueue = struct open Queue
    type 'a t = 'a queue

    let decons = function
        | Queue ([], xs) -> (
            match rev xs with [] -> None
                | x :: xs -> Some (x, Queue (xs, [])))
        | Queue (x :: xs, t) -> Some (x, Queue (xs, t))
end

module MyAVL = struct open AVL
    type 'a t = 'a avl

    let decons avl =
        match extract_min avl with
        | None, avl -> None
        | Some (a, avl) -> Some (a, avl)
end


module FoldAVL = Fold (MyAVL);;
module FoldQueue = Fold (MyQueue);;

let sort list = FoldAVL.list_of (AVL.from_list list);;

