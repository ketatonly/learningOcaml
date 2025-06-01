(* Another definition of the same variable does not assign a new value to it, but creates a new variable with the same name.
  The old variable is now hidden but still there. The new variable may even have a different type. *)

let seven = 42;;
let seven = "seven";;


(* More Complex Datatypes 
  Pairs  *)
(3, 4);;
(1=2, "Hello");;



(* Tuples: *)
(2, 3, 4, 5);;
("Hello", true, 3.14159);;



(* Simultaneous definition of Variables  *)
let (x, y) = (3, 4.0);;
let (3, y) = (3, 4.0);; (* throws a partial-match warning *)




(* Records: *)
type person = {given:string; sur:string; age:int};;

let paul = {given="Paul"; sur="Meier"; age=24};;
let hans = {given="Kohl"; age=23; sur="hans"};;
(* let hansi = {age=23; sur="Kohl"};; *) (*throws an error because all the record fields should be defined *)
(* Records are tuples with named components whose orderings is irrelevant *)
(* As a new type, a record must be introduced before its use by means of a type declaration *)

(* Access to record components: *)
paul.given;;

(* with pattern matching: *)
let {given=x; sur=y; age=z} = paul;;

let {given=x; _} = paul;;

(* Case distinction: match and if *)

let test n = 
  match n with
    0 -> "null"
  | 1 -> "one"
  | _ -> "uncountable!";;

test 4;;
test 0;;
test 1;;


(* Lists: *)
let mt = [];;
let l1 = 1::mt;;
let l = [1;2;3];;
let l2 = 1::2::3::[];;
(* All the list elements should have the same type *)


(* Pattern matching on Lists *)
let first l = 
  match l with 
   [] -> -1
  | x::xs -> x;;
  
first [1;2;3];;


(* Definition of functions: *)
let double x = 2 * x;;
(double 3, double (double 1));;

(* Variable whose value is a function: *)
let doubled = fun x -> 2 * x;;
doubled 4;;

(* functions may additionally access the values of variables which have been visible at their point of definition: *)
let factor = 2;;
let dbl x = factor * x;;
let factor = 4;;
dbl 3;;  (* uses factor = 2 *)


(* A function is recursive if it calls itself directly or indirectly: *)
let rec fac n = if n < 2 then 1 else n * fac (n-1);;
fac 4;;

let rec fib = fun x -> if x <= 1 then 1 else fib (x-1) + fib (x-2);;
fib 4;;

(* If functions call themselves indirectly via other functions, they are called mutually recursive: *)
let rec even n = if n=0 then true else odd (n-1) 
    and odd n = if n=0 then false else even (n-1);;

  
(* Definition by case distinction *)
let rec length = fun l -> 
  match l with
   [] -> 0
  | x::xs -> 1 + length xs;;
  
length [1;2;3];;  (* 3 *)

(* shorter version: *)
let rec len = function
  | [] -> 0
  | x::xs -> 1 + len xs;;

len [1;2;3];; (* 3 *)


(* case distinction for several arguments *)
let rec app l y = 
  match l with
   [] -> y
  | x::xs -> x:: app xs y;;

app [1;2] [3;4];;  (* [1;2;3;4] *)

(* shorter version: *)
let rec append = function 
   [] -> fun y -> y
  | x::xs -> fun y -> x:: append xs y;;

append [1;2] [3;4];;


(* local definitions: *)
let x = 5
in let sq = x * x
in sq + sq;;

let facit n = let rec iter m yet = if m > n then yet else iter (m+1) (m*yet)
in iter 2 1;;

(* enumeration types: *)
type color = Diamonds | Hearts | Gras | Clubs;;
type value = Seven | Eight | Nine | Jack | Queen | King | Ten | Ace;;

Clubs;;  (* color = Clubs *)
let gras_jack = (Gras, Jack);;  (* val gras_jack : color * value = (Gras,Jack) *)

(*Typos are recognized*)
(*the alternatives are called constructors and are separated by | 
and starts with a capital letter *)

(*constructors can be compared: *)
Clubs < Diamonds;; (*false*)
Clubs > Diamonds;; (*true*)

(* pattern matching on constructors: *)
let is_trump = function 
  | (Hearts, _) -> true
  | (_, Jack) -> true
  | (_, Queen) -> true 
  | (_,_) -> false ;;

is_trump (Gras, Jack);; (*true*)
is_trump (Clubs, Nine);; (*false*)

let string_of_color = function 
 | Diamonds -> "Diamonds"
 | Hearts -> "Hearts"
 | Gras -> "Gras" 
 | Clubs -> "Clubs";;


(* Now we can "play" cards: *)
let takes c1 c2 = match (c1,c2) with 
    ((f1, Queen), (f2, Queen)) -> f1>f2 
  | ((_, Queen),_) -> true 
  | (_, (_, Queen)) -> false 
  | ((f1, Jack), (f2, Jack)) -> f1>f2
  | ((_,Jack),_) -> true 
  | (_,(_,Jack)) -> false 
  | ((Hearts,w1), (Hearts,w2)) -> w1 > w2
  | ((Hearts,_),_) -> true 
  | (_,(Hearts,_)) -> false 
  | ((f1,w1),(f2,w2)) -> if f1=f2 then w1>w2 else false;;

let take card2 card1 = if takes card2 card1 then card2 else card1;;

let trick card1 card2 card3 card4 = 
  take card4 (take card3 (take card2 card1));;

 trick (Gras,Ace) (Gras,Nine) (Hearts,Ten) (Clubs,Jack);;
 (* (Clubs, Jack) *)
trick (Clubs,Eight) (Clubs,King) (Gras,Ten) (Clubs,Nine);;
(* (Clubs, King) *)


(* Sum types: *)
type 'a option = None | Some of 'a;;
let is_some x = match x with 
   Some _ -> true 
  | None -> false;;

let get x = match x with
| Some y -> y
| None -> y
let value x a = match x with
| Some y -> y
| None -> a
let map f x = match x with
| Some y -> Some (f y)
| None -> None
let join a = match a with
| Some a' -> a'
| None -> None


(* useful for defining partial functions *)
let rec get_value a l = match l with 
   [] -> None 
  | (b,z)::rest -> if a=b then Some z else get_value a rest;;


(* datatypes can be recursive: *)
type sequence = End | Next of (int * sequence);;

Next (1, Next (2, End));;

(* recursive datatypes lead to recursive functions: *)
let rec nth n s = match (n, s) with 
   (_, End) -> None 
  | (0, Next (x, _)) -> Some x 
  | (n, Next (_, rest)) -> nth (n-1) rest;;

nth 4 (Next (1, Next (2, End)));; (*None*)
nth 2 (Next (1, Next(2, Next (5, Next (17, End)))));; (*Some 5*)

(* another exmaple: *)
let rec down = function 
   0 -> End 
  | n -> Next (n, down (n-1));;

down 3;; (* Next (3, Next (2, Next (1, End))) *)

(* tail-recursive and non-tail-recursive functions: *)
let f x = x + 5;;
(* tail recursive since it directly returns the result.
reuses stack *)
let g y = let z = 7
in if y > 5 then f (-y)
else z + f y;;
(* non tailrecursive, does an additional computation after a function call,
potential stack overflow for big inputs *)

(* tail recursive functions: *)
let fac x = let rec facit n acc =
if n <= 1 then acc
else facit (n - 1) (n * acc)
in facit x 1;;

let rec loop x = if x < 2 then x
else if x mod 2 = 0 then loop (x / 2)
else loop (3 * x + 1);;


(* reversing a list: version 1 *)
let rec rev list = match list with 
   [] -> [] 
  | x::xs -> app (rev xs) [x];;
(* quadratic running time *)

(* version 2: *)
let reverse list = let rec helper a l = 
  match l with 
   [] -> a 
  | x::xs -> helper (x::a) xs 
in helper [] list;;
(* tail recursive and linear run time*)


(* higher order functions: *)
let f (a, b) = a + b + 1;; (*uncurried function*)
(* - : int * int -> int = <fun> *)
let g a b = a + b + 1;; (*curried function*) (*is converted into a series of functions each taking a single argument*)
(* - : int -> int -> int = <fun> *)

(* function f has a single argument, (a, b). 
function g has the one argument a of type int. *)
f (3, 5);; (* 9 *)
let g1 = g 3;;
g1 5;; (* 9 *)

(* curried functions are called higher order functions, because their result is again a function.
the aplication of g to a single argument is called partial, because the result takes another argument 
before the body is evaluated. *)

let curry f a b = f (a, b);; (*the argument of a function can again be a function. *)

let plus (x,y) = x + y;;
curry plus;;

let plus2 = curry plus 2;;
let plus3 = curry plus 3;;

plus2 (plus3 4);; (* 9 *)


(* some list functions: *)
let rec mapp f = function 
   [] -> [] 
  | x::xs -> f x :: mapp f xs;;

let double x = 2 * x;;
mapp double [1;2;3;4];;


let rec fold_left f a = function 
   [] -> a 
  | x::xs -> fold_left f (f a x) xs;;

fold_left (+) 0 [1;2;3;4];;

let rec fold_right f = function 
   [] -> fun b -> b 
  | x::xs -> fun b -> f x (fold_right f xs b);;

fold_right (+) [1;2;3;4] 0;;

let rec find_opt f = function 
   [] -> None 
  | x::xs -> if f x then Some x else find_opt f xs;;

let even x = x mod 2 = 0;;
find_opt even [1;2;3;4;5;6];;



(* polymorphic functions: *)

(* they can operate on values of different types - they are generic *)
(* instead of being a specific type like int they use type variables like 'a, 'b *)
(* for example: find_opt : ('a -> bool) -> 'a list -> 'a option *)

let cons_r xs x = x::xs;;
let rev l = fold_left cons_r [] l;;

(* polymorphic datatypes: *)
type 'a tree = Leaf of 'a | Node of ('a tree * 'a tree);;
(* a tree is called type constructor, because it allows to create a new type from another type. *)

Leaf 1;;
Node (Leaf ('a', true), Leaf ('b', false));;

(* functions for polymorphyc datatypes are again polymorphic: *)
let rec size = function 
   Leaf _ -> 1 
  | Node (t, t') -> size t + size t';;

let rec flatten = function 
   Leaf x -> [x]
  | Node (t, t') -> flatten t @ flatten t';; (* non tail recursive *)

let flatten1 t = let rec doit t xs = 
  match t with 
   Leaf x -> x::xs 
  | Node (t, t') -> let xs = doit t' xs 
in doit t xs 
in doit t [];;  (* tail recursive *)

let t = Node (Node (Leaf 1, Leaf 5), Leaf 3);;
size t;;
flatten t;;
flatten1 t;;


(* queues: *)

(* first idea: *)
type 'a queue = 'a list;;

let dequeue = function 
   [] -> (None, [])
  | x::xs -> (Some x, xs);;

let enqueue x xs = xs @ [x];;
(* tail recursive and better version: but uses more memory *)
let enqueue1 x xs = let rec helper acc = function 
   [] -> List.rev (x::acc) 
  | x::xs ->  helper (x::acc) xs
  in helper [] xs;;

(* second idea: *)
type 'a queue2 = Queue of 'a list * 'a list;;

let is_empty = function 
   Queue ([], []) -> true 
  | _ -> false;;

let queue_of_list list = Queue (list, []);;

let list_of_queue = function 
   Queue (first, []) -> first 
  | Queue (first, last) -> first @ List.rev last;;

let enqueue x (Queue (first, last)) = Queue (first, x::last);;

let dequeue = function 
   Queue ([], last) -> (match List.rev last with 
                [] -> (None, Queue ([], []))
              | x::xs -> (Some x, Queue (xs, [])) )
  | Queue (x::xs, last) -> (Some x, Queue (xs, last));;



(* Anonymous functions: *)
fun x y z -> x + y + z;;
(* the notion originates from λ-calculus. *)
(* recursive functions cannot be defined this way *)

(* pattern matching: *)
function None -> 0 
  | Some x -> x * x + 1;;

(* often anonymous functions are used as arguments to functionals: *)
mapp (fun x -> x*x) [1;2;3];;
(* [1; 4; 9] *)

let make_undefined () = fun x -> None;;
make_undefined () 3;;  (*None*)

let def_one (x,y) = fun x' -> if x = x' then Some y else None;;
def_one (1, "Hello!") 1;; (*Hello!*)
def_one (1, "Hello!") 2;; (*None*)


(* Exceptions: *)
1/0;;   (* Exception: Division_by_zero. *)
List.tl (List.tl [1]);; (* Exception: Failure "tl". *)

