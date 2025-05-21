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

  
