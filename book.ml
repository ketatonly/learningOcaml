let x = 20 in x*x*x ;;

let a = 500 in (let b = a*a in a + b);;

(* names and functions: *)

let cube x = x * x * x;;

let neg x = if x<0 then true else false;;
(* or: *)
let isneg x = x < 0;;

let isvowel c = c='a' || c='e' || c='i' || c='o' || c='u';;


let addtoten a b = a + b = 10;;

(* factorial: *)
let rec factorial a = if a = 1 then 1 else a * factorial (a-1);;

(* greatest common divisor: *)
let rec gcd a b = if b = 0 then a else gcd b (a mod b);;

(* negation: *)
let not x = if x then false else true;;

(* multiplying by ten: *)
let tentimes x = 10*x;;

(* both zero: *)
let bothzero a b = if a=0 && b=0 then true else false;;

(* sumn: *)
let rec sumn n = if n = 1 then 1 else n+sumn(n-1);;

(* power: *)
let rec power x n = if n = 0 then 1 else x*power x (n-1);;

(* isconsonant: *)
let isconsonant c = not(c='a' || c='e' || c='i' || c='o' || c='u');;

(* factorial: *)
let rec fact n = if n<0 then -1 else if n=0 then 1 else fact n*fact (n-1);;



(* case by case: *)

(* pattern matching: *)
let rec matchfactorial a = 
    match a with 
     1 -> 1
    | _ -> a * matchfactorial (a - 1);;

let matchvowel c =
    match c with
     'a' -> true
    | 'e' -> true
    | 'i' -> true
    | 'o' -> true
    | 'u' -> true
    | _ -> false;;

    (* this is a better version: *)

    let combinedvowel c = 
        match c with
         'a' | 'e' | 'i' | 'o' | 'u' -> true
        | _ -> false;;



(* not function with matching: *)
let matchnot x = 
    match x with
     true -> false
    | false -> true;;

let rec matchsum x =
    match x with
     1 -> 1
    | _ -> x + matchsum (x-1);;

    
let isupper c =
    match c with
     'A'..'Z' -> true
    | _ -> false;;

let islower c = 
    match c with 
     'a'..'z' -> true
    | _ -> false;;



(* making lists: *)
let isnil l =
    match l with
     [] -> true
    | _ -> false;;


(* counting elements: *)
let rec length l =
    match l with
     [] -> 0
    | _::t -> 1 + length t;;


(* adding lists: *)
let rec sum l =
    match l with
     [] -> 0
    | h::t -> h + sum t;;


(* accumulative length: *)
let acclength l = 
    let rec length_inner l n =
        match l with
         [] -> n
        | _::t -> length_inner t (n+1)
    in length_inner l 0;;


(* returnin odd indexed elements: *)
let rec odd_index l =
    match l with
     [] -> []
    | [a] -> [a]
    | a::_::t -> a::odd_index t;;
(* non-tail recursive *)

(* reduced version: *)
let rec odd_indexed l =
    match l with
     a::_::t -> a::odd_index t
    | _ -> l;;


(* appending lists: *)
let rec append a b =
    match a with
     [] -> b 
    | h::t -> h:: append t b;;

(* reversing the list: *)
let rec reverse l =
    match l with
     [] -> []
    | h::t -> reverse t @ [h];;


(* take and drop: *)
let rec take n l = if n = 0 then [] else
    match l with
     []->[]
    | h::t -> h :: take (n - 1) t;;

let rec drop n l = if n = 0 then l else
    match l with 
     [] -> []
    | h::t -> drop (n-1) t;;


(* returning even indexed elements: *)
let rec evens l = 
    match l with
     _::h::t -> h:: evens t
    | _ -> [];;

(* counting true elements: *)
let rec trues l =
    match l with
     [] -> 0
    | h::t -> if h=true then 1+trues t else trues t;;

(* or: *)

let rec counttrue l =
    match l with
     [] -> 0
    | true::t -> 1 + counttrue t
    | false::t -> counttrue t;;

let acctrue l =
    let rec helper l acc =
        match l with
         [] -> acc
        | true::t -> helper t (acc+1)
        | false::t -> helper t acc
    in helper l 0;;


(* palindrome builder: *)
let palindrome l = l@List.rev l;;
    
(* checking palindrome: *)
let ispalindrome l = l = List.rev l;;

(* dropping last element: *)
let rec droplast l = 
    match l with
     [] -> []
    | [a] -> []
    | h::t -> h::droplast t;;

(* tail recursive version: *)
let drop_last l = let rec helper l acc =
    match l with 
     [] -> []
    | [a] -> List.rev acc
    | h::t -> helper t (h::acc)
in helper l [];;


(* checking the element existence in a list *)
let rec member el l = 
    match l with
     [] -> false
    | h::t -> if h=el then true else member el t;;


(* removing duplicates from a list *)
let rec make_set l =
    match l with
     [] -> []
    | h::t -> if member h t then make_set t else h::make_set t;;


(* better version of reversing a list: *)
let rev l = let rec helper l acc =
    match l with
     [] -> acc
    | h::t -> helper t (h::acc)
in helper l [];;