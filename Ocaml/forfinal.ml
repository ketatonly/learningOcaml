
  (* btree *)

type tree = Empty | Node of int * tree * tree;;

let rec insert x t = match t with
  Empty -> Node (x, Empty, Empty)
| Node (y, l, r) -> if x<y then Node (y, insert x l, r)
else if x>y then Node (y, l, insert x r)
else t;;

let mytree = Node (15, Node(7, Empty, Empty), Node (17, Empty, Empty));;
insert 8 mytree;;

let rec find x t = match t with
  Empty -> false
| Node(y, l, r) -> if x=y then true
else if x>y then find x r else find x l;;


let rec findmax t = match t with
  Empty -> raise Not_found
| Node (y, l, Empty) -> y
| Node (y, l, r) -> findmax r;;



let rec findmin t = match t with
  Empty -> raise Not_found
| Node (y, Empty, r) -> y
| Node (y, l, r) -> findmin l;;


let rec delete x t = match t with
  Empty -> Empty
| Node (y, l, r) -> if x<y then Node (y, delete x l, r)
else if x>y then Node (y, l, delete x r)
else if l = Empty then r
else if r = Empty then l
else let m = findmax l in
  Node (m, delete m l, r);;

  let mytree1 = Node (15, Node(7, Empty, Empty), Node (17, Empty, Empty));;

  

let tolist t = let rec help t acc = match t with
  Empty -> acc
| Node (y, l, r) -> let m = findmax t in
  help (delete m t) (m::acc)
in help t [];;

let rec fromlist l = match l with
  [] -> Empty
| x::xs -> insert x (fromlist xs);;

let list = [5; 2; 7; 8; 3];;

let rec height t = match t with
  Empty -> 0
| Node (y, Empty, Empty) -> 1
| Node (y, l, r) -> if height l > height r then 1 + height l else 1 + height r;;


(* Lazy Lists *)

type 'a llist = Cons of 'a * (unit -> 'a llist);;

let rec lnat (n : int) = Cons (n, fun () -> lnat (n+1));;

let lfib () = let rec fib a b = Cons (a, fun () -> fib b (a+b))
in fib 0 1;;

let rec ltake n llist = match n, llist with
  0, _ -> []
| _, Cons (h, t) -> h :: ltake (n-1) (t());;

let rec lfilter f llist = match llist with
  Cons (h, t) -> if f h then Cons (h, fun () -> lfilter f (t())) else lfilter f (t());;

let fib = lfib ();;
let filtered = lfilter (fun n -> n mod 2 = 0) fib;;

  (* One-two tree *)

type 'a onetwo = Null | One of 'a * 'a onetwo | Two of 'a onetwo * 'a * 'a onetwo ;;
         

let rec extract_min tr = match tr with
Null -> None, Null
|One(h,t)-> Some h, t
|Two(l, h, r)-> 
    match extract_min l with
    |None, x -> Some h, r 
    |Some y, tail -> Some y, Two(tail, h, r);;


let verify tree = let rec ver small tr big = match tr with
  Null -> true
| One (h, t) -> if h > small && h < big then ver h t big else false
| Two (Null, h, r) -> if h > small && h < big then ver h r big else false 
| Two (l, h, Null) -> if h > small && h < big then ver small l h else false 
| Two (l, h, r) -> if h > small && h < big then ver small l h && ver h r big else false
in ver min_int tree max_int;;


let trees =  Two ( One (5, Null), 2, One (3, Null) );;
let treee = One (1, One(2, One(3, Null)));;

let rec normal tr = match tr with
  Null -> Null
| One (h, t) -> Two (Null, h, normal t)
| Two (l, h, r) -> Two (normal l, h, normal r);;



let rec insert x tr = match tr with
      Null -> One (x, Null)
    | One (h, t) -> if x > h then Two (Null, h, insert x t)  
        else if x < h then Two (One(x, Null),  h, t) 
        else One(h, t)
    | Two (l, h, r) -> if x = h then Two (l, h, r) 
        else if x < h then Two ((insert x l), h, r)
        else Two (l, h, (insert x r));;


let from_list list = let rec fromlist lst tr = match lst with
  [] -> tr
| x::xs -> fromlist xs (insert x tr)
in fromlist list Null;;

let lst = [3; 1; 4; 1; 5; 5];;
let tree = from_list lst


  (* infinite Trees *)

type 'a ltree = LNode of 'a * (unit -> 'a ltree) * (unit -> 'a ltree);;

let rec layer_tree r = 
  LNode (r, (fun () -> (layer_tree (r + 1))), (fun () -> layer_tree (r + 1)) );;

let rec interval_tree l h = 
  LNode ((l, h), (fun () -> interval_tree l ((l+.h) /. 2.)), (fun () -> interval_tree ((l+.h) /. 2.) l));;

let rec rational_tree n d = 
  LNode ((n, d), (fun () -> rational_tree n (d+1)), (fun () -> rational_tree (n+1) d));;

type 'a tree = Empty | Node of 'a * 'a tree * 'a tree;;

let rec top n ltr = if n <= 0 then Empty else
  match ltr with LNode (h, l, r) ->
    Node (h, (top (n-1) (l())), top (n-1) (r()));;

let rec map f ltr = match ltr with LNode (h, l, r) -> 
  LNode (f h, (fun () -> map f (l())), (fun () -> map f (r())) );;

let rec find p t  = let LNode (h, l, r) = t in
  if p h then LNode (h, (fun () -> find p (l ())), (fun () -> find p (r ())))
else
  try find p (l ()) with _ -> find p (r ());;




