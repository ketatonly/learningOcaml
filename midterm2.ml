type 'a tree = Leaf of 'a | Node of int * int * 'a tree * 'a tree;;

let t = Node (3, 2, Node (2, 1, Leaf 'a', Leaf 'b'), Leaf 'c');;



let rec update t i x = match t with 
   Leaf _ -> Leaf x 
  | Node (a, b, l, r) -> if i>=a then failwith "out of bounds" else
     (if i < b then Node(a, b, update l i x, r)
  else Node (a, b, l, update r i x));;


let rec insert t i x = match t with 
   Leaf y -> Node (2, 1, Leaf x, Leaf y)
  | Node (a, b, l, r) -> if i >= b then Node ((a+1), b, l, insert r i x)
  else Node ((a+1), (b+1), insert l i x, r);;


let to_list t = let rec helper acc t = match t with 
   Leaf x -> x::acc 
  | Node (a, b, l, r) -> (helper acc l) @ (helper acc r)
in helper [] t;;



let mul l = List.fold_right (fun x acc -> x * acc) l 1;;

let count l = List.fold_right (fun x acc -> 1 + acc) l 0;;
