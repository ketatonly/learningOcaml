let x = 1;;

let slice list n m = let rec help acc lst k = match acc with 
  [] -> acc
| x::xs -> if k = 0 then help (x::acc) xs (m - n) else help acc xs (n - 1)
in help [] list n
;;



let rand_select lst n = 
  let extracted z list =
    let rec helper acc z = function
      | [] -> acc
      | x::xs -> if z = 0 then acc @ xs else helper (acc @ [x]) (z-1) xs
    in helper [] z list
  in
  let rec help acc n list = match list with 
    | [] -> acc
    | _ -> 
      if n = 0 then acc 
      else 
        let randomint = Random.int (List.length list) in
        help (acc @ [List.nth list randomint]) (n - 1) (extracted randomint list)
  in
  help [] n lst;;

(* Example usage *)
let () =
  let result = rand_select [1; 2; 3; 4; 5] 3 in
  List.iter (Printf.printf "%d ") result;;





  let rec search t i =
    match t with
    | Leaf x -> if i = 0 then Some x else None
    | One (size, subtree) ->
        if i < size then search subtree i else None
    | Two (lsize, rsize, left, right) ->
        if i + 1 <= lsize then search left i-1
        else search right (i - 1)

        let rec update t i x =
          match t with
          | Leaf _ -> if i = 0 then (true, Leaf x) else (false, t)
          | One (size, subtree) ->
              if i +1 <= size then (true, One (size, update subtree i-1 x))
              else (false, One (size, subtree))
          | Two (lsize, rsize, left, right) ->
              if i +1 <= lsize then  (true, Two (lsize, rsize, update left i-1 x, right))
              else (true, Two (lsize, rsize, left, update right i-1 x))
        


              let rec insert t i v =
                match t with
                | Leaf x ->
                    if i = 0 then Two (i, 1, Leaf v, Leaf x)
                    else Failure "Index_out_of_bounds"
                | One (size, subtree) ->
                    if i +1<= size then One (size + 1, insert subtree i-1 v)
                    else Failure "Index_out_of_bounds"
                | Two (lsize, rsize, left, right) ->
                    if i+1 <= lsize then Two (lsize + 1, rsize, insert left i-1 v, right)
                    else Two (lsize, rsize + 1, left, insert right (i - 1) v)


                    let rec remove t i =
                      match t with
                      | Leaf x ->
                          if i = 0 then (Some x, None)
                          else Failure "Index_out_of_bounds"
                      | One (size, subtree) ->
                          if i +1 <= size then One (size -1, remove subtree i-1)
                          else Failure "Index_out_of_bounds"
                      | Two (lsize, rsize, left, right) ->
                          if i +1<= lsize then Two (lsize-1, rsize, remove left i-1, right)
                          else Two (lsize, rsize-1, left, remove right i-1)
                    
              
  