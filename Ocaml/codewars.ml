
let even_or_odd (n: int): string = 
  if n mod 2 = 0 then "Even" else "Odd";;


  let make_negative (number: int): int = 
    if number > 0 then -number else number;;
  
    

let check_for_factor (base: int) (factor: int): bool = 
  if base >= 0 && factor > 0 then
      if base mod factor = 0 then true else false
  else false;;


let is_uppercase (s: string): bool =
  s = String.uppercase_ascii s;;



let expression_matter a b c = 
  let results = [
    a + b + c;
    a + (b + c);
    (a + b) + c;
    a * b * c;
    a * (b + c);
    (a * b) + c;
    a + (b * c);
    (a + b) * c;
    (a * b) * c
  ] in
  List.fold_left max min_int results;;  (* or: max result1 (max result2 (max result3 (max result4 result5))) *)




