open Funs

(***********************************)
(* Part 1: Non-Recursive Functions *)
(***********************************)

let rev_tup (a, b, c) = (c, b, a)

let is_even x = 
  if x mod 2 = 0 then true
  else false

let area (a, b) (x, y) = 
  abs((y - b) * (x - a))

(*******************************)
(* Part 2: Recursive Functions *)
(*******************************)

let rec fibonacci n = 
  if n = 0 then n
  else if n = 1 then 1
  else fibonacci(n-1) + fibonacci (n-2)
  
let rec pow x p = 
  if p = 0 then 1 
  else x * pow x (p-1) 
          
let rec is_prime_helper x y =
  if y = 1 then true 
  else if x mod y = 0 then false
  else is_prime_helper x (y-1)
         
let rec is_prime x = 
  if x < 0 then false
  else if x = 1 then false
  else if x = 2 then true
  else is_prime_helper x (x-1)

let rec maxFuncChain init funcs = 
  match funcs with
  |[] -> init 
  |h::t -> max(maxFuncChain init t)(maxFuncChain(h init) t)

(*****************)
(* Part 3: Lists *)
(*****************)

                             
let reverse lst = 
  let rec reverse2 lst = 
    match lst with
    |[] -> []
    | h::t -> (reverse2 t) @ [h] 
  in reverse2 lst

let rec merge lst1 lst2 = 
  match lst1, lst2 with
  |lst1, [] -> lst1
  |[], lst2 -> lst2 
  |head1::tail1, head2::tail2 ->
      if head1 < head2 then head1::(merge tail1 lst2)
      else head2::(merge tail2 lst1) 

(*
let rec count_list lst = 
  match lst with
  | [] -> 0
  | h1::t1 -> count_list t1 + 1
                    
  
let rec is_palindrome lst = 
  let length = (count_list lst) / 2 in
  let rec is_palindrome_helper lst length =
    if length = 0 then true else
      let lst2 = reverse lst in
      match lst, lst2 with
      | [], [] -> true
      | [_], [_] -> true
      | h1::t1, h2::t2 -> 
          if h1 = h2 then is_palindrome_helper t1 (length-1)
          else false 
  in is_palindrome_helper lst length
    *)

let rec is_palindrome lst =
  let lst2 = reverse lst in
  if lst = lst2 then true
  else false

let jumping_tuples lst1 lst2 = 
  let index = 0 in 
  let rec helper lst1 lst2 index = 
    match lst1, lst2 with
    | lst1, [] -> []
    | [], lst2 -> []
    |h1::t1, h2::t2 ->
        if (index mod 2 = 0) then
          let (a, b) = h2 in
          b::helper t1 t2 (index+1) 
        else
          let (a, b) = h1 in
          a::helper t1 t2 (index+1)
  in helper lst1 lst2 index

let rec square_primes lst = 
  match lst with
  |[] -> []
  |h::t -> 
      if is_prime(h) then (h, h*h)::square_primes t
      else square_primes t

let rec flatten lst = 
  match lst with
  | [] -> []
  |h::t -> 
      h @ flatten t

let rec partition p lst = 
  match lst with 
  |[] -> ([], [])
  |h::t -> 
      let (a, b) = partition p t in
      if p h then ([h]@a, b)
      else (a, [h]@b) 

(*****************)
(* Part 4: HOF *)
(*****************)

let is_present lst x = 
 List.fold_left(fun a curr -> if curr = x then a@[1] else a@[0])[] lst 

let count_occ lst target = 
  List.fold_left(fun a curr -> if curr = target then a+1 else a) 0 lst 

let is_uniq_helper lst x = 
  List.fold_left(fun a curr -> if curr = x then false else a)true lst 

let uniq lst = 
  List.fold_left(fun a curr -> if is_uniq_helper a curr then a@[curr] else a) [] lst 
