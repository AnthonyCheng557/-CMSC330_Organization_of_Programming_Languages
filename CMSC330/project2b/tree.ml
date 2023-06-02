type 'a tree =
  | Node of 'a tree * 'a * 'a tree
  | Leaf

let rec tree_fold f init tree = 
  match tree with
  |Leaf -> init
  |Node(left, value, right) -> 
      f (tree_fold f init left) value (tree_fold f init right) 
            
let rec tree_fold f init tree = 
  match tree with
  |Leaf -> init
  |Node(left, value, right) -> 
      f (tree_fold f init left) value (tree_fold f init right) 
  
        
let map tree f = 
  tree_fold (fun left value right -> Node(left, f value, right)) Leaf tree 
  
  
let mirror tree =
  let do_nothing = fun x -> x in 
  tree_fold (fun left value right -> Node(right, do_nothing value, left)) Leaf tree 

let in_order tree = 
  let empty_list = [] in
  tree_fold (fun left value right -> left @ [value] @ right) empty_list tree 
    
let pre_order tree = 
  let empty_list = [] in
  tree_fold (fun left value right -> [value] @ left @ right) empty_list tree 

let compose tree = 
  tree_fold (fun left equation right -> (fun a -> (right (equation (left a))))) (fun a -> a) tree 

let depth tree = 
  let num = 0 in
  tree_fold (fun left value right -> 1 +  max left right) num tree 

(* Assume complete tree *)
let trim tree n = 
  let length = depth tree in
  tree_fold (fun left value right -> 
      if depth left = length - n 
      then 
        Node(Leaf, value, Leaf)
      else
        Node(left, value, right)
    ) Leaf tree 

let rec split lst v =
  match lst with 
  |[] -> (lst, [])
  |h::t -> if h = v then ([], t) else 
        let (a, b) = split t v in
        (h::a, b)
        
let rec take lst n =
  match lst, n with
  | [], _ -> []
  | _, 0 -> []
  | h :: t, n -> h :: take t (n-1)
                   
let rec drop lst n =
  let length_list = List.length lst in
  let rev_list = List.rev lst in
  let temp = take rev_list (length_list - n) in
  List.rev temp
  
  
let rec from_pre_in pre in_ord =
  match pre, in_ord with
  |[],[] -> Leaf 
  |([], _::_) -> Leaf
  |(_::_, []) -> Leaf
  |h1::t1, h2::t2 ->
      let root = h1 in
      let (a, b) = split in_ord h1 in
      let length = List.length a in 
      let c = take pre (length+1) in 
      let c = drop c 1 in
      let d = drop pre (length+1) in
      Node(from_pre_in a c, root, from_pre_in d b)