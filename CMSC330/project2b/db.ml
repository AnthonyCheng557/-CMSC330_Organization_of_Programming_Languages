type person = { name: string;
                age: int;
                hobbies: string list }

(* Define the type of db below *)
type db = {theList: person list}

let newDatabase = {theList = []}

let insert person db = {theList = person :: db.theList} 
                                 
                       
let rec remove name db = {theList = List.filter (fun x -> x.name <> name) db.theList}

type condition =
  | True
  | False
  | Age of (int -> bool)
  | Name of (string -> bool)
  | Hobbies of (string list -> bool)
  | And of condition * condition
  | Or of condition * condition
  | Not of condition
  | If of condition * condition * condition

let rec queryHelper condition curr =
  match condition with
  | True -> true
  | False -> false
  | Age (f) -> f curr.age
  | Name (f) -> f curr.name 
  | Hobbies (f) -> f curr.hobbies
  | And (a, b) -> queryHelper(a) curr && queryHelper(b) curr
  | Or (a, b) -> queryHelper(a) curr || queryHelper(b) curr
  | Not (a) -> not (queryHelper(a) curr) 
  | If (a, b, c) -> if queryHelper(a) curr then queryHelper(b) curr else queryHelper(c) curr
          
let rec query condition db = 
  List.filter(fun curr -> queryHelper condition curr) db.theList 

type comparator = person -> person -> int

let rec sort comparator db = List.sort comparator db.theList

let queryBy condition db comparator =   
  let x = query condition (db) in
  let y = sort comparator ({theList = x}) in
  y

let deleteAll condition db = 
  let y = List.filter(fun curr -> not(queryHelper condition curr)) db.theList in
  let z = {theList = y} in
  z

let update condition db personData = 
  let x = query condition (db) in 
  let y = List.map personData x in
  let z = deleteAll condition db in
  {theList = y @ z.theList} 
