open List
open Sets

(*********)
(* Types *)
(*********)

type ('q, 's) transition = 'q * 's option * 'q
(* sigma A list of states the NFA accept
   qs= A list of inputs(a) in NFA
   q0= the starting states
   fs= final states
   delta= a list of transitions, 
 *) 
type ('q, 's) nfa_t = {
  sigma: 's list;
  qs: 'q list;
  q0: 'q;
  fs: 'q list;
  delta: ('q, 's) transition list;
}

(***********)
(* Utility *)
(***********)

(* explode converts a string to a character list *)
let explode (s: string) : char list =
  let rec exp i l =
    if i < 0 then l else exp (i - 1) (s.[i] :: l)
  in
  exp (String.length s - 1) []

(****************)
(* Part 1: NFAs *)
(****************)

let move (nfa: ('q,'s) nfa_t) (qs: 'q list) (s: 's option) : 'q list = 
  let reached_list = List.filter(fun (s1, input, s2) -> List.mem s1 qs && input = s) nfa.delta in
  let reached_list = List.map(fun(s1, input, s2) -> s2) reached_list in
  let reached_list = List.sort_uniq Stdlib.compare reached_list in
  reached_list
  

let e_closure (nfa: ('q,'s) nfa_t) (qs: 'q list) : 'q list =
  let rec e_closure_helper (nfa: ('q,'s) nfa_t) (qs: 'q list): 'q list =
    let queue = move nfa qs None in 
    let reached_states = List.filter(fun s1 -> not (List.mem s1 qs)) queue in
    if reached_states = [] then
      qs
    else
      e_closure_helper nfa (qs @ reached_states) in
  e_closure_helper nfa qs

let accept (nfa: ('q,char) nfa_t) (s: string) : bool =
  let rec accept_helper (possible: 'q list) (char_list: char list): bool =
    match char_list with
    |[] -> if List.fold_left(fun acc curr -> if List.mem curr nfa.fs then true else acc)false possible then true else false
    |h::t -> 
        let list_paths = List.fold_left(fun acc curr -> (e_closure nfa (move nfa [curr] (Some h)))@acc) [] possible in
        accept_helper list_paths t in
  accept_helper(e_closure nfa [nfa.q0]) (explode s)

(*******************************)
(* Part 2: Subset Construction *)
(*******************************)

let new_states (nfa: ('q,'s) nfa_t) (qs: 'q list) : 'q list list =
  List.map(fun states -> e_closure nfa (move nfa qs (Some states))) nfa.sigma

let new_trans (nfa: ('q,'s) nfa_t) (qs: 'q list) : ('q list, 's) transition list = 
  List.map(fun states -> 
      let nexts = e_closure nfa (move nfa qs (Some states)) in 
      (qs, Some states, nexts)
    ) nfa.sigma

let new_finals (nfa: ('q,'s) nfa_t) (qs: 'q list) : 'q list list =
  let x = intersection nfa.fs qs in
  if x = [] then [] else [qs]                       

let flatten lst =
  List.fold_left (fun acc x -> acc @ x) [] lst
 
let rec nfa_to_dfa_step (nfa: ('q,'s) nfa_t) (dfa: ('q list, 's) nfa_t) (work: 'q list list) =
  match work with
  |[] -> dfa
  |h::t ->
      if List.mem h dfa.qs then nfa_to_dfa_step nfa dfa t 
      else 
        let new_qs = insert h dfa.qs in
        let new_states = remove [] (new_states nfa h) in
        let new_final = if new_finals nfa h <> [] then insert h dfa.fs else dfa.fs in
        let new_work = (insert_all new_states t) in
        let curr = {
          sigma = dfa.sigma; (*Do not edit*)
          qs = new_qs;
          q0 = dfa.q0; (*Do not edit*)
          fs = new_final;
          delta = new_trans nfa h @ dfa.delta;
        } in 
        nfa_to_dfa_step nfa curr new_work 
  
let nfa_to_dfa (nfa: ('q,'s) nfa_t) : ('q list, 's) nfa_t = 
  let initial_dfa = {
    sigma = nfa.sigma;
    qs = [];
    q0 = e_closure nfa [nfa.q0];
    fs = [];
    delta = [];
  } in 
  nfa_to_dfa_step nfa initial_dfa [initial_dfa.q0]

  







