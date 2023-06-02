open MicroCamlTypes
open Utils
open TokenTypes

(* Provided functions - DO NOT MODIFY *)

(* Matches the next token in the list, throwing an error if it doesn't match the given token *)
let match_token (toks: token list) (tok: token) =
  match toks with
  | [] -> raise (InvalidInputException(string_of_token tok))
  | h::t when h = tok -> t
  | h::_ -> raise (InvalidInputException(
      Printf.sprintf "Expected %s from input %s, got %s"
        (string_of_token tok)
        (string_of_list string_of_token toks)
        (string_of_token h)))

(* Matches a sequence of tokens given as the second list in the order in which they appear, throwing an error if they don't match *)
let match_many (toks: token list) (to_match: token list) =
  List.fold_left match_token toks to_match

(* Return the next token in the token list as an option *)
let lookahead (toks: token list) = 
  match toks with
  | [] -> None
  | h::t -> Some h

(* Return the token at the nth index in the token list as an option*)
let rec lookahead_many (toks: token list) (n: int) = 
  match toks, n with
  | h::_, 0 -> Some h
  | _::t, n when n > 0 -> lookahead_many t (n-1)
  | _ -> None

(* Part 2: Parsing expressions *)

  (*Working on this*)
  let rec parse_expr toks = 
  match lookahead toks with
    |Some Tok_Let -> let_helper toks
    |Some Tok_Fun -> fun_helper toks
    |Some Tok_If -> if_helper toks
    |_ -> or_helper toks
  
  (*Let helper*)
  and let_helper toks =
    let t_1 = match_token toks Tok_Let in
      (match lookahead t_1 with
      |Some Tok_Rec ->
        let t_2 = match_token t_1 Tok_Rec in
        (match lookahead t_2 with
        |Some Tok_ID(temp_id) ->      
          (*If it is the ID*)
          let t_3 = match_token t_2 (Tok_ID temp_id) in
          let t_4 = match_token t_3 Tok_Equal in
          let(t_5, expr_1) = parse_expr t_4 in
          let t_6 = match_token t_5 Tok_In in
          let(t_7, expr_2) = parse_expr t_6 in
          (t_7, Let (temp_id, true, expr_1, expr_2))
        |_ -> raise (InvalidInputException "fail2"))

      |Some Tok_ID(temp_id) -> 
        let t_2 = match_token t_1 (Tok_ID temp_id) in
        (*If it is the ID*)
        let t_3 = match_token t_2 Tok_Equal in
        let(t_4, expr_1) = parse_expr t_3 in
        let t_5 = match_token t_4 Tok_In in
        let (t_6, expr_2) = parse_expr t_5 in
        (t_6, Let(temp_id, false, expr_1, expr_2))
      |_ -> raise (InvalidInputException "fail3"))


(*Fun helper*)
  and fun_helper toks =
    (*check if Tok_Fun exist*)
    let t_1 = match_token toks Tok_Fun in
    (*check if Tok_ID exist*)
    (match lookahead t_1 with 
    |Some Tok_ID(temp_id) -> 
      let t_2 = match_token t_1 (Tok_ID temp_id) in
      (*if Tok_ID exist, extract it from first_t*)
      let t_3 = match_token t_2 Tok_Arrow in
      let (t_4, expression1) = parse_expr t_3 in
      (t_4, Fun(temp_id, expression1))
    |_ -> raise (InvalidInputException "fail4"))
  

  (*If helper*)
  and if_helper toks =
    (*check if Tok_if exist*)
    let t_1 = match_token toks Tok_If in
    (*check if bool_express ecist*)
    let (t_2, bool_expr) = parse_expr t_1 in
    let t_3 = match_token t_2 Tok_Then in
    let (t_4, first_expr) = parse_expr t_3 in
    let t_5 = match_token t_4 Tok_Else in
    let (t_6, second_expr) = parse_expr t_5 in
      (t_6, If(bool_expr, first_expr, second_expr)) 

    (*Or helper*)
  and or_helper toks =
    let (t_1, and_expr) = and_helper toks in
      (match lookahead t_1 with 
      |Some Tok_Or -> 
        let t_2 = match_token t_1 Tok_Or in
        let (t_3, or_expr) = or_helper t_2 in
        (t_3, Binop(Or, and_expr, or_expr))
      |_ -> 
        (t_1, and_expr))


    (*And helper*)
  and and_helper toks =
    let (t_1, equal_expr) = equal_helper toks in
    (match lookahead t_1 with 
    |Some Tok_And -> 
      let t_2 = match_token t_1 Tok_And in
      let (t_3, and_expr) = and_helper t_2 in
      (t_3, Binop(And, equal_expr, and_expr))
    |_ -> 
      (t_1, equal_expr))
  
  (*Equal helper*)
  and equal_helper toks =
    let (t_1, rela_expr) = relational_helper toks in
    (match lookahead t_1 with
      |Some Tok_Equal ->
        let t_2 = match_token t_1 Tok_Equal in
        let (t_3, equal_expr) = equal_helper t_2 in
        (t_3, Binop(Equal, rela_expr, equal_expr))
      |Some Tok_NotEqual -> 
        let t_2 = match_token t_1 Tok_NotEqual in
        let (t_3, equal_expr) = equal_helper t_2 in
        (t_3, Binop(NotEqual, rela_expr, equal_expr))
      |_ -> (t_1, rela_expr))

  (*Relational helper*)
  and relational_helper toks =
    let(t_1, add_expr) = add_helper toks in
    (match lookahead t_1 with
    |Some Tok_Less -> 
      let t_2 = match_token t_1 Tok_Less in
      let (t_3, rel_expr_1) = relational_helper t_2 in
      (t_3, Binop(Less, add_expr, rel_expr_1))
    |Some Tok_Greater ->
      let t_2 = match_token t_1 Tok_Greater in
      let (t_3, rel_expr_1) = relational_helper t_2 in
      (t_3, Binop(Greater, add_expr, rel_expr_1))
    |Some Tok_LessEqual ->
      let t_2 = match_token t_1 Tok_LessEqual in
      let (t_3, rel_expr_1) = relational_helper t_2 in
      (t_3, Binop(LessEqual, add_expr, rel_expr_1))
    |Some Tok_GreaterEqual ->
      let t_2 = match_token t_1 Tok_GreaterEqual in
      let (t_3, rel_expr_1) = relational_helper t_2 in
      (t_3, Binop(GreaterEqual, add_expr, rel_expr_1))
    |_ -> (t_1, add_expr))
  
  and add_helper toks =
    let(t_1, mult_expr) = mult_helper toks in
      (match lookahead t_1 with
      |Some Tok_Add ->
        let t_2 = match_token t_1 Tok_Add in
        let (t_3, add_expr) = add_helper t_2 in
        (t_3, Binop(Add, mult_expr, add_expr))
      |Some Tok_Sub ->
        let t_2 = match_token t_1 Tok_Sub in
        let (t_3, add_expr) = add_helper t_2 in
        (t_3, Binop(Sub, mult_expr, add_expr))
      |_ -> (t_1, mult_expr))


    (*mult helper*)
    and mult_helper toks =
      let(t_1, concat_expr) = concat_helper toks in
        (match lookahead t_1 with
        |Some Tok_Mult ->
          let t_2 = match_token t_1 Tok_Mult in
          let (t_3, mult_expr) = mult_helper t_2 in
          (t_3, Binop(Mult, concat_expr, mult_expr))
        |Some Tok_Div ->
          let t_2 = match_token  t_1 Tok_Div in
          let (t_3, mult_expr) = mult_helper t_2 in
          (t_3, Binop(Div, concat_expr, mult_expr))
        |_ -> (t_1, concat_expr))

  (*concat helper*)
  and concat_helper toks =
    let(t_1, unary_expr) = unary_helper toks in
    (match lookahead t_1 with
    |Some Tok_Concat ->
      let t_2 = match_token t_1 Tok_Concat in
      let(t_3, concat_expr) = concat_helper t_2 in
      (t_3, Binop(Concat, unary_expr, concat_expr))
    |_-> (t_1, unary_expr))

  (*unary_expr*)
  and unary_helper toks =
    (match lookahead toks with
    |Some Tok_Not ->
      let t_1 = match_token toks Tok_Not in
      let(t_2, unary_expr) = unary_helper t_1 in
      (t_2, Not(unary_expr))
    |_ -> function_call_helper toks)

   (*function expression*)
    and function_call_helper toks =
    let(t_1, func_call_1) = primary_helper toks in
    (match lookahead t_1 with
    |Some Tok_Int x ->
      let(t_2, func_call_2) = primary_helper t_1 in
      (t_2, FunctionCall(func_call_1, func_call_2))
    |Some Tok_Bool x ->
      let(t_2, func_call_2) = primary_helper t_1 in
      (t_2, FunctionCall(func_call_1, func_call_2))
    |Some Tok_String x ->
      let(t_2, func_call_2) = primary_helper t_1 in
      (t_2, FunctionCall(func_call_1, func_call_2))
    |Some Tok_ID x ->
      let(t_2, func_call_2) = primary_helper t_1 in
      (t_2, FunctionCall(func_call_1, func_call_2))
    |Some Tok_LParen ->
      let(t_2, func_call_2) = primary_helper t_1 in
      (t_2, FunctionCall(func_call_1, func_call_2))
    |_ -> (t_1, func_call_1))

  and primary_helper toks =
    (match lookahead toks with
    |Some Tok_Int(x) -> 
      let t_1 = match_token toks (Tok_Int x) in
      (t_1, Value(Int x))
    |Some Tok_Bool(x) ->
      let t_1 = match_token toks (Tok_Bool x) in
      (t_1, Value(Bool x))
    |Some Tok_String(x) ->
      (*let _ = print_endline "here string" in
      let _ = print_endline x in*)
      let t_1 = match_token toks (Tok_String x) in
      (t_1, Value(String x))
    |Some Tok_ID(x) ->
      let t_1 = match_token toks (Tok_ID x) in
      (t_1, (ID x))
    |Some Tok_LParen ->
      let t_1 = match_token toks Tok_LParen in
      let(t_2, expr) = parse_expr t_1 in
      let t_3 = match_token t_2 Tok_RParen in
      (t_3, expr)
    |_ -> raise (InvalidInputException("fail5???")))


(* Part 3: Parsing mutop *)
let rec parse_mutop toks = 
  (*let _ = print_endline "here 2" in*)
  match lookahead toks with
  |Some (Tok_Def) -> 
    let t_1 = match_token toks Tok_Def in
    (match lookahead t_1 with
    | Some (Tok_ID(temp_id)) ->
      let t_2 = match_token (t_1) (Tok_ID(temp_id)) in
      let t_3 = match_token t_2 Tok_Equal in
      let(t_4, expr_1) = parse_expr t_3 in
      let t_5 = match_token t_4 Tok_DoubleSemi in
      (t_5, Def(temp_id, expr_1))
    |_ -> raise (Failure "fail6"))
  |Some (Tok_DoubleSemi)-> 
    let t_1 = match_token toks Tok_DoubleSemi in
    (t_1, NoOp)
  |_ -> (*do Expr Mutop*)
    let (t_1, expr_1) = parse_expr toks in
    (*let _ = print_endline (string_of_list string_of_token t_1) in *)
    let t_2 = match_token t_1 Tok_DoubleSemi in
    (*let _ = print_endline "here 5" in*)
    (t_2, Expr(expr_1))

    
    