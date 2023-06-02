open MicroCamlTypes
open Utils

exception TypeError of string
exception DeclareError of string
exception DivByZeroError 

(* Provided functions
  Helpful for creating an environment. You can do this with references 
  (not taught) or without. 
  You do not have to use these and you can modify them if you want. 
  If you do not use references, you will need the following data type:
*)
(*type values = Int of int|Bool of bool|String of string*)

(* Adds mapping [x:v] to environment [env] *)
let ref_extend env x v = (x, ref v)::env

let extend env x v = (x,v)::env

(* Returns [v] if [x:v] is a mapping in [env]; uses the
   most recent if multiple mappings for [x] are present *)
let rec ref_lookup env x =
  match env with
  | [] -> raise (DeclareError ("Unbound variable " ^ x))
  | (var, value)::t -> if x = var then !value else ref_lookup t x

let rec lookup env x = 
  match env with
  [] -> raise (DeclareError ("Unbound variable " ^ x))
  | (var, value)::t -> if x = var then value else lookup t x

(* Creates a placeholder mapping for [x] in [env]; needed
   for handling recursive definitions *)
let ref_extend_tmp env x = (x, ref (Int 0))::env

(* Updates the (most recent) mapping in [env] for [x] to [v] *)
let rec ref_update env x v =
  match env with
  | [] -> raise (DeclareError ("Unbound variable " ^ x))
  | (var, value)::t -> if x = var then (value := v) else ref_update t x v
        
(* Removes the most recent variable,value binding from the environment *)
let rec remove env x = match env with
  [] -> []
  | (var,value)::t -> if x = var then t else (var,value)::(remove t x)

(* Part 1: Evaluating expressions *)

(* Evaluates MicroCaml expression [e] in environment [env],
   returning a value, or throwing an exception on error *)


let rec eval_expr env e = 
  (match e with
  |Value(x) -> x
  |Fun(x, ex1) -> Closure(env, x, ex1)
  |ID(x) -> ref_lookup env x 
  |Not(ex1) -> not_unary_helper env ex1 
  |If(ex1, ex2, ex3) -> if_helper env ex1 ex2 ex3
  |FunctionCall(ex1, ex2) -> function_call_helper env ex1 ex2 
  |Let(str, bool1, ex1, ex2) -> let_helper env str bool1 ex1 ex2
  (*Look later*)
  |Binop(Add, ex1, ex2) -> 
    add_helper env ex1 ex2
  |Binop(Sub, ex1, ex2) -> 
    sub_helper env ex1 ex2
  |Binop(Mult, ex1, ex2) -> 
    mult_helper env ex1 ex2
  |Binop(Div, ex1, ex2) -> 
    div_helper env ex1 ex2
  |Binop(Concat, ex1, ex2) -> 
    concat_helper env ex1 ex2
  |Binop(Greater, ex1, ex2) -> 
    greater_helper env ex1 ex2
  |Binop(Less, ex1, ex2) -> 
    less_helper env ex1 ex2
  |Binop(GreaterEqual, ex1, ex2) -> 
    greater_equal_helper env ex1 ex2
  |Binop(LessEqual, ex1, ex2) -> 
    less_equal_helper env ex1 ex2
  |Binop(Equal, ex1, ex2) -> 
    equal_helper env ex1 ex2
  |Binop(NotEqual, ex1, ex2) -> 
    not_equal_helper env ex1 ex2
  |Binop(Or, ex1, ex2) -> 
    or_helper env ex1 ex2
  |Binop(And, ex1, ex2) -> 
    and_helper env ex1 ex2)

  and let_helper envr str bool_1 ex1 ex2 = 
  (*if the Rec is true*)
  if bool_1 then
    let rec_envr = (ref_extend_tmp envr str) in
    let v_1 = eval_expr rec_envr ex1 in
    let _ = ref_update rec_envr str v_1 in
    (*let rec_envr = (ref_extend rec_envr str) in*)
    eval_expr rec_envr ex2
  else
    let v_1 = eval_expr envr ex1 in
    (*Potential Error???/*)
    let extended_evr = ref_extend envr str v_1 in
    eval_expr extended_evr ex2

  and function_call_helper env p1 p2 =
    let value_1 = eval_expr env p1 in
    let value_2 = eval_expr env p2 in
    (match value_1, value_2 with
    |Closure(env_1, str_1, expr_1), value_2 ->
      eval_expr (ref_extend env_1 str_1 value_2 ) expr_1
    |_, _-> raise(TypeError "not closure"))

  (*If(ex1, ex2, ex3) -> if_helper ex1 ex2 ex3*)
  and if_helper envr p1 p2 p3 =
    let bool_1 = eval_expr envr p1 in
    (match bool_1 with
    |Bool(true) ->
      eval_expr envr p2
    |Bool(false) ->
      eval_expr envr p3
    |_ -> raise(TypeError "neither true or false"))

  (*Look later*)
  and not_unary_helper envr expr =
    let bool_1 = eval_expr envr expr in
    (match bool_1 with 
    |Bool(x) -> Bool(not x)
    |_-> raise (TypeError "lmao"))
  and add_helper envr p1 p2 =
    let num_1 = eval_expr envr p1 in
    let num_2 = eval_expr envr p2 in
    (match num_1, num_2 with
    |Int(x), Int(y) -> Int(x+y)
    |_, _ -> raise (TypeError "add: not x and y not type Int"))

  and sub_helper envr p1 p2 =
    let num_1 = eval_expr envr p1 in
    let num_2 = eval_expr envr p2 in
    (match num_1, num_2 with
    |Int(x), Int(y) -> Int(x-y)
    |_, _ -> raise (TypeError "sub: not x and y not type Int"))

  and mult_helper envr p1 p2 =
    let num_1 = eval_expr envr p1 in
    let num_2 = eval_expr envr p2 in
    (match num_1, num_2 with
    |Int(x), Int(y) -> Int(x*y)
    |_, _ -> raise (TypeError "mult: not x and y not type Int"))
  
  and div_helper envr p1 p2 =
    let num_1 = eval_expr envr p1 in
    let num_2 = eval_expr envr p2 in
    (match num_1, num_2 with
    |Int(x), Int(y) -> 
      if y = 0 then raise (DivByZeroError)
      else Int(x/y)
    |_, _ -> raise (TypeError "div: not x and y not type Int"))

  and concat_helper envr p1 p2 =
    let str_1 = eval_expr envr p1 in
    let str_2 = eval_expr envr p2 in
    (match str_1, str_2 with
    |String(x), String(y) -> String(x ^ y)
    |_, _ -> raise (TypeError "concat x and y not type String"))
  
  and greater_helper envr p1 p2 =
    let num_1 = eval_expr envr p1 in
    let num_2 = eval_expr envr p2 in
    (match num_1, num_2 with 
    |Int(x), Int(y) -> Bool(x > y)
    |_, _ -> raise (TypeError "greater x and y not type Int"))

  and less_helper envr p1 p2 =
    let num_1 = eval_expr envr p1 in
    let num_2 = eval_expr envr p2 in
    (match num_1, num_2 with 
    |Int(x), Int(y) -> Bool(x < y)
    |_, _ -> raise (TypeError "less x and y not type Int"))

  and greater_equal_helper envr p1 p2 =
    let num_1 = eval_expr envr p1 in
    let num_2 = eval_expr envr p2 in
    (match num_1, num_2 with 
    |Int(x), Int(y) -> Bool(x >= y)
    |_, _ -> raise (TypeError "greater equal x and y not type Int"))

  and less_equal_helper envr p1 p2 =
    let num_1 = eval_expr envr p1 in
    let num_2 = eval_expr envr p2 in
    (match num_1, num_2 with 
    |Int(x), Int(y) -> Bool(x <= y)
    |_, _ -> raise (TypeError "less equal x and y not type Int"))

  and equal_helper envr p1 p2 =
    let num_1 = eval_expr envr p1 in
    let num_2 = eval_expr envr p2 in
    (match num_1, num_2 with 
    |Int(x), Int(y) -> Bool(x = y)
    |_, _ -> raise (TypeError "equal x and y not type Int"))

  and not_equal_helper envr p1 p2 =
    let num_1 = eval_expr envr p1 in
    let num_2 = eval_expr envr p2 in
    (match num_1, num_2 with 
    |Int(x), Int(y) -> Bool(x <> y)
    |_, _ -> raise (TypeError "not equal x and y not type Int"))

  and or_helper envr p1 p2 =
    let bool_1 = eval_expr envr p1 in
    let bool_2 = eval_expr envr p2 in
    (match bool_1, bool_2 with 
    |Bool(x), Bool(y) -> Bool(x || y)
    |_, _ -> raise (TypeError "equal not x and y not type Bool"))

  and and_helper envr p1 p2 =
    let bool_1 = eval_expr envr p1 in
    let bool_2 = eval_expr envr p2 in
    (match bool_1, bool_2 with 
    |Bool(x), Bool(y) -> Bool(x && y)
    |_, _ -> raise (TypeError "equal x and y not type Bool"))





(* Part 2: Evaluating mutop directive *)

(* Evaluates MicroCaml mutop directive [m] in environment [env],
   returning a possibly updated environment paired with
   a value option; throws an exception on error *)
let eval_mutop env m = 
  match m with
  |Def(str, expr) -> 
    let rec_envr = ref_extend_tmp env str in
    let v_1 = eval_expr rec_envr expr in
    let _ = ref_update rec_envr str v_1 in
    (rec_envr,Some v_1)
  |Expr(expr) -> (env, Some(eval_expr env expr))
  |NoOp -> (env, None)
