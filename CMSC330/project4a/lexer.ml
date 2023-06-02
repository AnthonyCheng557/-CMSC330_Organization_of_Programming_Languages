open TokenTypes

(* Part 1: Lexer - IMPLEMENT YOUR CODE BELOW *)

(*Define custom exceptions*)
exception IllegalExpression of string

let tokenize input = 
  let rec tokenize_helper input pos =
    (*Double Semi Ending Case*)
    if String.length input <= pos then []

    else if String.sub input pos 1 = " " then tokenize_helper input (pos+1)

    else if Str.string_match (Str.regexp "\\bthen\\b") input pos then 
      Tok_Then::(tokenize_helper input (pos + 4)) 

    else if Str.string_match (Str.regexp "\\belse\\b") input pos then 
      Tok_Else::(tokenize_helper input (pos + 4)) 

    else if Str.string_match (Str.regexp "\\bnot\\b") input pos then
      Tok_Not::(tokenize_helper input (pos + 3))
      
    else if Str.string_match (Str.regexp "\\brec\\b") input pos then
      Tok_Rec::(tokenize_helper input (pos + 3))

    else if Str.string_match (Str.regexp "\\bdef\\b") input pos then
      Tok_Def::(tokenize_helper input (pos + 3))

    else if Str.string_match (Str.regexp "\\bfun\\b") input pos then
      Tok_Fun::(tokenize_helper input (pos + 3))

    else if Str.string_match (Str.regexp "\\blet\\b") input pos then
      Tok_Let::(tokenize_helper input (pos + 3))

    else if Str.string_match (Str.regexp "->") input pos then
      Tok_Arrow::(tokenize_helper input (pos + 2))
      
    else if Str.string_match (Str.regexp "<>") input pos then
      Tok_NotEqual::(tokenize_helper input (pos + 2))  

    else if Str.string_match (Str.regexp ">=") input pos then
      Tok_GreaterEqual::(tokenize_helper input (pos + 2))  

    else if Str.string_match (Str.regexp "<=") input pos then
      Tok_LessEqual::(tokenize_helper input (pos + 2)) 
      
    else if Str.string_match (Str.regexp "||") input pos then
      Tok_Or::(tokenize_helper input (pos + 2)) 
      
    else if Str.string_match (Str.regexp "&&") input pos then
      Tok_And::(tokenize_helper input (pos + 2)) 

    else if Str.string_match (Str.regexp "\\bif\\b") input pos then 
      Tok_If::(tokenize_helper input (pos + 2)) 

    else if Str.string_match (Str.regexp "\\bin\\b") input pos then
      Tok_In::(tokenize_helper input (pos + 2)) 

    else if Str.string_match (Str.regexp "=") input pos then
      Tok_Equal::(tokenize_helper input (pos + 1))

    else if Str.string_match (Str.regexp "+") input pos then
      Tok_Add::(tokenize_helper input (pos + 1)) 

    else if Str.string_match (Str.regexp "-") input pos then
      Tok_Sub::(tokenize_helper input (pos + 1)) 

    else if Str.string_match (Str.regexp "*") input pos then
      Tok_Mult::(tokenize_helper input (pos + 1)) 

    else if Str.string_match (Str.regexp "/") input pos then
      Tok_Div::(tokenize_helper input (pos + 1)) 

    else if Str.string_match (Str.regexp "\\^") input pos then
      Tok_Concat::(tokenize_helper input (pos + 1)) 

    else if Str.string_match (Str.regexp ">") input pos then
      Tok_Greater::(tokenize_helper input (pos + 1)) 

    else if Str.string_match (Str.regexp "<") input pos then
      Tok_Less::(tokenize_helper input (pos + 1))

    else if Str.string_match (Str.regexp "true\\|false") input pos then
      if Str.string_match(Str.regexp "true") input pos 
        then
        (*True*)
          Tok_Bool true ::(tokenize_helper input (pos + 4))
        else
        (*False*)
          Tok_Bool false ::(tokenize_helper input (pos + 5))

    else if Str.string_match (Str.regexp "[0-9]+\\|\\((-[0-9]+\\))") input pos then
      let temp = (Str.matched_string input) in
      let edit =
        if String.sub temp 0 1  = "(" then
          String.sub temp (1) 2
        else
          temp
      in
      Tok_Int (int_of_string edit) ::(tokenize_helper input (pos + (String.length temp)))

    else if Str.string_match (Str.regexp ")") input pos then
      Tok_RParen::(tokenize_helper input (pos + 1))

    else if Str.string_match (Str.regexp "(") input pos then
      Tok_LParen::(tokenize_helper input (pos + 1))
    
    else if Str.string_match (Str.regexp "\"[^\"]*\"") input pos then
      let temp = Str.matched_string input in
      let temp2 = String.sub temp 1 ((String.length temp)-2) in
      Tok_String temp2 ::(tokenize_helper input (pos + (String.length temp)))

    else if Str.string_match (Str.regexp "[a-zA-Z][a-zA-Z0-9]*") input pos then
      let temp = Str.matched_string input in
      Tok_ID temp ::(tokenize_helper input (pos + (String.length temp))) 

    else if Str.string_match (Str.regexp ";;") input pos then
      Tok_DoubleSemi::(tokenize_helper input (pos + 2))

    else raise (IllegalExpression ("welp-tokenise: " ^ input))
    in
    tokenize_helper input 0
    

          


