//
// Parser for simple C programs.  This component checks 
// the input program to see if it meets the syntax rules
// of simple C.  The parser returns a string denoting
// success or failure. 
//
// Returns: the string "success" if the input program is
// legal, otherwise the string "syntax_error: ..." is
// returned denoting an invalid simple C program.
//
// <<Ahmed Shah>>
//


namespace compiler

module parser =
  //
  // NOTE: all functions in the module must be indented.
  //

  //
  // matchToken
  //
  let private matchToken expected_token (tokens: string list) =
    //
    // if the next token matches the expected token,  
    // keep parsing by returning the rest of the tokens.
    // Otherwise throw an exception because there's a 
    // syntax error, effectively stopping compilation
    // at the first error.
    //
    let next_token = List.head tokens 
    if expected_token = next_token then
      List.tail tokens
    elif ((next_token:string).StartsWith(expected_token)) then
      List.tail tokens
    else
      failwith ("expecting " + expected_token + ", but found " + next_token)
      
    
/////////////
  let private operatorCheck next_Token = 
    if (next_Token = "+" || next_Token = "-" || next_Token = "*" || next_Token =  "/" || next_Token = "^" || next_Token = "<" || next_Token = "<=" || next_Token = ">" || next_Token = ">=" || next_Token = "==" || next_Token = "!=") then 
      true 
    else 
      false 

  
  let rec private stmt tokens = 
    //printfn "stmt %A" tokens
    let nextToken = List.head tokens
    match nextToken with 
    |";" -> empty tokens
    |"int" -> varDecl tokens 
    |"cin" -> input tokens 
    |"cout" -> output tokens
    |asgnStr when asgnStr.StartsWith("identifier") -> assignment tokens 
    |"if" -> ifstmt tokens
    | _ -> failwith ("expecting statement, but found " + nextToken)
    
    


 
  and private then_part tokens =
    //printfn "then %A" tokens
    let T2 = stmt tokens 
    T2


 
  and private else_part tokens =
    //printfn "else %A" tokens
    let next_token = List.head tokens
    if next_token = "else" then
      let T2 = matchToken "else" tokens // match and discard “else”
      stmt T2 // parse the stmt with remaining tokens
    else
      tokens // EMPTY is legal, so do nothing and return tokens unchanged
    // end of else 

 
  and private ifstmt tokens =
    //let next_token = List.head tokens 
    //if next_token = "if" then 
    //printfn "ifstmt %A" tokens
    let T2  = matchToken "if" tokens // match and discard "if"
    let T3  = matchToken "(" T2 
    let T4  = functCond T3
    let T5 = matchToken ")" T4
    let T6  = then_part T5
    let T7  = else_part T6 
    T7
   
  and private functCond tokens = 
    //printfn "condition %A" tokens
    let T2  = expr tokens 
    T2

  
  and private expr tokens =  
    //printfn "expr %A" tokens
    
    let T2  = exprValue tokens
    let next_Token = List.head T2 
    if (operatorCheck next_Token) then 
        let T3 = exprOp T2 
        let T4 = exprValue T3
        T4
    else 
        T2
    
    
   
  and private exprValue tokens = 
    //printfn "exprValue %A" tokens
    let nextToken =  List.head tokens 
    if nextToken = "true" then 
      matchToken "true" tokens 
    elif nextToken = "false" then 
      matchToken "false" tokens
    elif (nextToken.StartsWith("identifier")) then
      matchToken "identifier" tokens 
    elif (nextToken.StartsWith("str_literal")) then 
      matchToken "str_literal" tokens 
    elif (nextToken.StartsWith("int_literal")) then 
      matchToken "int_literal" tokens 
    else 
      failwith ("expecting identifier or literal, but found " + nextToken)

    
   

  and private exprOp tokens = 
    //printfn "exprOp %A" tokens
    let next_Token = List.head tokens
    if (operatorCheck next_Token) then 
      matchToken next_Token tokens
    else
      failwith ("expecting expression operator, but found " + next_Token)


   

  and private empty tokens = 
    let nextToken = List.head tokens 
    let T2 = matchToken ";" tokens 
    T2
    
  and private varDecl tokens = 
    //let next_token = List.head tokens 
    //printfn "varDecl %A" tokens
    let T2 = matchToken "int" tokens 
    let T3 = matchToken "identifier" T2
    let T4 = matchToken ";" T3
    T4

  and private input tokens = 
    //let next_token = List.head tokens 
    let T2 = matchToken "cin" tokens
    let T3 = matchToken ">>" T2 
    let T4 = matchToken "identifier" T3
    let T5 = matchToken ";" T4 
    T5

  and private output tokens = 
    let T2 = matchToken "cout" tokens
    let T3 = matchToken "<<" T2 
    let nextToken = List.head T3
    if nextToken = "endl" then 
      let T4 = matchToken "endl" T3 
      let T5 = matchToken ";" T4 
      T5 
    else 
      let T7 = exprValue T3
      let T8 = matchToken ";" T7 
      T8

  and private assignment tokens = 
    //printfn "assignment %A" tokens
    let T2 = matchToken "identifier" tokens
    let T3 = matchToken "=" T2
    let T4 = expr T3
    let T5 = matchToken ";" T4
    T5
    
 
  

  //
  // stmts
  //
  let rec private morestmts (tokens:string List) = 
    let next_token = List.head tokens 
    if next_token <> "}" then  
      let T1 = stmt tokens 
      let T2 = morestmts T1
      T2
    else 
      tokens

  
  let private stmts tokens = 
    //printfn "stmts %A" tokens
    let T1 = stmt tokens 
    morestmts T1
    

  //
  // simpleC
  //
  let private simpleC tokens = 
    let T2 = matchToken "void" tokens
    let T3 = matchToken "main" T2
    let T4 = matchToken "(" T3
    let T5 = matchToken ")" T4
    let T6 = matchToken "{" T5
    let T7 = stmts T6
    let T8 = matchToken "}" T7
    let T9 = matchToken "$" T8 // $ => EOF, there should be no more tokens
    T9


  //
  // parse tokens
  //
  // Given a list of tokens, parses the list and determines
  // if the list represents a valid simple C program.  Returns
  // the string "success" if valid, otherwise returns a 
  // string of the form "syntax_error:...".
  //
  let parse tokens = 
    try
      let result = simpleC tokens
      "success"
    with 
      | ex -> "syntax_error: " + ex.Message
