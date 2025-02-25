//
// F# program to compile simple C programs. This is the main
// function denoting the compiler, calling the lexer, 
// parser, etc.
//
// <<Ahmed Shah>>
//


//##################################################################
//
// main
//
// Compiles the simple C program; the filename is input by the user.
//
[<EntryPoint>]
let main argv =
  //
  printf "simpleC filename> "
  let filename = System.Console.ReadLine()
  printfn ""
  //
  if not (System.IO.File.Exists(filename)) then
    printfn "**Error: file '%s' does not exist." filename
    0
  else
    printfn "compiling %s..." filename
    //
    // Run the lexer to get the tokens, and then
    // pass these tokens to the parser to see if
    // the input program is legal:
    //
    let tokens = compiler.lexer.analyze filename
    //
    printfn ""
    printfn "%A" tokens
    printfn ""
    //
    let result = compiler.parser.parse tokens
    printfn "%s" result
    printfn ""
    //
    0
