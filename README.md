# Regarding Assignment

This assignment deals with converting the AST generated in the previous assignment to a postfix stack (control stack) and then executing the program using a VMC machine.

# About the Files

1. while.lex -> Contains the description for Lexer
2. while.yacc -> Contains the grammar and semantic rules for Parser
3. ast_dt.sml -> Contains the datatype for the Abstract Syntax Tree using a structure AST. It also contains various other functions for type-checking and symbol table management. It also contains the datatype defined for the Stacks and the various functions required for converting the AST to postfix notation.
4. stack.sml -> This contains the signature and structure of the polymorphic Stack 
5. mem.sml -> This contains the global memory array as well as the VMC signature and the structure. Various other helper functions are also present which are used for the rules function
6. while_LexParse.sml -> This file contains glue code for joining the Lexer and Parser. It also contains the main parsing function ParseProgram filename which generates the AST. This now also contains a function postFix which takes input a filename and outputs a stack. It also has an Execute function which takes as input the filename and then first computes its AST, then its postfix notation and finally uses the execute function in mem.sml file to evaluate the program. It also produces the stage wise configuration of the stacks and the memory.

7. while_ast.sml -> This file is used for loading all the .lex.sml, .yacc.sml, .yacc.sig etc. in the sequence required.
   There are also some test files present in the tests folder

# Running the Program

In the command prompt run the following commands sequentially

1. `ml-lex while.lex`
2. `ml-yacc while.yacc`
3. `sml while_ast.sml`
   The 3rd command will open up the sml terminal. 
   After this you can run the following functions.
   `ParseProgram(filename)` ->  generates AST
   `postFix(filename)` -> generates the postfix stack of the AST constructed by the ParseProgram function
   `Execute(filename)` -> evaluates the program using the VMC machine defined using the rules function. Also prints the configuration at each step
   filename is the name of the file(essentially a string) which contains any test program written in the WHILE Language Syntax.
   The `postfix` function has been defined the ast_dt.sml file and the `execute` function has been defined the mem.sml file


# Auxiliary functions and Data
1. `PostFixExp` -> converts expressions into postfix
2. `PostFixCmd` -> converts the commands into their postfix form
3. `PostFixSeq` -> converts a sequence of commands into their postfix form
4. `stElems2Str`-> used for converting different types of elements present in a stack into their string format
5. `arr2Str`-> produces the string expression for the memory represented through an array
6. `addvar` -> used for adding the variable name its address location into a hash table
7. `getVarAdd` -> used for retrieving the address of the given variable from the hash table
8. `PushElem` -> This is used for pushing another stack contents into the original stack
9. `helperExecute` -> This has been used for starting the execution process, this stops when the control stack becomes empty. It also prints out the configuration of the VMC machine in each step


# Other Design Decisions

1. I have used a somewhat recursive datatype for defining the stack datatype. This has been taken into consideration the arbitrary lookahead issue which arises if the constructors are not used. These provide ease of execution during the computation process.

2. The PostFix functions defined are quite similar in their way to my typecheck functions. This is because of the fact that we are traversing the AST in both the cases.

# Acknowledgements

All the decisions and code written in the assignment have been done by myself. Apart from the two resources which I had mentioned in the previous assignment, I havent used any such resource.