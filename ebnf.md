# Pascal LSP EBNF Grammar Specification

This document provides the EBNF grammar specification for Pascal adjusted to match the AST node token classes implemented in `pascal-lsp`. Non-terminals with a dedicated AST node class start with an uppercase letter (PascalCase), while non-terminals without a dedicated class start with a lowercase letter (camelCase).

---

## EBNF Grammar

```ebnf
(* ==================================================================== *)
(*           PASCAL-LSP EBNF SPECIFICATION & AST CLASS MAPPING          *)
(* ==================================================================== *)

(* --- 1. Program & Unit Structure --- *)

ProgramFile = "program" , Identifier , [ "(" , identifierList , ")" ] , ";" , Block , "." ;
UnitFile    = "unit" , qualifiedUnitIdentifier , ";" , InterfaceBlock , ImplementationBlock , "." ;

qualifiedUnitIdentifier = Identifier , { "." , Identifier } ;

InterfaceBlock = "interface" , [ UsesClause ] , { interfaceDeclaration } ;
ImplementationBlock = "implementation" , [ UsesClause ] , { blockDeclaration } , [ initializationSection ] , "end" ;

initializationSection = ( "begin" , statementList )
                      | ( "initialization" , statementList , [ "finalization" , statementList ] ) ;

UsesClause = "uses" , identifierList , ";" ;

interfaceDeclaration = TypeSection
                     | ConstSection
                     | FunctionDecl , ";" ;

blockDeclaration = ConstSection
                 | TypeSection
                 | VarSection
                 | FunctionImpl ;

Block = { blockDeclaration } , CompoundStatement ;


(* --- 2. Type & Constant Definitions --- *)

ConstSection = "const" , { ConstDecl } ;
ConstDecl    = Identifier , [ ":" , TypeSpec ] , "=" , ConstValue , ";" ;

ConstValue   = Expression ;

TypeSection = "type" , { TypeDecl } ;
TypeDecl    = Identifier , "=" , TypeSpec , ";" ;

TypeSpec = simpleType
         | PointerSpec
         | ArraySpec
         | RecordSpec
         | SetSpec
         | FileSpec
         | EnumSpec
         | RangeSpec
         | ClassSpec
         | qualifiedIdentifier ;

simpleType = "Integer" | "Real" | "Boolean" | "Char" | "String" | Identifier ;

PointerSpec = "^" , TypeSpec ;

ArraySpec = [ "packed" ] , "array" , [ "[" , indexType , { "," , indexType } , "]" ] , "of" , TypeSpec ;
indexType = RangeSpec | qualifiedIdentifier ;

RecordSpec = [ "packed" ] , "record" , fieldList , "end" ;

fieldList = [ VarDecl , { ";" , VarDecl } , [ ";" ] ] ;

SetSpec = [ "packed" ] , "set" , "of" , TypeSpec ;

FileSpec = "file" , [ "of" , TypeSpec ] ;

EnumSpec = "(" , identifierList , ")" ;
RangeSpec = ConstValue , ".." , ConstValue ;

ClassSpec = "class" , [ "(" , qualifiedIdentifier , ")" ] , { classMember } , "end" ;


(* --- 3. Variables & Routines --- *)

VarSection = "var" , { VarDecl , ";" } ;
VarDecl    = identifierList , ":" , TypeSpec , [ "=" , ConstValue ] ;

FunctionDecl = ( "procedure" | "function" ) , Identifier , [ ParameterDecl ] , [ ":" , TypeSpec ] ;

ParameterDecl = "(" , parameterGroup , { ";" , parameterGroup } , ")" ;
parameterGroup = [ "var" | "const" | "out" | "constref" ] , identifierList , ":" , TypeSpec ;

FunctionImpl = FunctionDecl , ";" , routineBody , ";" ;
routineBody  = Block | "forward" ;


(* --- 4. Statements --- *)

statementList = Statement , { ";" , Statement } ;

Statement = [ unlabelledStatement ] ;

unlabelledStatement = CompoundStatement
                    | AssignmentStatement
                    | Call
                    | IfStatement
                    | CaseStatement
                    | WhileStatement
                    | RepeatStatement
                    | ForStatement
                    | WithStatement
                    | TryStatement ;

CompoundStatement = "begin" , statementList , "end" ;

AssignmentStatement = Designator , ":=" , Expression ;
Call                = Designator , "(" , [ expressionList ] , ")" ;

IfStatement = "if" , Expression , "then" , Statement , [ "else" , Statement ] ;

CaseStatement = "case" , Expression , "of" ,
                CaseBranch , { ";" , CaseBranch } , [ ";" ] ,
                [ "else" , statementList , [ ";" ] ] ,
                "end" ;
CaseBranch = caseLabelList , ":" , Statement ;
caseLabelList = ConstValue , [ ".." , ConstValue ] , { "," , ConstValue , [ ".." , ConstValue ] } ;

WhileStatement = "while" , Expression , "do" , Statement ;
RepeatStatement = "repeat" , statementList , "until" , Expression ;

ForStatement = "for" , Identifier , ":=" , Expression , ( "to" | "downto" ) , Expression , "do" , Statement ;
WithStatement = "with" , designatorList , "do" , Statement ;
TryStatement  = "try" , statementList , ( "except" | "finally" ) , statementList , "end" ;


(* --- 5. Expressions & Designators --- *)

Expression = SimpleExpression , [ relationalOperator , SimpleExpression ] ;

SimpleExpression = [ "+" | "-" ] , Term , { additiveOperator , Term } ;

Term = Factor , { multiplicativeOperator , Factor } ;

Factor = Designator
       | Number
       | StringToken
       | SetConstructor
       | "(" , Expression , ")"
       | "not" , Factor
       | Call ;

SetConstructor = "[" , [ setElement , { "," , setElement } ] , "]" ;
setElement     = Expression , [ ".." , Expression ] ;

Designator = Identifier , { selector } ;

selector = "[" , expressionList , "]"
         | "." , Identifier
         | "^" ;


(* --- 6. Operators & Primitives --- *)

relationalOperator = "=" | "<>" | "<" | "<=" | ">" | ">=" | "in" ;
additiveOperator   = "+" | "-" | "or" | "xor" ;
multiplicativeOperator = "*" | "/" | "div" | "mod" | "and" | "shl" | "shr" ;

expressionList = Expression , { "," , Expression } ;
identifierList = Identifier , { "," , Identifier } ;
designatorList = Designator , { "," , Designator } ;

Identifier = letter , { letter | digit | "_" } ;
Number     = integer_literal | real_literal ;
StringToken = string_literal ;
```
