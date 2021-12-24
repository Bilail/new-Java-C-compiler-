(* Dependencies *)
%{
  Ast
%}

(** TOKENS **)

(* Complex tokens *)
%token <string> ID (* myIdentifier *)
%token <string> CLASSNAME (* MyClass *)
%token <int> CSTE (* 42 *)

(* Symbols *)
%token <Ast.opComp> RELOP (* = <> > < >= <= *)
%token PLUS MINUS TIMES DIV (*  + - * /  *)
%token LPAREN RPAREN (* ( ) *)
%token LBRACKET RBRACKET    (* { } *)
%token COMMA (* , *)
%token SEMICOLON (* ; *)
%token ASSIGN (* := *)
%token SELECTION (* . *)
%token EOF (* End of file symbol *)

(* Keywords *)
(* All written as in the code *)
%token IF THEN ELSE
%token VAR
%token NEW
%token THIS SUPER RESULT
%token CLASS
%token IS
%token DEF
%token STATIC
%token OVERRIDE
%token RETURN




(**
  ____________________________________________
/                    ------------°°°°------------                      \
|              GRAMMAIRE - GENERALITES
\ ___________________________________________ /
**)

(* Non-terminaux et leur type équivalent OCaml (voir Ast.ml) *)
(* 
Ajouter = Règles de grammaire à définir
Typer = Doit être associé à un type OCaml avec <string>, <Ast.classType>, etc... (je sais pas si c'est obligatoire)
Coder = On doit encore écrire le code OCaml qui définit ce qu'on renvoie entre { }
*)


%type <Ast.classType> class (* Coder *)
%type <string> extends (* Coder *)
%type classBody (* Typer *)
%type anyClassDecl (* Typer *)
%type factoredAttributes (* Typer *)
%type method (* Typer *)
%type constructor (* Typer *)
%type superclassCall (* Typer *)

%type factoredVarParamList (* Typer *)
%type factoredVarParam (* Typer *)
%type argumentsList (* Typer *)
%type returnedType (* Typer *)

%type block (* Typer *)
%type <Ast.instrType> instruction (* Coder *)
%type container (* Typer *)
%type containerA (* Typer *)
%type methodCall (* Typer *)

%type expression (* Typer *)
%type expr1 (* Typer *)
%type expr2 (* Typer *)
%type expr3 (* Typer *)
%type instanciation  (* Typer *)
%type castedExpr (* Typer *)




(* Axiome *)
%start <Ast.progType> prog (* Do we have progType ? *)
%%


(**
  ____________________________________________
/                    ------------°°°°------------                      \
|               GRAMMAIRE - ARBITRAGES
\ ___________________________________________ /
**)

(*

  [1] "var" et non-"var" assimilés
  Je pense que distinguer des non-terminaux factoredVarParam (arguments de constructeurs)
  pouvant optionnellement avoir le mot-clé "var" et nonfactoredVarParam (arguments de méthdes)
  ne pouvant avoir "var" amènerait des ambiguités. Le problème serait qu'une liste d'arguments
  sans "var" peut à la fois être un factoredVarParam et et nonfactoredVarParam. -Gaël

  [2] CLASSNAME et ID distingués
  Dans les spécifications du langage, il est indiqué que les noms de classe commencent par une
  majuscule et les autres identificateurs par une minuscule. Cela signifie qu'un nom de classe
  CLASSNAME peut être identifié dès l'analyse lexicale. Je choisis donc de le faire, ce qui simplifie
  grandement le travail des analyses lexicale et contextuelle. -Gaël


  [3] super et this peuvent être hors d'une méthode
  Il est simple dans cette grammaire de faire en sorte que this et super ne soient utilisables que
  au début d'un appel de méthode ou attribut. Il est cependant plus difficile de savoir s'ils sont
  dans une classe. Je choisis donc de reconnaître les mauvaises utilisations lorsque super et this
  ne sont pas au tout début d'une suite de sélections lors de l'analyse syntaxique, mais de laisser
  les mauvaises utilisations à l'extérieur d'une méthode à l'analyse contextuelle. 



*)




(**
  ____________________________________________
/                    ------------°°°°------------                      \
|         GRAMMAIRE - REGLES : CLASSES
\ ___________________________________________ /
**)

(* 1 programme = Des classes + un bloc de programme principal à la fin *)
prog: cl=list(class) il=block


(* Classe *)
(* Ex : class Point(var xc, yc : Integer, name:String) IS { **CorpsClasse** } *)
class: CLASS CLASSNAME factoredVarParamList option(extends) IS delimited(LBRACKET, classBody, RBRACKET)


(* Extends d'une classe, optionnel *)
extends : EXTENDS CLASSNAME

(* Corps de la classe *)
(* Ex : attributs, méthode, méthode, constructeur, méthode, attribut ... *)
(* Puisqu'on sait qu'il doit y avoir un constructeur par classe, on le cherche directement à l'analyse syntaxique *)
classBody : anyClDeclAndConstructor list(anyClassDecl)


(* Auxiliaire de la règle précédente pour éviter un conflit shift-reduce si on écrivait : *)
(* classBody : list(anyClassDecl) constructor list(anyClassDecl)  *)
anyClDeclAndConstructor :
  anyClassDecl anyClDeclAndConstructor
| constructor



(* Une déclaration quelconque dans une classe : méthode ou attributs *)
(* On sépare les méthodes en constructeur, méthode, et méthodeOuConstructeur, car il y a ambiguité lors de l'analyse syntaxique *)
anyClassDecl: factoredAttributes | method


(* Attributs de classe *)
(* Ex : var static x1, x2 : Integer *)
factoredAttributes: VAR boption(STATIC) list(ID) COLON returnedType


(* Méthode de classe *)
method:
  (* cas finissant par un bloc *)
  DEF boption(STATIC) boption(OVERRIDE) ID factoredVarParamList option(returnedType) IS block

  (* cas finissant par "nomClasse := expression" *)
| DEF boption(STATIC) boption(OVERRIDE) ID factoredVarParamList returnedType ASSIGN expression



(* Constructeur de classe *)
constructor:
  DEF CLASSNAME factoredVarParamList option(superclassCall) IS block



superclassCall: COLON CLASSNAME argumentsList




(**
  ____________________________________________
/                    ------------°°°°------------                      \
|  GRAMMAIRE - REGLES : PARAMETRES & ARGS
\ ___________________________________________ /
**)




(* Liste de paramètres optionnellement VAR entouré de parenthèses ( ) *)
factoredVarParamList: delimited(LPAREN, separated_list(COMMA, factoredVarParam), RPAREN)


(* Paramètre ou paramètres groupés optionnellement VAR *)
(* Ex: var x1, x2, x3 : Integer *)
factoredVarParam: boption(VAR) separated_list(COMMA, ID) returnedType


(* Liste d'arguments, c'est-à-dire les expressions qu'on met comme paramètres lorsqu'on fait un appel (à une méthode par exemple) *)
(* Ex:    ( 3, z, Point3D.getHeight() )     *)
argumentsList: delimited(LPAREN, separated_list(COMMA, expression), RPAREN)


(* Ex:   : Point3D *)
returnedType: COLON CLASSNAME




(**
  ____________________________________________
/                    ------------°°°°------------                      \
|      GRAMMAIRE - REGLES : INSTRUCTIONS
\ ___________________________________________ /
**)


(* Bloc d'instructions entouré d'accolades *)
block: delimited(LBRACKET, list(instruction), RBRACKET)



(* N'importe quelle instruction du programme principal (sauf RETURN) ou des méthodes *)
instruction:
  expression SEMICOLON
| block
| RETURN SEMICOLON
| IF expression THEN instruction ELSE instruction
| container ASSIGN expression SEMICOLON



(* Variable ou attribut, n'importe quoi pouvant contenir une valeur *)
(* Ex:     x   ou    Point2D.multiply(3*y).length    *)
container:
  CLASSNAME SELECTION containerA
| THIS SELECTION containerA
| SUPER SELECTION containerA
| containerA

containerA:
  ID
| ID SELECTION containerA
| ID argumentsList SELECTION containerA


(* Appel de méthode *)
(* A peu près comme un container, mais avec des arguments à la fin et au moins 1 sélection "." *)
(* Ex:   text.getSize()     ou    Point2D.multiply(3*y).substract(myPoint)    *)
methodCall:
  CLASSNAME SELECTION containerA argumentsList
| ID SELECTION containerA argumentsList
| SUPER SELECTION containerA argumentsList
| THIS SELECTION containerA argumentsList







(**
  ____________________________________________
/                    ------------°°°°------------                      \
|      GRAMMAIRE - REGLES : EXPRESSIONS
\ ___________________________________________ /
**)


expression:
  expr1 RELOP expr1
| expr1

expr1:
  expr1 PLUS expr2
| expr1 MINUS expr2
| expr2

expr2:
  expr2 TIMES expr3
| expr2 DIV expr3
| expr3

expr3:
  CSTE
| PLUS e=expr3  { e }
| MINUS e=expr3 %prec UMINUS   { UMinus e }
| container
| methodCall
| instanciation
| delimited(LPAREN, expression, RPAREN)
| castedExpr


instanciation: NEW CLASSNAME argumentsList


castedExpr: delimited(LPAREN, CLASSNAME expression, RPAREN) 




(** Ajouter commentaires : Fait dans l'analyse lexicale ? **)
