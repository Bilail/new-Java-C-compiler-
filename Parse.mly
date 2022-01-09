(* Dependencies *)
%{
  open Ast
%}

(** TOKENS **)

(* Complex tokens *)
%token <string> ID (* myIdentifier *)
%token <string> CLASSNAME (* Myclasse *)
%token <int> CSTE (* 42 *)

(* Symbols *)
%token <Ast.opComp> RELOP (* = <> > < >= <= *)   
%token PLUS MINUS TIMES DIV (*  + - * /  *)
%token LPAREN RPAREN (* ( ) *)
%token LBRACKET RBRACKET    (* { } *)
%token COMMA (* , *)
%token SEMICOLON (* ; *)
%token COLON  (* , *)
%token ASSIGN (* := *)
%token SELECTION (* . *)
%token EOF (* End of file symbol *)

(* Keywords *)
(* All written as in the code *)
%token IF THEN ELSE
%token VAR
%token NEW
%token THIS SUPER RESULT
%token CLASSE
%token IS
%token DEF
%token STATIC
%token OVERRIDE
%token RETURN
%token EXTENDS



(* Non-terminaux et leur type équivalent OCaml (voir Ast.ml) *)
(* 
Ajouter = Règles de grammaire à définir
Typer = Doit être associé à un type OCaml avec <string>, <Ast.classeType>, etc... (je sais pas si c'est obligatoire)
Coder = On doit encore écrire le code OCaml qui définit ce qu'on renvoie entre { }
*)


%type <Ast.class_def> classe (* Coder *)
%type <string> extends (* Coder *)

%type <Ast.attrsMethsConstructor> classeBody  (* Typer *)
%type <Ast.attrsMethsConstructor> anyClDeclAndConstructor
%type <Ast.attrsMethsConstructor> anyClassDecl  (* Typer ?? Méthode ou atttribut *)
%type <Ast.variable_def list> factoredAttributes (* Typer *) (* A verif *)
%type <Ast.methode_def> methode (* Typer *) (* A verif *)

%type <Ast.constructor> constructor (* Typer *) (* A verif *) 
%type <Ast.superconstructor_call> superclasseCall 

%type <Ast.variable_def list> factoredVarParamList (* A verif *)
%type <Ast.variable_def list> factoredVarParam  (* A verif *)
%type <Ast.expression_t list> argumentsList (* A verif *)
%type <string> returnedType (* Typer *)

%type <Ast.block_t> block
%type <Ast.variable_def list> blockFactoredVarsList (* Typer *)
%type <Ast.variable_def list> blockFactoredVars (* Typer *)
%type <Ast.instruction_t> instruction (* Coder *) 
%type <Ast.container_t> container (* Typer *)
%type <Ast.selection_beg_t> classeCallBeginning (* Typer *)
%type <Ast.selection_end_t list> methodeCallEnd (* Typer *)
%type <Ast.selection_end_t list> attributeCallEnd (* Typer *)
%type <Ast.method_call> methodeCall (* Typer *)
%type <Ast.attribute_call> attributeCall (* Typer *)

%type <Ast.expression_t> expression (* Typer *)
%type <Ast.expression_t> expr1 (* Typer *)
%type <Ast.expression_t> expr2 (* Typer *)
%type <Ast.expression_t> expr3 (* Typer *)
%type <Ast.expression_t> instanciation  (* Typer *)
%type <Ast.expression_t> castedExpr (* Typer *)



(* Priorités *)
(* AUCUNE POUR L'INSTANT *)



(**
  ____________________________________________
/                    ------------°°°°------------                      \
|              GRAMMAIRE - GENERALITES
\ ___________________________________________ /
**)



(* Axiome *)
%start <Ast.prog_def> prog (* Do we have progType ? *)
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
  Dans les spécifications du langage, il est indiqué que les noms de classee commencent par une
  majuscule et les autres identificateurs par une minuscule. Cela signifie qu'un nom de classee
  CLASSNAME peut être identifié dès l'analyse lexicale. Je choisis donc de le faire, ce qui simplifie
  grandement le travail des analyses lexicale et contextuelle. -Gaël


  [3] super et this peuvent être hors d'une méthode
  Il est simple dans cette grammaire de faire en sorte que this et super ne soient utilisables que
  au début d'un appel de méthode ou attribut. Il est cependant plus difficile de savoir s'ils sont
  dans une classee. Je choisis donc de reconnaître les mauvaises utilisations lorsque super et this
  ne sont pas au tout début d'une suite de sélections lors de l'analyse syntaxique, mais de laisser
  les mauvaises utilisations à l'extérieur d'une méthode à l'analyse contextuelle. 



*)




(**
  ____________________________________________
/                    ------------°°°°------------                      \
|         GRAMMAIRE - REGLES : CLASSES
\ ___________________________________________ /
**)

(* 1 programme = Des classees + un bloc de programme principal à la fin *)
prog: cl=list(classe) b=block EOF {
  {
    classes = cl;
    program = b;
  }
}



(* classee *)
(* Ex : classe Point(var xc, yc : Integer, name:String) IS { **Corpsclassee** } *)
classe: CLASSE n=CLASSNAME p=factoredVarParamList s=option(extends) IS LBRACKET b=classeBody RBRACKET 
{ 
  {
    name = n;
    superclass = s; 
    attributes = b.attrs;
    methods = b.meths;
    constructor = nonOptionalConstr b.construct
  }
}



(* Extends d'une classee, optionnel *)
(* CSTR : Renvoie la string contenant le nom de la superclasse ou None *)
extends : EXTENDS s = CLASSNAME { s }




(* Corps de la classee *)
(* Ex : attributs, méthode, méthode, constructeur, méthode, attribut ... *)
(* Puisqu'on sait qu'il doit y avoir un constructeur par classee, on le cherche directement à l'analyse syntaxique *)
classeBody : beg=anyClDeclAndConstructor endl=list(anyClassDecl)
{ 
  {
    attrs = (beg.attrs @ getAttrsFromAMCList endl);
    meths = (beg.meths @ getMethsFromAMCList endl);
    construct = beg.construct;
  }
 }

(* Auxiliaire de la règle précédente pour éviter un conflit shift-reduce si on écrivait : *)
(* clasesBody : list(anyClassDecl) constructor list(anyClassDecl)  *)
anyClDeclAndConstructor :
  newAny=anyClassDecl next=anyClDeclAndConstructor {
    {
      attrs = (newAny.attrs @ next.attrs);
      meths = (newAny.meths @ next.meths);
      construct = None
    }
  }
| c=constructor {
   { attrs = []; meths = []; construct = Some c }
  }


(* Une déclaration quelconque dans une classee : méthode ou attributs *)
(* On sépare les méthodes en constructeur, méthode, et méthodeOuConstructeur, car il y a ambiguité lors de l'analyse syntaxique *)
anyClassDecl: 
  a=factoredAttributes{ { attrs=a; meths=[]; construct=None } }
| m=methode { {attrs=[]; meths=[m]; construct=None } }


(* Attributs de classee *)
(* Ex : var static x1, x2 : Integer *)
factoredAttributes: VAR s=boption(STATIC) v=separated_nonempty_list(COMMA, ID) COLON r = returnedType 
{
  List.map (fun n -> {
    name = n
    is_var = true;
    is_static = s;
    typ = r
  }) v
}



(* Méthode de classee *)
methode:
  (* cas finissant par un bloc et retournant un objet *)
  DEF s=boption(STATIC) o=boption(OVERRIDE) n=ID p=factoredVarParamList r=returnedType IS b=block 
  { 
    {
    name = n;
    parameters = p;
    body = {declarations=({var=false; stati=false; typ=r; nom="result"}::b.declarations); instructions=b.instructions};
    is_static = s;
    is_override = o;
    return_type = Some r
    }
   }
   (* cas finissant par un bloc mais ne retournant aucun objet *)
| DEF s=boption(STATIC) o=boption(OVERRIDE) n=ID p=factoredVarParamList IS b=block 
  { 
    {
    name = n;
    parameters = p;
    body = b;
    is_static = s;
    is_override = o;
    return_type = None
    }
   }
  (* cas finissant par "nomclassee := expression" *)
| DEF s=boption(STATIC) o=boption(OVERRIDE) n=ID p=factoredVarParamList r=returnedType ASSIGN e=expression  
{  (*let rt = match r with | None -> [] | Some m -> m in*)
  {
    name = n;
    parameters = p;
    body = {declarations=[]; instructions=[Affectation(Id("result"), e); Return] };
    is_static = s;
    is_override = o;
    return_type = Some r
    }
   }

(* Constructeur de classee *)
constructor:
  DEF n = CLASSNAME p = factoredVarParamList s=option(superclasseCall) IS b = block 
  { (* Ajouter dans l'AST UN ARGUMENT DASN CONSTRUCTOR POUR PRENDRE EN COMPLE LE SuperClasseCall ?? *)
    {
    name = n;
    parameters = p; 
    body = b;
    super_call = s
    } 
  }


superclasseCall: COLON n=CLASSNAME al=argumentsList { {superclass=n; arguments=al} }


(**
  ____________________________________________
/                    ------------°°°°------------                      \
|  GRAMMAIRE - REGLES : PARAMETRES & ARGS
\ ___________________________________________ /
**)

(* !!! flatten *)
(* Liste de paramètres optionnellement VAR entouré de parenthèses ( ) *)
factoredVarParamList: f = delimited(LPAREN, separated_list(COMMA, factoredVarParam), RPAREN) { List.flatten f }



(* Paramètre ou paramètres groupés optionnellement VAR *)
(* Ex: var x1, x2, x3 : Integer *)
factoredVarParam: v=boption(VAR) ids=separated_nonempty_list(COMMA, ID) COLON r=returnedType {
  List.map (fun n -> {
    name = n
    is_var = v;
    is_static = false;
    typ = r
  }) ids
}


(* Liste d'arguments, c'est-à-dire les expressions qu'on met comme paramètres lorsqu'on fait un appel (à une méthode par exemple) *)
(* Ex:    ( 3, z, Point3D.getHeight() )     *)
argumentsList: el=delimited(LPAREN, separated_list(COMMA, expression), RPAREN) { el }


(* Ex:   : Point3D *)
returnedType: COLON n = CLASSNAME { n }


(**
  ____________________________________________
/                    ------------°°°°------------                      \
|      GRAMMAIRE - REGLES : INSTRUCTIONS
\ ___________________________________________ /
**)


(* Bloc d'instructions entouré d'accolades *)
block:
  il=delimited(LBRACKET, list(instruction), RBRACKET) { {declarations=[]; instructions=il} }
| LBRACKET dl=blockFactoredVarsList IS il=list(instruction) RBRACKET { {declarations=dl, instructions=il} }


(* Déclarations des variables locales au début d'un bloc *)
(* Ex (début seulement): { x, y: Integer; p1: Point; is x := 0; p1 := new Point(x, x); } *)
blockFactoredVarsList: dl=separated_nonempty_list(SEMICOLON, blockFactoredVars) { List.flatten dl }

(* Un seul groupe de variables locales factorisées *)
(* Ex:    x, y : Integer      *)
blockFactoredVars: idL=separated_nonempty_list(COMMA, ID) r=returnedType { 
  List.map (fun id -> {
    name = id;
    is_var = false;
    is_static = false;
    typ = r
  }) idL
 }



(* N'importe quelle instruction du programme principal ou des méthodes *)
instruction:
  e=expression SEMICOLON {Exp(e)}
| b=block {Bloc(b)}
| RETURN SEMICOLON {Return}
| IF si=expression THEN alors=instruction ELSE sinon=instruction {Ite(si,alors,sinon)}
| g=container ASSIGN d=expression SEMICOLON {Affectation(g,d)}



(* Variable ou attribut, n'importe quoi pouvant contenir une valeur *)
(* Ex:     x   ou    Point2D.multiply(3*y).length    *)
container:
  n=ID { LocalVar n }
| RESULT { Result }
| a=attributeCall { Select a }


(* Premier token du début de n'importe quel appel de méthode ou attribut *)
(* Ex :    this       ou      myVariable      ou     MaClasse     *)
classeCallBeginning:
  id=ID { VarSelect(Container(LocalVar(id))) }
| CLASSNAME { ClassSelect }
| THIS { ThisSelect }
| SUPER { SuperSelect }



(* Dernier élément d'un appel de méthode en cascade *)
(* Ex : .getZ() *)
methodeCallEnd:
  SELECTION id=ID el=argumentsList { [MethSelect(id, el)] }
| SELECTION id=ID selecL=methodeCallEnd { (AttrSelect id)::selecL }
| SELECTION id=ID el=argumentsList selecL=methodeCallEnd { (MethSelect(id, el))::selecL }

(* Appel de méthode et tous ses constituants *)
(* Ex : myVariable.name.clone().getZ() *)
methodeCall:
  b=classeCallBeginning sl=methodeCallEnd { 
    {
      beginning = b;
      selections_to_meths = sl;
    } 
  }


(* Dernier élément d'un appel d'attributs en cascade *)
(* Ex : .length *)
attributeCallEnd:
  SELECTION id=ID { [AttrSelect id] }
| SELECTION id=ID selecL=attributeCallEnd { (AttrSelect id)::selecL }
| SELECTION id=ID el=argumentsList selecL=attributeCallEnd { (MethSelect(id, el))::selecL }

(* Appel d'un attribut d'une classe ou instance de classe, et tous les constituants de l'appel en cascade *)
(* Ex : MaClasse.name.clone().length  *)
attributeCall:
  b=classeCallBeginning sl=attributeCallEnd {
    {
      beginning = b;
      selections_to_attrs = sl
    }
  }



(**
  ____________________________________________
/                    ------------°°°°------------                      \
|      GRAMMAIRE - REGLES : EXPRESSIONS
\ ___________________________________________ /
**)


expression:
g = expr1 op = RELOP d = expr1 { Comp(op, g,d) (*Binary(op,g,d) *)}
| e = expr1 { e }

expr1:
  g = expr1 PLUS d = expr2 { Binary(PLUS,g,d)}
| g = expr1 MINUS d = expr2 { Binary(MINUS,g,d)}
| e=expr2 { e }

expr2:
  g = expr2 TIMES d = expr3 { Binary(TIMES,g,d)}
| g = expr2 DIV d = expr3 { Binary(DIV,g,d)}
| expr3 {}

expr3:
  v = CSTE { Cste v}  (* A Voir comment faire comme ca peut etre float/int etc..*)
| PLUS e=expr3  { e }
| MINUS e=expr3  { UMinus e }
| container {}
| methodeCall {}
| instanciation {}
| delimited(LPAREN, expression, RPAREN) {}
| castedExpr {}


instanciation: NEW CLASSNAME argumentsList {}

(* Cast d'une expression *)
(* Ex :        (Point p) *)
(* avec p un PointColoré. Le résultat de cette expression est un Point normal avec les mêmes attributs que p *)
castedExpr: LPAREN CLASSNAME expression RPAREN {}