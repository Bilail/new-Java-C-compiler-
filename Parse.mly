(* Dependencies *)
%{
  Ast
  intermediateSyntaxStructs
%}

(** TOKENS **)

(* Complex tokens *)
%token <string> ID
%token <int> CSTE

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
%token OVERRIDE

(* Opérateur *)
(*
%token EQ		/* = */
%token GT		/* > */
%token LT		/* < */
%token NOT		/* ! */
%token COMPL		/* ~ */
%token COND		/* ? */
%token COLON		/* : */
%token EQ_EQ		/* == */
%token LE		/* <= */
%token GE		/* >= */
%token NOT_EQ		/* != */
%token AND_AND		/* && */
%token OR_OR		/* || */
%token INCR		/* ++ */
%token DECR		/* -- */
%token PLUS		/* + */
%token MINUS		/* - */
%token TIMES		/* * */
%token DIV		/* / */
%token AND		/* & */
%token OR		/* | */
%token XOR		/* ^ */
%token MOD		/* % */
*)



(** PRIORITIES **)
(*
%nonassoc SEMICOLON
//%nonassoc RELOP
%left ELSE
%left PLUS MINUS 
%left TIMES DIV 
*)




(** GRAMMAR : GENERAL INFO **)

(* Non-terminaux et leur type équivalent OCaml (voir Ast.ml) *)

%type <Ast.classType> class
%type <Ast.instrType> instruction (* A faire *)
%type varableParam (* A faire *)
%type <string> extends (* A faire *)
%type anyClassDecl (* A faire *)


(* Axiom *)
%start <Ast.progType> prog (* Do we have progType ? *)
%%


(** GRAMMAR : RULES **)

(* 1 programme = Des classes + un programme principal à la fin *)
prog: cl=list(class) il=list(instruction)


(* Classe *)
(* Ex : class Point(var xc, yc : Integer, name:String) IS { **CorpsClasse** } *)
class: CLASS ID LPAREN separated_list(COMMA, varableParam) RPAREN option(extends) IS LBRACKET list(anyClassDecl) RBRACKET


(* Paramètres optionnellement VAR *)
(* Ex : var x1, x2, x3 : Integer *)
varableParam: option(VAR) separated_list(COMMA, ID) COLON ID



(* Déclaration dans une classe *)
anyClassDecl: 






















%type <int> prog
%type <decl> declaration
%type <expType> expr
%type <instrType> instr
%type <classeType> classe


%type <listparametre> listParam
%type <parametre> param  



declaration :
  boption(VAR) x = ID : y = ID { }


instr : 
| IF si=expr THEN alors=expr ELSE sinon = expr
    { Ite(si, alors, sinon) }

expr:
    x = ID                        { Id x }
  | v = CSTE                      { Cste v }
  | g = expr PLUS d = expr        { Plus (g, d) }
  | g = expr MINUS d = expr       { Minus(g, d) }
  | g = expr TIMES d = expr       { Times(g, d) }
  | g = expr DIV d = expr         { Div(g, d) }
  | PLUS e = expr                 { e }
  | MINUS e = expr %prec UMINUS   { UMinus e }
  | e = delimited (LPAREN, expr, RPAREN) { e }
  
  | g = expr SELECTION d = Id  {Selection(g , d)} 
  | NEW g = classe {Instanciation (g)}  
  | 


classe:
  CLASSE ID lp = delimited(LPAREN,listParamOpt,RPAREN) extendsOpt IS c = delimited (LBRACKET, corps ,RBRACKET) {}

corps : 
  
extendsOpt :  extends ID  | NONE


listParam : p = separated_list( VIRGULE, listParam ) { p }
                param VIRGULE listParam 
              | param

ListParam : Param, ListParam 
          | Param

parametre : var ListArg : type 
ListArg : Id, ListArg 
        | Id 
        | ε  
        { }

param : b = boption(VAR) ID : ID { }


attrib : VAR staticOpt ID : ID 
staticOpt : STATIC | 


Super:
	EXTENDS n = ID  { n }
;





