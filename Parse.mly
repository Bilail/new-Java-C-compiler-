%{
(* dependance *)
%}

(* Tous nos token *)

%token <string> ID
%token <int> CSTE
%token <Ast.opComp> RELOP
%token PLUS MINUS TIMES DIV
%token LPAREN RPAREN
%token SEMICOLON
%token ASSIGN
%token EOF
%token IF THEN ELSE
%token VAR
%token NEW
%token THIS SUPER RESULT
%token CLASSE
%token SELECTION
%token VIRGULE
%token IS 

(* Les priorités*)
%nonassoc SEMICOLON
//%nonassoc RELOP
%left ELSE
%left PLUS MINUS 
%left TIMES DIV 


(* declaration des types *)
%type <int> prog
%type <decl> declaration
%type <expType> expr
%type <expType> exprBool
%type <classe> class


(* Par Wassim *)
%type <listparametre> listParam
%type <parametre> param  

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
  | IF si=bexpr THEN alors=expr ELSE sinon = expr
    { Ite(si, alors, sinon) }
  | g = expr SELECTION d = classe  {Selection(g , d)} 
  | NEW g = classe {Instanciation (g)}  

bexpr :
    g = expr op = RELOP d = expr  { Comp(op, g, d) }
  | e = delimited (LPAREN, bexpr, RPAREN) { e }

class: CLASSE ID ( listParamOpt ) extendsOpt IS { corps }


extendsOpt :  extends ID  | ε


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
