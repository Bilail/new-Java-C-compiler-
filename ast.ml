(* Pour les type string et integer *)
type var_type = {
  | String of string 
  | Integer of int 
  }

type classType = {
  name : string; 
  superClasse : string option; 
  attribut : decl list; (* param + attribut *)
  meth : methode list;
  construct : constructor ;
  }

and constructor = {
  name : string;
  param : decl list; 
  body : instrType;
  } 

and decl = {
    typ : string;
    nom : string;
  }

and methode = {
  nom : string;
  param : decl list;
  body : instrType;
  static : bool;
  override : bool;
  retour : string option (* le type est un string ex : int est INTEGER *)
}



type instrType = 
    Exp of exp_type
  | Bloc of bloc
  | Ite of exp_type*instr*instr
  | Return 
  | Affectation of exp_type * exp_type
  

type bloc = {
  declarations : decl list;
  instructions : instr list;
}
 (*
type exp_type =
| Id of string (* var_type *)
| Cste of int (* var_type *)
| Plus of exp_type*exp_type
| Minus of exp_type*exp_type
| Times of exp_type*exp_type
| Div of exp_type*exp_type
| UMinus of exp_type
| Comp of opComp*exp_type*exp_type
| Selection of exp_type*Id (* Selection expression *)
| Instantiation of Id * listArgOpt (* Instanciation expression *)
| Cast of classType * exp_type (* (nomClasse expression) *)
| Envoi of (* envoie de message*)
*) 

exp_type =
  | Assignment of assignment
  | Conditional of expr_e * (expr_e * expr_e)
  | Binary of binary_op * expr_e * expr_e
  | Unary of unary_op * expr_e
  | Variable of name
  | Integer_constant of Int64.t
  | Float_constant of float
  | Bool_constant of bool
  | Null
  | Char_constant of string
  | String_constant of string
  | This
  | New of instance_creation
  | Field_access of  field_obj * string
  | Method_call of method_name * (expr_e list)
  | New_array of array_creation
  | Instanceof of expr_e * var_type

  type unary_op =
    | Not
    | Cast of var_type
    | Complement
    | Pre_decr
    | Post_decr
    | Pre_incr
    | Post_incr
    | Minus

type binary_op =
  (* Logic *)
  | Or | And | Nor
  (* Comparaison *)
  | Eq | Neq | Lt | Le | Gt | Ge
  (* Arithmetic *)
  | PLUS | MINUS | TIMES | DIVE 



and type_decl =
  | Class_declaration of class_decl

type prog_type = classType list
