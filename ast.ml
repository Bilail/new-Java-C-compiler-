(* Pour les type string et integer *)
type var_type = 
  | String of string 
  | Integer of int 
  

type class_type = {
  name : string; 
  superClasse : string option; 
  attributs : decl list; (* param + attribut *)
  meth : methode list;
  construct : constructor ;
  }

and constructor = {
  name_constuctor : string;
  param_constuctor : decl list; 
  body_constuctor : bloc;
  super_call : constructor_call option
  } 

and decl = {
    var : bool;
    stati : bool;
    typ : string;
    nom : string;
  }

and methode = {
  name_methode : string;
  param_methode : decl list;
  body_methode : bloc;
  static_methode : bool;
  override : bool;
  retour_methode : string option  (* le type est un string ex : int est INTEGER *)
}



and instrType = 
    Exp of exp_type
  | Bloc of bloc
  | Ite of exp_type*instrType*instrType
  | Return 
  | Affectation of exp_type * exp_type
  

and bloc = {
  declarations : decl list;
  instructions : instrType list;
}


and constructor_call = {
  classe : string;
  arguments : exp_type list
}

 (*
type exp_type =
 (* var_type *)
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

and exp_type =
  | Id of string
  | Result
  | Selection of exp_type*exp_type
  (*| Assignment of assignment
  | Conditional of expr_e * (expr_e * expr_e) *)
  | Binary of binary_op * exp_type * exp_type
  | Unary of unary_op * exp_type
  (*| Variable of *)
  (*| Integer_constant of Int64.t
  | Float_constant of float
  | Bool_constant of bool
  | Null
  | Char_constant of string
  | String_constant of string *)
  | This
  | Cste of int 
  (*| New of instance_creation
  | Field_access of  field_obj * string *)
  | Method_call of methode * (exp_type list) (* S'assurer que la méthode renvoie quelque chose à l'analyse contextuelle *)
  | Comp of opComp*exp_type*exp_type

  and unary_op =
    | Not
    | Cast of var_type
    | Complement
    | Pre_decr
    | Post_decr
    | Pre_incr
    | Post_incr
    | Minus

and binary_op =
  (* Logic *)
  | Or | And | Nor
  (* Comparaison *)
  | OpComp 
  (* Arithmetic *)
  | PLUS | MINUS | TIMES | DIV 

  and opComp = 
  | Eq | Neq | Lt | Le | Gt | Ge

(*
and type_decl =
  | Class_declaration of class_decl *)

type prog_type = {
  cl : class_type list;
  b : bloc 
} 












(**
  ____________________________________________
/                    ------------°°°°------------                      \
|                 TYPES INTERMEDIAIRES
|            Utiles pour construire l'AST, et
|               absents de la version finale
\ ___________________________________________ /
**)

type attrsMethsConstructor = {attrs: decl list; meths: methode list; construct: constructor option}



(**
  ____________________________________________
/                    ------------°°°°------------                      \
|     FONCTIONS UTILES A LA CONSTRUCTION
|    Utiles pour construire l'AST dans Parse.mly
\ ___________________________________________ /
**)


let getAttrsFromAMCList (amcList:attrsMethsConstructor list) =
  List.fold_left (fun (acc:decl list) (attrib:decl) -> attrib::acc ) [] (* Inverse la liste à nouveau *)
    (List.fold_left (fun (acc:decl list) (amc:attrsMethsConstructor) ->
      (List.fold_left (fun (acc:decl list) (attrib:decl) -> attrib::acc) acc amc.attrs))
    [] amcList)
;;

let getMethsFromAMCList (amcList:attrsMethsConstructor list) =
  List.fold_left (fun (acc:methode list) (meth:methode) -> meth::acc ) [] (* Inverse la liste à nouveau *)
    (List.fold_left (fun (acc:methode list) (amc:attrsMethsConstructor) ->
      (List.fold_left (fun (acc:methode list) (meth:methode) -> meth::acc) acc amc.meths)
    ) [] amcList)
;;

let nonOptionalConstr constr =
  match constr with
  | Some c -> c
  | None -> {
      name_constuctor="ERRORCONSTR";
      param_constuctor=[];
      body_constuctor={declarations=[];instructions=[]};
      super_call=None
    }
;;
