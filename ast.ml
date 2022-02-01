(**
   ___________________________________________________________
   /                                ------------°°°°------------                                  \
   |                                TYPES DEFINITIONS
   |        Stockent les définitions déclarés dans le programme
   \ __________________________________________________________ /
 **)

type prog_def = {
  classes : class_def list;
  program : block_t 
} 


and class_def = {
  name_class : string;
  params_class : variable_def list;
  superclass : string option; 
  attributes : variable_def list; (* param + attribut *)
  methods : methode_def list;
  constructor : constructor_def ;
}

and constructor_def = {
  name_constructor : string;
  param_constructor : variable_def list; 
  body_constructor : block_t;
  super_call : superconstructor_call option
}

and superconstructor_call = {
  superclass_constructor : string;
  arguments : expression_t list
}

and methode_def = {
  name_method : string;
  param_method : variable_def list;
  body_method : block_t;
  is_static_method : bool;
  is_override : bool;
  return_type : string option  (* le type est un string ex : int est INTEGER *)
}

and variable_def = {
  name : string;
  is_var : bool;
  is_static : bool;
  typ : string;
}



(**
   ___________________________________________________________
   /                                ------------°°°°------------                                  \
   |                               TYPES INSTRUCTIONS
   |        Stockent les différentes instructions dans les blocs
   \ __________________________________________________________ /
 **)


and block_t = {
  declarations : variable_def list;
  instructions : instruction_t list;
}

and instruction_t = 
    Exp of expression_t
  | Block of block_t
  | Ite of expression_t * instruction_t * instruction_t
  | Return 
  | Affectation of container_t * expression_t


and container_t =
  | Select of attribute_call
  | LocalVar of string
  | This
  | Super


and attribute_call = {
  beginning : selection_beg_t;
  selections_to_attrs : selection_end_t list
}

and method_call = {
  beginning_call : selection_beg_t;
  selections_to_meths : selection_end_t list;
}







(**
   ___________________________________________________________
   /                                ------------°°°°------------                                  \
   |                                TYPES EXPRESSIONS
   |                   Stockent des expressions à évaluer
   \ __________________________________________________________ /
 **)

and expression_t =
  | IntLiteral of int
  | StringLiteral of string
  | Container of container_t
  | Method of method_call (* S'assurer que la méthode renvoie quelque chose à l'analyse contextuelle *)
  | Binary of binary_operator_t * expression_t * expression_t
  | Unary of unary_operator_t * expression_t
  | Cast of string * expression_t (* string = Nom de la classe de destination ; expression_t = Valeur à caster *)
  | NewClass of string * expression_t list

and unary_operator_t =
    UMINUS 

and binary_operator_t =
  | IntBinOp of int_binary_operator_t
  | StringConcat 


and int_binary_operator_t =
  (* Comparaison *)
  | EQ | NEQ | LT | LE | GT | GE
  (* Arithmétique *)
  | PLUS | MINUS | TIMES | DIV


and selection_beg_t =
  | ExpSelect of expression_t
  | ClassSelect of string

and selection_end_t =
  | AttrSelect of string
  | MethSelect of string * expression_t list


(**
   ____________________________________________
   /                    ------------°°°°------------                      \
   |                 TYPES INTERMEDIAIRES
   |            Utiles pour construire l'AST, et
   |               absents de la version finale
   \ ___________________________________________ /
 **)

exception MISC_Error of string

type attrsMethsConstructor = {attrs: variable_def list; meths: methode_def list; construct: constructor_def option}



(**
   ____________________________________________
   /                    ------------°°°°------------                      \
   |     FONCTIONS UTILES A LA CONSTRUCTION
   |    Utiles pour construire l'AST dans Parse.mly
   \ ___________________________________________ /
 **)


let getAttrsFromAMCList (amcList:attrsMethsConstructor list) =
  List.fold_left (fun (acc:variable_def list) (attrib:variable_def) -> attrib::acc ) [] (* Inverse la liste à nouveau *)
    (List.fold_left (fun (acc:variable_def list) (amc:attrsMethsConstructor) ->
         (List.fold_left (fun (acc:variable_def list) (attrib:variable_def) -> attrib::acc) acc amc.attrs))
        [] amcList)
;;

let getMethsFromAMCList (amcList:attrsMethsConstructor list) =
  List.fold_left (fun (acc:methode_def list) (meth:methode_def) -> meth::acc ) [] (* Inverse la liste à nouveau *)
    (List.fold_left (fun (acc:methode_def list) (amc:attrsMethsConstructor) ->
         (List.fold_left (fun (acc:methode_def list) (meth:methode_def) -> meth::acc) acc amc.meths)
       ) [] amcList)
;;

let nonOptionalConstr constr =
  match constr with
  | Some c -> c
  | None -> {
      name_constructor="ERRORCONSTR";
      param_constructor=[];
      body_constructor={declarations=[];instructions=[]};
      super_call=None
    }
;;
