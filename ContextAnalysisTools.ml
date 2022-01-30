open Ast

(**
  ____________________________________________
/         ------------°°°°------------        \
|        QUELQUES FONCTIONS AUXILIAIRES        
\ ___________________________________________ /
**)


(*-----------------------------------------------------------------------------------------------
                                      Types auxiliaires
-----------------------------------------------------------------------------------------------*)


let emptyClass =
  let emptyConstructor = {
    name_constructor="ERRORCLASS_CONSTRUCTOR";
    param_constructor=[];
    body_constructor={declarations=[];instructions=[]};
    super_call=None
  }
  in
  {name_class="ERRORCLASS"; params_class=[]; superclass=None; attributes=[]; methods=[]; constructor=emptyConstructor}



(*  Contient : classes existantes ; variables locales ; validité jusqu'à présent *)
type environment = {
	decl_classes : class_def list;
	decl_vars : variable_def list;
  current_class : class_def option;
  current_method : methode_def option;
	is_correct_env : bool
}
let emptyEnv = {decl_classes=[]; decl_vars=[]; current_class=None; current_method=None; is_correct_env=true}



(* Est renvoyé par la vérification d'une expression *)
(* Contient type de retour de l'expression ; validité jusqu'à présent *)
type exprUpward = {
  expr_return_type : string;
  is_correct_expr : bool
}
let (emptyExprUpw:exprUpward) = {expr_return_type="ERROR"; is_correct_expr=false}



type exprListUpward = {
  expr_return_types : string list;
  is_correct_exprs : bool
}


exception Error_class_not_found of string
exception Error_inherit_cycle of string
exception Error_no_selection_after_class of string
exception Error_classless_meth_env of string


(*-----------------------------------------------------------------------------------------------
                                           divers
-----------------------------------------------------------------------------------------------*)

(* Compte les occurrences d'un élément dans une liste *)
let rec countOccurrences elem lis =
    List.fold_left (fun count el -> if elem = el then count+1 else count) 0 lis



(* Indique si aucun couple de la liste ne satisfait la condition de predicate *)
let rec eachUnique (predicate: 'a -> 'a -> bool) lis =
  match lis with
  | [] -> None
  | x::lis -> (
    match List.find_opt (fun y -> predicate x y) lis with
    | Some el -> Some el
    | None -> eachUnique predicate lis
  )
      


and print_bool b = match b with
| true -> print_string "YES"
| false -> print_string "NO"



(*-----------------------------------------------------------------------------------------------
                    pour les appels de méthodes/constructeurs
-----------------------------------------------------------------------------------------------*)



and getVarDeclTypes (variables: variable_def list) =
  {
    expr_return_types = List.map (fun var -> var.typ) variables;
    is_correct_exprs = true
  }


and find_attribute_in (name:string) (attributes:variable_def list) =
  List.find_opt (fun attrib -> attrib.name = name) attributes

and find_attribute (name: string) (classe: class_def) =
  find_attribute_in name classe.attributes


and find_method_in (name:string) (methods:methode_def list) =
  List.find_opt (fun meth -> meth.name_method = name) methods

and find_method (name: string) (classe: class_def) =
  find_method_in name classe.methods


and filter_attribs_static attributes is_static =
  List.filter (fun attrib -> attrib.is_static = is_static) attributes

and filter_meths_static methods is_static =
  List.filter (fun meth -> meth.is_static_method = is_static) methods


and filter_attribs_var attributes is_var =
  List.filter (fun attrib -> attrib.is_var = is_var) attributes



(*-----------------------------------------------------------------------------------------------
                                       pour les classes
-----------------------------------------------------------------------------------------------*)

(* Indique si un nom de classe existe dans une liste *)
and countClassnameAmong classes name =
    (countOccurrences name (List.map (fun c -> c.name_class) classes))


      
(* Retoune la classe associée au nom de classe donnée en param *)
and find_class s l_c =
  List.find_opt(fun c -> c.name_class = s) l_c


(* Vérifie qu'un type est sous type d'un autre type *)
and is_subclass m f l_cl =
  let rec issc m f l_cl =
    if m = f then
      true
    else
      let fi = find_class f l_cl
      in match fi with
      | None -> print_string f; print_string " inexistante\n"; false
      | Some fille -> ( 
        match fille.superclass with 
          | None -> print_string "Pas de superclasse pour "; print_string fille.name_class; print_newline (); false
          | Some s -> is_subclass m s l_cl
      )
  in issc m f l_cl
  




and is_subclass_cyclesafe parentName childName classes = 
    let rec findInInheritance parentName childName classes (alreadyVisited: string list) =
        if parentName = childName then
            true
        else if List.mem childName alreadyVisited then
            raise (Error_inherit_cycle childName)
        else
            let child = find_class childName classes
            in
            match child with
            | None -> false
            | Some c -> (
                match c.superclass with
                | None -> false
                | Some supername -> findInInheritance parentName supername classes (childName::alreadyVisited)
            )
    in
    findInInheritance parentName childName classes []


(* Vérifie pour chaque couple (parentL[i], childL[i]) si childL[i] est bien sous-classe de parentL[i], et renvoie true si ils le sont tous *)
and is_subclasses_cyclesafe parentL childL classes =
      match (parentL, childL) with
      | (parent::parentL, child::childL)
        when is_subclass_cyclesafe parent child classes
        -> is_subclasses_cyclesafe parentL childL classes
      | ([],[]) -> true
      | _ -> false




(*-----------------------------------------------------------------------------------------------
                                    pour les environnements
-----------------------------------------------------------------------------------------------*)

(* Verifie qu'une variable est dans l'envrionnement *)
and var_in_env env e = 
  List.exists (fun c -> c.name == e) env.decl_vars


(* Prend un environnement et écrase une variable avec une nouvelle de même nom *)
and replace_env_var v env =
  let rec replace v variables =
    match variables with
    | var::lis when var.name = v.name -> (v::lis, true)
    | var::lis ->
      let (lis2, hasReplaced) = replace v lis in (var::lis2, hasReplaced) 
    | [] -> ([], false)
  in
    replace v env.decl_vars




(* Ajout d'une variable à un environnement *)
and add_env_var v env =
  let newVars =
    let (replaceVars, hasReplaced) = replace_env_var v env
    in
      if hasReplaced then
        replaceVars
      else
        v::env.decl_vars
  in
  let newEnv = 
    {
      decl_classes = env.decl_classes;
      decl_vars = newVars;
      current_class = env.current_class;
      current_method = env.current_method;
      is_correct_env = env.is_correct_env
    }
  in newEnv


(* Ajout d'une liste de variables à un environnement *)
and add_env_varList (vl:variable_def list) env =
  
  List.fold_left (fun env v -> add_env_var v env) env vl



and set_env_method (meth:methode_def option) env =
  {
    decl_classes = env.decl_classes;
    decl_vars = env.decl_vars;
    current_class = env.current_class;
    current_method = meth;
    is_correct_env = env.is_correct_env
  }

(**
(* Ajout d'une classe à un environnement *)
  let add_env_classe env c = 
    let new = {
      decl_classes = c::env.decl_classes;
		  decl_vars = env.decl_vars;
	  	is_correct_env =
			  env.is_correct_env &&
			  forbidClassName env.decl_classes c.name_class &&
            chckParamsInClaAndConstr c
	}
**) 



(**
   ____________________________________________
   /                    ------------°°°°------------                      \
   |   PRINTS DE TYPES ANALYSE CONTEXTUELLE
   \ ___________________________________________ /
 **)
(* let printAll programme  =
   List.iter printClass programme.classes;
   print_newline ();
*)

and printEnv env =
  print_string "ENVIRONMENT {"; print_newline ();
  print_string "  Classes : "; print_int (List.length env.decl_classes); print_newline ();
  print_string "  Local variables : "; List.iter (fun var -> print_string "-"; print_string var.name) env.decl_vars; print_string " ("; print_int (List.length env.decl_vars); print_string ")"; print_newline ();
  print_string "  Inside class : "; print_string (match env.current_class with | Some c -> c.name_class | None -> "-"); print_newline ();
  print_string "  Valid ? "; print_string (match env.is_correct_env with | true -> "Yes" | false -> "No"); print_newline ();
  print_string "}"; print_newline ()


and print_string_list lis =
  print_string "(";
  (
    match lis with
    | x::lis -> print_string x; List.iter (fun text -> print_string ", "; print_string text) lis
    | [] -> ()
  );
  print_string ")"