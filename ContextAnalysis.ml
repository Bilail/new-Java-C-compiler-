open Ast

(**
		
**)
(**
  ____________________________________________
/                    ------------°°°°------------                      \
|     				ACTUELLEMENT TESTE                     
\ ___________________________________________ /
**)
(**

1. Classes
    1. Plusieurs classes ne peuvent pas avoir le même nom
    2. Les paramètres de la classe sont les mêmes que les params du constructeur
    3. Le constructeur et sa classe extend-ent la même superclasse
    4. On doit extend une classe qui existe (pas forcément déclarée plus haut)
    5. On ne peut pas s'extend soi-même // A tester
    6. On ne peut pas faire d'héritage circulaire // A tester



A faire
    Les params du constructeur correspondent aux args de l'instanciation
    Les params des méthodes correspondent aux arguments des appels
    L'appel à la superclasse dans le constructeur a des arguments qui correspondent à la superclasse
    On ne peut pas déclarer de variables locales avec le même nom dans la même portée
    Les membres gauche et droit d'une affectation doivent avoir même type

**)


(**
  ____________________________________________
/                    ------------°°°°------------                      \
|     		FONCTIONS UTILES PLUS BAS               
\ ___________________________________________ /
**)




(* Compte les occurrences d'un élément dans une liste *)
let countOccurrences elem lis =
    List.fold_left (fun count el -> if elem = el then count+1 else count) 0 lis


(* Indique si un nom de classe existe dans une liste *)
let countClassnameAmong classes name =
    (countOccurrences name (List.map (fun c -> c.name_class) classes))


(* Retoune la classe associé au nom de classe donnée en param*)
let find_class s l_c =
  List.find_opt (fun c -> c.name_class == s) l_c


(*Vérifier qu'un type est sous type d'un autre type *)
let rec is_subclass m f l_cl =
  let fi = find_class f l_cl in 
  match fi with
  | None -> false
  | Some fille -> (
    if (m == fille.name_class) then
      true 
    else 
      (match fille.superclass with 
        | None -> false 
        | Some s -> is_subclass m s l_cl)
  )


(* Affiche une erreur si "name" est un nom porté par une classe dans "classes" *)
let forbidClassName (classes:class_def list) name =
	let check = ((countClassnameAmong classes name) = 1)
	in match check with
	| true -> true
	| false ->
		print_string "[Error] Two classes cannot have the same name : "; print_string name; print_newline ();
		false


(* Affiche une erreur si les paramètres d'une classe et de son constructeur sont différents *)
let chckParamsInClaAndConstr c =
    let check = c.params_class = c.constructor.param_constructor
    in match check with
    | true -> true
    | false -> 
        print_string "[Error] A class and its constructor must have the same parameters in "; print_string c.name_class;
        false
 
(* Affiche une erreur si la superclasse n'existe pas *)
let chckSuperclassExistence call c classes =
    let check = (countClassnameAmong classes call.superclass_constructor) = 1
    in match check with
    | true -> true
    | false ->
        (print_string "[Error] "; print_string c.name_class; print_string " class extends a class that doesn't exist : "; print_string call.superclass_constructor; print_newline (); (print_string "Superclasse existe OK : "; print_string call.superclass_constructor; print_newline ());
        false)


(* Affiche une erreur si une classe hérite d'elle-même *)
let forbidInheritanceCycle c classes =
    let check =
        match c.superclass with
        | Some supername -> not (is_subclass c.name_class supername classes) 
        | None -> true
    in match check with
    | true -> true
    | false ->
        (print_string "[Error] Class "; print_string c.name_class; print_string " inherits itself, which is forbidden"; print_newline ());
        false



(* Affiche une erreur s'il y a un problème dans le graphe d'héritage *)
let chckSuperclass call c (classes:class_def list) =
    chckSuperclassExistence call c classes &&
    forbidInheritanceCycle c classes


(* Affiche une erreur si la classe et son constructeur n'appellent pas la même superclasse *)
let chckSuperclassInClaAndConstr c classes = 
    match c.superclass with
    | Some s ->
      (match c.constructor.super_call with
      | Some call ->
        (if call.superclass_constructor = s then
          chckSuperclass call c classes
        else
          (print_string "[Error] Mismatch between the superclass of "; print_string c.name_class; print_string " and the superclass called by its constructor : "; print_string s; print_string " and "; print_string call.superclass_constructor; print_newline ();
          false)
        )
      | None ->
        (print_string "[Error] The constructor of class "; print_string c.name_class; print_string " doesn't call its superclass : "; print_string s; print_newline ();
        false)
      )
    | None ->
      (match c.constructor.super_call with
      | Some call ->
        (print_string "Class "; print_string c.name_class; print_string " doesn't extend any superclass but its constructor calls superclass "; print_string call.superclass_constructor; print_newline ();
        false)
      | None ->
        true
      )


    


(*
let chckSuperclassCallParams call env =
TODO : A faire quand les expressions renverront leur type 
*) 
    







(**
  ____________________________________________
/                    ------------°°°°------------                      \
|     FONCTIONS D'ANALYSE CONTEXTUELLE
\ ___________________________________________ /
**)


type environment = {
	decl_classes : class_def list;
	decl_vars : variable_def list;
	is_correct_env : bool
}
let emptyEnv = {decl_classes=[]; decl_vars=[]; is_correct_env=true}



let analyseClass env c =
	let newEnv = {
		decl_classes = c::env.decl_classes;
		decl_vars = env.decl_vars;
		is_correct_env =
			env.is_correct_env &&
			forbidClassName env.decl_classes c.name_class &&
      chckParamsInClaAndConstr c &&
      chckSuperclassInClaAndConstr c env.decl_classes
	}
    in
    (match newEnv.is_correct_env with
    | true -> print_string c.name_class; print_string " OK"; print_newline ()
    | false -> print_string c.name_class; print_string " KO"; print_newline ());
    newEnv.is_correct_env


let analyseProgram prog =
    let env = {
        decl_classes = prog.classes;
        decl_vars = [];
        is_correct_env = true
    }
    in
	List.fold_left (fun is_correct c -> (analyseClass env c) && is_correct) true prog.classes


