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



A faire
    Les paramètres de la classe sont les mêmes que les params du constructeur
    Les params du constructeur correspondent aux args de l'instanciation
    Le constructeur et sa classe extend-ent la même superclasse
    L'appel à la superclasse dans le constructeur a des arguments qui correspondent à la superclasse
    Les params des méthodes correspondent aux arguments des appels
    On doit extend une classe qui existe (pas forcément déclarée plus haut)
    On ne peut pas faire d'héritage circulaire
    

**)


(**
  ____________________________________________
/                    ------------°°°°------------                      \
|     		FONCTIONS UTILES PLUS BAS
\ ___________________________________________ /
**)

(* Affiche une erreur si "name" est un nom porté par une classe dans "classes" *)
let forbidClassName (classes:class_def list) name =
	let check = not (List.exists (fun c -> c.name_class = name) classes)
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
        print_string "[Error] A class and its constructor must have the same parameters in "; print_string c.name_class; print_newline ();
        false






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



let analyseClass c env =
	let newEnv = {
		decl_classes = c::env.decl_classes;
		decl_vars = env.decl_vars;
		is_correct_env =
			env.is_correct_env &&
			forbidClassName env.decl_classes c.name_class &&
            chckParamsInClaAndConstr c
	}
    in
    (match newEnv.is_correct_env with
    | true -> print_string c.name_class; print_string " OK"; print_newline ()
    | false -> print_string c.name_class; print_string " KO"; print_newline ());
    newEnv


let analyseProgram prog =
	List.fold_left (fun env c -> analyseClass c env) emptyEnv prog.classes






(**
  ____________________________________________
/                    ------------°°°°------------                      \
|  FONCTIONS PRINT (A METTRE DANS PRINT.ML)
\ ___________________________________________ /
**)


let printEnv e =
   print_string "ENVIRONMENT {"; print_newline ();
   print_string "  Classes : "; print_int (List.length e.decl_classes); print_newline ();
   print_string "  Local variables : "; print_int (List.length e.decl_vars); print_newline ();
   print_string "  Valid ? "; print_string (match e.is_correct_env with | true -> "Yes" | false -> "No"); print_newline ();
   print_string "}"; print_newline ()
