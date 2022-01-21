open Ast

(**
		
**)
(**
  ____________________________________________
/         ------------°°°°------------        \
|             ACTUELLEMENT TESTE               
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
/         ------------°°°°------------        \
|        QUELQUES FONCTIONS AUXILIAIRES        
\ ___________________________________________ /
**)


(*-----------------------------------------------------------------------------------------------
                                      Types auxiliaires
-----------------------------------------------------------------------------------------------*)

type environment = {
	decl_classes : class_def list;
	decl_vars : variable_def list;
	is_correct_env : bool
}
let emptyEnv = {decl_classes=[]; decl_vars=[]; is_correct_env=true}



(*-----------------------------------------------------------------------------------------------
                                           divers
-----------------------------------------------------------------------------------------------*)

(* Compte les occurrences d'un élément dans une liste *)
let countOccurrences elem lis =
    List.fold_left (fun count el -> if elem = el then count+1 else count) 0 lis



let print_bool b = match b with
| true -> print_string "YES"
| false -> print_string "NO"



(*-----------------------------------------------------------------------------------------------
                                       pour les classes
-----------------------------------------------------------------------------------------------*)

(* Indique si un nom de classe existe dans une liste *)
let countClassnameAmong classes name =
    (countOccurrences name (List.map (fun c -> c.name_class) classes))


      
(* Retoune la classe associée au nom de classe donnée en param *)
let find_class s l_c =
  List.find_opt(fun c -> c.name_class = s) l_c


(* Vérifie qu'un type est sous type d'un autre type *)
let rec is_subclass m f l_cl =
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


exception Error_inherit_cycle of string

let is_subclass_cyclesafe parentName childName classes = 
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





(*-----------------------------------------------------------------------------------------------
                                    pour les environnements
-----------------------------------------------------------------------------------------------*)

(**
(* Ajout d'une variable à un environnement *)
let add_env_var env v = 
  let new = {
    decl_classes = env.decl_classes;
	  decl_vars = v::env.decl_vars;
    is_correct_env =
			env.is_correct_env && e.is_correct_env
  }
**)

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
/         ------------°°°°------------        \
|             ANALYSE : 1- CLASSES             
\ ___________________________________________ /
**)




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
    match c.superclass with
    | None -> true
    | Some supername -> (
        match (
                try not (is_subclass_cyclesafe c.name_class supername classes) with
                | Error_inherit_cycle classname -> print_string "[Error] "; print_string c.name_class; print_string " is part of an inheritance cycle involving "; print_string classname; print_newline (); false
        ) with
        | true -> true
        | false -> print_string "[Error] "; print_string c.name_class; print_string " extends a class that's part of an inheritance cycle"; print_newline (); false
    )



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

(*-----------------------------------------------------------------------------------------------
                                           Appel
-----------------------------------------------------------------------------------------------*)

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


(**

(**
  ____________________________________________
/         ------------°°°°------------        \
|           ANALYSE : 2- Expressions           
\ ___________________________________________ /
**)



let expr_verif expr env =
  match expr with 
  | IntLiteral i -> (*add_env_var env i*)
  | StringLiteral s -> in_env env s 
  | Container c  -> container_verif c env
  | Method m -> methode_verif m env 
  | Binary (op,e1,e2) -> binary_verif op e1 e2 env 
  | Unary (u,e) -> unary_verif u e env
  | Cast(s,e) -> is_subclass_cyclesafe s e env.decl_classes
  | NewClasse (s,e_list) -> add_env_classe env c (* A vérifier *)

let container_verif c env = 
  match c with
    Select s -> 
    LocalVar -> 
    This -> 
    Super -> 

let methode_verif m env = 
  match m with   

let binary_verif op e1 e2 env = 
  match op with 
    IntBinOp int_op -> (
      match int_op with 
      (* Comparaison *)
       | EQ -> 
       | NEQ -> 
       | LT -> 
       | LE -> 
       | GT -> 
       | GE -> 

      (* Arithmétique *)
       | PLUS -> e1.typ = "Integer" &&  e2.typ = "Integer"
                              && bf e1 && bf e2        
       | MINUS -> e1.typ = "Integer" &&  e2.typ = "Integer"
                              && bf e1 && bf e2 
       | TIMES -> e1.typ = "Integer" &&  e2.typ = "Integer"
                              && bf e1 && bf e2 
       | DIV -> e1.typ = "Integer" &&  e2.typ = "Integer"
                              && bf e1 && bf e2 
    )

let instr_verif instr env = 
  match instr with 
  | Exp(e) -> expr_verif e env
  | Block(b) -> block_verif b env
  | Ite(si, alors, sinon) ->  expr_verif si env; 
                              instr_verif alors env; 
                              instr_verif alors env
  | Return ->
  | Affectation(c,e) -> (* Vérification des types module heritage *)
    (* On doit vérifié que le type de e est un type ou sous type de c *)


let block_verif b env =
  



(*-----------------------------------------------------------------------------------------------
                              Fonction Utile pour les verifs 
-----------------------------------------------------------------------------------------------*)

(* Verifie qu'une variable est dans l'envrionnement *)
let in_env env e = 
  List.find(fun c -> c.name == e) env.decl_vars


**)
(**
  ____________________________________________
/         ------------°°°°------------        \
|             ANALYSE : Programme              
\ ___________________________________________ /
**)


let analyseProgram prog =
    let env = {
        decl_classes = prog.classes;
        decl_vars = [];
        is_correct_env = true
    }
    in
	List.fold_left (fun is_correct c -> (analyseClass env c) && is_correct) true prog.classes

