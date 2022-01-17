
type environment = {
	decl_classes : class_def list;
	decl_vars : variable_def list;
	is_correct_env : bool
}
let emptyEnv = {decl_classes=[]; decl_vars=[]; is_correct_env=true}


let expr_verif expr env =
  match expr with 
  | IntLiteral i -> (*add_env_var env i*)
  | StringLiteral s -> (*add_env_var env s*)
  | Container c  -> container_verif c env
  | Method m -> methode_verif m env 
  | Binary (op,e1,e2) -> binary_verif op e1 e2 env 
  | Unary (u,e) -> unary_verif u e env
  | Cast(s,e) -> 
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
  | Ite(si, alors, sinon) -> 
  | Return ->
  | Affectation(c,e) -> (* Vérification des types module heritage *)


let block_verif b env =



(*-----------------------------------------------------------------------------------------------
                              Fonction Utile pour les verifs 
-----------------------------------------------------------------------------------------------*)

(* Ajout d'une variable à l'environnement *)
let add_env_var env v = 
  let new = {
    decl_classes = env.decl_classes;
	  decl_vars = v::env.decl_vars;
    is_correct_env =
			env.is_correct_env && e.is_correct_env
  }

(* Ajout d'une classe à l'environnement *)
  let add_env_classe env c = 
    let new = {
      decl_classes = c::env.decl_classes;
		  decl_vars = env.decl_vars;
	  	is_correct_env =
			  env.is_correct_env &&
			  forbidClassName env.decl_classes c.name_class &&
            chckParamsInClaAndConstr c
	}
