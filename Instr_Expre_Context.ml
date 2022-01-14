
type environment = {
	decl_classes : class_def list;
	decl_vars : variable_def list;
	is_correct_env : bool
}
let emptyEnv = {decl_classes=[]; decl_vars=[]; is_correct_env=true}


let expr_verif expr env =
  match expr with 
  | IntLiteral i -> 
  | StringLiteral s -> 
  | Container c  -> container_verif c env
  | Method m -> methode_verif m env 
  | Binary (op,e1,e2) -> binary_verif op e1 e2 env 
  | Unary (u,e) -> 
  | Cast(s,e) -> 
  | NewClasse (s,e_list) -> 

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
       | PLUS -> typ e1 = Integer && typ e2 = IntLiteral 
                              && bf e1 && bf e2 
               
       | MINUS -> 
       | TIMES -> 
       | DIV -> 
    )

let instr_verif instr env = 
  match instr with 
  | 



(*-----------------------------------------------------------------------------------------------
                              Fonction Utile pour les verifs 
-----------------------------------------------------------------------------------------------*)

type typ = class_def

let t e = 
  match e with 
  | IntLiteral e -> 


