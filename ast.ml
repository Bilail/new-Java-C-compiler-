(* Pour les type string et integer *)
type vartype = {
  | String of string 
  | Integer of int 
  }

type classType = {
  name : string; 
  superClasse : string option; 
  attribut : decl list; (* param + attribut *)
  meth : methode list;
  constructeur : methode;
  }

and decl = {
    typ : string;
    nom : string;
  }

and methode = {
  nom : string;
  param : decl list;
  instruction : instrType;
  static : bool;
  override : bool;
  retour : string option (* le type est un string ex : int est INTEGER *)
}


type opComp =
  Eq | Neq | Lt | Le | Gt | Ge

type instrType = 
    Exp of expType
  | Bloc of bloc
  | Ite of expType*instr*instr
  | Return 
  | Affectation of expType * expType
  

type bloc = {
  declarations : decl list;
  instructions : instr list;
}

type expType =
| Id of string (* vartype *)
| Cste of int (* vartype *)
| Plus of expType*expType
| Minus of expType*expType
| Times of expType*expType
| Div of expType*expType
| UMinus of expType
| Comp of opComp*expType*expType
| Selection of expType*Id (* Selection expression *)
| Instantiation of Id * listArgOpt (* Instanciation expression *)
| Cast of classType * expType (* (nomClasse expression) *)
| Envoi of (* envoie de message*)


exception VC_Error of string
exception RUN_Error of string
exception MISC_Error of string
