type classe = {
  nom : string; 
  superClasse : string;
  attribut : decl list;
  methode : expType list;
  constructeur : expType;
  }

type opComp =
  Eq | Neq | Lt | Le | Gt | Ge

type instr = 
    Exp of expType
  | Bloc of bloc
  | Ite of expType*instr*instr

type bloc = {
  declarations : decl list;
  instructions : instr list;
}

type expType =
  Id of string
| Cste of int
| Plus of expType*expType
| Minus of expType*expType
| Times of expType*expType
| Div of expType*expType
| UMinus of expType
| Comp of opComp*expType*expType

| Selection of expType*classe (* Selection expression *)
| Instanciation of classe * listArg (* Instanciation expression *)

type listArg = 
 | Param of decl
 
type decl = {
    typ : string;
    nom : string;
  }


type progType = decl list*expType
type listeparametre = param list;
type param = listArg list;
type listArg =  




exception VC_Error of string
exception RUN_Error of string
exception MISC_Error of string
