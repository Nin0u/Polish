(** Syntaxe abstraite Polish (types imposés, ne pas changer sauf extensions) *)

(** Position : numéro de ligne dans le fichier, débutant à 1 *)
type position = int

(** Nom de variable *)
type name = string

(** Opérateurs arithmétiques : + - * / % *)
type op = Add | Sub | Mul | Div | Mod

(** 
  Expressions arithmétiques 

  Le type que contient Num a été changé pour implémenter
  l'évaluation avec zarith.
*)
type expr =
  | Num of Z.t
  | Var of name
  | Op of op * expr * expr

(** Opérateurs de comparaisons *)
type comp =
| Eq (* = *)
| Ne (* Not equal, <> *)
| Lt (* Less than, < *)
| Le (* Less or equal, <= *)
| Gt (* Greater than, > *)
| Ge (* Greater or equal, >= *)

(** Condition : comparaison entre deux expressions *)
type cond = expr * comp * expr

(** Instructions *)
type instr =
  | Set of name * expr
  | Read of name
  | Print of expr
  | If of cond * block * block
  | While of cond * block
and block = (position * instr) list

(** Un programme Polish est un bloc d'instructions *)
type program = block


(** 
  Structure de l'environnement lors de l'évaluation 

  on utilise une donnée mutable pour éviter de créer et renvoyer
  de nouveau environnements à chaque évaluation d'instruction.  
*)
type env = {
  varName : name;
  mutable value : Z.t
}

type sign =
| Neg
| Zero
| Pos
| Error of int

type env_sign = {
  varName : name;
  mutable varSign : sign list;
}

(**
  Structure utilisée pour varsPolish
*)
module Names = Set.Make(String)

(** 
  EXCEPTIONS UTILES
  chaque exception ayant un int conserve la ligne où s'est produite l'erreur.
*)

(** Exceptions liées au parsing *)
exception Not_an_expression of position
exception Not_a_condition of position
exception Not_an_operation of position
exception Not_an_instr of position
exception Set_error of position
exception Arguments_error of position
exception Indentation_error of position

(** Exceptions d'environnement *)
exception Varname_already_exists of int
exception No_such_varName of name * int
exception Var_never_initialized of name

(** Exceptions Arithmétiques *)
exception Division_by_zero of int
exception Modulo_by_zero of int