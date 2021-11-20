(** Syntaxe abstraite Polish (types imposés, ne pas changer sauf extensions) *)

(** Position : numéro de ligne dans le fichier, débutant à 1 *)
type position = int

(** Nom de variable *)
type name = string

(** Opérateurs arithmétiques : + - * / % *)
type op = Add | Sub | Mul | Div | Mod

(** Expressions arithmétiques *)
type expr =
  | Num of int
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

(** EXCEPTIONS UTILES *)
exception Not_an_expression (* Pour readExpression dans readPolish.ml *)
exception Not_a_condition   (* Pour readCondition dans readPolish.ml*)

(* 
  Pour parseLine et parseBlock 
  La chaîne désigne le nom de l'erreur 
  L'entier désigne la ligne à laquelle s'est produite l'erreur
*)
exception Parsing_error of (string * int) 

(** 
  Est levée quand une erreur se produit à l'affectation
  se référer à la foncton readSet de readPolish
*)
exception Set_error
