(** 
  Ce fichier teste des fonctions utilisées dans readPolish
*)

open FileNames
open Lib.ReadPolish
open Lib.DataTypes

(** 
  TESTS POUR LA FONCTION read
 *)
let rec printLines (filename : string) (lines : (position * string) list) : unit =
  match lines with
        | [] -> Printf.printf "%s Read.\n\n" filename;
        | (pos,str) :: otherLines -> Printf.printf "%d,%s\n" pos str; printLines filename otherLines
;;

let rec testRead (fileList: string list) : unit = 
    match fileList with 
    | [] -> print_string("testRead Done.\n")
    | filename :: otherFiles-> printLines filename (read filename); testRead otherFiles
;;

(*--------------------------------------------------------------------------------------------------*)

(** 
  TESTS POUR LA FONCTION getIndent
 *)
let rec printIndent (filename : string) (lines : (position * string) list) : unit =
  match lines with
  | [] -> Printf.printf "%s Done.\n\n" filename;
  | (pos,str) :: otherLines -> Printf.printf "%d\n" (getIndent (String.split_on_char ' ' str)); printIndent filename otherLines
;;

let rec testGetIndent (fileList : string list) : unit =
  match fileList with
  | [] -> print_string ("testGetIndent Done.\n")
  | filename :: otherFiles ->  printIndent filename (read filename); testGetIndent otherFiles
;;

(*--------------------------------------------------------------------------------------------------*)

(**
  TEST POUR LA FONCTION toExpression
 *)

let expressions = [
  ["+";"+";"1";"2";"3"];          (* YES *)
  [];                             (*  NO *)
  ["*";"/";"n";"3";"b"];          (* YES *)
  ["%"];                          (*  NO *)
  ["%";"+";"a";"*";"3";"n";"2"]   (*  NO *)

]
;;

let rec testToExpression (expressions : (string list) list) : unit = 
  match expressions with 
  | [] -> print_string "testToExpression Done.\n"
  | head :: tail -> 
    try
      match toExpression (head) with 
      | _ -> print_string "YES\n"; testToExpression tail
    with Not_an_expression -> print_string "NO\n"; testToExpression tail
;;

(*--------------------------------------------------------------------------------------------------*)

(** 
  TEST POUR LA FONCTION toCondition
 *)
 let conditions = [
    ["1";"=";"1"];                      (* YES *)
    ["+";"x";"y";"<=";"+";"a";"b"];     (* YES *)
    ["%";"3";"y";"<=";"/";"10";"2"];    (* YES *)
    ["x";"y"];                          (*  NO *) 
    [];                                 (*  NO *)
    ["x";"+";"1";"=";"2"];              (*  NO *)
    ["2";"=";"x";"+";"1"];              (*  NO *)
 ]
 ;;

let rec testToCondition (conditions : (string list) list) : unit = 
  match conditions with 
  | [] -> print_string "testToCondition Done.\n"
  | head :: tail -> 
    try
      match toCondition (head) with 
      | _ -> print_string "YES\n"; testToCondition tail
    with Not_a_condition -> print_string "NO\n"; testToCondition tail
;;

(**
  MAIN DU FICHIER TEST
 *)

let main() = 
    match Sys.argv with
  | [|_;"read"|] -> testRead example_files
  | [|_;"indent"|] -> testGetIndent example_files
  | [|_;"toExpression"|] -> testToExpression expressions
  | [|_;"toCondition"|] -> testToCondition conditions
  | _ -> print_string 
  "Unknown function.\n
    List of testable functions :\n
    -read \n
    -indent \n
    -toExpression \n
    -toCondition
  \n"
;;

let () = main ();;