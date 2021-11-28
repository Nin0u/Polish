(** 
  Ce fichier teste des fonctions utilisées dans readPolish
*)

open FileNames
open Lib.DataTypes
open Lib.ReadPolish

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
  TESTS POUR LA FONCTION indentAndSplit
 *)

let rec print_list (list : string list) : unit =
  match list with
  | [] -> ()
  | head :: [] ->
    print_string head;
    print_string "]\n"
  | head :: tail -> 
    print_string head;
    print_string ";";
    print_list tail;
;;

let rec printIndentAndSplit (filename : string) (lines : (position * string) list) : unit =
  match lines with
  | [] -> Printf.printf "%s Done.\n\n" filename
  | (_,str) :: otherLines -> 
    let (indent,split) = indentAndSplit str
    in 
      print_int indent;
      print_string " : [";
      print_list split; 
      print_string "\n";
      printIndentAndSplit filename otherLines
;;

let rec testIndentAndSplit (fileList : string list) : unit =
  match fileList with
  | [] -> print_string ("testGetIndent Done.\n")
  | filename :: otherFiles ->  
    printIndentAndSplit filename (read filename); 
    testIndentAndSplit otherFiles
;;

(*--------------------------------------------------------------------------------------------------*)

(**
  TEST POUR LA FONCTION readExpression
 *)

let expressions = [
  [];                                         (*  NO *)
  ["%"];                                      (*  NO *)
  ["1"];                                      (* YES *)
  ["a"];                                      (* YES *)
  ["+";"+";"1";"2";"3"];                      (* YES *)
  ["*";"/";"n";"3";"b"];                      (* YES *)
  ["%";"+";"a";"*";"3";"n";"2"];              (* YES *)
  ["+";"*";"%";"4";"2";"n";"*";"1";"3"];      (* YES *)
  ["+";"*";"%";"4";"2";"n";"*";"1";"3";"B"];  (*  NO *) 
]
;;

let rec testReadExpression (expressions : (string list) list) : unit = 
  match expressions with 
  | [] -> print_string "testReadExpression Done.\n"
  | head :: tail -> 
    try
      match readExpression (head) with 
      | _ -> print_string "YES\n"; testReadExpression tail
    with Not_an_expression _-> print_string "NO\n"; testReadExpression tail
;;

(*--------------------------------------------------------------------------------------------------*)

(** 
  TEST POUR LA FONCTION readCondition
 *)
 let conditions = [
    ["1";"=";"1"];                                      (* YES *)
    ["+";"x";"y";"<=";"+";"a";"b"];                     (* YES *)
    ["%";"3";"y";"<=";"/";"10";"2"];                    (* YES *)
    ["x";"y"];                                          (*  NO *) 
    [];                                                 (*  NO *)
    ["x";"+";"1";"=";"2"];                              (*  NO *)
    ["2";"=";"x";"+";"1"];                              (*  NO *)
    ["+";"*";"%";"4";"2";"n";"*";"1";"3";"<=";"0"]      (* YES *)
 ]
 ;;

let rec testReadCondition (conditions : (string list) list) : unit = 
  match conditions with 
  | [] -> print_string "testReadCondition Done.\n"
  | head :: tail -> 
    try
      match readCondition (head) with 
      | _ -> print_string "YES\n"; testReadCondition tail
    with
      | Not_an_expression  _ | Not_a_condition _ -> 
        print_string "NO\n"; testReadCondition tail
;;

(*--------------------------------------------------------------------------------------------------*)

(** 
  TEST POUR LA FONCTION readSet
 *)

 let set = [
    [];                                 (*  NO *)
    [""];                               (*  NO *)
    ["1"];                              (*  NO *)
    ["a"];                              (*  NO *)
    [":="];                             (*  NO *)
    [":=";"b"];                         (*  NO *)
    ["a";":="];                         (*  NO *)
    ["a";":=";"b"];                     (* YES *)
    ["a";":=";"1"];                     (* YES *)
    ["1";":=";"3"];                     (*  NO *)
    ["+";"n";"1";":=";"*";"3";"2"];     (*  NO *)
    ["a";":=";"+";"n";"1"]              (* YES *)
 ]

 let rec testReadSet (set : (string list) list) : unit = 
  match set with 
  | [] -> print_string "testReadSet Done.\n"
  | head :: tail -> 
    try
      match readSet (head) with 
      | _ -> print_string "YES\n"; testReadSet tail
    with
      | Not_an_expression _ | Set_error _ ->
        print_string "NO\n"; testReadSet tail
;;

(*--------------------------------------------------------------------------------------------------*)
(** 
  TEST de read_polish 
*)

(**
  Affiche le nombre de commandes dans le bloc principal.
*)
let rec testReadPolish (example_files : string list) : unit =
  
    match example_files with
    | [] -> print_string "testReadPolish Done.\n"
    | head :: tail ->
      try
        print_int (List.length (read_polish head));
        print_string "\n";
        testReadPolish tail
      with
      Arguments_error pos -> 
        Printf.printf "Arguments_error at line %d\n" pos;
        testReadPolish tail
 
;;

(** On testera la justesse du parsing avec print_polish *)


(**
  Execute tous les tests.
*)
let allTests : unit = 
  testRead example_files;
  testIndentAndSplit example_files;
  testReadExpression expressions;
  testReadCondition conditions;
  testReadSet set;
  testReadPolish example_files
;;

(*--------------------------------------------------------------------------------------------------*)

(**
  MAIN DU FICHIER TEST
 *)

let main() = 
    match Sys.argv with
  | [|_;"read"|] -> testRead example_files
  | [|_;"indentAndSplit"|] -> testIndentAndSplit example_files
  | [|_;"readExpression"|] -> testReadExpression expressions
  | [|_;"readCondition"|] -> testReadCondition conditions
  | [|_;"readSet"|] -> testReadSet set
  | [|_;"read_polish"|] -> testReadPolish example_files
  | [|_;"all"|] -> allTests
  | _ -> 
    print_string 
      "Unknown function.\n
        List of available arguments :\n
        -read : prints the number and the content of read lines.\n
        -indentAndSplit : prints the indent and the list of words without spaces.\n
        -readExpression : prints YES if the tested list is an expression. Prints NO otherwise.\n
        -readCondition : prints YES if the tested list is a condition. Prints NO otherwise. \n
        -readSet : prints YES if the tested list is a SET. Prints NO otherwise.\n
        -read_polish : prints the number of commands in the main block.\n
        -all : do all the tests.\n
      \n"

(** Lancement du main *)
let () = main ();;