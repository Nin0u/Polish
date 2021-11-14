(** Ce fichier test des fonctions read *)

open FileNames
open Lib.ReadPolish
open Lib.DataTypes

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

(**
let testParseLines = failwith "TODO" 

let restReadPolish = failwith "TODO"
*)

let main() = 
    match Sys.argv with
  | [|_;"read"|] -> testRead example_files
  | _ -> print_string "Unknown function name"
;;

let () = main ();;