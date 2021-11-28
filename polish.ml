(** Projet Polish -- Analyse statique d'un mini-langage impératif *)
open Lib.DataTypes
open Lib.ReadPolish
open Lib.PrintPolish
open Lib.EvalPolish

let usage () =
  print_string "Polish : analyse statique d'un mini-langage\n";
  print_string "usage: à documenter (TODO)\n"

let main () =
  match Sys.argv with
  | [|_;"--reprint";file|] -> print_polish (read_polish file)
  | [|_;"--eval";file|] -> eval_polish (read_polish file)
  | _ -> usage ()

(* lancement de ce main *)
let () = main ()
