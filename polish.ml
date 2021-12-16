(** Projet Polish -- Analyse statique d'un mini-langage impératif *)
open Lib.DataTypes
open Lib.ReadPolish
open Lib.PrintPolish
open Lib.EvalPolish
open Lib.SimplPolish

let usage () =
  print_string "Polish : analyse statique d'un mini-langage\n";
  print_string "usage:\n";
  print_string "  --eval <fichier Polish> : évalue le programme Polish donné";
  print_string "  --reprint <fichier Polish> : réaffiche le programme Polish donné\n";
;;

let main () =
  match Sys.argv with
  | [|_;"--reprint";file|] -> print_polish (read_polish file)
  | [|_;"--eval";file|] -> eval_polish (read_polish file)
  | [|_;"--simpl";file|] -> print_polish (simpl_poish (read_poish file))
  | [|_;"--vars";file|] -> ()
  | [|_;"--sign";file|] -> ()
  | _ -> usage ()
;;

(* lancement de ce main *)
let () = main ()