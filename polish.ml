(** Projet Polish -- Analyse statique d'un mini-langage impératif *)
open Lib.ReadPolish
open Lib.PrintPolish
open Lib.EvalPolish
open Lib.SimplPolish
open Lib.VarsPolish
open Lib.SignPolish

let usage () =
  print_string "Polish : analyse statique d'un mini-langage\n";
  print_string "usage:\n";
  print_string "  --reprint <fichier Polish> : réaffiche le programme Polish donné.\n";
  print_string "  --eval <fichier Polish> : évalue le programme Polish donné.\n";
  print_string "  --simp <fichier Polish> : Simplification du programme en effectuant une propagation des constantes et en éliminant les blocs “morts”.\n";
  print_string "  --vars <fichier Polish> : Calcul statique des variables risquant d’être accédées avant d’être écrites.\n";
  print_string "  --sign <fichier Polish> : Analyse statique du signe possible des variables lors du déroulement du programme, détermination du risque de division par zéro.\n\n"
;;

let main () =
  match Sys.argv with
  | [|_;"--reprint";file|] -> print_polish (read_polish file)
  | [|_;"--eval";file|] -> eval_polish (read_polish file)
  | [|_;"--simpl";file|] -> print_polish (simpl_polish (read_polish file))
  | [|_;"--vars";file|] -> vars_polish(read_polish file)
  | [|_;"--sign";file|] -> sign_polish(read_polish file)
  | _ -> usage ()
;;

(* lancement de ce main *)
let () = main ()