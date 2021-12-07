(** 
  Ce fichier teste des fonctions utilisées dans evalPolish

  Remarque : 
    Ce fichier ne teste que applyOp pour tester
    les Divisions par 0 et les modulo 0.

    Le bon fonctionnement des autres fonctions et
    d'eval_polish seront testées directement
    en exécutant polish.ml avec l'argument --eval
*)

open Lib.DataTypes
open Lib.EvalPolish

(**
    TEST POUR applyOp
*)
let testApplyOp (exec : int) =
    print_string "1+1 = ";
    Printf.printf "%d\n" (applyOp Add 1 1 exec);

    print_string "1-1 = ";
    Printf.printf "%d\n" (applyOp Sub 1 1 exec);

    print_string "1*1 = ";
    Printf.printf "%d\n" (applyOp Mul 1 1 exec);

    print_string "1/1 = ";
    Printf.printf "%d\n" (applyOp Div 1 1 exec);
    
    print_string "10%7 = ";
    Printf.printf "%d\n" (applyOp Mod 10 7 exec);

    print_string "1/2 = ";
    Printf.printf "%d\n" (applyOp Div 1 2 exec);

    try 
        print_string "1/0 = ";
        Printf.printf "%d\n" (applyOp Div 1 0 exec);
    with Division_by_zero _ -> 
        print_string "Division_by_zero\n";
        try 
            print_string "20%0 = ";
            Printf.printf "%d\n" (applyOp Mod 10 0 exec);
        with Modulo_by_zero _ -> print_string "Modulo_by_zero\n"
;;


(*---------------------------------------------------------------------------*)

(**
  MAIN DU FICHIER TEST
 *)

let main() = 
    testApplyOp 0
;;

(** Lancement du main *)
let () = main ();;