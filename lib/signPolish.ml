open DataTypes

(**
    --                  SIGNPOLISH.ML                   --
    -- Analyse statique du signe possible des variables --
    --         lors du déroulement du programme,        --
    --   détermination du risque de division par zéro.  --

    /!\ NON FINI !
*)

(*---------------------------------------------------------------------------*)

let sign_instr (pos : int) (ins:instr) (env : env_sign list) : env_sign_list = [] ;;

(**
    Prend un block (bloc de code), 
    ainsi qu'une env_sign list (liste de variables et infos les concernant)
    et crée une nouvelle liste à partir de la liste fournie,
    selon les variables rencontrées lors de la lecture du bloc de code. 
    Renvoie la nouvelle liste. 
 *)
let sign_block (b : block) (env : env_sign list): env_sign list = 
    let rec loop bl acc = match bl with
    | [] -> acc
    | (pos, ins) :: blr -> loop blr (sign_instr pos ins acc) : 
    in loop b env ;;

(**
    Prend un sign list (d'une variable) et renvoie le string attendu
    c'est à dire composé de -, +, 0, !,
    selon les valeurs finales possibles de la variable 
 *)
let varSign_to_string (vs : sign list) : string = 
    let rec loop acc tab = 
    match tab with 
    | [] -> acc
    | e :: l -> match e with
        | Neg -> loop (acc^"-") l 
        | Zero -> loop (acc^"0") l 
        | Pos -> loop (acc^"+") l 
        | Error(_)-> loop (acc^"!") l
in loop "" vs ;;

(**
    Prend un env_sign d'une variable,
    et si cette variable peut être la cause d'une erreur, 
    return la ligne où l'erreur se produit. 
    Sinon, un retour de -1 signifie que la variable ne peut pas causer d'erreurs
 *)
let error_line (env : env_sign) : int = match env with 
| [] -> -1
| Error(a) :: l -> a 
| e :: l -> error_line(l) ;;

(**
    Petite fonction auxiliaire prenant deux int, 
    et rendant : 
        si les deux sont -1 : rend -1
        si l'un des deux est positif : rend le positif
        si les deux sont positifs : rend le plus petit
    Utilisée pour trouver la 1ère ligne d'erreur si elle existe. 
 *)
let first_error_line (acc:int) (e:int) = if (e<>-1) && (e<acc) then e else acc ;;

(**
    Attend un env_sign list = liste d'infos collectés sur les variables,
    c-à-d leur nom et leurs valeurs finables possibles.
    Les print une par une et ligne par ligne.
    De plus, renvoie la 1è ligne où une erreur peut se produire, 
    sinon -1 si aucune erreur possible.
 *)
let print_env_sign_list (env : env_sign list) : int = 
let rec loop env acc = 
match env with 
| [] -> print_string "\n"; acc
| e :: l -> Printf.printf "%s %s\n" e.varName varSign_to_string(e.varSign) ; 
    loop l (first_error_line (acc error_line e))
in loop env -1;;

(**
    Attend un int correspondant à la 1è ligne où une erreur peut se produire.
    Print alors la ligne d'erreur avec divbyzero, ou sinon "safe" si aucune erreur possible
 *)
let print_error (posi:int) : unit = if (posi=-1) then print_string "Safe\n" 
    else Printf.printf "divbyzero %d\n" posi;;

(**
    Fonction de départ du fichier.
    Prend un programme (program), c'est-à-dire un bloc (block) de code,
    et utilise les fonctions annexes pour d'abord print toutes les variables du fichier,
    avec leur valeurs finales possibles (+ 0 - !)
    et termine par print la première ligne où une erreur risque de se produire, sinon "safe".
 *)

let sign_polish (p : program) : unit = print_error(print_env_sign_list (sign_block p [])) ;;

(**let print_env_sign_list (env : env_sign list) : unit = match env with 
| [] -> print_string "\n";
| e :: l -> Printf.printf "%s %s\n" e.varName varSign_to_string(e.varSign) ; 
    print_env_sign_list l ;;

let sign_polish (p : program) : unit = print_env_sign_list (sign_block p []) ;;**)

(** let sign_polish (p : program) : unit = match p with  _ -> ();; *)