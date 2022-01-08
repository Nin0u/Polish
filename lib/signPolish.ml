open DataTypes

(**
    --                  SIGNPOLISH.ML                   --
    -- Analyse statique du signe possible des variables --
    --         lors du déroulement du programme,        --
    --   détermination du risque de division par zéro.  --

    /!\ NON FINI !
*)

(*---------------------------------------------------------------------------*)

let union_signlist (sl1 : sign list) (sl2 : sign list) : sign list =
    

(**
    Prend un name et une env_sign list, et regarde si
    la liste contient un env_sign avec varName correspond à name.
    /!\ Pour des raisons de compatibilité avec d'autres fonctions,
    elle renvoie une liste contenant 1 élément : le env_list correspondant,
    et non le env_list directement
 *)
let rec look_for_varname (n:name) (el: env_sign list) : env_sign list = 
match el with
| [] -> []
| e :: elr -> if e.varName=n then [e] else look_for_varname n elr ;;

(**
    Prend une expr et une env_sign list, et rend une sign list.
    concrètement : analyse l'expression et en déduit le varSign (sign list) 
    de la variable pour qui cette fonction a été appellée 
 *)
let expr_to_signlist (pos : position) (e : expr) (env : env_sign list) : sign list = 
match e with 
| Num(z) -> let i = (Z.to_int z) in
    if i>0 then [Pos] else if i==0 then [Zero] else [Neg]
| Var(n) -> (match (look_for_varname n env) with
    | [var] -> var.varSign
    | _ -> raise (Var_never_initialized n) )
| Op(o,e1,o2) -> [] ;; (*TODO*)

(**
    Prend une position, une instr (ligne de code), et env_sign list
    rend la liste modifiée selon la ligne de code.
 *)
let matchsign_instr (pos : int) (ins:instr) (el : env_sign list) : env_sign list =
match ins with 
| Set(n,e) -> 
    (match look_for_varname n el with
    | [var] -> var.varSign <- (expr_to_signlist pos e el); el
    | _ -> {varName = n; varSign = (expr_to_signlist pos e el)} :: el)
| Read(n) -> el (*TODO*)
| Print(e) -> el (*TODO*)
| If (c,b1,b2) -> el (*TODO*)
| While (c,b) -> el (*TODO*)
;;

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
    | (pos, ins) :: blr -> loop blr (matchsign_instr pos ins acc)
    in loop b env ;;

(**
    Prend un sign list (d'une variable) et renvoie le string attendu
    c'est à dire composé de -, +, 0, !,
    selon les valeurs finales possibles de la variable 

    TODO : print dans le bon ordre : - 0 + ! 
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
let rec error_line (l : sign list) : int = match l with 
| [] -> -1
| Error(a) :: lr -> a 
| var :: lr -> error_line lr ;;

(**
    Petite fonction auxiliaire prenant deux int, 
    et rendant : 
        si les deux sont -1 : rend -1
        si l'un des deux est positif : rend le positif
        si les deux sont positifs : rend le plus petit
    Utilisée pour trouver la 1ère ligne d'erreur si elle existe. 
 *)
let first_error_line (acc:int) (e:int) : int = if (e<>(-1)) && (e<acc) then e else acc ;;

(**
    Attend un env_sign list = liste d'infos collectés sur les variables,
    c-à-d leur nom et leurs valeurs finables possibles.
    Les print une par une et ligne par ligne.
    De plus, renvoie la 1è ligne où une erreur peut se produire, 
    sinon -1 si aucune erreur possible.
 *)
let print_env_sign_list (env : env_sign list) : int = 
    let rec loop (env : env_sign list) (acc:int) : int = 
    match env with 
    | [] -> print_string "\n"; acc
    | e :: l -> 
        print_string (((e.varName^(" "))^varSign_to_string(e.varSign))^("\n")) ; 
        loop l (first_error_line acc (error_line e.varSign))
in loop env (-1);;

(**
    Attend un int correspondant à la 1è ligne où une erreur peut se produire.
    Print alors la ligne d'erreur avec divbyzero, ou sinon "safe" si aucune erreur possible
 *)
let print_error (posi:int) : unit = if (posi=(-1)) then print_string "Safe\n" 
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