open DataTypes

(**
    --                  SIGNPOLISH.ML                   --
    -- Analyse statique du signe possible des variables --
    --         lors du déroulement du programme,        --
    --   détermination du risque de division par zéro.  --

    /!\ NON TERMINE il manque un détail au IF, et WHILE 
*)

(*---------------------------------------------------------------------------*)

(**
    Prend deux sign list et renvoie l'union de leur champ Error dans une liste
    Si aucun n'a Error, rend []. Si l'un des deux a Error, le renvoie.
    Si les deux ont un Error, renvoie celui à l'int le plus petit.
 *)
let rec union_error (sl1 : sign list) (sl2 : sign list) : sign list =
match sl1 with
| (Error a) :: _ ->
    (match sl2 with 
    | (Error b) :: _ -> if a<b then [(Error a)] else [(Error b)]
    | _ :: lr2 -> union_error [(Error a)] lr2
    | [] -> [(Error a)] )
| _ :: lr1 -> union_error lr1 sl2
| [] ->
    (match sl2 with
    | (Error b) :: _ -> [(Error b)]
    | _ :: lr2 -> union_error [] lr2
    | [] -> [])
;;

(** 
    Prend deux sign list et en fait l'union 
    On évite les duplicata (si les deux listes ont Pos, le return a 1 Pos)
    Et pour Error, on garde le Error à valeur int la plus faible 
    (on veut montrer l'erreur le plus tôt possible)
 *)
let union_signlist (sl1 : sign list) (sl2 : sign list) : sign list =
    (if (List.mem Neg sl1) || (List.mem Neg sl2) then [Neg] else []) @
    (if (List.mem Zero sl1) || (List.mem Zero sl2) then [Zero] else []) @
    (if (List.mem Pos sl1) || (List.mem Pos sl2) then [Pos] else []) @
    (union_error sl1 sl2)
;;

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
| e :: elr -> if e.varName=n then [e] else look_for_varname n elr 
;;

(**
    NON TERMINE
    Fait l'union de deux env_sign list, utile par exemple suite à l'analyse 
    de deux blocs if
 *)
let union_envsignlist (_:env_sign list) (_:env_sign list) : env_sign list = []
(*  TODO finir derniers détails

let rec loop el1 el2 acc =
    match el1 with 
    | [] -> []
    | e :: lr1 ->
        let f = look_for_varname e.varName el2 in 
        match f with 
        |[] -> loop lr1 el2 
        |
        {varName=e.varName; 
        varSign=(union_signlist e.varSign (look_for_varname e.varName el2).varSign)} :: acc
in loop el1 el2 []*)
;;

(* ============================== CONTAINS ==============================*)

(**
    Prend une sign list, et teste si elle contient Neg
 *)
let rec contains_neg (sl : sign list) : bool =
match sl with
| Neg :: _ -> true
| _ :: lr -> contains_neg lr
| [] -> false
;;

(**
    Prend une sign list, et teste si elle contient Zero
 *)
let rec contains_zero (sl : sign list) : bool =
match sl with
| Zero :: _ -> true
| _ :: lr -> contains_zero lr
| [] -> false
;;

(**
    Prend une sign list, et teste si elle contient Pos
 *)
let rec contains_pos (sl : sign list) : bool =
match sl with
| Pos :: _ -> true
| _ :: lr -> contains_pos lr
| [] -> false
;;

(* ============================== OPERATIONS ==============================
    Les fonctions suivantes sont utilisées pour déterminer le signe
    après une opération (+, -, *, /, %)
*)

let mod_signlist (sl1 : sign list) (sl2 : sign list) (po:position) : sign list =
    (if ((contains_neg sl1) && (contains_neg sl2)) ||
        ((contains_pos sl1) && (contains_neg sl2)) then [Neg] else []) @

    (if ((contains_zero sl1) && (contains_pos sl2)) ||
        ((contains_zero sl1) && (contains_neg sl2)) then [Zero] else []) @

    (if ((contains_neg sl1) && (contains_pos sl2)) ||
        ((contains_pos sl1) && (contains_pos sl2)) then [Pos] else []) @

    (union_error (union_error sl1 sl2) 
        (if (contains_zero sl2) then [Error(po)] else []))
;;

let div_signlist (sl1 : sign list) (sl2 : sign list) (po:position) : sign list =
    (if ((contains_neg sl1) && (contains_pos sl2)) ||
        ((contains_pos sl1) && (contains_neg sl2)) then [Neg] else []) @

    (if ((contains_zero sl1) && (contains_pos sl2)) ||
        ((contains_zero sl1) && (contains_neg sl2)) then [Zero] else []) @

    (if ((contains_neg sl1) && (contains_neg sl2)) ||
        ((contains_pos sl1) && (contains_pos sl2)) then [Pos] else []) @

    (union_error (union_error sl1 sl2) 
        (if (contains_zero sl2) then [Error(po)] else []))
;;

let mul_signlist (sl1 : sign list) (sl2 : sign list) : sign list =
    (if ((contains_neg sl1) && (contains_pos sl2)) ||
        ((contains_pos sl1) && (contains_neg sl2)) then [Neg] else []) @

    (if ((contains_zero sl1) || (contains_zero sl2)) then [Zero] else []) @

    (if ((contains_neg sl1) && (contains_neg sl2)) ||
        ((contains_pos sl1) && (contains_pos sl2)) then [Pos] else []) @

    (union_error sl1 sl2)
;;

let sub_signlist (sl1 : sign list) (sl2 : sign list) : sign list =
    (if (contains_neg sl1) || (contains_pos sl2) then [Neg] else []) @

    (if ((contains_zero sl1) && (contains_zero sl2)) || 
        ((contains_neg sl1) && (contains_pos sl2)) ||
        ((contains_pos sl1) && (contains_neg sl2)) then [Zero] else []) @
    
    (if (contains_pos sl1) || (contains_neg sl2) then [Pos] else []) @

    (union_error sl1 sl2)    
;;

let add_signlist (sl1 : sign list) (sl2 : sign list) : sign list =
    (if (contains_neg sl1) || (contains_neg sl2) then [Neg] else []) @

    (if ((contains_zero sl1) && (contains_zero sl2)) || 
        ((contains_neg sl1) && (contains_pos sl2)) ||
        ((contains_pos sl1) && (contains_neg sl2)) then [Zero] else []) @

    (if (contains_pos sl1) || (contains_pos sl2) then [Pos] else []) @

    (union_error sl1 sl2)    
;;

(* ============================== EXPR ==============================*)

(**
    Prend une expr et une env_sign list, et rend une sign list.
    concrètement : analyse l'expression et en déduit le varSign (sign list) 
    de la variable pour qui cette fonction a été appellée 

    TODO : si l'une des variables concernée n'a QUE Error et ni pos ni neg ni zero, 
        pour match |Op il faudra rendre Error uniquement
        (((1 + Error = [Error], et pas [Pos,Error])))
 *)
let rec expr_to_signlist (po : position) (e : expr) (env : env_sign list) : sign list = 
match e with 
| Num(z) -> let i = (Z.to_int z) in
    if i>0 then [Pos] else if i==0 then [Zero] else [Neg]
| Var(n) -> (match (look_for_varname n env) with
    | [var] -> var.varSign
    | _ -> raise (Var_never_initialized n) )
| Op(o,e1,e2) -> (match o with 
    | Add -> add_signlist (expr_to_signlist po e1 env) (expr_to_signlist po e2 env)
    | Sub -> sub_signlist (expr_to_signlist po e1 env) (expr_to_signlist po e2 env)
    | Mul -> mul_signlist (expr_to_signlist po e1 env) (expr_to_signlist po e2 env)
    | Div -> div_signlist (expr_to_signlist po e1 env) (expr_to_signlist po e2 env) po
    | Mod -> mod_signlist (expr_to_signlist po e1 env) (expr_to_signlist po e2 env) po )
;; 

(* ============================== COND ==============================*)
(* TODO : propagation du cond *)
(* TODO : si sl1 ou sl2 est [Error] et rien d'autre tout devrait être false*)

(**
    Vérifie si la condition est possible (true) ou jamais possible (false)
 *)
let sign_cond (po:position) ((e1,com,e2) : cond) (env : env_sign list) : bool =
let (sl1, sl2) = ((expr_to_signlist po e1 env),(expr_to_signlist po e2 env)) in
match com with 
| Eq -> if ((contains_neg sl1) && (contains_neg sl2)) ||
        ((contains_zero sl1) && (contains_zero sl2)) ||
        ((contains_pos sl1) && (contains_pos sl2)) 
        then true else false

| Ne -> if (not (contains_pos sl1)) && (not (contains_pos sl1)) &&
        (not (contains_neg sl1)) && (not (contains_neg sl1))
        then false else true

| Lt -> if (contains_neg sl1) ||
        (((contains_zero sl1) || (contains_pos sl1)) && (contains_pos sl2))
        then true else false

| Le -> if (contains_neg sl1) ||
        ((contains_zero sl1) && ((contains_zero sl2) || contains_pos sl2)) ||
        ((contains_pos sl1) && (contains_pos sl2))
        then true else false

| Gt -> if (contains_neg sl2) ||
        (((contains_zero sl2) || (contains_pos sl2)) && (contains_pos sl1))
        then true else false

| Ge -> if (contains_neg sl2) ||
        ((contains_zero sl2) && ((contains_zero sl1) || contains_pos sl1)) ||
        ((contains_pos sl2) && (contains_pos sl1))
        then true else false
;;

(**
    Prend une condition et renvoie la condition "inverse".
 *)
let reverse_comp (c:comp) =
match c with 
| Eq -> Ne
| Ne -> Eq
| Lt -> Ge
| Le -> Gt
| Gt -> Le
| Ge -> Lt
;;

(* ============================== IF ==============================*)

(**
    NON TERMINEE
    Fonction mettant à jour l'environnement suite à un IF et ses blocs.
 *)
let rec sign_if (po:position) ((e1,c,e2):cond) 
            (b1:block) (b2:block) (env : env_sign list) : env_sign list =
if (sign_cond po (e1,c,e2) env) then
    if (sign_cond po (e1,(reverse_comp c),e2) env)  
        then env (* en attendant d'avoir écrit union_envsignlist *)
        else sign_block b1 env
else if (sign_cond po (e1,(reverse_comp c),e2) env)
    then sign_block b2 env
    else env

(* ============================== autre ==============================*)

(**
    Prend une position, une instr (ligne de code), et env_sign list
    rend la liste modifiée selon la ligne de code.
 *)
and matchsign_instr (pos : int) (ins:instr) (el : env_sign list) : env_sign list =
match ins with 
| Set(n,e) -> 
    (match look_for_varname n el with
    | [var] -> var.varSign <- (expr_to_signlist pos e el); el
    | _ -> {varName = n; varSign = (expr_to_signlist pos e el)} :: el)
| Read(n) ->
    (match look_for_varname n el with
    | [var] -> var.varSign <- Neg :: Zero :: Pos :: [] ; el
    | _ -> {varName = n; varSign = (Neg :: Zero :: Pos :: [])} :: el)
| Print(_) -> el
| _ -> el (*TODO : If(presque terminé) et While*)

(**
    Prend un block (bloc de code), 
    ainsi qu'une env_sign list (liste de variables et infos les concernant)
    et crée une nouvelle liste à partir de la liste fournie,
    selon les variables rencontrées lors de la lecture du bloc de code. 
    Renvoie la nouvelle liste. 
 *)
and sign_block (b : block) (env : env_sign list): env_sign list = 
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
| Error(a) :: _ -> a 
| _ :: lr -> error_line lr ;;

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