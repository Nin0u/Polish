open DataTypes

(**
    --                  SIGNPOLISH.ML                   --
    -- Analyse statique du signe possible des variables --
    --         lors du déroulement du programme,        --
    --   détermination du risque de division par zéro.  --

    /!\ La propagation n'a pas été implémentée
*)

(*---------------------------------------------------------------------------*)

(** 
    Recupère le signe Error d'une sign list
    Type option au cas où sl n'a pas Error
*)
let rec get_error (sl : sign list) : sign option =
    match sl with
    | [] -> None
    | Error(n) :: _ -> Some(Error(n))
    | _ :: r -> get_error r 
;;

(**
    Prend deux sign list et renvoie l'union de leur champ Error dans une liste
    Si aucun n'a Error, rend []. Si l'un des deux a Error, le renvoie.
    Si les deux ont un Error, renvoie celui à l'int le plus petit.
 *)
let union_error (sl1 : sign list) (sl2 : sign list) : sign list =
    match get_error sl1, get_error sl2 with
    | Some(Error(n)),Some(Error(m)) -> 
        if n > m 
        then [Error(m)]
        else [Error(n)]
    | Some(e), _ | _ , Some(e) -> [e]
    | _ ,_ -> []
;;

(** 
    Vérifie si une sign list contient un sign s 
    Error(n) et Error(m) sont considérés comme différents
*)
let rec contains_sign (sl : sign list) (s : sign) : bool =
    match sl with
    | [] -> false
    | si :: lr -> 
        if si = s
        then true
        else contains_sign lr s
;;

(** 
    Prend deux sign list et en fait l'union sans doublons.
    Pour Error, on garde le Error à valeur int la plus faible.
 *)
let union_signlist (sl1 : sign list) (sl2 : sign list) : sign list =
    let rec loop acc sl2 =
        match sl2 with
        | [] -> acc
        | s :: r -> 
            if contains_sign acc s
            then loop acc r
            else
                match s,get_error acc with
                | Error(n),Some(Error(m)) ->
                    if n < m
                    then loop (s :: (List.filter (fun x->x != Error(m)) acc)) r
                    else loop acc r
                | _,_ -> loop (s :: acc) r
            
    in loop sl1 sl2
;;

(** Vérifie l'égalité de deux sign list *)
let rec signlist_equals (sl1 : sign list) (sl2 : sign list) : bool =
    match sl1 with
    | [] -> sl2 = []
    | s1 :: r -> 
        match List.find_opt (fun x-> x = s1) sl2 with
        | None -> false
        | Some(_) ->
            let sl3 = (List.filter (fun x-> x != s1) sl2)
            in signlist_equals r sl3
;;


(**
    Fait l'union de deux env_sign list, utile par exemple suite à l'analyse 
    d'un if/else
 *)
let union_envsignlist (el1 :env_sign list) 
                        (el2 :env_sign list) 
                        : env_sign list =
    let rec loop acc el2  =
        match el2 with 
        | [] -> acc
        | e :: lr ->
            match List.find_opt (fun x-> x.varName = e.varName) acc with 
            | None -> loop (e :: acc) lr
            | Some(sl) ->
                sl.varSign <-(union_signlist sl.varSign e.varSign);
                loop acc lr
    in loop el1 el2
;;

(** Vérifie l'égalité de deux env_sign list *)
let rec env_equals (env1 : env_sign list) 
                    (env2 : env_sign list)
                    : bool =
    match env1 with
    | [] -> env2 = []
    | s1 :: r -> 
        match List.find_opt (fun x-> x.varName = s1.varName) env2 with
        | None -> false
        | Some(s2) ->
            if  signlist_equals (s1.varSign) (s2.varSign)
            then 
                let env3 = (List.filter (fun x-> x.varName != s1.varName) env2)
                in env_equals r env3
            else false
;;

(* ============================== OPERATIONS ==============================
    Les fonctions suivantes sont utilisées pour déterminer le signe
    après une opération (+, -, *, /, %)
*)

let mod_signlist (sl1 : sign list) (sl2 : sign list) (po : position) : sign list =
    (if ((contains_sign sl1 Neg) && (contains_sign sl2 Neg)) ||
        ((contains_sign  sl1 Pos) && (contains_sign sl2 Neg)) then [Neg] else []) @

    (if ((contains_sign sl1 Zero) && (contains_sign sl2 Pos)) ||
        ((contains_sign sl1 Zero) && (contains_sign sl2 Neg)) then [Zero] else []) @

    (if ((contains_sign sl1 Neg) && (contains_sign sl2 Pos)) ||
        ((contains_sign sl1 Pos) && (contains_sign sl2 Pos)) then [Pos] else []) @

    (union_error (union_error sl1 sl2) 
        (if (contains_sign sl2 Zero) then [Error(po)] else []))
;;

let div_signlist (sl1 : sign list) (sl2 : sign list) (po : position) : sign list =
    (if ((contains_sign sl1 Neg) && (contains_sign sl2 Pos)) ||
        ((contains_sign sl1 Pos) && (contains_sign sl2 Neg)) then [Neg] else []) @

    (if ((contains_sign sl1 Zero) && (contains_sign sl2 Pos)) ||
        ((contains_sign sl1 Zero) && (contains_sign sl2 Neg)) then [Zero] else []) @

    (if ((contains_sign sl1 Neg) && (contains_sign sl2 Neg)) ||
        ((contains_sign sl1 Pos) && (contains_sign sl2 Pos)) then [Pos] else []) @

    (union_error (union_error sl1 sl2) 
        (if (contains_sign sl2 Zero) then [Error(po)] else []))
;;

let mul_signlist (sl1 : sign list) (sl2 : sign list) : sign list =
    (if ((contains_sign sl1 Neg) && (contains_sign sl2 Pos)) ||
        ((contains_sign sl1 Pos) && (contains_sign sl2 Neg)) then [Neg] else []) @

    (if ((contains_sign sl1 Zero) || (contains_sign sl2 Zero)) then [Zero] else []) @

    (if ((contains_sign sl1 Neg) && (contains_sign sl2 Neg)) ||
        ((contains_sign sl1 Pos) && (contains_sign sl2 Pos)) then [Pos] else []) @

    (union_error sl1 sl2)
;;

let sub_signlist (sl1 : sign list) (sl2 : sign list) : sign list =
    (if (contains_sign sl1 Neg) || (contains_sign sl2 Pos) then [Neg] else []) @

    (if ((contains_sign sl1 Zero) && (contains_sign sl2 Zero)) || 
        ((contains_sign sl1 Neg) && (contains_sign sl2 Pos)) ||
        ((contains_sign sl1 Pos) && (contains_sign sl2 Neg)) then [Zero] else []) @
    
    (if (contains_sign sl1 Pos) || (contains_sign sl2 Neg) then [Pos] else []) @

    (union_error sl1 sl2)    
;;

let add_signlist (sl1 : sign list) (sl2 : sign list) : sign list =
    (if (contains_sign sl1 Neg) || (contains_sign sl2 Neg) then [Neg] else []) @

    (if ((contains_sign sl1 Zero) && (contains_sign sl2 Zero)) || 
        ((contains_sign sl1 Neg) && (contains_sign sl2 Pos)) ||
        ((contains_sign sl1 Pos) && (contains_sign sl2 Neg)) then [Zero] else []) @

    (if (contains_sign sl1 Pos) || (contains_sign sl2 Pos) then [Pos] else []) @

    (union_error sl1 sl2)    
;;

(* ============================== EXPR ==============================*)

(**
    Prend une expr et une env_sign list, et rend une sign list.
    concrètement : analyse l'expression et en déduit le varSign (sign list) 
    de la variable pour qui cette fonction a été appellée 
 *)
let rec expr_to_signlist (po : position) 
                        (e : expr) 
                        (env : env_sign list) 
                        : sign list = 
    match e with 
    | Num(z) ->
        if Z.gt z Z.zero
        then [Pos] 
        else 
            if Z.equal z Z.zero
            then [Zero] 
            else [Neg]
    | Var(n) -> 
        (match List.find_opt (fun x-> x.varName = n) env with
        | Some(s) -> s.varSign
        | None -> raise (Var_never_initialized n))
    | Op(o,e1,e2) -> 
        let sl1 = (expr_to_signlist po e1 env) in
        let sl2 = (expr_to_signlist po e2 env) in
        match sl1,sl2 with
        | [Error(n)],[Error(m)] -> 
            if n > m 
            then [Error(n)] 
            else [Error(m)]
        | [Error(n)], _-> [Error(n)]
        | _ , [Error(m)] -> [Error(m)]
        | _ , _ ->
            match o with
            | Add -> add_signlist sl1 sl2
            | Sub -> sub_signlist sl1 sl2
            | Mul -> mul_signlist sl1 sl2
            | Div -> div_signlist sl1 sl2 po
            | Mod -> mod_signlist sl1 sl2 po 
    ;; 

(* ============================== COND ==============================*)

(**
    Vérifie si la condition est possible (true) ou jamais possible (false)
 *)
let sign_cond (po:position) ((e1,com,e2) : cond) (env : env_sign list) : bool =
    let sl1 = expr_to_signlist po e1 env in
    let sl2 = expr_to_signlist po e2 env in
    match sl1,sl2 with
        | [Error(_)], _-> false
        | _ , [Error(_)] -> false
        | _ , _ ->
            match com with 
            | Eq -> ((contains_sign sl1 Neg) && (contains_sign sl2 Neg)) ||
                    ((contains_sign sl1 Zero) && (contains_sign sl2 Zero)) ||
                    ((contains_sign sl1 Pos) && (contains_sign sl2 Pos)) 

            | Ne -> not( (not (contains_sign sl1 Pos)) && (not (contains_sign sl1 Pos)) &&
                    (not (contains_sign sl1 Neg)) && (not (contains_sign sl1 Neg)))

            | Lt -> (contains_sign sl1 Neg) ||
                    (((contains_sign sl1 Zero) || (contains_sign sl1 Pos)) && (contains_sign sl2 Pos))

            | Le -> (contains_sign sl1 Neg) ||
                    ((contains_sign sl1 Zero) && ((contains_sign sl2 Zero) || contains_sign sl2 Pos)) ||
                    ((contains_sign sl1 Pos) && (contains_sign sl2 Pos))

            | Gt -> (contains_sign sl2 Neg) ||
                    (((contains_sign sl2 Zero) || (contains_sign sl2 Pos)) && (contains_sign sl1 Pos))

            | Ge -> (contains_sign sl2 Neg) ||
                    ((contains_sign sl2 Zero) && ((contains_sign sl1 Zero) || contains_sign sl1 Pos)) ||
                    ((contains_sign sl2 Pos) && (contains_sign sl1 Pos))
;;

(**
    Prend une comparaison et renvoie la comparaison opposée.
 *)
let reverse_comp (c:comp) : comp =
    match c with 
    | Eq -> Ne
    | Ne -> Eq
    | Lt -> Ge
    | Le -> Gt
    | Gt -> Le
    | Ge -> Lt
;;

(**
    Prend une position, une instr (ligne de code), et env_sign list
    rend la liste modifiée selon la ligne de code.
 *)
let sign_set (pos : position) 
                    (n :name) 
                    (e : expr) 
                    (el : env_sign list) 
                    : env_sign list = 
    match  List.find_opt (fun x -> x.varName = n) el with
    | Some(envs) -> 
        envs.varSign <- (expr_to_signlist pos e el); 
        el
    | None -> 
        { 
            varName = n;
            varSign = []
        } :: el
;;

let sign_read (n : name) 
                (el : env_sign list)
                : env_sign list =
     match  List.find_opt (fun x -> x.varName = n) el with
    | Some(envs) -> 
        envs.varSign <- [Neg;Zero;Pos]; 
        el
    | None -> 
        {   
            varName = n;
            varSign = [Neg;Zero;Pos]
        } :: el
;;

(** TODO : pour IF et WHILE, coder la propagation *)
let rec sign_if (pos : position)
                ((e1,c,e2) : cond)
                (b1 : block)
                (b2 : block)
                (el : env_sign list)
                : env_sign list = 
    if (sign_cond pos (e1,c,e2) el) 
    then
        if (sign_cond pos (e1,(reverse_comp c),e2) el)  
        then 
            let el1 = sign_block b1 el in
            let el2 = sign_block b2 el in
            union_envsignlist el1 el2 
        else sign_block b1 el
    else 
        if (sign_cond pos (e1,(reverse_comp c),e2) el)
        then sign_block b2 el
        else el

and sign_while (pos : position)
                ((e1,c,e2) : cond)
                (b : block)
                (el : env_sign list)
                : env_sign list =
    if (sign_cond pos (e1,c,e2) el) 
    then
        let new_env = (union_envsignlist el (sign_block b el))
        in
            if not (env_equals el new_env)
            then
                sign_while pos (e1,c,e2) b new_env
            else el
    else el

and sign_instr (pos : position) 
                    (ins : instr) 
                    (el : env_sign list) 
                    : env_sign list =
    match ins with 
    | Set(n,e) -> sign_set pos n e el
    | Read(n) -> sign_read n el
    | Print(_) -> el
    | If(c,b1,b2) -> sign_if pos c b1 b2 el
    | While(c,b) -> sign_while pos c b el

(**
    Prend un block (bloc de code), 
    ainsi qu'une env_sign list (liste de variables et infos les concernant)
    et crée une nouvelle liste à partir de la liste fournie,
    selon les variables rencontrées lors de la lecture du bloc de code. 
    Renvoie la nouvelle liste. 
 *)
and sign_block (b : block) (env : env_sign list) : env_sign list = 
    let rec loop bl accenv = 
        match bl with
        | [] -> accenv
        | (pos, ins) :: blr -> loop blr (sign_instr pos ins accenv)
    in loop b env 
;;

(**
    Prend un sign list (d'une variable) et renvoie le string attendu
    c'est à dire composé de -, +, 0, !,
    selon les valeurs finales possibles de la variable 
*)
let varSign_error_and_string (vs : sign list) : (int * string) = 
    let rec loop acci accstr tab = 
    match tab with 
    | [] -> (acci,accstr)
    | e :: l -> 
        match e with 
        | Neg -> loop acci (accstr ^ "-") l 
        | Zero -> loop acci (accstr ^"0") l 
        | Pos -> loop acci (accstr ^"+") l 
        | Error(i)-> loop i (accstr ^"!") l
    in loop (-1) "" vs 
;;

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
        | e :: l -> 
            (
            let (i,str) = varSign_error_and_string(e.varSign)
            in 
                Printf.printf "%s %s\n" e.varName str;
                if i < acc || acc = (-1)
                then loop l i
                else loop l acc
            )
    in loop env (-1)
;;

(**
    Fonction de départ du fichier.
    Prend un programme (program), c'est-à-dire un bloc (block) de code,
    et utilise les fonctions annexes pour d'abord print toutes les variables du fichier,
    avec leur valeurs finales possibles (+ 0 - !)
    et termine par print la première ligne où une erreur risque de se produire, sinon "Safe".
 *)
let sign_polish (p : program) : unit = 
    let env = sign_block p [] 
    in  
        let i = print_env_sign_list env;
        in 
            if (i = (-1)) 
            then print_string "safe\n"
            else Printf.printf "divbyzero %d\n" i
;;