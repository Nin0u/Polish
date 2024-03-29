open DataTypes

(**
    PROPAGATION DES CONSTANTES
*)

(*---------------------------------------------------------------------------*)

(**
    Simplification de somme
*)
let rec simplAdd (e1 : expr) (e2 : expr) : expr = 
    match e1,e2 with
        | Num(n),e2 when n = Z.zero -> e2
        | e1, Num(n) when n = Z.zero  -> e1
        | Num(n) , Num(m) -> Num(Z.add n m)
        | _ -> Op(Add,e1,e2)

(**
    Simplification de différence
*)
and simplSub (e1 : expr) (e2 : expr) : expr =
    match e1,e2 with
    | e1, Num(n) when n = Z.zero -> e1
    | Num(n),Num(m) -> Num(Z.sub n m)
    | _ -> Op(Sub,e1,e2)

(**
    Simplification de produit
*)
and simplMul (e1 : expr) (e2 : expr) : expr =
    match e1,e2 with
    | Num(n), _ when n = Z.zero -> Num(Z.zero)
    | _ , Num(n) when n = Z.zero -> Num(Z.zero)

    | Num(n), e2 when n = Z.one -> e2
    | e1 , Num(n) when n = Z.one -> e1

    | Num(n),Num(m) -> Num (Z.mul n m)
    | _ -> Op(Mul,e1,e2)

(**
    Simplification de division et modulo
*)
and simplDivOrMod (o : op) (e1 : expr) (e2 : expr) : expr =
    match e1,e2 with
    | Num(n),_ when n = Z.zero -> Num(Z.zero)
    | e1, Num(n) when n = Z.zero -> 
        (
        match o with
        | Div -> Op(Div,e1,Num(Z.zero))
        | Mod -> Op(Mod,e1,Num(Z.zero))
        | _ -> failwith "ERROR : called simplDivOrMod on another operator"
        )
    | Num(n),Num(m) ->
        (
        match o with
        | Div -> Num(Z.div n m)
        | Mod -> Num(Z.rem n m) 
        | _ -> failwith "ERROR : called simplDivOrMod on another operator"
        )
    | _ -> 
        match o with
        | Div -> Op(Div,e1,e2)
        | Mod -> Op(Mod,e1,e2)
        | _ -> failwith "ERROR : called simplDivOrMod on another operator"

(**
    Simplification d'un opérateur
*)
and simplOp (o : op) (e1 : expr) (e2 : expr) : expr =
    match o with
    | Add -> simplAdd e1 e2
    | Sub -> simplSub e1 e2
    | Mul -> simplMul e1 e2
    | Div | Mod -> simplDivOrMod o e1 e2

(**
    Simplification d'une expression
*)
and simplExpr (e : expr) : expr =
    match e with
    | Op(o,e1,e2) -> simplExpr(simplOp o (simplExpr e1) (simplExpr e2))
    | _ -> e
;;

(**
    Simplification d'une condition
*)
let simplCond ((e1,c,e2): cond) : cond =
    (simplExpr e1,c,simplExpr e2)
;;

(**
    Simplification d'une instruction
*)
let rec simplInst (instr : instr) : instr =
    match instr with 
    | Set(n,e) -> Set(n, simplExpr e) 
    | Print(e) -> Print(simplExpr e)
    | If(c,b1,b2) -> If(simplCond c,simplBlock b1,simplBlock b2)
    | While(c,b) -> While(simplCond c, simplBlock b)
    | _ -> instr

(**
    Simplification d'un bloc
*)
and simplBlock (b : block) : block = 
    let rec loop acc b = 
        match b with
        | [] -> List.rev acc 
        | (pos,instr) :: res -> loop ((pos,simplInst instr) :: acc) res
        
    in 
        loop [] b
;;

(**
    ELIMINATION DES BLOCS
*)

(**
    Fonction qui applique la comparaison entre deux nombres
    Renvoie un booleen
*)
let compare (n : Z.t) (m : Z.t) (cmp : comp) : bool option =
    match cmp with
    | Eq -> Some(Z.equal n m)
    | Ne -> Some(not (Z.equal n m))
    | Lt -> Some(Z.lt n m)
    | Le -> Some(Z.leq n m)
    | Gt -> Some(Z.gt n m)
    | Ge -> Some(Z.geq n m)

(**
    Fonction qui elimine une condition
    Renvoie Some(bool) si la condition peut-être éliminée
    Renvoie None sinon
*)
let elimCond ((e1,cmp,e2) : cond) : bool option = 
    match e1,e2 with
    | Num(n),Num(m) -> compare n m cmp
    | _ , _ -> None


(**
    Fonction qui ajoute les lignes d'un bloc dans un autre
*)
let add (pos : position) (b : block) (acc : block) : block =
    let rec loop pos acc b =
        match b with
        | [] -> acc
        | (_,instr) :: res -> loop (pos + 1) ((pos,instr) :: acc) res
    in 
        loop pos acc b

(**
    Fonction qui elimine les blocs en cas de comapraisons entre constantes
    dans des conditions.

    Pour IF : si la condition est vrai on elimine le bloc ELSE 
    Sinon on elimine le bloc IF

    Pour WHILE : si la condition est fausse on élimine le bloc
    Sinon on a une boucle infinie et on garde le bloc

*)
let rec elimInstr (pos : position) (instr : instr) : block =
    match instr with
    | If(c,b1,b2) -> 
        (
        match elimCond c with
        | None -> [(pos,instr)]
        | Some(b) -> 
            if b
            then elimBlock b1
            else elimBlock b2
        )
    | While (c,bl) ->
        (
        match elimCond c with
        | None -> [(pos,While(c, elimBlock bl))]
        | Some(b) -> 
            if b 
            then [(pos,While(c, elimBlock bl))]
            else []
        )
    | _ -> [(pos,instr)]
        
(**
    Fonction qui parcours le bloc pour eliminer ceux qui peuvent être eliminés
*)
and elimBlock (b : block) : block = 
    let rec loop acc b =
        match b with
        | [] -> List.rev acc
        | (pos,instr) :: res -> 
            let bl = elimInstr pos instr 
            in 
                loop (add pos bl acc) res
    in
        loop [] b
;;

(**
    Fonction qui simplifie le code et elimine les blocs morts
*)
let simpl_polish (p : program) = elimBlock (simplBlock p);;