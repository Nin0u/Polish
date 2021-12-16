open DataTypes

(**
    PROPAGATION DES CONSTANTES
*)

(*---------------------------------------------------------------------------*)

let simplAdd (e1 : expr) (e2 : expr) : expr = 
    match e1,e2 with
        | Num(0),e2 -> simplExpr e2
        | e1, Num(0) -> simplExpr e1
        | Num(n) , Num(m) -> Num(Z.add n m)
        | _ -> Op(Add,simplExpr e1, simplExpr e2)

and simplSub (e1 : expr) (e2 : expr) : expr =
    match e1,e2 with
    | e1, Num(0) -> simplExpr e1
    | _ -> Op(Sub,simplExpr e1,simplExpr e2)

and simplMul (e1 : expr) (e2 : expr) : expr =
    match e1,e2 with
    | Num(0), _ -> Num(0)
    | _, Num(0) -> Num(0)
    | Num(n),Num(m) -> Num (Z.mul n m)
    | _ -> Op(Mul,simplExpr e1,simplExpr e2)

and simplDivOrDiv (o : op) (e1 : expr) (e2 : expr) : expr =
    match e1,e2 with
    | Num(0),_ -> Num(0)
    | _ ->
        match o with
        | Div -> 
            Op(Div,
                simplExpr e1,
                if e2 = Num(0) 
                then Num(0)
                else simplExpr e2
            )
        | Mod -> 
            Op(Mod,
                simplExpr e1,
                if e2 = Num(0) 
                then Num(0) 
                else simplExpr e2
            )
        | _ -> failwith "ERROR : called simplDivOrMod on another operator"

and simplOp (o : op) (e1 : expr) (e2 : expr) : expr =
    match o with
    | Add -> simpAdd e1 e2
    | Sub -> simplSub e1 e2
    | Mul -> simplMul e1 e2
    | Div | Mod -> simplDivOrMod o e1 e2

and simplExpr (e : expr) : expr =
    match e with
    | Op(o,e1,e2) -> simplOp o e1 e2
    | e -> e
;;

(**
    ELIMINATION DU CODE MORT
*)

(*---------------------------------------------------------------------------*)

let simpl_polish (p : program) = ();;