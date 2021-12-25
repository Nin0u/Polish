open DataTypes

(**
    PROPAGATION DES CONSTANTES
*)

(*---------------------------------------------------------------------------*)

let rec simplAdd (e1 : expr) (e2 : expr) : expr = 
    match e1,e2 with
        | Num(n),e2 when n = Z.zero -> e2
        | e1, Num(n) when n = Z.zero  -> e1
        | Num(n) , Num(m) -> Num(Z.add n m)
        | _ -> Op(Add,e1,e2)

and simplSub (e1 : expr) (e2 : expr) : expr =
    match e1,e2 with
    | e1, Num(n) when n = Z.zero -> e1
    | Num(n),Num(m) -> Num(Z.sub n m)
    | _ -> Op(Sub,e1,e2)

and simplMul (e1 : expr) (e2 : expr) : expr =
    match e1,e2 with
    | Num(n), _ when n = Z.zero -> Num(Z.zero)
    | _ , Num(n) when n = Z.zero -> Num(Z.zero)

    | Num(n), e2 when n = Z.one -> e2
    | e1 , Num(n) when n = Z.one -> e1

    | Num(n),Num(m) -> Num (Z.mul n m)
    | _ -> Op(Mul,e1,e2)

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

and simplOp (o : op) (e1 : expr) (e2 : expr) : expr =
    match o with
    | Add -> simplAdd e1 e2
    | Sub -> simplSub e1 e2
    | Mul -> simplMul e1 e2
    | Div | Mod -> simplDivOrMod o e1 e2

and simplExpr (e : expr) : expr =
    match e with
    | Op(o,e1,e2) -> simplExpr(simplOp o (simplExpr e1) (simplExpr e2))
    | _ -> e
;;

let simplCond ((e1,c,e2): cond) : cond =
    (simplExpr e1,c,simplExpr e2)
;;

let rec simplInst (instr : instr) : instr =
    match instr with 
    | Set(n,e) -> Set(n, simplExpr e) 
    | Print(e) -> Print(simplExpr e)
    | If(c,b1,b2) -> If(simplCond c,simplBlock b1,simplBlock b2)
    | While(c,b) -> While(simplCond c, simplBlock b)
    | _ -> instr

and simplBlock (b : block) : block = 
    let rec loop acc b = 
        match b with
        | [] -> List.rev acc 
        | (pos,instr) :: res -> loop ((pos,simplInst instr) :: acc) res
        
    in 
        loop [] b
;;

let simpl_polish (p : program) = simplBlock p;;