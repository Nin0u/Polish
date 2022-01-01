open DataTypes

(**
    Analyse statique d'une expression
*)
let rec varsExpr (e : expr) 
            (allVars : Names.t)
            (initVars : Names.t)
            : (Names.t * Names.t) =
    match e with
    | Num(_) -> (allVars, initVars)
    | Var(n) -> (Names.add n allVars, initVars)
    | Op(_,e1,e2) -> 
        let (a,i) = varsExpr e1 allVars initVars
        in 
            varsExpr e2 a i
;;

(**
    Analyse statique d'une condition
*)
let varsCond ((e1,_,e2) : cond)
            (allVars : Names.t)
            (initVars : Names.t)
            : (Names.t * Names.t) =
    let (a,i) = varsExpr e1 allVars initVars
    in 
        varsExpr e2 a i
;;

(**
    Analyse statique d'une instruction et d'un bloc
*)
let rec varsInstr (instr : instr) 
                (allVars : Names.t) 
                (initVars : Names.t)
                : (Names.t * Names.t) =
    match instr with
    | Set(n,e) -> 
        (
        let (a,i) = varsExpr e allVars initVars
        in 
            (Names.add n a,Names.add n i)
        )
    | Read(n) -> (Names.add n allVars, Names.add n initVars)
    | Print(e) -> varsExpr e allVars initVars
    | If(c,b1,b2) ->
        (
        let (a,i) = varsCond c allVars initVars 
        in
            let (ifa,ifi) = varsBlock b1 Names.empty Names.empty
            in
                let (elsea,elsei) = varsBlock b2 Names.empty Names.empty
                in
            (Names.union a (Names.union ifa elsea), 
            Names.union i (Names.inter ifi elsei))
        )
    | While(c,bl) -> 
        (
        let (a,i) = varsCond c allVars initVars
        in
            varsBlock bl a i
        )

and varsBlock (b : block) 
            (allVars : Names.t)
            (initVars : Names.t)
            : (Names.t * Names.t) = 
    let rec loop b allVars initVars =
        match b with
        | [] -> (allVars,initVars)
        | (_,instr) :: res -> 
            (
            let (a,i) = varsInstr instr allVars initVars
            in 
                loop res a i
            )
    in loop b allVars initVars
;;

(**
    Fonction qui affiche le contenu d'un Names.t
    On affiche les variables étudiées toutes séparées par un espaces
*)
let printSet (set : Names.t) =
    Names.iter (fun x-> print_string (x ^ " ")) set;
    print_string "\n"
;;

let vars_polish (p : program) : unit =
    let (allVars, initVars) = varsBlock p Names.empty Names.empty
    in 
        printSet allVars;
        printSet (Names.diff allVars initVars);
;;