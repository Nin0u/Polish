open DataTypes

(**
    EVALUATION D'EXPRESSIONS
*)
(*---------------------------------------------------------------------------*)

(**
    Fonction qui évalue une opération binaire.

    Lève une exception en cas de division par 0
    ou modulo par 0.
*)
let applyOp (op : op) 
            (val1 : Z.t) 
            (val2 : Z.t) 
            (pos : int)
            : Z.t =

    match op with
    | Add -> Z.add val1 val2
    | Sub -> Z.sub val1 val2
    | Mul -> Z.mul val1 val2 
    | Div -> 
        if (Z.equal val2 (Z.zero))
        then raise (Division_by_zero pos)
        else Z.div val1 val2
    | Mod -> 
        if (Z.equal val2 (Z.zero))
        then raise (Modulo_by_zero pos)
        else Z.rem val1 val2
;;

(**
    Fonction qui évalue une expression.
*)

let rec evalExpr (env: env list) 
                (exp : expr)  
                (pos : int)
                : Z.t =
    match exp with 
    | Num(n) -> n 
    | Var(s) -> 
        (
        try 
            let elem = List.find (fun x -> x.varName = s) env
            in 
                elem.value 
        with
            Not_found -> raise (No_such_varName (s,pos))
        )
    | Op(op,exp1,exp2) ->
        applyOp op (evalExpr env exp1 pos) (evalExpr env exp2 pos) pos
;;

(**
    EVALUATION DE CONDITIONS
*)

(*---------------------------------------------------------------------------*)

(**
    Fonction qui évalue une comparaison
*)
let compare (comparison : comp) (val1 : Z.t) (val2 : Z.t) : bool =
    match comparison with
    | Eq -> Z.equal val1 val2
    | Ne -> not (Z.equal val1 val2)
    | Lt -> Z.lt val1 val2
    | Le -> Z.leq val1 val2
    | Gt -> Z.gt val1 val2
    | Ge -> Z.geq val1 val2
;;

(**
    Fonction qui évalue une condition
*)
let evalCond (env: env list) 
                ((exp1, comparison, exp2) : cond) 
                (pos : int)
                : bool =
    let eval1 = evalExpr env exp1 pos
    and eval2 = evalExpr env exp2 pos
    in 
        compare comparison eval1 eval2
;;


(**
    EVALUATION DES INSTRUCTIONS ET PROGRAMME
    Chaque fonction prend et renvoie un environnement

    Pour l'instant on n'utilise qu'un seul environnement :
    Il n'y a donc pas de concept de variable locale,
    elles sont toutes globales peut importe l'endroit où
    elles sont déclarées.
*)
(*---------------------------------------------------------------------------*)

(**
    Fonction qui traite l'instruction SET
*)
let evalSet (env : env list) 
                (varName : name)
                (exp : expr)
                (pos : int) 
                : env list =
    let e = evalExpr env exp pos
    in
        try
            let elem = List.find (fun x -> x.varName = varName) env
            in 
                elem.value <- e;
                env
            
        with Not_found -> 
            { 
                varName = varName;
                value = e
            } :: env
;;

(**
    Fonction qui traite l'instruction READ
*)
let evalRead (env : env list)
                (varName : name)  
                (pos : int) 
                : env list =
    print_string (varName ^ "?\n");
    let n = Z.of_string (read_line())
    in 
        try
           match List.find (fun x -> x.varName = varName) env with
           | _ -> raise (Varname_already_exists pos)
        with Not_found -> 
        {
            varName = varName;
            value = n
        } :: env
;;

(**
    Fonction qui traite l'instruction PRINT
*)
let evalPrint (env : env list) 
                    (exp : expr)  
                    (pos : int) 
                    : env list =
    print_string (Z.to_string (evalExpr env exp pos)); 
    print_string ("\n");
    env
;;

(**
    Fonction qui traite l'instruction IF
*)
let rec evalIf (env: env list) 
                (condition : cond)
                (blk1 : block)
                (blk2 : block)
                (pos : int)
                : env list =
    let bool = evalCond env condition pos
    in 
        if bool
        then evalBlock env blk1
        else evalBlock env blk2

(**
    Fonction qui traite l'instruction WHILE
*)
and evalWhile (env: env list) 
                (condition : cond)
                (blk : block)
                (pos : int)
                : env list = 
    let b = evalCond env condition pos
    in
        if b
        then
            evalWhile (evalBlock env blk) condition blk pos
        else 
            env

(**
    Fonction qui traite une instruction
*)
and evalInstr (env : env list) 
                (pos : int) 
                (instr : instr) 
                : env list =
    match instr with
    | Set(varName, exp) -> evalSet env varName exp pos
    | Read(varName) -> evalRead env varName pos
    | Print(exp) -> evalPrint env exp pos
    | If(condition, blk1, blk2) -> evalIf env condition blk1 blk2 pos
    | While (condition, blk) -> evalWhile env condition blk pos

(**
    Fonction qui traite un bloc
*)
and evalBlock (env : env list) 
                (blk : block) 
                : env list =
    match blk with
    | [] -> env
    | (pos,instr) :: resInstr ->
        let newEnv = evalInstr env pos instr
        in 
            evalBlock newEnv resInstr
;;

(**
    Fonction qui traite le programme avec les exceptions
*)
let evalProgram (p : program) : unit =
    try
        match evalBlock [] p with
        | _ -> ()
    with
        | Division_by_zero pos -> 
            Printf.printf "\nDivision by zero at line %d\n" pos;
            exit (-1)
        | Modulo_by_zero pos ->
            Printf.printf "\nModulo by zero at line %d\n" pos;
            exit (-1)
        | No_such_varName (s,pos) -> 
            Printf.printf "\nNo such variable name '%s' at line %d\n" s pos;
            exit (-1)
        | Varname_already_exists pos ->
            Printf.printf "\nVariable's name already exists at line %d\n" pos;
            exit (-1)
;;

(*---------------------------------------------------------------------------*)
 
(**
    FONCTION eval_polish à implémenter :
    Evalue un programme 
*)

let eval_polish (p:program) : unit =  
    evalProgram p
;;