open DataTypes

(**
    EVALUATION D'EXPRESSIONS
*)
(*---------------------------------------------------------------------------*)

let applyOp (op : op) 
            (val1 : int) 
            (val2 : int) 
            (pos : int)
            : int =

    match op with
    | Add -> val1 + val2
    | Sub -> val1 - val2
    | Mul -> val1 * val2 
    | Div -> 
        if val2 <> 0
        then val1 / val2
        else raise (Division_by_zero pos)
    | Mod -> 
        if val2 <> 0
        then val1 mod val2
        else raise (Modulo_by_zero pos)
;;

let rec evalExpr (env: env list) 
                (exp : expr)  
                (pos : int)
                : int =
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

let compare (comparison : comp) (val1 : int) (val2 : int) : bool =
    match comparison with
    | Eq -> val1 = val2
    | Ne -> val1 <> val2
    | Lt -> val1 < val2
    | Le -> val1 <= val2
    | Gt -> val1 > val2
    | Ge -> val1 >= val2
;;

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
    Chaque fonction renvoie un environnement
*)
(*---------------------------------------------------------------------------*)

(**
    Fonction qui traite l'instruction SET
*)
let handleSet (env : env list) 
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
let handleRead (env : env list)
                (varName : name)  
                (pos : int) 
                : env list =
    print_string (varName ^ "? ");
    let n = read_int()
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
let handlePrint (env : env list) 
                    (exp : expr)  
                    (pos : int) 
                    : env list =
    print_string (string_of_int (evalExpr env exp pos)); 
    print_string ("\n");
    env
;;

(**
    Fonction qui traite l'instruction IF
*)
let rec handleIf (env: env list) 
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
and handleWhile (env: env list) 
                (condition : cond)
                (blk : block)
                (pos : int)
                : env list = 
    let b = evalCond env condition pos
    in
        if b
        then
            handleWhile (evalBlock env blk) condition blk pos
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
    | Set(varName, exp) -> handleSet env varName exp pos
    | Read(varName) -> handleRead env varName pos
    | Print(exp) -> handlePrint env exp pos
    | If(condition, blk1, blk2) -> handleIf env condition blk1 blk2 pos
    | While (condition, blk) -> handleWhile env condition blk pos

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
        | _ -> print_string "\neval Done.\n"
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