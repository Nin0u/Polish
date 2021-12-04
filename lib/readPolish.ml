open DataTypes

(** 
    LECTURE DE FICHIER ET INITIALISATION DE DONNEES
*)

(*---------------------------------------------------------------------------*)

(** Fonction auxiliaire pour lire une ligne d'une entrée *)
let readLine (input : in_channel) : (string option) = 
    try 
        Some (input_line input) 
    with 
        End_of_file -> 
            close_in input; 
            None
;;

(** 
    Fonction qui stock les numéros des lignes 
    ainsi que les contenus des lignes dans une liste.
*)
let read (filename : string) : (position * string) list = 
    let file = open_in filename
    in 
        let rec loop (acc : (position * string) list) (i : position)= 
            match (readLine file) with
            | None -> List.rev(acc);
            | Some(s) -> loop ((i ,s)::acc) (i+1)
        in 
        loop [] 1
;;

(*---------------------------------------------------------------------------*)

(**
    LECTURE DES EXPRESSIONS ET DES CONDITIONS ET DES ATTRIBUTIONS
*)

(*---------------------------------------------------------------------------*)

(**
    Fonction associant une chaîne de caractères à un opérateur
    Sauf les Num et les Var
*)
let matchOp (str : string) : op =
    match str with
    | "+" -> Add
    | "-" -> Sub
    | "*" -> Mul
    | "/" -> Div
    | "%" -> Mod 
    | _ -> raise Num_or_var
;;

(**
    Fonction qui teste si un tableau de mots constitue une expression.
    Renvoie (expr * []) si c'est le cas et sinon lève une exception.
*)
let rec toExpression (words : string list) 
                        (pos : position)
                        : (expr * string list) = 
    match words with
    | [] -> raise (Not_an_expression pos)
    | word :: resWords -> 
        match word with
        | "+" | "-" | "*" | "/" | "%" ->   
            let (expr1, residuals) = toExpression resWords pos
            in
                let (expr2, residuals2) = toExpression residuals pos
                in 
                    (Op(matchOp word,expr1,expr2),residuals2)
        | _ ->
            try 
                (Num(int_of_string word),resWords) 
            with 
                Failure _ -> (Var(word),resWords)
;;

(**
    Renvoie l'expression issue de la fonction toExpression.
    Vérifie également que le tableau des résiduels est vide
    car dans le cas contraire ce ne serait pas une expression.
 *)

let readExpression (words : string list) (pos : int): expr = 
    let (expr,res) = toExpression words pos
    in 
        if res = []
        then expr
        else raise (Not_an_expression pos)
;;

(**
    Fonction qui teste si un tableau de mots constitue une condition.
    En appliquand toExpression, le tableau résiduel contient 
    le comparateur et le membre de droite.
    Il suffit alors d'identifier le comparateur et de lire le membre
    de droite sous forme d'expression avec readExpression
 *)

let readCondition (words : string list) (pos : int): cond =
    let (expr1, residuals) = toExpression words pos
    in 
        match residuals with 
        | [] -> raise (Not_a_condition pos)
        | res :: resRes ->
            let (expr2) = readExpression resRes pos
            in 
                match res with
                | "=" -> (expr1,Eq,expr2)
                | "<>" -> (expr1,Ne,expr2)
                | "<" -> (expr1,Lt,expr2)
                | "<=" -> (expr1,Le,expr2)
                | ">" -> (expr1,Gt,expr2)
                | ">=" -> (expr1,Ge,expr2)
                | _ -> raise (Not_a_condition pos)
;;

(**
    Fonction qui teste si une ligne représente une affectation.
    Lève le
*)
let readSet (words : string list) (pos : int) : instr = 
    match words with
    | s :: ":=" :: tail -> 
        if s = ""
        then raise (Set_error pos)
        else
            Set(s,readExpression tail pos)
    | _ -> raise (Set_error pos)
;;

(*---------------------------------------------------------------------------*)

(** 
    Fonction qui renvoie l'indentation d'une ligne 
    ainsi que la liste des mots sans espaces.

    On se fixe la taille d'une indentation à 2 espaces.
*) 
let indentAndSplit (words: string) : (int * (string list)) = 
    let split = String.split_on_char ' ' words
    in
        let rec loop accIndent accSplit split=
            match split with
            | [] -> (accIndent /2 , List.rev accSplit)
            | word :: resWords -> 
                if word = "" 
                then loop (1 + accIndent) (accSplit) resWords
                else loop (accIndent) (word :: accSplit) resWords
        in loop 0 [] split
;;

(** 
    PARSING DES BLOCKS ET DES LIGNES
*)

(*---------------------------------------------------------------------------*)

let rec parseBlock (lines : (position * string) list) 
                (indent : int)
                : block * ((position * string) list) =    
    let rec loop acc lines =
        match lines with
        | [] -> (List.rev acc,[])
        | (pos,str) :: resLines ->
            let (ind,split) = indentAndSplit str
            in
                if ind = indent 
                then 
                (
                    let parsedLine = parseLine pos split resLines indent
                    in 
                        match parsedLine with
                        | None -> loop acc resLines
                        | Some(parsedInstr, residuals) -> 
                            loop (parsedInstr :: acc) residuals
                )
                else (List.rev acc, lines)
    in
        loop [] lines

and parseLine (pos : int)
                (line : string list)
                (resLines : (position * string) list)
                (indent : int)
                : ((position * instr) * ((position * string) list)) option =

    match line with
    | [] -> None
    | word :: resWords -> 
        match word with
        | "COMMENT" -> None
        | "READ" ->
            if List.length resWords = 1
            then 
                Some(
                    (pos,Read(List.hd resWords))
                    , resLines
                )
            else raise (Arguments_error pos)
                

        | "PRINT" ->
            Some(
                (pos,Print(readExpression resWords pos))
                ,resLines
            )

        | "IF" -> 
            (
            let (ifBlock,ifRes) = parseBlock resLines (indent+1)
            in 
                let (elseBlock, elseRes) = elseBlock ifRes indent
                in 
                    Some(
                        (pos,If(readCondition resWords pos,ifBlock,elseBlock))
                        ,elseRes
                    )
            )
        | "WHILE" ->
            (
            let (whileBlock,residuals) = parseBlock resLines (indent+1)
            in 
                Some(
                    (pos,While(readCondition resWords pos,whileBlock))
                    ,residuals
                )
            )
        | _ ->  
            Some(
                (pos,readSet line pos)
                ,resLines
            )

(**
    Fonction qui vérifie la présence d'un bloc else
    Elle est appelée dans parseLine dans le cas d'un IF.
*)
and elseBlock (ifRes : (position * string) list) 
                (indent : int)
                : block * ((position * string) list) =
    match ifRes with
    | [] -> ([],[])
    | (_,line) :: resLines ->
        let (ind,split) = indentAndSplit line
        in 
            if ind = indent
            then
            (
                if List.length split = 1
                then
                (
                    if(List.hd split = "ELSE")
                    then
                        parseBlock resLines (indent+1)
                    else 
                        ([],ifRes)
                )
                else ([], ifRes)
            )
            else ([],ifRes)
;;

(**
    Fonction qui parse le block entier.
*)
let readProgram (lines : (position * string) list) : program=
    try
        let (program,residuals) = parseBlock lines 0
        in
            if residuals != []
            then (Printf.printf "Parsing error\n"; exit(-1))
            else program
    with
        | Not_an_expression pos -> 
            Printf.printf "Not an expression at line %d\n" pos; exit (-1)
        | Not_a_condition pos -> 
            Printf.printf "Not a condition at line %d\n" pos; exit (-1)
        | Arguments_error pos -> 
            Printf.printf "Argument error at line %d\n" pos; exit (-1)
        | Set_error pos -> 
            Printf.printf "Set error at line %d\n" pos; exit (-1)

(*---------------------------------------------------------------------------*)
 
(**
    FONCTION read_polish à implémenter :
    Lit un fichier et le transforme en program
*)

let read_polish (filename:string) : program = readProgram(read filename) 
;;