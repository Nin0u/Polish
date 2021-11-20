open DataTypes

(** 
    LECTURE DE FICHIER ET INITIALISATION DE DONNEES
*)

(*--------------------------------------------------------------------------------------------------*)

(** Fonction auxiliaire pour lire une ligne d'une entrée *)
let readLine (input : in_channel) : (string option) = 
    try Some (input_line input) with End_of_file -> close_in input; None
;;

(** 
    Fonction qui stock les numéros des lignes ainsi que les contenus des lignes dans une liste.
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

(*--------------------------------------------------------------------------------------------------*)

(**
    LECTURE DES EXPRESSIONS ET DES CONDITIONS ET DES ATTRIBUTIONS
*)

(*--------------------------------------------------------------------------------------------------*)

(**
    Fonction auxiliaire factorise du code
    et 
    Fonction qui teste si un tableau de mots constitue une expression.
    Renvoie (expr * []) si c'est le cas et sinon lève une exception.
*)

let rec opAndRes (otherWords : string list) (op:op)  =
    let (expr1, residuals) = toExpression otherWords
    in
        let (expr2, residuals2) = toExpression residuals
        in
            (Op(op,expr1,expr2),residuals2)

and toExpression (words : string list) : (expr * string list) = 
    match words with
    | [] -> raise Not_an_expression
    | word :: otherWords -> 
         match word with
                | "+" -> opAndRes otherWords Add
                | "-" -> opAndRes otherWords Sub
                | "*" -> opAndRes otherWords Mul
                | "/" -> opAndRes otherWords Div
                | "%" -> opAndRes otherWords Mod
                |  _  -> 
                    try 
                        (Num(int_of_string word),otherWords) 
                    with 
                        Failure _ -> (Var(word),otherWords)
;;

(**
    Renvoie l'expression issue de la fonction toExpression.
    Vérifie également que le tableau des résiduels est vide
    car dans le cas contraire ce ne serait pas une expression.
 *)

let readExpression (words : string list) : expr = 
    try
        let (expr,res) = toExpression words
        in 
            if res != []
            then 
                raise Not_an_expression
            else expr
    with Not_an_expression -> raise Not_an_expression
;;

(**
    Fonction qui teste si un tableau de mots constitue une condition.
    En appliquand toExpression, le tableau résiduel contient 
    le comparateur et le membre de droite.
    Il suffit alors d'identifier le comparateur et de lire le membre
    de droite sous forme d'expression avec readExpression
 *)

let readCondition (words : string list) : cond =
    let (expr1, residuals) = toExpression words
    in 
        match residuals with 
        | [] -> raise Not_a_condition
        | res :: otherRes ->
            try 
                let (expr2) = readExpression otherRes
                in 
                    match res with
                    | "=" -> (expr1,Eq,expr2)
                    | "<>" -> (expr1,Ne,expr2)
                    | "<" -> (expr1,Lt,expr2)
                    | "<=" -> (expr1,Le,expr2)
                    | ">" -> (expr1,Gt,expr2)
                    | ">=" -> (expr1,Ge,expr2)
                    | _ -> raise Not_a_condition
            with 
                Not_an_expression -> raise Not_a_condition
;;

(**
    Fonction qui teste si une ligne représente une affectation.
    Lève le
*)
let readSet (words : string list) : instr = 
    let (expr1,residuals) = toExpression words
    in 
    match expr1 with
    | Var(s) -> 
        (
        match residuals with
        | [] -> raise Set_error
        | res :: otherRes ->
            match res with
            | ":=" -> 
                (
                try
                    Set(s,readExpression otherRes)
                with Not_an_expression -> raise Not_an_expression 
                )
            | _ -> raise Set_error
        )
    | _ -> raise Set_error
;;

(*--------------------------------------------------------------------------------------------------*)

(** 
    Fonction qui renvoie l'indentation d'une ligne découpée en mots.
    En fait, elle compte le nombre de "" avant le premier mot non vide.
    La division par deux à la fin est due à la taille d'une indentation qui,
    sur la plupart des éditeurs, est de 2 espaces.
    On se fixe cette taille de tabulation pour l'instant.
*)
let getIndent (words: string list) = 
    let rec loop acc words=
        match words with
        | [] -> acc
        | word :: otherWords -> 
            if word = "" 
            then loop (1 + acc) otherWords
            else acc
    in (loop 0 words)/2
;;

(** 
    PARSING DES BLOCKS ET DES LIGNES
*)

(*--------------------------------------------------------------------------------------------------*)
    
(** 
    Fonction pour extraire un block (avec même indentation)
    Renvoie le block extrait et le reste des lignes.
*)
let extractBlockLines (lines : (position * string) list) (indent : int) : ((position * string) list * (position * string)) =
    let rec loop acc =
        match lines with 
        | [] -> (List.rev acc,[])
        | (pos, line) :: otherLines ->
            if getIndent (String.split_on_char line) = indent
            then loop ((pos,line) :: acc)
            else (List.rev acc, otherLines)
    in loop [] 

(*--------------------------------------------------------------------------------------------------*)
 
let read_polish (filename:string) : program = failwith "TODO"