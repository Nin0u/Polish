open DataTypes

(** Fonction auxiliaire pour lire une ligne d'une entrée *)
let readLine (input : in_channel) : (string option) = 
    try Some (input_line input) with End_of_file -> close_in input; None
;;

(** 
    Fonction qui stock les numéros des lignes ainsi que les contenus des lignes dans une liste.
    Récursion terminale.
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
    Fonction auxiliaire qui associe un caracère à un opérateur
 *)

 let toOp (word : string) : op option =
    match word with
    | "+" -> Some(Add)
    | "-" -> Some(Sub)
    | "*" -> Some(Mul)
    | "/" -> Some(Div)
    | "%" -> Some(Mod)
    |  _  -> None    
;;

(**
    Fonction qui teste si un tableau de mots constitue une expression.
    Renvoie (expr * []) si c'est le cas et sinon lève une exception.
*)

let rec toExpression (words : string list) : (expr * string list) = 
    match words with
    | [] -> raise Not_an_expression
    | word :: otherWords -> 
         match word with
                | "+" | "-" | "*" | "/" | "%" -> 
                    let (expr1, residuals) = toExpression otherWords
                    in
                        let (expr2, residuals2) = toExpression residuals
                        in
                        (Op(Option.get(toOp word),expr1,expr2),residuals2)
                |  _  -> 
                    try 
                        (Num(int_of_string word),otherWords) 
                    with 
                        Failure _ -> (Var(word),otherWords)
;;

let readExpression (words : string list) : expr = 
    let (expr,res) = toExpression words
    in 
        if res != []
        then 
            raise Not_an_expression
        else expr
;;

(**
    Fonction qui teste si un tableau de mots constitue une condition.
    On sépare la liste à l'endoit du comparateur on parse le comparateur et on appelle toExpression sur les deux sous listes.
    Recursive terminale.
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

(*
let read_polish (filename:string) : program = failwith "TODO";;
*)