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
    Fonction auxiliaire qui renvoie un tableau sans son dernier élément.
    Elle sert notamment dans la fonction toExpression juste en dessous.
 *)
let removeLast (list : string list) : string list = 
    match List.rev list with 
    | [] -> [] 
    | _ :: tail -> List.rev tail

(** 
    Fonction qui teste si un tableau de mots constitue une expression.
    Renvoie un expr si c'est le cas et sinon lève une exception.
*)
let rec toExpression (words : string list) : expr = 
    match words with 
    | [] -> raise Not_an_expression
    | word :: []-> 
        (
            match word with
            | "+" | "-" | "*" | "/" | "%" -> raise Not_an_expression
            | _ -> 
                try
                    Num(int_of_string word)
                with 
                    Failure _ -> Var(word)
        )
    | word :: otherWords -> 
        let lastElement = List.hd (List.rev otherWords)
        in 
            let lastExpression = 
                try 
                    Num(int_of_string lastElement)
                with 
                    Failure _ -> Var(lastElement)
            in
                match word with
                | "+" -> Op(Add,toExpression(removeLast otherWords),lastExpression)
                | "-" -> Op(Sub,toExpression(removeLast otherWords), lastExpression)
                | "*" -> Op(Mul,toExpression(removeLast otherWords), lastExpression)
                | "/" -> Op(Div,toExpression(removeLast otherWords), lastExpression)
                | "%" -> Op(Mod,toExpression(removeLast otherWords), lastExpression)
                |  _  -> raise(Not_an_expression)
                
(**
    Fonction qui teste si un tableau de mots constitue une condition.
    On sépare la liste à l'endoit du comparateur on parse le comparateur et on appelle toExpression sur les deux sous listes.
    Recursive terminale.
 *)
let toCondition (words : string list) : cond =
    let rec loop (expr1 : string list) 
                 (comp : comp option) 
                 (expr2 : string list)  
                 (words : string list) =
        match words with
        | [] -> 
            (
                try 
                    (toExpression (List.rev expr1),Option.get comp,toExpression (List.rev expr2))
                with 
                    Not_an_expression -> raise Not_a_condition
            )
        | word :: otherWords -> 
            if comp = None
            then
                match word with 
                | "="  -> loop expr1 (Some(Eq)) expr2 otherWords
                | "<>" -> loop expr1 (Some(Ne)) expr2 otherWords
                | "<=" -> loop expr1 (Some(Le)) expr2 otherWords
                | "<"  -> loop expr1 (Some(Lt)) expr2 otherWords
                | ">=" -> loop expr1 (Some(Ge)) expr2 otherWords
                | ">"  -> loop expr1 (Some(Gt)) expr2 otherWords
                | _ -> loop (word :: expr1) None expr2 otherWords
            else 
                match word with
                | "=" | "<>" | "<=" | "<" | ">=" | ">" -> raise Not_a_condition
                | _ -> loop expr1 comp (word :: expr2) otherWords
    in 
        loop [] None [] words
;;

let read_polish (filename:string) : program = faiwith TODO;;