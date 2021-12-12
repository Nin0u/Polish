open DataTypes

(**
    AFFICHAGE DU PROGRAMME POLISH DEPUIS LISTE D'INSTRUCTIONS  
*)

(*---------------------------------------------------------------------------*)

(** 
    Fonction qui prend un opérateur (op)
    et renvoie le string correspondant au caractère en Polish.
*)
let op_to_string (o:op) : string = match o with 
| Add -> "+"
| Sub -> "-"
| Mul -> "*"
| Div -> "/"
| Mod -> "%" ;;

(**
    Fonction qui prend un comparateur (comp)
    et renvoie le string correspondant au caractère en Polish.
*)
let comp_to_string (c:comp) : string = match c with
| Eq -> "="
| Ne -> "<>"
| Lt -> "<"
| Le -> "<="
| Gt -> ">"
| Ge -> ">=" ;;

(**
    Fonction qui prend une expression (expr)
    et renvoie le string correspondant au caractère en Polish.
*)
let rec expr_to_string (e:expr) : string = match e with 
| Num(n) -> Z.to_string n
| Var(n) -> n
| Op(o, e1, e2) -> 
    (op_to_string o)^(" ")^
    (expr_to_string e1)^(" ")^
    (expr_to_string e2)
;;

(**
    Fonction qui prend une condition (cond)
    et renvoie le string correspondant au caractère en Polish.
*)
let cond_to_string (c:cond) : string = match c with
| (e1, c, e2) -> (expr_to_string e1)^(" ")^
    (comp_to_string c)^(" ")^
    (expr_to_string e2)
;;

(**
    Fonction qui prend un int représentant l'indentation actuelle (indent)
    et renvoie un string composé de 2*indent espaces.
*)
let rec indent_to_string (indent:int) : string = if indent>0 
    then "  "^(indent_to_string (indent-1))
    else "" 
;;

(**
    Fonction qui prend une instruction et l'indentation attendue,
    et print la bonne indentation suivi 
    de l'équivalent Polish de l'instruction. 
*)
let rec match_instr (ins:instr) (indent:int) : unit = 
Printf.printf "%s" (indent_to_string indent);
match ins with
| Set(n,e) -> Printf.printf "%s := %s\n" n (expr_to_string e)
| Read(n) -> Printf.printf "READ %s\n" n
| Print(e) -> Printf.printf "PRINT %s\n" (expr_to_string e)
| If(c,b1,b2) -> 
    Printf.printf "IF %s\n" (cond_to_string c) ; 
    print_block b1 (indent+1) ;
    if b2 <> [] 
    then (
        Printf.printf "%sELSE\n" (indent_to_string indent) ; 
        print_block b2 (indent+1)
    )
| While(c,b) -> 
    Printf.printf "WHILE %s\n" (cond_to_string c) ; 
    print_block b (indent+1)

(**
    Fonction qui prend un bloc (block) de code et l'indentation attendue,
    et l'envoie à la fonction ci-dessus pour qu'il soit print en Polish 
    avec la bonne indentation.
*)
and print_block (b:block) (indent:int) : unit = match b with 
| [] -> ()
| (_,ins) :: r -> match_instr ins indent ; print_block r indent ;;

(** 
    Fonction de départ du fichier. 
    Prend un programme (program), c'est-à-dire un bloc (block) de code,
    et l'envoie aux fonctions ci-dessus pour qu'il soit traduit 
    et print correctement en Polish.
*)
let print_polish (p:program) : unit = print_block p 0 ;;