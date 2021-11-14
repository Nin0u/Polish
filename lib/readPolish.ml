open DataTypes

(** Fonction auxiliaire pour lire une ligne d'une entrée *)
let readline (input : in_channel) : (string option) = 
    try Some (input_line input) with End_of_file -> close_in input; None
;;

(** Fonction qui stock les numéros des lignes ainsi que les contenus des lignes dans une liste *)
let read (filename : string) : (position * string) list = 
    let file = open_in filename
    in 
        let rec loop (acc : (position * string) list) (i : position)= 
            match (readline file) with
            | None -> List.rev(acc);
            | Some(s) -> loop ((i ,s)::acc) (i+1)
        in 
        loop [] 1
;;

(**
let parseLines (lines : (position * string) list) : program = failwith "TODO"

let read_polish (filename:string) : program = parseLines (read filename);;
*)