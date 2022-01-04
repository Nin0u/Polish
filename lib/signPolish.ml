open DataTypes


let sign_block (b : block) (env : env_sign list): env_sign list = 
    let rec loop bl acc = match bl with
    | [] -> acc
    | _ -> []
    in loop b env ;;


let varSign_to_string (vs : sign list) : string = 
    let rec loop acc tab = 
    match tab with 
    | [] -> acc
    | e :: l -> match e with
        | Neg -> loop (acc^"-") l 
        | Zero -> loop (acc^"0") l 
        | Pos -> loop (acc^"+") l 
        | Error(_)-> loop (acc^"!") l
in loop "" vs ;;

let error_line (env : env_sign) : int = match env with 
| [] -> -1
| Error(a) :: l -> a 
| e :: l -> error_line(l) ;;

let first_error_line (acc:int) (e:int) = if (e<>-1) && (e<acc) then e else acc ;;

let print_env_sign_list (env : env_sign list) : int = 
let rec loop env acc = 
match env with 
| [] -> print_string "\n"; acc
| e :: l -> Printf.printf "%s %s\n" e.varName varSign_to_string(e.varSign) ; 
    loop l (first_error_line (acc error_line e))
in loop env -1;;

let print_error (posi:int) : unit = if (posi=-1) then print_string "Safe\n" 
    else Printf.printf "divbyzero %d\n" posi;;

let sign_polish (p : program) : unit = print_error(print_env_sign_list (sign_block p [])) ;;

(**let print_env_sign_list (env : env_sign list) : unit = match env with 
| [] -> print_string "\n";
| e :: l -> Printf.printf "%s %s\n" e.varName varSign_to_string(e.varSign) ; 
    print_env_sign_list l ;;

let sign_polish (p : program) : unit = print_env_sign_list (sign_block p []) ;;**)

(** let sign_polish (p : program) : unit = match p with  _ -> ();; *)