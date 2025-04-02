(* EX1 *)

let commence_par x = function
  | [] -> false
  | hd :: _  when hd = x -> true
  | _ -> false;;

(* Il y a deux premiers problèmes à corriger:
- Premièrement on ne pense pas au cas initial de la liste vide
- Secondement le x dans x :: _ n'est pas le même que dans nos paramètres
*)

let print_bool expr =
  if expr = true
    then "true"
  else "false" ;;

(* print_string(print_bool(commence_par 3 [3;1;2;3])) *)

(* EX2 *)

let val_abs x = match x with
  | y when y < 0. -> (-. x)
  | _ -> x ;;

(* print_float (val_abs (-.3.)) *)

let signe x = match x with
  | y when y = 0 -> 0
  | y when y > 0 -> 1
  | _ -> -1 ;;

(* print_int(signe (0)) *)

let rec fac x = match x with
  | y when y = 0 -> 1
  | _ -> x * fac (x-1) ;;

(* print_int (fac 1) ;; *)

let rec ackermann m n = match (m, n) with 
  | (0, n) -> n + 1
  | (m, 0) -> ackermann (m - 1) 1
  | _ -> ackermann (m - 1) (ackermann m (n-1))   ;;

(* print_int(ackermann 0 3) *)

(* EX3 *)

let rec print_list = function 
[] -> ()
| e::l -> print_int e ; print_string " " ; print_list l

let rec print_list2 l = match l with
[] -> ()
| (a,b)::f -> print_string "(" ;print_int a ; print_string "," ;print_int b ; print_string ")" ; print_string " " ; print_list2 f;;

let rec nombre_occurrences x = function
  | [] -> 0
  | hd :: tl -> 
    if x = hd 
    then 1 + nombre_occurrences x tl
    else nombre_occurrences x tl ;;

(* print_int(nombre_occurrences 0 [0; 1; 2; 0; 3; 4; 0; 5]) *)

let rec aplatit = function 
  | [] -> []
  | hd :: tl -> hd @ (aplatit tl) ;;

(* print_list(aplatit [[0; 1; 2]; [3]; [4; 5]]) *)

let rec compacte = function
  | [] -> []
  | h1::h2::tl when h1 = h2 -> (let l = compacte (h1::tl) in
      match l with
       | (x, c)::t -> (x, c+1)::t
       | _ -> invalid_arg "")
   | h1::tl -> (h1, 1)::(compacte tl) ;;

print_list2 (compacte [0; 0; 1; 2; 2; 2; 1; 1])