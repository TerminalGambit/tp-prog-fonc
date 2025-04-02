let val_abs x = if x < 0. then -.x else x;;
let signe x = if x = 0 then 0 else if x > 0 then 1 else -1;;
let rec fac n = if n == 1 then 1 else n * fac(n-1);;
let est_diviseur n d = n mod d = 0;;
let bool_to_string b = if b then "true" else "false";;

(* let () =
  let value = est_diviseur 10 5 in
  Printf.printf "%s\n" (bool_to_string value)
*)

let rec est_valide x y = if y = 1 then true else if est_diviseur x y then false else est_valide x (y-1);;
let est_premier n = est_valide n (n-1) ;;

(*
let () =
  let value = est_premier 11 in
  Printf.printf "%s\n" (bool_to_string value)
*)

let hello nom age = print_string("Hello, my name is " ^ nom ^ ", and I am " ^ (string_of_int age) ^ ".\n") ;;

(*
hello "Jack" 20 ;;
*)

(* (x ** 2 +. y ** 2) ** 0.5 *)
let distance x y = (x ** 2. +. y ** 2.) ** 0.5;;

let rand_point x = (Random.float (x *. 2.)) -. x, (Random.float (x *. 2.))-. x;;

(* 
  pour savoir si un point est dans le cercle on doit faire distance x y < distance 1 1
  on doit initialiser un compteur pour compter le nombre de points dans le cercle
  on décrémente n à chaque itération
  on n'utilise pas de boucle
*)

let approche_pi n = 
  let rec aux compteur = if compteur < 1. then 0. 
  else
  let a = rand_point 1. in
  let x = fst a in
  let y = snd a in
  if distance x y < 1. then aux (compteur -. 1.) +. 1. else aux (compteur-.1.) 
  in (aux n) *. 4. /. n;;

(*print_float (approche_pi 100000.) ;;*)

let abs_float x =
  if x < 0.0 then -.x
  else x ;;


let rec approche0 f a b p = 
  let c = (a +. b) /. 2. in
  if abs_float (f c) < p then c
  else if (f a) *. (f c) < 0. then 
    approche0 f a c p
  else 
    approche0 f c b p ;;

(* print_float (let f x = x *. x -. 2.0 in approche0 f 1.0 2.0 0.000001) ;; *)


let rec count_inside_circle n acc =
  if n = 0 then acc
  else
    let x, y = rand_point 1.0 in
    if distance x y <= 1.0 then count_inside_circle (n - 1) (acc + 1)
    else count_inside_circle (n - 1) acc
;;

let rec approche_pi_dichotomique a b p =
  let c = (a +. b) /. 2.0 in
  let f x =
    let inside = count_inside_circle (int_of_float x) 0 in
    (float_of_int inside /. x) *. 4.0
  in
  if abs_float (f c -. f a) < p then f c
  else if (f a -. f c) < 0.0 then
    approche_pi_dichotomique a c p
  else
    approche_pi_dichotomique c b p
;;

print_float (approche_pi_dichotomique 1000.0 100000.0 0.00001);;

(* EX5 *)
let min a = fun b -> if a < b then a else b;;

let plafonne_a n = fun m -> if m > n then n else m;;

let permute_args f = fun a -> fun b -> f b a;;

(* EX6 *)

let est_facteur_position u v i = 
  let rec aux u v i = 
    if i = String.length u then true
    else if u.[i] = v.[i] then aux u v (i+1)
    else false
  in aux u v i;;