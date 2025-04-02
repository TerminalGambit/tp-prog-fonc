type poly = float list ;;

let eval x (polynome:poly) = 
  let rec aux x polynome i = 
    match polynome with
    | [] -> 0.
    | hd::tl -> hd *. (x ** i) +. (aux x polynome (i +. 1.))
  in aux x polynome 0.
;;

let rec prod_ext x = function
| [] -> []
| hd :: tl -> (hd *. x) :: prod_ext x tl 
;;

let rec somme (l1:poly) (l2:poly) = 
  match l1, l2 with
  | [], [] -> []
  | hd1::tl1, hd2::tl2 -> (hd1 +. hd2) :: (somme tl1 tl2)
  | [], l | l, [] -> l
;;

let produit (l1:poly) (l2:poly) =
  let rec mult_shift l1 l2 i =
    match l1 with
    | [] -> []
    | hd::tl ->
      let scaled = prod_ext hd l2 in
      let shifted = (List.init i (fun _ -> 0.)) @ scaled in
      somme shifted (mult_shift tl l2 (i + 1))
  in
  mult_shift l1 l2 0
;;

let p:poly = [1.; 0.; 1.] ;;     (* X^2 + 1 *)
let q:poly = [-1.; 1.; 0.; 1.] ;; (* X^3 + X - 1 *)

let pq:poly = produit p q ;;
let deux_p:poly = prod_ext (-.2.) p ;;
let r = somme pq deux_p ;;

let afficher (p:poly) = 
  let rec aux p x = 
    match p with
    | [] -> Printf.printf ""
    | hd::tl when x = 0 -> aux tl (x+1); Printf.printf "%.f" hd
    | hd::tl when hd=0. -> aux tl (x+1)
    | hd::tl when hd=1. -> aux tl (x+1); Printf.printf "X^%d " x
    | hd::tl -> aux tl (x+1); Printf.printf "%.fX^%d " hd x
  in aux p 0;
  Printf.printf "\n"
;;

let () = afficher r ;;

(* #load "graphics.cma";; *)
(* sinon compiler avec `ocamlfind ocamlc - package graphics graphics.cma tp5.sol.ml` *)

Graphics.open_graph "400x400 "

