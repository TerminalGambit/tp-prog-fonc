(* Exercice 2 *)

let print_tableau tab n =
  for i=0 to n-1 do
    print_int tab.(i);
    print_string " ";
  done;
  print_newline() 
;;

let print_matrice mat m n =
  for i=0 to m-1 do
    print_tableau mat.(i) n;
  done;
;;

let print_couple couple =
  print_int (fst couple);
  print_string " ";
  print_int (snd couple);
  print_newline()
;;

let dimension mat =
  let m = Array.length mat in
    let n = Array.length mat.(0) in
    (m, n) ;;

let matrice = [|[|1;2|];[|3;4|]|] ;;

(* print_couple (dimension matrice) ;; *)

(* Le problème était que l'on instantiait une référence au même élément à chaque fois *)

let make_matrix m n e =
  Array.init m (fun _ -> Array.init n (fun _ -> e));;

let m = make_matrix 2 2 0 ;;
m.(0).(0) <- 1 ;;
print_matrice m 2 2 ;;

let +~ mat1 mat2 =
  let n1 = Array.length mat1 in
  let m1 = Array.length mat1.(0) in
  let n2 = Array.length mat2 in
  let m2 = Array.length mat2.(0) in
  if (n1 = n2) then
    if (m1 = m2) then
      let res = make_matrix n1 m1 0 in
        for i=0 to 


(* let *~ mat1 mat2 = 
  let n1 = Array.length mat1 in
  let m1 = Array.length mat1.(0) in
  let n2 = Array.length mat2 in
  let m2 = Array.length mat2.(0) in
  if (n1 = m2) then
    if (n2 = m1) then
      for i=0 to n1 do
        f *)
    




(* 
      [1, 2]
      [3, 4]
[1, 2] 
[3, 4]

for i in range(len(mat)):
for j in range(len(mat[0]))

*)