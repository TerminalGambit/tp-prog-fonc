(* Exercice 1 *)

let print_tableau tab n =
  for i=0 to n-1 do
    print_int tab.(i);
  done;
  print_newline() 
;;

let swap tab i j =
  let tmp = tab.(i) in
  tab.(i) <- tab.(j);
  tab.(j) <- tmp
;;

let tableau = [|1;2;3;4|] ;; (* tableau mutable *)

print_tableau tableau 4 ;; (*1234*)

swap tableau 1 2 ;;

(* print_tableau tableau 4 ;; 1324 *)

let shuffle tab n =
  for _=0 to (n*10) do
    let i = Random.int n in
    let j = Random.int n in
    swap tab i j;
    print_tableau tab n
  done;
;;

shuffle tableau 4 ;;
