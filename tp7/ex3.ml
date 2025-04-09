type 'a item = Item of 'a * 'a flot and 'a flot = 'a item Lazy.t ;;

let scons x f = lazy (Item(x, f)) ;;
let get_item = function lazy (Item(hd, tl)) -> (hd, tl) ;;

(* let rec ints_from_strict n = scons n (ints_from_strict (n+1)) ;; *)
let rec ints_from n = lazy(Item(n, (ints_from (n+1)))) ;;

let shead s = 
  let (hd, _) = get_item s in hd
;;

let stail s = 
  let (_, tl) = get_item s in tl
;;

let rec snth s n =
  if n = 0 then (shead s)
  else snth (stail s) (n-1) 
;;

let rec sfilter p s =
  let (hd, tl) = get_item s in
  if p hd then lazy(Item(hd, sfilter p tl))
  else sfilter p tl 
;;

let rec smap2 f s1 s2 =
  let (hd1, tl1) = get_item s1 in
  let (hd2, tl2) = get_item s2 in
  lazy(Item((f hd1 hd2), (smap2 f tl1 tl2)))
;;

let ( ++ ) = smap2 ( + ) ;;
let ( ** ) = smap2 ( * ) ;;

let rec siter n f s =
  if n > 0 then
    let (hd, tl) = get_item s in
    f hd;
    siter (n-1) f tl
;;

let sprint n s =
  print_string "[";
  let rec aux i s =
    if i = 0 then print_string ", ..."
    else
      let (hd, tl) = get_item s in
      print_int hd;
      if i > 1 then print_string ", ";
      aux (i - 1) tl
  in
  aux n s;
  print_string "]";
  print_newline()
;;

let () = sprint 10 (ints_from 0) ;;

(* let rec odds_from n = scons n (odds_from (n + 2)) ;; *)

let is_odd = fun x -> x mod 2 = 1 ;;
let odds_stream = sfilter is_odd (ints_from 0) ;;

let () = sprint 10 odds_stream ;;

let factorials =
  let rec fact_aux n acc =
    lazy(Item(acc, (fact_aux (n + 1) (acc * (n + 1)))))
  in
  fact_aux 0 1
;;

let () = sprint 10 factorials ;;

let fib =
  let rec fib_aux a b =
    lazy (Item(a, fib_aux b (a + b)))
  in
  fib_aux 0 1
;;

let () = sprint 10 fib ;;
