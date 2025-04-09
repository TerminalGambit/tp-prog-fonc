type 'a item = Item of 'a * 'a flot
and 'a flot = 'a item Lazy.t

let scons x f = lazy (Item(x, f))
let get_item = function lazy (Item(hd, tl)) -> (hd, tl)

let rec ints_from_strict n = scons n (ints_from_strict (n+1)) ;;
let rec ints_from n = lazy(Item(n, (ints_from (n+1)))) ;;
