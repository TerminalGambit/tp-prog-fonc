type booleen = Vrai | Faux ;;
type lazybool = booleen Lazy.t ;;

let ou a b =
  Lazy.from_fun (fun () ->
    match Lazy.force a with
    | Vrai -> Vrai
    | Faux -> Lazy.force b)
;;

let vrai = lazy (print_string "!"; Vrai) ;;
let faux = lazy (print_string "!"; Faux) ;;
let _ = Lazy.force (ou (ou faux faux) (ou vrai vrai)) ;;

