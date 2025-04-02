(* decommenter la ligne ci-dessous si vous copier/coller dans un toplevel*)
(* #load "graphics.cma";; *)
(* sinon compiler avec `ocamlfind ocamlc - package graphics graphics.cma tp5.sol.ml` *)

Graphics.open_graph " 900x600";;

type point = {abs : float; ord : float}
type vector = {x : float; y : float}
type segment = {p1 : point; p2 : point}

let make_vector p1 p2 =
  let x = p2.abs -. p1.abs in
  let y = p2.ord -. p1.ord in
  {x; y}

let mul_scal k {x; y} =
  {x = k *. x; y = k *. y}

let rotate_vector {x; y} theta =
  let c = cos theta in
  let s = sin theta in
  let x' = x *. c -. y *. s in
  let y' = x *. s +. y *. c in
  {x = x'; y = y'}

let translate_point {abs; ord} {x; y} =
  {abs = abs +. x; ord = ord +. y}

let draw_segment {p1; p2} =
  Graphics.moveto (int_of_float p1.abs) (int_of_float p1.ord);
  Graphics.lineto (int_of_float p2.abs) (int_of_float p2.ord)

let process_segment {p1 = a; p2 = b} theta =
  let v = make_vector a b in
  let d = translate_point a (rotate_vector v (Float.pi /. 2.)) in
  let c = translate_point d v in
  let e = translate_point d @@ mul_scal  (cos theta) @@ rotate_vector v theta in
  let ab = {p1 = a; p2 = b} in
  let bc = {p1 = b; p2 = c} in
  let cd = {p1 = c; p2 = d} in
  let da = {p1 = d; p2 = a} in
  let de = {p1 = d; p2 = e} in
  let ec = {p1 = e; p2 = c} in
  draw_segment ab;
  draw_segment bc;
  draw_segment cd;
  draw_segment da;
  draw_segment de;
  draw_segment ec;
  (de, ec)

let rec pythagore n theta segment =
  if n = 0
  then draw_segment segment
  else begin
    let (s1,s2)=process_segment segment theta
    in pythagore (n-1) theta s1; pythagore (n-1) theta s2
  end

(* (* version recursive terminale *)
let pythagore n theta segment =
  let rec step acc = function
    | [] -> acc
    | hd :: tl ->
      let (s1, s2) = process_segment hd theta in
      step (s1 :: s2 :: acc) tl in
  let rec iter n l =
    if n = 0 then l else iter  (n - 1) @@ step [] l in
   draw_segment segment;
   ignore @@ iter n [segment]
*)

let _ =
  let a = {abs = 500.; ord = 100.} in
  let b = {abs = 600.; ord = 100.} in
  let ab = {p1 = a; p2 = b} in
  draw_segment ab;
  pythagore 4 (Float.pi /. 6.) ab


type pixel = {
  coord : int * int;
  affix : Complex.t;
  mutable value : Complex.t;
  mutable out : bool;
}

let init_pixel i j x y d =
  let re = x +. d *. (float i) in
  let im = y +. d *. (float j) in
  {coord = (i, j); affix = {re; im}; value = Complex.zero; out = false}

let make_pixels m n x y d =
  Array.init m (fun i -> Array.init n (fun j -> init_pixel i j x y d))

let compute_next pixel =
  let {
    coord = (i, j);
    affix = c;
    value = z;
    out
  } = pixel in
  if not out then
    let z' = Complex.add (Complex.mul z z) c in
    pixel.value <- z';
    if Complex.norm z' > 2.0 then
      begin
        pixel.out <- true;
        Graphics.plot i j
      end

let make_colors n =
   let colors = Array.make (3 * n) Graphics.white in
   let d = 255 / n in
   for i = 0 to n - 1 do
     colors.(i) <- Graphics.rgb (255 - i * d) (i * d) 0;
     colors.(n + i) <- Graphics.rgb 0 (255 - i * d) (i * d);
     colors.(2 * n + i) <- Graphics.rgb (i * d) 0 (255 - i * d)
   done;
   colors

let get_value array k =
  let n = Array.length array in
  array.(k mod n)

let mandelbrot w h n colors =
  let d = max (3.0 /. (float w)) (2.0 /. (float h)) in
  let pixels = make_pixels w h (-. 2.) (-. 1.) d in
  Graphics.set_color Graphics.black;
  Graphics.fill_rect 0 0 w h;
  for k = 0 to n - 1 do
    Graphics.set_color (get_value colors k);
    for i = 0 to w - 1 do
      for j = 0 to h - 1 do
        compute_next pixels.(i).(j)
      done
    done
  done


let _ =
  mandelbrot 900 600 1000 (make_colors 16)
