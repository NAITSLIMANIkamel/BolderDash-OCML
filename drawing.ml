(***********************)
(* Projet 2I008, 18/19 *)
(***********************)

(** Ce fichier contient les fonctions d'affichage graphique du jeu *)
open Structures
open Graphics
open Type
open Matrix

type scale = (int * int) -> (int * int)

let gray = rgb 105 105 105
let brown = rgb 139 69 19
let diamond = rgb 185 242 255

let compute_scaler (g: game): scale =
  let sx = size_x () in
  let sy = size_y () in
  let rx = (sx - sx / 5) / g.map.larg in
  let ry = (sy - sy / 5) / g.map.haut in
  let sxi = rx * g.map.larg in
  let syi = ry * g.map.haut in
  let margx = (sx - sxi) / 2 in
  let margy = (sy - syi) / 2 in
  fun (x, y) ->
    (margx + x * rx, margy + y * ry)

(* à compléter *)
let draw_rect_cell (c: color) ((i, j): int * int) (scaler: scale): unit =
  set_color c;
  let (x,y)=scaler (i,j) in
  let (z,t)=scaler(i+1,j+1)in
  draw_rect x y z t

(* à compléter *)
let fill_rect_cell (c: color) ((i, j): int * int) (scaler: scale): unit =
set_color c;
let (x,y)=scaler (i,j) in
let (z,t)=scaler(i+1,j+1)in
fill_rect x y (z-x) (t-y)

(* à compléter *)
let fill_diamond_cell (c: color) ((i, j): int * int) (scaler: scale): unit =
set_color c;
let (x,y)=scaler (i,j) in
let (z,t)=scaler(i+1,j+1)in
fill_poly [|(x,(t+y)/2);((x+z)/2,y);(z,(t+y)/2);((x+z)/2,t)|]
(* à compléter *)
let fill_circle_cell (c: color) ((i, j): int * int) (scaler: scale): unit =
set_color c;
let (x,y)=scaler (i,j) in
let (z,t)=scaler(i+1,j+1)in
fill_circle (x+((z-x)/2)) (y+((t-y)/2)) ((t-y)/2)

let draw_walnut (i,j) scaler=
let (x,y)=scaler (i,j) in
let (z,t)=scaler(i+1,j+1) in
draw_ellipse  ((x+z)/2) ((y+t)/2) (z-x) (((z-x)*2)/3)

(* à compléter *)
let draw_cell (c: cell) (i, j) (scaler: scale): unit =
  match c with
    Empty->()
  |Stone->fill_rect_cell black (i,j) scaler
  |Dirt->fill_rect_cell brown (i,j) scaler
  |Diamonts->fill_diamond_cell blue (i,j) scaler
  |Boulder->fill_circle_cell gray (i,j) scaler
  |Walnut->draw_walnut (i,j) scaler
(* à compléter *)
let draw_map (m: map) (scaler: scale): unit =
  Matrix.iter (fun i j c-> draw_cell c (j,i) scaler) m
(* à compléter *)
let draw_player ((i,j): (int * int)) (scaler: scale): unit =
  fill_circle_cell red (i,j) scaler 
let draw_game (g: game) (scaler: scale) =
  draw_map g.map scaler;
  draw_player g.player scaler;
  synchronize ()

let reinit_graphics (): unit =
  clear_graph ()

let init_graphics () =
  open_graph "";
  auto_synchronize false
