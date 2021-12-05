open Boulder_dash
open Graphics
open Game
open Type
let handle (press: Graphics.status) (g: game) : game =  
  let g=world_turn g in
  if press.key='E' then world_turn (player_turn g N)
  else if press.key='F'then world_turn (player_turn g E)
  else if press.ke='S'then world_turn (player_turn g W)
  else if press.key='D'then world_turn (player_turn g S )
  else if press.key='Q' then exit 1
  else g
  
let rec turn g =
  let st = wait_next_event [Key_pressed] in
  let g = handle st g in
  let scale = Drawing.compute_scaler g in
  Drawing.reinit_graphics ();
  let () = Drawing.draw_game g scale in
  turn g

let game ()  =
  let game = Parse.parse_file "data/level0.lv" in
  Drawing.init_graphics ();
  let scale = Drawing.compute_scaler game in
  Drawing.reinit_graphics ();
  let () = Drawing.draw_game game scale in
  turn game

let () =
  game ()
