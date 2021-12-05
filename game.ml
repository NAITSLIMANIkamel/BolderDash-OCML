open Structures
open Type
open Matrix
exception Dead
exception Bould of int*int
exception Win
let position_after_move (i,j) dir=
  match dir with
    E-> (i,j+1)
  |W->(i,j-1)
  |S->(i-1,j)
  |N->(i+1,j)

let move (_, _) (_: dir) =assert  false

let player_turn (g: game) (d: dir) =
  let (x,y)=position_after_move g.player d
  in
  match(Matrix.read x y g.map) with
  |Empty->{g with player=(x,y)}
  |Dirt->let (i,j)=g.player in {map=(Matrix.set i j Empty g.map);player=(x,y);diamonds=g.diamonds}
  |Diamonts->let (i,j)=g.player in {map=(Matrix.set i j Empty g.map);player=(x,y);diamonds=g.diamonds+1}
  |Stone->g
  |Walnut|Boulder->match d with
    E|W->let(z,t)=position_after_move (x,y) d in
      (match  (Matrix.read z t g.map)  with
         Empty->let (i,j)=g.player in
         {map=Matrix.set i j Empty (Matrix.set z t Boulder g.map);player=(x,y);diamonds=g.diamonds}
       |_->g)
  |_->g
let world_turn (_: game) =()

let is_empty (i,j) (g:game) =
  match (read i j g.map) with
  |Empty->true
  |_->false

let position_after_fall g (i,j)=
  if is_empty (i-1,j) g then (i-1,j)
  else if is_empty (i-1,j-1) g && is_empty (i,j-1) g && (read (i-1) j g.map)=Boulder then (i-1,j-1)
  else if is_empty (i-1,j+1) g && is_empty (i,j+1) g && (read (i-1) j g.map)=Boulder then (i-1,j+1)
  else (i,j)

let move_boulder_step g (i,j)=
  let (x,y)=position_after_fall g (i,j)
  in if(x,y)=g.player then raise Dead
  else
    {g with map=Matrix.set i j Empty (Matrix.set x y Boulder g.map)}

let find_mouvable_boulder g =
  try
  Matrix.iter (fun i j a -> if a!=Boulder then()
                else if position_after_fall g (i,j)=(i,j) then()
                else raise (Bould(i,j))) g.map;None
  with Bould(x,y)->Some(x,y)

let word_turn g=
  let rec loop g=
    match find_mouvable_boulder g with
      None->g
    |Some pos->loop (move_boulder_step g pos) 
  in
      loop g

let win (g:game) = g.diamonds=0

let compter_diamonts m=
  Matrix.fold (fun i j a nb->
      if i=j then ();
        match a with
        Diamonts->nb+1
          |_->nb) m
