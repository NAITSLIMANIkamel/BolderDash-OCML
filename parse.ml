open Structures
open Type

(* TODO *)
let cell_of_char c: cell option = match c with
  | ' ' -> Some Empty
  | 'O' -> Some Boulder
  | 'X' -> Some Stone
  | '-' -> Some Dirt
  | 'V' -> Some Diamonts
  | 'N'-> Some Walnut
  | 'P' -> None
  | _ -> failwith "unrecognized char"

let parse_file (f: string) =
  let ic = open_in f in
  let haut = int_of_string (input_line ic) in
  let larg = int_of_string (input_line ic) in
  let joueur = ref (0,0) in
  let diamonds=ref 0 in
  let map = ref (Matrix.make haut larg Empty) in
  (* i : ligne (inversion à cause de Graphics, j : colonne *)
  for i = haut - 1 downto 0 do
    let line = input_line ic in
    for j = 0 to larg - 1 do
      match cell_of_char line.[j] with
      | None -> joueur := (j,i)
      | Some b -> if (b=Diamonts|| b=Walnut) then diamonds:= !diamonds+1 ; map := Matrix.set i j b !map;
    done
  done;
  { player = !joueur;
    map = !map;
    diamonds = !diamonds
  }
