(***********************)
(* Projet 2I008, 18/19 *)
(***********************)

(** Ce fichier contient une structure de map (int -> 'a), les paires
   clé/valeur sont stockés dans une liste ordonnée sur les clés. *)

(* à compléter *)
let remove_assoc (k: int) (l: (int * 'a) list): (int * 'a) list =
  let rec loop i l =
  match l with
      []->l
    |(x,a)::ls->if (x>i)then l
      else if(x=i) then ls
      else (x,a)::(loop i ls)
  in loop k l

(* à ne pas toucher *)
let pp_assoc
    (pp1: Format.formatter -> 'a -> unit)
    (fmt: Format.formatter)
    (l: (int * 'a) list): unit =
  Format.fprintf fmt "@[[%a@]]"
    (Format.pp_print_list
       ~pp_sep:(fun fmt () -> Format.fprintf fmt ",")
       (fun fmt (k, v) -> Format.fprintf fmt "(%a, %a)" Format.pp_print_int k pp1 v)
    ) l

(* Définition du type des fonctions des entiers vers 'a *)
type 'a t =
  { assoc : (int * 'a) list;
    default : 'a
  }

(* à compléter *)
let constant (d: 'a): 'a t = {assoc=[];default=d}
(* à compléter *)
let find (k: int) (m: 'a t): 'a =
  try
  let rec loop l =
    match l with
    []->raise Not_found
    |(x,a)::ls->if (k==x) then a
      else if(k<x) then raise Not_found
      else loop ls
in loop m.assoc
with Not_found ->m.default

(* à compléter *)
let set (k: int) (v: 'a) (m: 'a t): 'a t =
  if v=m.default then {m with assoc=remove_assoc k m.assoc }
  else
    let rec loop l =
      match l with
        []->[(k,v)] 
      |(x,a)::ls-> if (k==x) then (x,v)::ls
        else if(k<x) then (k,v)::(x,a)::ls
        else (x,a)::loop ls
    in {m with assoc=loop m.assoc}

(* à compléter *)
let fold (f: int -> 'a -> 'b -> 'b) (a: int) (b: int) (m: 'a t) (init: 'b): 'b =
  let rec loop b m acc=
    if(a=b)then f b (find b m) acc
    else f b (find b m) (loop (b-1) m acc)
  in loop b m init

(* à ne pas toucher *)
let pp
    (pp1: Format.formatter -> 'a -> unit)
    (fmt: Format.formatter)
    (m: 'a t): unit =
  Format.fprintf fmt "@[{default:%a;@, assoc:[@[%a@]]@,}@]"
    pp1 m.default
    (Format.pp_print_list
       ~pp_sep:(fun fmt () -> Format.fprintf fmt ",@,")
       (fun fmt (k, v) -> Format.fprintf fmt "(%a, %a)" Format.pp_print_int k pp1 v)
    ) m.assoc
    
