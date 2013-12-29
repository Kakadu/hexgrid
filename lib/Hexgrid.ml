open Printf

module IntPair = struct
  type t = int * int
  let make x y = (x,y)
  let hash (a,b) = a*1000 + b
  let equal (a,b) (c,d) = (a=c) && (b=d)
  let compare (a,b) (c,d) =
    let ans1 = compare a c in
    if ans1=0 then compare b d
    else ans1
end

module CellHash = Hashtbl.Make(IntPair)

module type CELLITEM = sig
  type t
  val create : unit -> t
(*  val left : t -> int
  val top  : t -> int *)
end
module type SIZE = sig
  val s : float
  val h : float
end

let pi = 2. *. asin 1.
let tan30 = tan (pi /. 6.)

module Make (Item: CELLITEM) (MainSizes: SIZE) = struct
  type grid = Item.t CellHash.t
  let create ~width ~height : grid =
    let h = CellHash.create 1103 in
    for i=0 to width-1 do
      for j=0 to height-1 do
        CellHash.add h (IntPair.make i j) (Item.create ())
      done;
    done;
    h

  module Sizes = struct
    include MainSizes
    let r = s *. cos (pi /. 6.)
    let b = s +. 2. *. h
    let a = 2. *. r
  end

  let get ~left ~top t = 
    try Some (CellHash.find t (IntPair.make left top))
    with Not_found -> None

  let to_screen ~left ~top =
    let x = float_of_int left in
    let y = float_of_int top in
    let ansy = y *. Sizes.(h +. s) in
    (*printf "toScreen.y = %f, ansy=%f\n%!" y ansy;*)

    if top mod 2 = 0 then Sizes.(x *. 2. *. r, ansy)
    else Sizes.(x *. 2. *. r +. r, ansy)

  let of_screen ~mousex ~mousey =
    let open Sizes in
    let _2r = 2. *. r in

    let sectx = int_of_float (float_of_int mousex /. _2r) in
    let secty = int_of_float (float_of_int mousey /. (s +. h)) in
    let sect_pix_x = float_of_int (mousex mod (int_of_float _2r)) in
    let sect_pix_y = float_of_int (mousey mod (int_of_float (s +. h))) in

    let typ = if secty mod 2 = 0 then `A else `B in
    (*let eps = 0.001 in*)
    let id = fun x -> x in
    let dec x = x - 1 in
    let (>>) (a,b) (f,g) = (f a, g b) in
    match typ with
    | `A ->
      (sectx,secty) >>
      (if sect_pix_x < r then begin
        if sect_pix_y /. (r -. sect_pix_x) < tan30 then (dec,dec)
        else (id,id)
      end else begin
        if sect_pix_y /. (sect_pix_x -. r) < tan30 then (id,dec)
        else (id,id)
      end)
    | `B ->
      (sectx, secty)
      >> (if sect_pix_x >= r then begin
        if sect_pix_y /. (_2r -. sect_pix_x) < tan30 then (id,dec)
        else (id,id)
      end else begin
        if sect_pix_y /. sect_pix_x < tan30 then (id,dec)
        else (dec,id)
      end)

end
