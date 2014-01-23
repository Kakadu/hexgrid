module List = struct
  include List
  let init f n =
    let ans = ref [] in
    for i=n-1 downto 0 do ans:=(f i):: !ans done;
    !ans
end

module Option = struct
  type 'a t = 'a option
  let map ~f = function
    | Some x -> Some (f x)
    | None -> None

  let (>>=) x f =
    match x with
    | Some x -> f x
    | None -> None
  let is_some = function Some _ -> true | _ -> false
  let is_none = function None -> true | _ -> false
  let get ~default = function Some x -> x | None -> default
end

let fold_for2 ~n1 ~n2 ~m1 ~m2 ~f ~init =
  let ans = ref init in
  for i=n1 to n2 do
    for j=m1 to m2 do
      ans := f !ans i j
    done
  done;
  !ans

module StringMap = Map.Make(String)
type texture_map = Tsdl.Sdl.texture StringMap.t

open Tsdl.Sdl

module MyColors = struct
  let red = Color.create ~r:0 ~g:255 ~b:0 ~a:255
end
