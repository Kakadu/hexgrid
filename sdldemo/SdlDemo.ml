open Tsdl.Sdl
open Printf
open Helpers

let (>>=) a f = match a with `Error -> `Error
                           | `Ok x -> f x
let (>>) a f  = match a with `Error -> `Error
                           | `Ok x -> `Ok (f x)

let app_title = "Hexgrid demo2: Details inside map hexagons"
let screen_width = 1000
let screen_height = 700
let tile_size = 160

let render_texture ~texture ~renderer ~rect =
  render_copy renderer texture ~dst:rect

let render_texture_in ~texture ~renderer ~x ~y =
  query_texture texture >>= fun (_,_,(w,h)) ->
  render_texture ~texture ~renderer ~rect:(Rect.create ~x ~y ~w ~h)

let uint8_of_int = Unsigned.UInt8.of_int


let main_loop ~poll ~draw =
  let e = Event.create () in
  let rec loop last =
    match last with
    | `Error -> `Error
    | `Ok _ -> begin
        let quit = ref false in
        while poll_event (Some e) do
          quit := poll e
        done;
        draw () >>= fun _ ->
        if not !quit then loop (`Ok ()) else `Error
     end
  in
  loop (`Ok ())

module Cell = struct
  type t = {
    mutable visible: [ `No | `Yes | `Fog ];
    mutable terrain: [ `Grass | `Hill | `Mountain ];
    mutable warUnit: int option;
  }

  let create () = { visible = `No
                  ; terrain = `Grass
                  ; warUnit = Some 1
                  }
end

module Map = Hexgrid.Make(Cell)
  (struct
     let pi = 2. *. asin 1.
    let s = 50.
    let h = s *. sin (pi /. 6.)
   end)

type options = {
  mutable view_x : int;
  mutable view_y : int;
         width_n : int;
        height_n : int;
  mutable textures: texture_map;
}

let options = {
    view_x = 0;
    view_y = 0;
   width_n = 16;
  height_n = 10;
  textures = StringMap.empty;
}

let maingrid = Map.create ~width:options.width_n ~height:options.height_n

let init_map () =
  let with_cell left top f =
    match Map.get ~left ~top maingrid with
    | Some x -> f x
    | None -> assert false
  in
  with_cell 1 1 (fun x -> x.Cell.visible <- `Fog);
  with_cell 1 2 (fun x -> x.Cell.visible <- `Yes;
    x.Cell.terrain <- `Hill
  );
  with_cell 2 1 (fun x -> x.Cell.visible <- `Yes;
    x.Cell.terrain <- `Mountain
  )

let () = init_map ()

let load_textures (renderer: Tsdl.Sdl.renderer) : unit  result =
  img_load_texture renderer "res/image.png" >>= fun image ->
  options.textures <- StringMap.add "smile" image options.textures;
  img_load_texture renderer "res/unit_icons/warrior.png" >>= fun image ->
  options.textures <- StringMap.add "warrior" image options.textures;
  `Ok ()

let get_texture name =
  try StringMap.find name options.textures
  with Not_found -> failwith (sprintf "No such texture: %s" name)

let paint_cell ?(xoffset=0) ?(yoffset=0) renderer ~left ~top : unit result =
  let cell = Map.(get ~left ~top maingrid) in
  Option.map cell ~f:(fun cell ->
    let (x,y) = Map.to_screen ~left ~top in
    let open Map.Sizes in
    set_render_draw_color renderer 255 255 0 255 >>= fun () ->
    (*     u
     *   f w b'
     *   e v c
     *     d
    **)
    let create x y = Point.create (int_of_float x - xoffset) (int_of_float y - yoffset) in
    let u = create (x +. a /. 2.) y in
    let f = create x (y +. h) in
    let e = create x (y +. h +. s) in
    let d = create (x +. a /. 2.) (y +. b) in
    let c = create (x +. a) (y +. h +. s) in
    let b'= create (x +. a) (y +. h) in
    let w = create (x +. a /. 2.) (y +. h) in (*
    let v = create (x +. a /. 2.) (y +. h +. s) in *)

    let fill_hex r g b a =
        let (_:bool) = SDL_gfx.filledTrigonRGBA renderer
          (Point.x u) (Point.y u) (Point.x f) (Point.y f) (Point.x b') (Point.y b')
          r g b a in
        let (_:bool) = SDL_gfx.filledTrigonRGBA renderer
          (Point.x e) (Point.y e) (Point.x c) (Point.y c) (Point.x d) (Point.y d)
          r g b a in
        let middle_rect = Rect.create ~x:(Point.x f) ~y:(Point.y f+1)
          ~w:(int_of_float Map.Sizes.a + 1) ~h:(int_of_float s - 1) in
        render_fill_rect renderer (Some middle_rect)
    in
    (* fog -- gray color, not explored -- red *)
    let () = match cell.Cell.visible,cell.Cell.terrain with
      | (`Fog,_) ->
        fill_hex (uint8_of_int 139) (uint8_of_int 131) (uint8_of_int 120) (uint8_of_int 255) |> ignore
      | (`Yes, `Grass) ->
        fill_hex (uint8_of_int 0) (uint8_of_int 255) (uint8_of_int 0) (uint8_of_int 64) |> ignore;
        ()
      | (`Yes, `Hill) ->
        fill_hex (uint8_of_int 0) (uint8_of_int 255) (uint8_of_int 64) (uint8_of_int 64) |> ignore;

        let (z,z') = (int_of_float (x+.a/.2.)-xoffset, int_of_float (y+.s+.h/.2.) - yoffset) in
        let hill_points =
          [ Point.(x e , y e )
          ; Point.(x c , y c )
          ; (int_of_float (x +. 2. *.  a /. 3.) - xoffset , int_of_float (y +. h +. s /. 2.) - yoffset)
          ; (int_of_float (x +.  a /. 3.) - xoffset , int_of_float (y +. h +. s /. 2.) - yoffset)
          ] in
        let _ans = SDL_gfx.filledPolygonRGBA renderer hill_points
          (uint8_of_int 0) (uint8_of_int 128) (uint8_of_int 64) (uint8_of_int 255) in
        set_render_draw_color renderer 0 0 0 255 |> ignore;
        render_draw_lines renderer
          [ e
          ; c
          ; create (x +. 2. *.  a /. 3.)  (y +. h +. s /. 2.)
          ; create (x +.        a /. 3.)  (y +. h +. s /. 2.)
          ] |> ignore;
        ()
      | (`Yes, `Mountain) ->
        fill_hex (uint8_of_int 0) (uint8_of_int 255) (uint8_of_int 128) (uint8_of_int 64) |> ignore;
        (* drawing mountain *)
        SDL_gfx.filledPieRGBA renderer
          (Point.x u) (Point.y f) 50 (90-30) (90+30)
          (uint8_of_int 99) (uint8_of_int 66) (uint8_of_int 33) (uint8_of_int 255) |> ignore;
        SDL_gfx.filledPieRGBA renderer
          (Point.x u) (Point.y f) 20 (90-30) (90+30)
          (uint8_of_int 255) (uint8_of_int 255) (uint8_of_int 255) (uint8_of_int 255) |> ignore;
        SDL_gfx.pieRGBA renderer
          (Point.x u) (Point.y f) 50 (90-30) (90+30)
          (uint8_of_int 0) (uint8_of_int 0) (uint8_of_int 0) (uint8_of_int 255) |> ignore;
        ()
      | (`No,_)  -> ()
    in
    let () = match cell.Cell.warUnit with
      | None -> ()
      | Some _ ->
        render_texture_in ~texture:(get_texture "warrior") ~renderer ~x:(Point.x w) ~y:(Point.y w) |> ignore
    in
    set_render_draw_color renderer 255 255 0 255 >>= fun () ->
    render_draw_lines renderer [ u; f; e; d; c; b'; u ]
  ) |> Option.get ~default:(`Ok ())

(* adjust coordinates of they are out of range *)
let adjust_viewxy () =
  let max_x = (float_of_int options.width_n  *. 2. +. 1.) *. Map.Sizes.r |> int_of_float in
  let max_y =
    (if options.height_n mod 2 = 0 then Map.Sizes.(float_of_int options.height_n *. (s +. h) +. h)
     else Map.Sizes.(float_of_int options.height_n *. (s +. h) +. h +. s)
    ) |> int_of_float
  in
  if max_x < screen_width then
    options.view_x <- 0
  else begin
    if options.view_x < 0  then options.view_x <- 0;
    if options.view_x + screen_width > max_x then options.view_x <- max_x - screen_width  - 1;
  end;

  if max_y < screen_height then
    options.view_y <- 0
  else begin
    if options.view_y < 0  then options.view_y <- 0;
    if options.view_y + screen_height> max_y then options.view_y <- max_y - screen_height
  end

let main () =
  create_window app_title ~x:300 ~y:50 ~w:screen_width ~h:screen_height Window.opengl >>= fun w ->
  create_renderer ~flags:Renderer.(accelerated + presentvsync) w >>= fun renderer ->
  load_textures renderer >>= fun () ->
  set_render_draw_color renderer 255 255 255 0 >>= fun () ->


  main_loop ~poll:(fun e ->
    let x = Event.get e Event.typ in
    if (x = Event.quit) ||
       (x=Event.key_down && Event.get e Event.keyboard_keycode = K.escape) then true
    else if x = Event.key_down  then begin
      (* handle key press *)
      let keycode = Event.get e Event.keyboard_keycode in
      let good_code =
        if keycode = K.right then `Right else
        if keycode = K.down then  `Down else
        if keycode = K.up then    `Up else
        if keycode = K.left then  `Left else
        `Dummy
      in
      let () = match good_code with
        | `Right  -> print_endline "right";
          options.view_x <- options.view_x + 30;
          adjust_viewxy ();
        | `Down   -> print_endline "down";
          options.view_y <- options.view_y + 30;
          adjust_viewxy ();
        | `Up     -> print_endline "up";
          options.view_y <- options.view_y - 30;
          adjust_viewxy ();
        | `Left   -> print_endline "left";
          options.view_x <- options.view_x - 30;
          adjust_viewxy ();
        | _       -> ()
      in
      false
    end else false
  ) ~draw:(fun () ->
    set_render_draw_color renderer 0 0 0 0 >>= fun () ->
    render_clear renderer >>= fun _ ->
    (*render_texture_in ~texture:image ~renderer ~x:0 ~y:0 >>= fun _ ->*)

    (* We should select cells to drawing according to options.view_(x/y).
     * We don't want to redraw everything.
     *)
    begin
      let (n1,m1,n2,m2) =
        (*  x1,y1         x2,y2
         *
         *  x3,y3         x4,y4
         *)
        let (x1,y1) = Map.of_screen ~mousex:options.view_x ~mousey:options.view_y in
        let (x2,y2) = Map.of_screen ~mousex:(options.view_x+screen_width) ~mousey:options.view_y in
        let (x3,y3) = Map.of_screen ~mousex:options.view_x ~mousey:(options.view_y+screen_height) in
        let (x4,y4) =
          Map.of_screen ~mousex:(options.view_x+screen_width) ~mousey:(options.view_y+screen_height) in
        (min x1 x3 - 1, min y1 y2 - 1,
         max x2 x4 + 1, max y3 y4 + 1)
        (*(0,0,width_n-1,height_n-1) *)
      in
      fold_for2 ~n1 ~m1 ~n2 ~m2 ~init:(`Ok ()) ~f:(fun acc left top -> acc >>= fun _ ->
        paint_cell renderer ~xoffset:options.view_x ~yoffset:options.view_y ~left ~top
      )
    end >>= fun () -> (*
    set_render_draw_color renderer 255 0 0 128 >>= fun () ->
    render_draw_line renderer 50 50 500 250 >>= fun () ->

    set_render_draw_color renderer 0 255 0 128 >>= fun () ->
    render_draw_line renderer 500 250  250 400 >>= fun () ->

    set_render_draw_color renderer 0 0 255 128 >>= fun () ->
    render_draw_line renderer 250 400 50 50 >>= fun () ->

    set_render_draw_color renderer 0 0 0 255 >>= fun () ->
    SDL_gfx.hline_color renderer 30 530 300 (Int32.of_string "0xFF0000FF") |> ignore;
    SDL_gfx.thickLineRGBA renderer 0 400 500 450 4
      (uint8_of_int 0) (uint8_of_int 255) (uint8_of_int 0) (uint8_of_int 128) |> ignore; *)
    render_present renderer;
    `Ok ()
  ) >>= fun _ ->
  destroy_renderer renderer;
  destroy_window w;
  `Ok ()


let (_ : _ result) =
  match init Init.video with
  | `Error -> exit 1
  | `Ok () -> `Ok (main ())
