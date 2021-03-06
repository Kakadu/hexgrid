open Tsdl.Sdl
open Printf
open Helpers

let (>>=) a f = match a with `Error -> `Error
                           | `Ok x -> f x
let (>>) a f  = match a with `Error -> `Error
                           | `Ok x -> `Ok (f x)

let app_title = "Hexgrid demo1: Scrolling map (buttons: left,right,up,down,ESC)"
let screen_width = 800
let screen_height = 600
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

module Map = Hexgrid.Make(struct include String let create () = "" end)
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
}

let options = {
    view_x = 0;
    view_y = 0;
   width_n = 14;
  height_n = 12;
}

let maingrid = Map.create ~width:options.width_n ~height:options.height_n

(* Will be used to count how many cells we paint to paint screen *)
let paint_counter  = ref 0

let paint_cell ?(xoffset=0) ?(yoffset=0) renderer ~left ~top : unit result =
  let cell = Map.(get ~left ~top maingrid) in
  Option.map cell ~f:(fun cell ->
    incr paint_counter;
    let (x,y) = Map.to_screen ~left ~top in
    let open Map.Sizes in
    set_render_draw_color renderer 255 255 0 255 >>= fun () ->
    let create x y = Point.create (int_of_float x - xoffset) (int_of_float y - yoffset) in
    let points =
      [ create (x +. a /. 2.) y
      ; create x ( y +. h)
      ; create x (y +. h +. s)
      ; create (x +. a /. 2.) (y +. b)
      ; create (x +. a) (y +. h +. s)
      ; create (x +. a) (y +. h)
      ; create (x +. a /. 2.) y
      ] in
    render_draw_lines renderer points
  ) |> Option.get ~default:(`Ok ())

(* adjust coordinates of they are out of range *)
let adjust_viewxy () =
  let max_x = (float_of_int options.width_n  *. 2. +. 1.) *. Map.Sizes.r |> int_of_float in
  let max_y =
    (if options.height_n mod 2 = 0 then Map.Sizes.(float_of_int options.height_n *. (s +. h) +. h)
     else Map.Sizes.(float_of_int options.height_n *. (s +. h) +. h +. s)
    ) |> int_of_float
  in (*
  printf "Map.Sizes.(float_of_int height_n *. (s +. h)) = %f\n%!"
    Map.Sizes.(float_of_int height_n *. (s +. h));
  printf "new max(x,y) = (%d,%d)\n%!" max_x max_y; *)
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
  set_render_draw_color renderer 255 255 255 0 >>= fun () ->
  img_load_texture renderer "res/image.png" >>= fun image ->

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
    render_texture_in ~texture:image ~renderer ~x:0 ~y:0 >>= fun _ ->

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
      paint_counter := 0;
      fold_for2 ~n1 ~m1 ~n2 ~m2 ~init:(`Ok ()) ~f:(fun acc left top -> acc >>= fun _ ->
        paint_cell renderer ~xoffset:options.view_x ~yoffset:options.view_y ~left ~top
      ) >> (fun x -> printf "Paint_counter = %d\n%!" !paint_counter; x)
    end >>= fun () ->
    set_render_draw_color renderer 255 0 0 128 >>= fun () ->
    render_draw_line renderer 50 50 500 250 >>= fun () ->

    set_render_draw_color renderer 0 255 0 128 >>= fun () ->
    render_draw_line renderer 500 250  250 400 >>= fun () ->

    set_render_draw_color renderer 0 0 255 128 >>= fun () ->
    render_draw_line renderer 250 400 50 50 >>= fun () ->

    set_render_draw_color renderer 0 0 0 255 >>= fun () ->
    SDL_gfx.hline_color renderer 30 530 300 (Int32.of_string "0xFF0000FF") |> ignore;
    SDL_gfx.thickLineRGBA renderer 0 400 500 450 4
      (uint8_of_int 0) (uint8_of_int 255) (uint8_of_int 0) (uint8_of_int 128) |> ignore;
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
