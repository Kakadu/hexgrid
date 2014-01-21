open Tsdl.Sdl
open Printf
open Helpers

let (>>=) a f = match a with `Error -> `Error
                           | `Ok x -> f x
let (>>) a f  = match a with `Error -> `Error
                           | `Ok x -> `Ok (f x)

let app_title = "Hexgrid demo: FPS"
let screen_width = 800
let screen_height = 600


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
module SDL_timer = struct
  type t = { mutable ticks: uint32; mutable frames: int; mutable fps:int }
  let create () = { ticks = get_ticks (); frames = 0; fps = 0 }
  let check timer =
    let ticks = get_ticks () in
    let d = Int32.((sub ticks timer.ticks) |> to_int) in
    if d > 1000 then begin
      let fps = timer.frames * 1000 / d in
      timer.frames <- 0;
      timer.ticks <- ticks;
      timer.fps <- fps;
      true
    end else false
  let fps timer = timer.fps
  let incr_frame timer = timer.frames <- 1 + timer.frames
end

let main () =
  create_window app_title ~x:300 ~y:50 ~w:screen_width ~h:screen_height Window.opengl >>= fun w ->
  create_renderer ~flags:Renderer.(accelerated + presentvsync) w >>= fun renderer ->
  set_render_draw_color renderer 255 255 255 0 >>= fun () ->
  img_load_texture renderer "res/image.png" >>= fun image ->

  let timer = SDL_timer.create () in
  main_loop ~poll:(fun e ->
    let x = Event.get e Event.typ in
    if (x = Event.quit) ||
       (x=Event.key_down && Event.get e Event.keyboard_keycode = K.escape) then true
    else false
  ) ~draw:(fun () ->
    set_render_draw_color renderer 0 0 0 0 >>= fun () ->
    render_clear renderer >>= fun _ ->
    render_texture_in ~texture:image ~renderer ~x:0 ~y:0 >>= fun _ ->

(*    SDL_timer.check timer; *)
    if SDL_timer.check timer then begin
      Printf.printf "FPS = %d\n%!" (SDL_timer.fps timer)
    end;
    SDL_timer.incr_frame timer;

(*
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
    *)
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
