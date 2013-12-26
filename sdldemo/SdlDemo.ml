open Tsdl.Sdl
open Printf

let (>>=) a f = match a with `Error -> `Error
                           | `Ok x -> f x
let (>>) a f  = match a with `Error -> `Error
                           | `Ok x -> `Ok (f x)

module List = struct
  include List
  let init f n =
    let ans = ref [] in
    for i=n-1 downto 0 do ans:=(f i):: !ans done;
    !ans
end

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

let width_n = 5
let height_n = 5

module Map = Hexgrid.Make(struct include String let create () = "" end)
  (struct
    let pi = 2. *. asin 1.
    let s = 50.
    let h = s *. sin (pi /. 6.)
   end)

let fold_for2 ~n1 ~n2 ~m1 ~m2 ~f ~init =
  let ans = ref init in
  for i=n1 to n2 do 
    for j=m1 to m2 do
      ans := f !ans i j
    done
  done;
  !ans

let paint_cell renderer ~left ~top : unit result =
  let (x,y) = Map.toScreen ~left ~top in
  (*printf "(x,y) = (%f,%f)\n%!" x y;*)
  let open Map.Sizes in
  set_render_draw_color renderer 255 255 0 255 >>= fun () ->
  let create x y = Point.create (int_of_float x) (int_of_float y) in
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

let main () = 
  create_window "Lesson 4" ~x:100 ~y:100 ~w:screen_width ~h:screen_height Window.opengl >>= fun w ->
  create_renderer ~flags:Renderer.(accelerated + presentvsync) w >>= fun renderer ->
  set_render_draw_color renderer 255 255 255 0 >>= fun () ->
  img_load_texture renderer  "res/image.png" >>= fun image ->  

  main_loop ~poll:(fun e ->
    let x = Event.get e Event.typ in
    if x=Event.quit || x = Event.mouse_button_down || x = Event.key_down then true
    else false
  ) ~draw:(fun () ->
        set_render_draw_color renderer 0 0 0 0 >>= fun () -> 
        render_clear renderer >>= fun _ -> 
        render_texture_in ~texture:image ~renderer ~x:0 ~y:0 >>= fun _ ->  
        fold_for2 ~n1:0 ~m1:0 ~n2:width_n ~m2:height_n ~init:(`Error) ~f:(fun acc left top ->
          paint_cell renderer ~left ~top
        ) >>= fun () ->
        (*
        paint_cell renderer ~top:5 ~left:5 >>= fun () -> *)
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

