open Tsdl.Sdl
open Printf
open Helpers

let (>>=) a f = match a with `Error -> `Error
                           | `Ok x -> f x
let (>>) a f  = match a with `Error -> `Error
                           | `Ok x -> `Ok (f x)

let app_title = "SdlTTF text demo"
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
}

let options = {
    view_x = 0;
    view_y = 0;
}

let main () =
  create_window app_title ~x:300 ~y:50 ~w:screen_width ~h:screen_height Window.opengl >>= fun w ->
  create_renderer ~flags:Renderer.(accelerated + presentvsync) w >>= fun renderer ->
  set_render_draw_color renderer 255 255 255 0 >>= fun () ->
  img_load_texture renderer "res/image.png" >>= fun image ->

  TTF.init () >>= fun () ->
  TTF.open_font "res/UbuntuMono-R.ttf" 12 >>= fun ubuntu12_font ->
  TTF.open_font "res/UbuntuMono-R.ttf" 16 >>= fun ubuntu16_font ->
  TTF.open_font "res/UbuntuMono-R.ttf" 20 >>= fun ubuntu20_font ->

  main_loop ~poll:(fun e ->
    let x = Event.get e Event.typ in
    if (x = Event.quit) ||
       (x=Event.key_down && Event.get e Event.keyboard_keycode = K.escape) then true
    else false
  ) ~draw:(fun () ->
    set_render_draw_color renderer 0 0 0 0 >>= fun () ->
    render_clear renderer >>= fun _ ->
    render_texture_in ~texture:image ~renderer ~x:0 ~y:0 >>= fun _ ->

    TTF.render_text_blended ubuntu12_font "Size 12" MyColors.red >>= fun fontSurface ->
    create_texture_from_surface renderer fontSurface >>= fun fontTexture ->
    render_texture_in ~texture:fontTexture ~renderer ~x:100 ~y:100 >>= fun _ ->

    TTF.render_text_blended ubuntu16_font "Size 16" MyColors.red >>= fun fontSurface ->
    create_texture_from_surface renderer fontSurface >>= fun fontTexture ->
    render_texture_in ~texture:fontTexture ~renderer ~x:100 ~y:120 >>= fun _ ->

    TTF.render_text_blended ubuntu20_font "Size 20" MyColors.red >>= fun fontSurface ->
    create_texture_from_surface renderer fontSurface >>= fun fontTexture ->
    render_texture_in ~texture:fontTexture ~renderer ~x:100 ~y:150 >>= fun _ ->

    set_render_draw_color renderer 0 0 0 255 >>= fun () ->
    SDL_gfx.hline_color renderer 30 530 300 (Int32.of_string "0xFF0000FF") |> ignore;
    SDL_gfx.thickLineRGBA renderer 0 400 500 450 4
      (uint8_of_int 0) (uint8_of_int 255) (uint8_of_int 0) (uint8_of_int 128) |> ignore;

    render_present renderer;
    `Ok ()
  ) >>= fun _ ->
  TTF.close_font ubuntu12_font;
  TTF.close_font ubuntu16_font;
  TTF.close_font ubuntu20_font;
  destroy_renderer renderer;
  destroy_window w;
  `Ok ()


let (_ : _ result) =
  match init Init.video with
  | `Error -> exit 1
  | `Ok () -> `Ok (main ())
