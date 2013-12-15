open Graphics
open Printf

let width_n = 5
let height_n = 5

module Map = Hexgrid.Make(struct include String let create () = "" end)
  (struct
    let pi = 2. *. asin 1.
    let s = 50.
    let h = s *. sin (pi /. 6.)
   end)

let map = Map.create width_n height_n;;

let movetof x y = moveto (int_of_float x) (int_of_float y)
let linetof x y = lineto (int_of_float x) (int_of_float y)

let paint_cell ~left ~top =
  let (x,y) = Map.toScreen ~left ~top in
  (*printf "x = %f, y = %f\n%!" x y;*)
  let open Map.Sizes in
  movetof (x +. a /. 2.) y;
  linetof x (y +. h);
  linetof x (y +. h +. s);
  linetof (x +. a /. 2.) (y +. b);
  linetof (x +. a) (y +. h +. s);
  linetof (x +. a) (y +. h);
  linetof (x +. a/.2.) y;
  ()

let mouse_to_map mousex mousey =
  let (u,v) = Map.of_screen ~mousex ~mousey in
  printf "Selected hex: %d %d\n%!" u v;
  ()

let () =
  open_graph " 800x600";
  for left=0 to width_n do
    for top=0 to height_n do
      paint_cell ~left ~top;
      ()
    done;
  done;
  loop_at_exit [Key_pressed; Button_down]
    (fun event ->
      if event.keypressed then draw_char event.key
      else
        let () = mouse_to_map event.mouse_x event.mouse_y in
        fill_circle event.mouse_x event.mouse_y 3
    )
