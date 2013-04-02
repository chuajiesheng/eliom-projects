{shared{
  open Ocsigen_messages
  open Eliom_lib
  open Eliom_content
  open Eliom_content.Html5.D (* provide functions to create HTML nodes *)
  open Lwt

  let count = ref 0

  let width = 700
  let height = 400

  type messages = (string * int * (int * int) * (int * int))
    deriving (Json)

  type shape = Line | Rectangle | Circle
      deriving (Json)
  type coord = (int * int)
    deriving (Json)
  type drawing =
    Line of (string * int * coord * coord)
  | Rectangle of (string * int * coord * coord)
  | Circle of (string * int * coord * coord)
      deriving(Json)

  let pt = (0, 0)
  let shape = Line ("#000000", 0, pt, pt)
  let drawing_list: drawing list ref = ref []
  let draw_shape = ""

  let rec remove list x =
    match x with
      0 ->
        (match list with
          [] -> []
        | hd::tl -> tl)
    | _ ->
      (match list with
        [] -> []
      | hd::tl -> hd :: (remove tl (x-1)))

  let keyCode = ref 76;
(* 99:c, 108:l, 114:r *)

}}

{client{
  let _ = Eliom_lib.alert "Hello!"

       let draw_line ctx (color, size, (x1, y1), (x2, y2)) =
         ctx##strokeStyle <- (Js.string color);
         ctx##lineWidth <- float size;
         ctx##beginPath();
         ctx##moveTo(float x1, float y1);
         ctx##lineTo(float x2, float y2);
         ctx##stroke()

       let draw_rect ctx (color, size, (x1, y1), (x2, y2)) =
         ctx##fillStyle <- (Js.string color);
         ctx##fillRect(float x1, float y1, (float (x2 - x1)), (float (y2 - y1)))

       let dist x y = sqrt(x *. x +. y *. y)
       let draw_circle ctx (color, size, (x1, y1), (x2, y2)) =
         ctx##fillStyle <- (Js.string color);
         ctx##lineWidth <- float size;
         ctx##moveTo(float x2, float y2);
         ctx##arc(float ((x1+x2)/2), float ((y1+y2)/2), (dist (float (x2-x1)) (float (y2-y1)))/.2., float 0, 3.142 *. 2., Js.bool true);
         ctx##fill()

       let draw_drawing ctx drawing =
         match drawing with
           Line pts -> draw_line ctx pts
         | Rectangle pts -> draw_rect ctx pts
         | Circle pts -> draw_circle ctx pts
}}

(* app registration *)
module Graffiti_app =
  Eliom_registration.App (
    struct
      let application_name = "graffiti"
    end)

let rpc_key = server_function Json.t<int>
  (fun key -> keyCode := key; console (fun() -> "[rpc_key] keyCode: " ^ (string_of_int !keyCode)); Lwt.return())

let rpc_get_key = server_function Json.t<unit>
  (fun () -> console(fun() -> "[rpc_get_key] get keyCode: " ^ (string_of_int !keyCode)); Lwt.return keyCode)

let rpc_log = server_function Json.t<string>
  (fun str -> console (fun () -> "[client] " ^ str); Lwt.return ())

let bus = Eliom_bus.create Json.t<drawing>

  let canvas_elt =
    canvas ~a:[a_width width; a_height height]
      [pcdata "your browser doesn't support canvas"]

(* image services *)

let rgb_from_string color = (* color is in format "#rrggbb" *)
  let get_color i =
    (float_of_string ("0x"^(String.sub color (1+2*i) 2))) /. 255.
  in
  try get_color 0, get_color 1, get_color 2 with | _ -> 0.,0.,0.

let draw_line ctx pts = (
  let ((color:string), size, pt1, pt2) = pts in
  let x1, y1 = pt1 in
  let x2, y2 = pt2 in
  (* set thickness of brush *)
  Cairo.set_line_width !ctx (float size);
  Cairo.set_line_join !ctx Cairo.JOIN_ROUND;
  Cairo.set_line_cap !ctx Cairo.ROUND;
  let red, green, blue = rgb_from_string color in
  Cairo.set_source_rgb !ctx red green blue;
  Cairo.move_to !ctx (float x1) (float y1);
  Cairo.line_to !ctx (float x2) (float y2);
  Cairo.Path.close !ctx;
  (* apply the ink *)
  Cairo.stroke !ctx;
)

let draw_rect ctx pts = (
  let ((color:string), size, pt1, pt2) = pts in
  let x1, y1 = pt1 in
  let x2, y2 = pt2 in
  let red, green, blue = rgb_from_string color in
  Cairo.set_source_rgb !ctx red green blue;
  Cairo.rectangle !ctx (float x1) (float y1) (float (x2 - x1)) (float (y2 - y1));
  Cairo.fill !ctx
)

let draw_circle ctx pts = (
  let ((color:string), size, pt1, pt2) = pts in
  let x1, y1 = pt1 in
  let x2, y2 = pt2 in
  Cairo.set_line_width !ctx (float size);
  Cairo.set_line_join !ctx Cairo.JOIN_ROUND;
  Cairo.set_line_cap !ctx Cairo.ROUND;
  let red, green, blue = rgb_from_string color in
  Cairo.set_source_rgb !ctx red green blue;
  Cairo.arc !ctx (float ((x1+x2)/2)) (float ((y1+y2)/2)) ((hypot (float (x2-x1)) (float (y2-y1)))/.2.) 0. 6.284;
  Cairo.stroke_preserve !ctx;
    (* apply the ink *)
  Cairo.fill !ctx;
)

let rec draw_drawing_list ctx list =
  match list with
    [] -> ()
  | hd::tl -> (
    draw_drawing_list ctx tl;
    (match hd with
      Line pts -> draw_line ctx pts
    | Rectangle pts -> draw_rect ctx pts
    | Circle pts -> draw_circle ctx pts
    | _ -> failwith  "unknown shape"))

let create_server, draw_server, image_string =
  let surface =
    Cairo.Image.create Cairo.Image.ARGB32 ~width ~height
  in
  let ctx = ref (Cairo.create surface)
  in
  ((fun() ->
    console (fun() -> "[create_server] recreate surface");
    let surface = Cairo.Image.create Cairo.Image.ARGB32 ~width ~height in
    ctx := Cairo.create surface
   ),
   (fun drawing ->
     match drawing with
       Line pts ->
         draw_line ctx pts;
         drawing_list := (drawing :: !drawing_list);
         console (fun () -> "[draw_server] appending line to drawing_list (length of " ^ (string_of_int (List.length !drawing_list)) ^ ")");
     | Rectangle pts ->
         draw_rect ctx pts;
         drawing_list := (drawing :: !drawing_list);
         console (fun() -> "[draw_server] appending rectangle to drawing_list (length of " ^ (string_of_int (List.length !drawing_list)) ^ ")");
     | Circle pts ->
         draw_circle ctx pts;
         drawing_list := (drawing :: !drawing_list);
         console (fun () -> "[draw_sever] appending circle to drawing_list (length of " ^ (string_of_int (List.length !drawing_list)) ^ ")");
     | _ -> failwith "unknown shapes"
   ),
   (fun() ->
     let surface = Cairo.Image.create Cairo.Image.ARGB32 ~width  ~height in
     ctx := Cairo.create surface;
     draw_drawing_list ctx !drawing_list;
     let b = Buffer.create 10000 in
     (* output a png in a string *)
     Cairo.PNG.write_to_stream surface (Buffer.add_string b);
     Buffer.contents b
   ))

let _ = Lwt_stream.iter draw_server (Eliom_bus.stream bus)

let imageservice =
  Eliom_registration.String.register_service
    ~path:["image"]
    ~get_params:Eliom_parameter.unit
    (fun () () -> Lwt.return (image_string(), "image/png"))

    {client{
      let init_client () =
        Eliom_lib.alert "[init_client] drawing_list of length %s" (string_of_int(List.length !(%drawing_list)));

        let canvas = Eliom_content.Html5.To_dom.of_canvas %canvas_elt in

        let ctx = canvas##getContext (Dom_html._2d_) in
        ctx##lineCap <- Js.string "round";

        (* the initial image *)
        let img =
          Eliom_content.Html5.To_dom.of_img
            (img ~alt:"canvas"
               ~src:(make_uri ~service:%imageservice ())
               ())
        in
        img##onload <- Dom_html.handler
          (fun ev -> ctx##drawImage(img, 0., 0.); Js._false);

        (* size of the brush *)
        let slider = jsnew Goog.Ui.slider(Js.null) in
        slider##setMinimum(1.);
        slider##setMaximum(80.);
        slider##setValue(10.);
        slider##setMoveToPointEnabled(Js._true);
        slider##render(Js.some Dom_html.document##body);

        (* color palette *)
        let pSmall =
          jsnew Goog.Ui.hsvPalette(Js.null, Js.null, Js.some (Js.string "goog-hsv-palette-sm")) in
        pSmall##render(Js.some Dom_html.document##body);

        let x = ref 0 and y = ref 0 and key = ref 0 in

        let set_coord ev =
          let x0, y0 = Dom_html.elementClientPosition canvas in
          x := ev##clientX - x0; y := ev##clientY - y0
        in

        let compute_line ev =
          let () = Eliom_lib.debug "[compute_line]" in
          let oldx = !x and oldy = !y in
          set_coord ev;
          let color = Js.to_string (pSmall##getColor()) in
          let size = int_of_float (Js.to_float (slider##getValue())) in
          let pt1:coord = (oldx, oldy) in
          let pt2:coord = (!x, !y) in
          let print k = Eliom_lib.debug "[compute_line] charCode: %i" k in
          let _ = Lwt.bind
            (%rpc_get_key ()) (fun k ->
              print !k; key := !k; Lwt.return ())  in
          match !key with
            99 -> Circle (color, size, pt1, pt2)
          | 114 -> Rectangle (color, size, pt1, pt2)
          | _ -> Line (color, size, pt1, pt2)
        in

        let line ev =
          let v = compute_line ev in
          let _ = Eliom_bus.write %bus v in
          draw_drawing ctx v;
          Lwt.return () in

        let press key =
          let () = Eliom_lib.debug "press ev" in
          Dom_html.window##alert(Js.string("key pressed!"));
          Eliom_lib.alert "[press] keycode %s" (string_of_int (key));
          Lwt.async
            (fun () ->
              %rpc_log (string_of_int key));
          Lwt.async
            (fun () ->
              %rpc_key key);
          Lwt.return () in

        Lwt.async
          (fun () ->
            let open Lwt_js_events in
            mousedowns canvas
              (fun ev _ ->
                Eliom_lib.debug "mouse @ %i,%i" ev##clientX ev##clientY;
                set_coord ev; line ev >>= fun () ->
                Lwt.pick [mousemoves Dom_html.document (fun x _ -> line x);
                          mouseup Dom_html.document >>= line]));

        Lwt_js_events.(
          async
            (fun () ->
              keypresses Dom_html.document
                (fun ev _ ->
                  let key = (Js.Optdef.get (ev##charCode) (fun() -> 0)) in
                  press key;
                  Eliom_lib.debug "key %i" key;
                  Lwt.return()
                )));


        (* Lwt.async *)
        (*   (fun() -> *)
        (*     Eliom_lib.debug "open keypresses canvas"; *)
        (*     let open Lwt_js_events in *)
        (*     keypresses Dom_html.document *)
        (*       (fun ev _ -> *)
        (*         Eliom_lib.debug "key %i" ev##keyCode; *)
        (*         press ev >>= fun() -> *)
        (*         Lwt.return() *)
        (*       ) *)
        (*   ); *)

        Lwt.async
          (fun () ->
            Lwt_stream.iter (draw_drawing ctx) (Eliom_bus.stream %bus))
    }}

let page =
  (html
     (Eliom_tools.F.head ~title:"Graffiti"
        ~css:[
          ["css";"common.css"];
          ["css";"hsvpalette.css"];
          ["css";"slider.css"];
          ["css";"graffiti.css"];
        ]
        ~js:[["graffiti_oclosure.js"]]())
     (body [h1 [pcdata "Graffiti"];
            div [canvas_elt]]))

let main_service =
  Graffiti_app.register_service
    ~path:["graff"]
    ~get_params:Eliom_parameter.unit
    (fun () () ->
      let count = incr count; !count in
      ignore { unit {
        init_client()
      }};
      (* ignore { unit { *)
      (*   Eliom_lib.alert "[main_service] hello! :D"; *)
      (*   Lwt_js_events.( *)
      (*     async *)
      (*       (fun() -> *)
      (*         keypresses canvas *)
      (*         (fun ev _ -> *)
      (*           Eliom_lib.alert ("[main_service] " ^(String_of_int (ev##keyCode))); *)
      (*           Lwt.return () *)
      (*         ))) *)
      (* }}; *)
      Lwt.return page)

let pop_service =
  Graffiti_app.register_service
    ~path:["pop"]
    ~get_params:Eliom_parameter.(int "i")
    (fun i () ->
      ignore { unit {
        Eliom_lib.alert "[pop_service] hello! ;]";
        (* doing thing here cant access server i think *)
        %drawing_list := remove !drawing_list 10;
        (* Stack.clear %drawing_list; *)
        Eliom_lib.alert "[pop_service] drawing_list of length %s"
          (string_of_int(List.length !(%drawing_list)));
      }};
      console (fun() -> "[pop_service] drawing_list of length " ^
        (string_of_int(List.length !drawing_list)));
      create_server();
      drawing_list := (remove !drawing_list i);
      console (fun() -> "[pop_service] drawing_list of length " ^
        (string_of_int(List.length !drawing_list)));
      Lwt.return page)
