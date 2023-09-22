open Brr
open Gg
open Vg

module Utils = struct
  let p2_round d =
    let round = Float.round_dfrac d in
    fun p2 -> P2.v (p2 |> P2.x |> round) (p2 |> P2.y |> round)

  (* Use the [round] param to avoid the float addition weirdness. NOTE: no
     checking of start and stop and such *)
  let float_range_by ~start ~end' ~step ~round =
    let round = Float.round_dfrac round in
    Iter.unfoldr
      (fun x -> if x <= end' then Some (round x, x +. step) else None)
      start
end

module Colors = struct
  let gray = I.const (Color.gray 0.3)
  let light_gray = I.const (Color.gray 0.95)
end

(* We want to track all the dots so user can push and pop them. *)
module Dots = struct
  let dots = ref []

  let pop () = match !dots with [] -> () | _h :: t -> dots := t

  let push dot = dots := dot :: !dots

  (* Blend all the dots. *)
  let img () = List.fold_left I.blend I.void !dots
end

(* These are separated to allow width and height to vary independently. *)
module Conf = struct
  (* pixels per milimeter *)
  let ppmm =
    (* Screen resolution is reported in pixels per meter. *)
    let ppm = Vgr_htmlc.screen_resolution () in
    V2.(ppm / 1000.)

  (* mm = millimeters. Physical sizes in Vg are in mm. *)
  let plot_width_mm = 75.
  let plot_height_mm = 75.
  let plot_size_mm = Size2.v plot_width_mm plot_height_mm

  (* pu = plot units. These represents units in the plot world. *)
  let plot_width_pu = 100.
  let plot_height_pu = 100.
  let plot_size_pu = Size2.v plot_width_pu plot_height_pu

  (* px = pixels. Pixels come from the mouse clicks. *)
  let plot_width_px = plot_width_mm *. P2.x ppmm
  let plot_height_px = plot_height_mm *. P2.y ppmm
  let plot_size_px = Size2.v plot_width_px plot_height_px

  let width_pu_per_mm = plot_width_pu /. plot_width_mm
  let height_pu_per_mm = plot_height_pu /. plot_height_mm
  let pu_per_mm = V2.v width_pu_per_mm height_pu_per_mm

  let min =
    let x = Float.neg (plot_width_pu /. 2.) in
    let y = Float.neg (plot_height_pu /. 2.) in
    P2.v x y

  let max =
    let x = P2.x min +. plot_width_pu in
    let y = P2.y min +. plot_width_pu in
    P2.v x y

  let plot_view =
    (* Origin is bottom left. *)
    let origin = min in
    Box2.v origin (Size2.v plot_width_pu plot_height_pu)

  (* For now, x and y have equal grid steps. *)
  let grid_step = 0.1 *. plot_width_pu

  let major_grid_start = P2.y min +. grid_step
  let major_grid_end = P2.y max -. grid_step

  let minor_grid_start = P2.x min +. (grid_step /. 2.)
  let minor_grid_end = P2.y max -. (grid_step /. 2.)

  let axis_width = 0.5
  let minor_grid_width = 0.05
  let major_grid_width = 0.15

  let dot_radius = 1.5

  let pp fmt () =
    Format.fprintf fmt
      "@[<v 0>== Config ==@;\
       ppmm: %a@;\
       plot_size_mm: %a@;\
       plot_size_pu: %a@;\
       plot_size_px: %a@;\
       pu_per_mm: %a@;\
       min: %a@;\
       max: %a@;\
       plot_view: %a@]" V2.pp ppmm V2.pp plot_size_mm V2.pp plot_size_pu V2.pp
      plot_size_px V2.pp pu_per_mm V2.pp min V2.pp max Box2.pp plot_view

  let to_string () =
    Format.fprintf Format.str_formatter "%a" pp () ;
    Format.flush_str_formatter ()
end

module Bounding_client_rectangle = struct
  (** [pos] (position) is the xy-coords of the top-left. *)
  type t = {size: Size2.t; pos: P2.t}

  (** Like [getBoundingClientRect]. *)
  let create el =
    { size= Size2.v (El.bound_w el) (El.bound_h el)
    ; pos= P2.v (El.bound_x el) (El.bound_y el) }
end

module Mouse_pos = struct
  (** [canvas_rect_size] is the bounding client rectangle size of the canvas. *)
  let canvas_scale ~canvas_size ~canvas_rect_size =
    V2.v
      Size2.(w canvas_size /. w canvas_rect_size)
      Size2.(h canvas_size /. h canvas_rect_size)

  let mouse_client_pos event =
    P2.v (Ev.Mouse.client_x event) (Ev.Mouse.client_y event)

  (* See https://stackoverflow.com/a/17130415 *)
  let mouse_pos' ~canvas_scale ~canvas_rect_pos ~mouse_client_pos =
    let x =
      (P2.x mouse_client_pos -. P2.x canvas_rect_pos) *. V2.x canvas_scale
    in
    let y =
      (P2.y mouse_client_pos -. P2.y canvas_rect_pos) *. V2.y canvas_scale
    in
    P2.v x y

  (** [mouse_pos canvas event] gets the raw position of the mouse click on the
      canvas. *)
  let mouse_pos canvas event =
    let canvas_size =
      let get = Jv.Float.get (El.to_jv canvas) in
      Size2.v (get "width") (get "height")
    in
    let canvas_rect = Bounding_client_rectangle.create canvas in
    let canvas_scale =
      canvas_scale ~canvas_size ~canvas_rect_size:canvas_rect.size
    in
    mouse_pos' ~canvas_scale ~canvas_rect_pos:canvas_rect.pos
      ~mouse_client_pos:(mouse_client_pos event)

  (** [to_plot_space offset] converts the raw mouse click offset pixels into the
      plot coordinate space (plot units). *)
  let to_plot_space offset =
    (* For mouse clickes, top-left is the zero spot, so need to subtract the
       plot height. *)
    let offset_x = V2.x offset in
    let offset_y = V2.y offset in
    let x_px = offset_x in
    let y_px = Conf.plot_height_px -. offset_y in
    (* px / (px / mm) => px * (mm / px) => mm *)
    let x_mm = x_px /. P2.x Conf.ppmm in
    let y_mm = y_px /. P2.y Conf.ppmm in
    (* mm * (pu / mm) => pu *)
    let x_pu = x_mm *. Conf.width_pu_per_mm in
    let y_pu = y_mm *. Conf.height_pu_per_mm in
    (* We use (0, 0) as the middle...so need to subtract half of the height and
       width before returning. *)
    let x =
      let x_adjust = Conf.plot_width_pu /. 2. in
      x_pu -. x_adjust
    in
    let y =
      let y_adjust = Conf.plot_height_pu /. 2. in
      y_pu -. y_adjust
    in
    P2.v x y

  (** [get canvas event] gets the pos of the mouse click in the plot coordinate
      space. *)
  let get canvas event = mouse_pos canvas event |> to_plot_space
end

module Plot_elements = struct
  (* Horizontal line *)
  let hline yintercept ~area =
    let xstart = P2.x Conf.min in
    let xend = P2.x Conf.max in
    let path =
      P.(empty |> sub (P2.v xstart yintercept) |> line (P2.v xend yintercept))
    in
    Colors.gray |> I.cut path ~area

  (* Vertical line *)
  let vline xintercept ~area =
    let ystart = P2.y Conf.min in
    let yend = P2.y Conf.max in
    let path =
      P.(empty |> sub (P2.v xintercept ystart) |> line (P2.v xintercept yend))
    in
    Colors.gray |> I.cut path ~area

  let grid major_minor x_y =
    let width, start, end' =
      match major_minor with
      | `Minor ->
          (Conf.minor_grid_width, Conf.minor_grid_start, Conf.minor_grid_end)
      | `Major ->
          (Conf.major_grid_width, Conf.major_grid_start, Conf.major_grid_end)
    in
    let line = match x_y with `X -> vline | `Y -> hline in
    Utils.float_range_by ~start ~end' ~step:Conf.grid_step ~round:1
    |> Iter.map (line ~area:(`O {P.o with width}))
    |> Iter.fold I.blend I.void

  let dot center r =
    let gray = I.const (Color.gray 0.3 ~a:0.75) in
    let circ = P.empty |> P.circle center r in
    I.cut circ gray

  (* The grid has major and minor grid lines in both x and y axes. *)
  let grid =
    [grid `Minor `X; grid `Minor `Y; grid `Major `X; grid `Major `Y]
    |> List.fold_left I.blend I.void

  let axes =
    let area = `O {P.o with P.width= Conf.axis_width} in
    let x_axis = hline 0. ~area in
    let y_axis = vline 0. ~area in
    x_axis |> I.blend y_axis

  let image = Colors.light_gray |> I.blend grid |> I.blend axes
end

module Dom_elements = struct
  let download = El.p [El.button [El.txt' "Download"]]

  let canvas = El.canvas []

  let last_added_pos = El.p [El.txt (Jstr.v "Last: none")]

  let pop_dot_btn =
    El.p [El.button ~at:[At.id (Jstr.v "pop_dot")] [El.txt' "Remove last dot"]]

  let app =
    El.div [El.div [canvas]; El.div [last_added_pos; pop_dot_btn; download]]
end

module Listeners = struct
  let pop_dot render =
    ignore
      ( Dom_elements.pop_dot_btn |> El.as_target
      |> Ev.listen Ev.click (fun _ev ->
             El.set_children Dom_elements.last_added_pos [El.txt' "Last: pop"] ;
             Dots.pop () ;
             let image = Plot_elements.image |> I.blend (Dots.img ()) in
             render image ) )

  let add_dot canvas render =
    ignore
      ( canvas |> Brr_canvas.Canvas.to_el |> El.as_target
      |> Ev.listen Ev.click (fun ev ->
             let click_pt =
               Mouse_pos.get (Brr_canvas.Canvas.to_el canvas) (Ev.as_type ev)
               |> Utils.p2_round 0
             in
             let click_pt_txt = Format.asprintf "Last: %a" V2.pp click_pt in
             Dots.push (Plot_elements.dot click_pt Conf.dot_radius) ;
             El.set_children Dom_elements.last_added_pos [El.txt' click_pt_txt] ;
             let image = Plot_elements.image |> I.blend (Dots.img ()) in
             render image ) )

  let download canvas =
    ignore
      ( Dom_elements.download |> El.as_target
      |> Ev.listen Ev.click (fun _ ->
             let url =
               Stdlib.Result.get_ok (Brr_canvas.Canvas.to_data_url canvas)
             in
             let a =
               El.a
                 ~at:[At.href url; At.v (Jstr.v "download") (Jstr.v "img.png")]
                 []
             in
             El.append_children (G.document |> Document.body) [a] ;
             El.click a ;
             El.remove a ) )

  let set_up canvas render =
    pop_dot render ; add_dot canvas render ; download canvas
end

let render r img =
  match Vgr.render r (`Image (Conf.plot_size_mm, Conf.plot_view, img)) with
  | `Ok ->
      ()
  | `Partial ->
      Console.(warn ["Partial render"])

let run () =
  Console.(log [Conf.to_string ()]) ;
  let canvas = Brr_canvas.Canvas.of_el Dom_elements.canvas in
  let renderer = Vgr.create (Vgr_htmlc.target canvas) `Other in
  (* Initial render of the plot. *)
  render renderer Plot_elements.image ;
  Listeners.set_up canvas (render renderer)

let main () =
  match Document.find_el_by_id G.document (Jstr.v "app") with
  | None ->
      Console.(warn ["No element with id 'app'"])
  | Some app_container ->
      El.set_children app_container [Dom_elements.app] ;
      run ()

let () = main ()
