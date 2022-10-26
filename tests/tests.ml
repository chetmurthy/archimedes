(*                                                              -*-tuareg-*- *)
open Printf

(* A list of couples (test_name, description, function_to_run). *)
let alltests = [ ("Arrays", Arrays.description, Arrays.draw);
  ("Backend_line", Backend_line.description, Backend_line.draw);
  ("Backend_path", Backend_path.description, Backend_path.draw);
  ("Backend_text", Backend_text.description, Backend_text.draw);
  ("Demo_zoom", Demo_zoom.description, Demo_zoom.draw);
  ("Layout_sync", Layout_sync.description, Layout_sync.draw);
  ("Marks", Marks.description, Marks.draw);
  ("Simple_plot", Simple_plot.description, Simple_plot.draw);
  ("Test_arrows", Test_arrows.description, Test_arrows.draw);
  ("Test_arrows2", Test_arrows2.description, Test_arrows2.draw);
  ("Test_axes", Test_axes.description, Test_axes.draw);
  ("Test_box", Test_box.description, Test_box.draw);
  ("Test_boxes", Test_boxes.description, Test_boxes.draw);
  ("Test_clip", Test_clip.description, Test_clip.draw);
  ("Test_cross", Test_cross.description, Test_cross.draw);
  ("Test_custom_layout", Test_custom_layout.description, Test_custom_layout.draw);
  ("Test_layout", Test_layout.description, Test_layout.draw);
  ("Test_layout2", Test_layout2.description, Test_layout2.draw);
  ("Test_layout_borders", Test_layout_borders.description, Test_layout_borders.draw);
  ("Test_ortho_axes", Test_ortho_axes.description, Test_ortho_axes.draw);
  ("Test_piecharts", Test_piecharts.description, Test_piecharts.draw);
  ("Test_plot", Test_plot.description, Test_plot.draw);
  ("Test_plot_partial", Test_plot_partial.description, Test_plot_partial.draw);
  ("Test_stack", Test_stack.description, Test_stack.draw);
  ("Test_two_viewports", Test_two_viewports.description, Test_two_viewports.draw);
  ("Test_xy_param", Test_xy_param.description, Test_xy_param.draw);
  ("Text", Text.description, Text.draw);
  ("Vector_field", Vector_field.description, Vector_field.draw);
  ("Viewport_autofit", Viewport_autofit.description, Viewport_autofit.draw);
  ("Viewport_path", Viewport_path.description, Viewport_path.draw) ]

let backends = ref []
let cairo out ext () =
  backends := (fun fname -> sprintf "cairo %s %s.%s" out fname ext) :: !backends
let tikz () =
  backends := (fun fname -> sprintf "tikz %s.tex" fname) :: !backends
let graphics () =
  backends := (fun _ -> "graphics hold") :: !backends

let list_tests () =
  List.iter (fun (name,_,_) -> Format.printf "%s@ " name) alltests;
  exit 0

let specs = Arg.align [
  "--ps",  Arg.Unit(cairo "PS" "ps"), " activate ps (cairo) output";
  "--pdf", Arg.Unit(cairo "PDF" "pdf"), " activate pdf (cairo) output";
  "--png", Arg.Unit(cairo "PNG" "png"), " activate png (cairo) output";
  "--tex", Arg.Unit tikz, " activate LaTeX (tikz) output";
  "--graphics", Arg.Unit graphics, " activate graphics output (done if no \
    option is given)";
  "--tests", Arg.Unit list_tests, " list all possible tests and stop";
]

let tests =
  let usage = "tests [option1] [option2] ...\n\
    where an option is a test name or one of the following:" in
  let tests = ref [] in
  Arg.parse specs (fun t -> tests := t :: !tests) usage;
  if !backends = [] then cairo "PNG" "png" ();
  if !tests = [] then alltests
  else (
    let add acc t =
      try List.find (fun (name,_,_) -> name = t) alltests :: acc
      with Not_found -> printf "Test not found: %s\n" t; acc in
    List.fold_left add [] !tests
  )

let () =
  List.iter (fun (name, description, test) ->
    List.iter (fun b ->
      Format.printf "@[<2>%s@;- %s@]@." name description;
      test(b name)
    ) !backends
  ) tests
