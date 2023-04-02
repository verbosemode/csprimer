exception Mailformed_data of string

type rgb_type =
  | Rgb of {
      r: int;
      g: int;
      b: int;
    }
  | Rgb_alpha of {
      r: int;
      g: int;
      b: int;
      a: int;
    }

let int_of_hex_char c =
  match c with
  | '0' .. '9' -> Char.code c - 48
  | 'a' .. 'f' -> Char.code c - 87
  | 'A' .. 'F' -> Char.code c - 55
  | _ -> invalid_arg (Printf.sprintf "char %c is not a valid hex digit" c)

let make_int s i j =
  let x = int_of_hex_char (String.get s i) in
  let y = int_of_hex_char (String.get s j) in
  (x lsl 4) lxor y

let hexstr_to_rgb s =
  match String.length s with
  | 3 -> Rgb { r = make_int s 0 0; g = make_int s 1 1; b = make_int s 2 2 }
  | 6 -> Rgb { r = make_int s 0 1; g = make_int s 2 3; b = make_int s 4 5 }
  | 4 ->
    Rgb_alpha
      {
        r = make_int s 0 0;
        g = make_int s 1 1;
        b = make_int s 2 2;
        a = make_int s 3 3;
      }
  | 8 ->
    Rgb_alpha
      {
        r = make_int s 0 1;
        g = make_int s 2 3;
        b = make_int s 4 5;
        a = make_int s 6 7;
      }
  | _ -> raise (Mailformed_data s)

let alpha_float_of_int i = Printf.sprintf "%.5f" (float_of_int i /. 255.)

let rgb_to_string = function
  | Rgb rgb -> Printf.sprintf "color: rgb(%i %i %i)" rgb.r rgb.g rgb.b
  | Rgb_alpha rgba ->
    Printf.sprintf "color: rgba(%i %i %i / %s)" rgba.r rgba.g rgba.b
      (alpha_float_of_int rgba.a)

let css_color_regex = Re.Pcre.regexp {|color: #([a-zA-Z0-9]{3,8})|}

let parse_rgb line =
  Re.replace css_color_regex
    ~f:(fun groups -> Re.Group.get groups 1 |> hexstr_to_rgb |> rgb_to_string)
    line

let read_from_stdin () =
  let rec aux = function
    | None -> ()
    | Some line ->
      (* Using stdout explicitly instead of print_endline / printf *)
      let line = String.concat "" [ parse_rgb line; "\n" ] in
      Out_channel.(output_string stdout line);
      aux In_channel.(input_line stdin)
  in
  aux In_channel.(input_line stdin);
  Out_channel.(flush stdout)

let () =
  (* assert (hexstr_to_rgb "123" = Rgb { r = 17; g = 34; b = 51 }); *)
  (* assert (hexstr_to_rgb "00ff00" = Rgb { r = 0; g = 255; b = 0 }); *)
  (* assert (hexstr_to_rgb "00f8" = Rgb_alpha { r = 0; g = 0; b = 255; a = 136 }); *)
  (* assert (hexstr_to_rgb "0000FFC0" = Rgb_alpha { r = 0; g = 0; b = 255; a = 192 }); *)
  read_from_stdin ()
