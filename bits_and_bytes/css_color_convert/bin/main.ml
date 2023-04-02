open Unsigned.UInt32
open Unsigned.UInt32.Infix

exception Mailformed_data of string

let int_of_hex_char c =
  match c with
  | '0' .. '9' -> Int.(sub (Char.code c) 48)
  | 'a' .. 'f' -> Int.(sub (Char.code c) 87)
  | 'A' .. 'F' -> Int.(sub (Char.code c) 55)
  | _ -> invalid_arg (Printf.sprintf "char %c is not a valid hex digit" c)

let uint32_of_hex_string s si sj =
  let x = int_of_hex_char (String.get s si) in
  let y = int_of_hex_char (String.get s sj) in
  of_int (Int.logxor (Int.shift_left x 4) y)

(* encode hexadecimal string into UInt32 of rrggbbaa
 * - aa of 00 is a regular RGB value
 *)
let hexstr_to_rgb s =
  let i = Unsigned.UInt32.zero in
  match String.length s with
  | 3 ->
    let i = i lxor (uint32_of_hex_string s 0 0 lsl 24) in
    let i = i lxor (uint32_of_hex_string s 1 1 lsl 16) in
    let i = i lxor (uint32_of_hex_string s 2 2 lsl 8) in
    i
  | 6 ->
    let i = i lxor (uint32_of_hex_string s 0 1 lsl 24) in
    let i = i lxor (uint32_of_hex_string s 2 3 lsl 16) in
    let i = i lxor (uint32_of_hex_string s 4 5 lsl 8) in
    i
  | 4 ->
    let i = i lxor (uint32_of_hex_string s 0 0 lsl 24) in
    let i = i lxor (uint32_of_hex_string s 1 1 lsl 16) in
    let i = i lxor (uint32_of_hex_string s 2 2 lsl 8) in
    let i = i lxor uint32_of_hex_string s 3 3 in
    i
  | 8 ->
    let i = i lxor (uint32_of_hex_string s 0 1 lsl 24) in
    let i = i lxor (uint32_of_hex_string s 2 3 lsl 16) in
    let i = i lxor (uint32_of_hex_string s 4 5 lsl 8) in
    let i = i lxor uint32_of_hex_string s 6 7 in
    i
  | _ -> raise (Mailformed_data s)

let alpha_float_of_int i = Printf.sprintf "%.5f" (float_of_int i /. 255.)

let has_alpha i = not (equal (i land of_int 0b1111_1111) zero)

let get_color_value i n = (i land (of_int 0b1111_1111 lsl n)) lsr n |> to_int

let rgb_to_string i =
  if has_alpha i then
    Printf.sprintf "color: rgba(%i %i %i / %s)" (get_color_value i 24)
      (get_color_value i 16) (get_color_value i 8)
      (alpha_float_of_int (get_color_value i 0))
  else
    Printf.sprintf "color: rgb(%i %i %i)" (get_color_value i 24)
      (get_color_value i 16) (get_color_value i 8)

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
  (* assert (hexstr_to_rgb "123" = of_string "287453952"); *)
  (* assert (hexstr_to_rgb "00ff00" = of_string "16711680"); *)
  (* assert (hexstr_to_rgb "00f8" = of_string "65416"); *)
  (* assert (hexstr_to_rgb "0000FFC0" = of_string "65472") *)
  read_from_stdin ()
