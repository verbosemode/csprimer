exception Malformed_data of string

(*
* Use Unsigned module again
* Read Byte-wise
* Implement get_le_u16, get_le_u32, get_le_u64 based on bitshifting and byte-wise reads
*)

(* Re-inventing the wheel. I know, there is Bytes.get_uint16_le *)
let get_uint16_le b pos =
  (Bytes.get_uint8 b (pos + 1) lsl 8) lxor Bytes.get_uint8 b pos

let get_uint32_le b pos =
  (Bytes.get_uint8 b (pos + 3) lsl 24)
  lxor (Bytes.get_uint8 b (pos + 2) lsl 16)
  lxor (Bytes.get_uint8 b (pos + 1) lsl 8)
  lxor Bytes.get_uint8 b pos

type bitmap_file = {
  bmp_type: int;
  size: int;
  offset: int;
}
[@@deriving show]

type bitmap_info = {
  bitmap_info_header_size: int;
  width: int;
  height: int;
  color_planes: int;
  pixel_bits: int;
}
[@@deriving show]

(* Read [n] bytes from [ch]. raises Malformed_data if there is not enough data on the channel to read.  *)
let read_bytes ch n =
  assert (n > 0);
  let b = Bytes.make n '0' in
  match In_channel.really_input ch b 0 n with
  | None -> raise (Malformed_data "Not enough data.")
  | Some _ -> b

let parse_bitmap_file ch =
  let b = read_bytes ch 14 in
  {
    bmp_type = get_uint16_le b 0;
    size = get_uint32_le b 2;
    offset = get_uint32_le b 10;
  }

let parse_bitmap_info ch =
  let b = read_bytes ch 4 in
  let bitmap_info_header_size = get_uint32_le b 0 in
  let b = read_bytes ch (bitmap_info_header_size - 4) in
  {
    bitmap_info_header_size;
    width = get_uint32_le b 0;
    height = get_uint32_le b 4;
    color_planes = get_uint16_le b 26;
    pixel_bits = get_uint16_le b 28;
  }

let read_header ch =
  let bitmap_file_data = parse_bitmap_file ch in
  let bitmap_info_data = parse_bitmap_info ch in
  assert (bitmap_file_data.bmp_type = 0x4d42);
  assert (bitmap_file_data.size = 529338);
  assert (bitmap_info_data.width = 420);
  assert (bitmap_info_data.height = 420);
  (* let pixel_array_size = bitmap_data.size - bitmap_data.offset in *)
  (* let pixel_array_data = read_bytes ch pixel_array_size in *)
  (* Printf.sprintf "\ntype: %i size: %i offset: %i width: %i height: %i\n%!" *)
  (*   bitmap_file_data.bmp_type bitmap_file_data.size bitmap_file_data.offset *)
  (*   bitmap_info_data.width bitmap_info_data.height *)
  bitmap_file_data, bitmap_info_data

(* let write_bytes ch n = *)
(*   assert (n > 0); *)
(*   let b = Bytes.make n '0' in *)
(*   match In_channel.really_input ch b 0 n with *)
(*   | None -> raise (Malformed_data "Not enough data.") *)
(*   | Some _ -> b *)

(* let write_bmp_file input_file ch (bmpf: bitmap_file) (bmpi : bitmap_info) : unit = *)
let write_bmp_file input_file ch : unit =
  (* let header, pixel_array = In_channel.with_open_bin input_file (fun ch2 -> read_bytes ch2 138, read_bytes ch2 529200) in *)
  let pa_offset = 138 in
  let pa_length = 529200 in
  let header, pixel_array =
    In_channel.with_open_bin input_file (fun ch ->
        let header = read_bytes ch pa_offset in
        let pixel_array = read_bytes ch pa_length in
        header, pixel_array)
  in
  Out_channel.output ch header 0 pa_offset;

  (* Out_channel.output ch pixel_array 0 529200; *)
  let b = Bytes.make pa_length '0' in
  (* let x, y = 420 - 1, 420 * 3 in *)
  let x, y = 420, 420 in
  let row = ref 0 in
  let column = ref 1 in
  let column_max = 420 * 3 in

  for i = 0 to pa_length - 1 do
    if !column = column_max then (
      column := 1;
      row := !row + 1
    ) else
      column := !column + 1;
    let bi = (x * (y - !column)) + !row * 3 in
    let bc = Bytes.get pixel_array i in
    (* Printf.printf "i: %i bi: %i column: %i row: %i\n%!" i bi !column !row; *)
    Bytes.set b bi bc
  done;

  Out_channel.output ch b 0 pa_length;

  (* for i = 0 to (3 * 420); do *)
  (*   let b  = Bytes.of_string "\x00\x00\xff" in *)
  (*   Out_channel.output_bytes ch b; *)
  (* done; *)
  (* for i = 0 to (3 * 419); do *)
  (*   let b  = Bytes.of_string "\x33\xff\x33" in *)
  (*   Out_channel.output_bytes ch b; *)
  (* done; *)
  (* for i = 0 to 529220 - ((3 * 419) * 2); do *)
  (*   (* let b  = Bytes.of_string "\x00\x00\x00" in *) *)
  (*   Out_channel.output_byte ch 0; *)
  (* done; *)
  (* let last_byte = Bytes.get pixel_array (pa_length - 1) in *)
  (* Printf.printf "Pixel array bytes length: %i\n%!" (Bytes.length pixel_array); *)
  Out_channel.flush ch

let () =
  let filename = Sys.argv.(1) in
  let bmpf, bmpi =
    In_channel.with_open_bin filename (fun ch -> read_header ch)
  in
  show_bitmap_file bmpf |> print_endline;
  show_bitmap_info bmpi |> print_endline;
  Out_channel.with_open_bin filename (fun ch ->
      write_bmp_file "test/teapot.bmp" ch)
