exception Malformed_data of string

let get_uint16_le b pos =
  (Bytes.get_uint8 b (pos + 1) lsl 8) lxor Bytes.get_uint8 b pos

let get_uint32_le b pos =
  (Bytes.get_uint8 b (pos + 3) lsl 24)
  lxor (Bytes.get_uint8 b (pos + 2) lsl 16)
  lxor (Bytes.get_uint8 b (pos + 1) lsl 8)
  lxor Bytes.get_uint8 b pos

(* Read [n] bytes from [ch]. raises Malformed_data if there is not enough data on the channel to read.  *)
let read_bytes ch n =
  assert (n > 0);
  let b = Bytes.make n '0' in
  match In_channel.really_input ch b 0 n with
  | None -> raise (Malformed_data "Not enough data.")
  | Some _ -> b

type bitmap_file_header = { bmp_type: int; size: int; offset: int }
[@@deriving show]

let parse_bitmap_file_header ch =
  let b = read_bytes ch 14 in
  {
    bmp_type= get_uint16_le b 0
  ; size= get_uint32_le b 2
  ; offset= get_uint32_le b 10
  }

type bitmap_info_header = {
    bitmap_info_header_size: int
  ; width: int
  ; height: int
  ; pixel_bits: int
}
[@@deriving show]

let parse_bitmap_info ch =
  let b = read_bytes ch 4 in
  let bitmap_info_header_size = get_uint32_le b 0 in
  let b = read_bytes ch (bitmap_info_header_size - 4) in
  {
    bitmap_info_header_size
  ; width= get_uint32_le b 0
  ; height= get_uint32_le b 4
  ; pixel_bits= get_uint16_le b 10
  }

let read_header ch =
  let bitmap_file_data = parse_bitmap_file_header ch in
  let bitmap_info_data = parse_bitmap_info ch in
  assert (bitmap_file_data.bmp_type = 0x4d42);
  assert (bitmap_file_data.size = 529338);
  assert (bitmap_info_data.width = 420);
  assert (bitmap_info_data.height = 420);
  assert (bitmap_info_data.pixel_bits = 24);
  (bitmap_file_data, bitmap_info_data)

let get_bgr o b =
  (Bytes.get_uint8 b o, Bytes.get_uint8 b (o + 1), Bytes.get_uint8 b (o + 2))

let set_bgr o b blue green red =
  Bytes.set_uint8 b o blue;
  Bytes.set_uint8 b (o + 1) green;
  Bytes.set_uint8 b (o + 2) red

let rotate b max_x max_y pixel_s =
  let max_y = if max_y > 0 then max_y - 1 else max_y in
  let blen = Bytes.length b in
  let b' = Bytes.create blen in
  let i = ref 0 in
  let y = ref max_y in
  let x = ref 0 in
  while !i <= blen - 1 do
    let new_offset = (max_x * pixel_s * !y) + !x in
    let b, g, r = get_bgr !i b in
    set_bgr new_offset b' b g r;
    y := if !y = 0 then max_y else !y - 1;
    x := if !y = max_y then !x + pixel_s else !x;
    i := !i + pixel_s
  done;
  b'

let rotate_bmp_file input_file ch : unit =
  let pa_offset = 138 in
  let pa_length = 529200 in
  let header, pixel_array =
    In_channel.with_open_bin input_file (fun ch ->
        let header = read_bytes ch pa_offset in
        let pixel_array = read_bytes ch pa_length in
        (header, pixel_array))
  in
  let pixel_array = rotate pixel_array 420 420 3 in
  Out_channel.output ch header 0 pa_offset;
  Out_channel.output ch pixel_array 0 pa_length;
  Out_channel.flush ch

let () =
  let filename = Sys.argv.(1) in
  Out_channel.with_open_bin "teapot-rotated.bmp" (fun ch ->
      rotate_bmp_file filename ch)
