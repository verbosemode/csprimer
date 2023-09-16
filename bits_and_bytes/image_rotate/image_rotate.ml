let bmp_file_header_size = 14

let get_uint16_le s pos =
  (String.get_uint8 s (pos + 1) lsl 8) lxor String.get_uint8 s pos

let get_uint32_le s pos =
  (String.get_uint8 s (pos + 3) lsl 24)
  lxor (String.get_uint8 s (pos + 2) lsl 16)
  lxor (String.get_uint8 s (pos + 1) lsl 8)
  lxor String.get_uint8 s pos

type bitmap_file_header = {
    bmp_type: int
  ; file_size: int
  ; pixel_array_offset: int
}
[@@deriving show]

let parse_bitmap_file_header s =
  {
    bmp_type= get_uint16_le s 0
  ; file_size= get_uint32_le s 2
  ; pixel_array_offset= get_uint32_le s 10
  }

type bitmap_info_header = {
    bitmap_info_header_size: int
  ; width: int
  ; height: int
  ; pixel_bits: int
}
[@@deriving show]

let parse_bitmap_info_header s =
  {
    bitmap_info_header_size= get_uint32_le s 14
  ; width= get_uint32_le s 18
  ; height= get_uint32_le s 22
  ; pixel_bits= get_uint16_le s 28
  }

let get_bgr offset s =
  ( String.get_uint8 s offset
  , String.get_uint8 s (offset + 1)
  , String.get_uint8 s (offset + 2) )

let set_bgr offset b (blue, green, red) =
  Bytes.set_uint8 b offset blue;
  Bytes.set_uint8 b (offset + 1) green;
  Bytes.set_uint8 b (offset + 2) red

let rotate pixel_array max_column max_row pixel_size =
  let pixel_array_len = String.length pixel_array in
  let b' = Bytes.create pixel_array_len in
  let pixel_offset = ref 0 in
  let max_row = if max_row > 0 then max_row - 1 else max_row in
  let row = ref max_row in
  let column = ref 0 in
  while !pixel_offset <= pixel_array_len - 1 do
    let target_offset = (max_column * pixel_size * !row) + !column in
    set_bgr target_offset b' (get_bgr !pixel_offset pixel_array);
    row := if !row = 0 then max_row else !row - 1;
    column := if !row = max_row then !column + pixel_size else !column;
    pixel_offset := !pixel_offset + pixel_size
  done;
  b'

let rotate_bmp_file input_file output_file =
  let bmp_file_data =
    In_channel.with_open_bin input_file In_channel.input_all
  in
  let bmp_fh = parse_bitmap_file_header bmp_file_data in
  let bmp_ih = parse_bitmap_info_header bmp_file_data in
  assert (bmp_fh.bmp_type = 0x4d42);
  assert (bmp_fh.file_size = 529338);
  assert (bmp_ih.width = 420);
  assert (bmp_ih.height = 420);
  (* verify and hardcode pixel bits ... for now *)
  assert (bmp_ih.pixel_bits = 24);
  let pixel_bytes = 3 in
  let bmp_header_size = bmp_file_header_size + bmp_ih.bitmap_info_header_size in
  let pixel_array_data =
    String.sub bmp_file_data bmp_fh.pixel_array_offset
      (bmp_fh.file_size - bmp_fh.pixel_array_offset)
  in
  let pixel_array =
    rotate pixel_array_data bmp_ih.height bmp_ih.width pixel_bytes
  in
  Out_channel.with_open_bin output_file (fun out_ch ->
      (* copy header from input file *)
      Out_channel.output_string out_ch
        (String.sub bmp_file_data 0 bmp_header_size);
      Out_channel.output_bytes out_ch pixel_array;
      Out_channel.flush out_ch)

let () =
  let input_file = "image-rotate/teapot.bmp" in
  let output_file = "/tmp/teapot-rotated.bmp" in
  let target_file = "image-rotate/rotated.bmp" in
  rotate_bmp_file input_file output_file;
  (* Compare the md5 hash of the rotated input file and the provided target example *)
  assert (Digest.equal (Digest.file output_file) (Digest.file target_file))
