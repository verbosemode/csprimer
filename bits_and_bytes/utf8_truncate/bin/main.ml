let truncate truncate_length utf8_string =
  assert (String.length utf8_string >= truncate_length && truncate_length > 0);
  let text_ptr = ref 0 in
  for i = 0 to truncate_length - 1 do
    match String.get_uint8 utf8_string i with
    (* advance text pointer for ascii compatible values *)
    | n when n land 0b1000_0000 = 0 -> text_ptr := i
    (* advance text pointer - 1, on a new multi-byte value. *)
    | n when n land 0b1100_0000 = 0b1100_0000 && i > 0 -> text_ptr := i - 1
    (* noop *)
    | _ -> ()
  done;
  if !text_ptr = 0 then
    ""
  else
    String.sub utf8_string 0 (!text_ptr + 1)

let rec process_file ch =
  match In_channel.input_line ch with
  | None -> ()
  | Some data ->
    let utf8_string = String.sub data 1 (String.length data - 1) |> String.trim in
    let truncated_string =
      match String.get_uint8 data 0 with
      | 0 -> ""
      | truncate_length when truncate_length >= String.length utf8_string ->
        utf8_string
      | truncate_length -> truncate truncate_length utf8_string
    in
    print_endline truncated_string;
    process_file ch

let () =
  let filename = Sys.argv.(1) in
  In_channel.with_open_bin filename (fun ch -> process_file ch)
