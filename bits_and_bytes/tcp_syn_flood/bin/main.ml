open Unsigned

(* Map for storing sequence-number:packet-number mappings *)
module UInt32_map = Map.Make (UInt32)

exception Malformed_data of string

exception No_more_data

(* Read [n] bytes from channel [ch]. raises No_more_data if there is not enough data on the channel to read.  *)
let read_bytes ch n =
  assert (n > 0);
  let b = Bytes.make n '0' in
  match In_channel.really_input ch b 0 n with
  | None -> raise No_more_data
  | Some _ -> b

let get_uint16_le b pos =
  let d1 = UInt16.of_int (Bytes.get_uint8 b (pos + 1)) in
  let d2 = UInt16.of_int (Bytes.get_uint8 b pos) in
  UInt16.Infix.((d1 lsl 8) lxor d2)

let get_uint32_le b pos =
  let d1 = UInt32.of_int (Bytes.get_uint8 b (pos + 3)) in
  let d2 = UInt32.of_int (Bytes.get_uint8 b (pos + 2)) in
  let d3 = UInt32.of_int (Bytes.get_uint8 b (pos + 1)) in
  let d4 = UInt32.of_int (Bytes.get_uint8 b pos) in
  UInt32.Infix.((d1 lsl 24) lxor (d2 lsl 16) lxor (d3 lsl 8) lxor d4)

let get_uint32 b pos =
  let d1 = UInt32.of_int (Bytes.get_uint8 b (pos + 3)) in
  let d2 = UInt32.of_int (Bytes.get_uint8 b (pos + 2)) in
  let d3 = UInt32.of_int (Bytes.get_uint8 b (pos + 1)) in
  let d4 = UInt32.of_int (Bytes.get_uint8 b pos) in
  UInt32.Infix.((d4 lsl 24) lxor (d3 lsl 16) lxor (d2 lsl 8) lxor d1)

type pcap_file_header = {
  magic_number: UInt32.t;
  major_version: UInt16.t;
  minor_version: UInt16.t;
  (* int32 reserved1 *)
  (* int32 reserved2 *)
  snaplen: UInt32.t;
  (* uint16 fcs - 4 bits *)
  link_type: UInt16.t;
}

let parse_pcap_file_header b =
  assert (Bytes.length b = 24);
  let data =
    {
      magic_number = get_uint32_le b 0;
      major_version = get_uint16_le b 4;
      minor_version = get_uint16_le b 6;
      snaplen = get_uint32_le b 16;
      link_type = get_uint16_le b 22;
    }
  in
  if
    (* Timestamp is microseconds *)
    data.magic_number = UInt32.of_int 0xa1b2c3d4
    (* Timestamp is nanoseconds *)
    || data.magic_number = UInt32.of_int 0xa1b23c4d
  then
    data
  else
    raise
      (Malformed_data
         (Printf.sprintf "invalid magic number: %s"
            (UInt32.to_string data.magic_number)))

type packet_record = {
  timestamp: UInt32.t;
  timestamp_mus_ns: UInt32.t;
  captured_packet_length: UInt16.t;
  original_packet_length: UInt16.t;
  packet_data: Bytes.t;
}
[@@deriving show]

let parse_packet_record ch =
  let b = read_bytes ch 16 in
  let captured_packet_length = get_uint16_le b 8 in
  {
    timestamp = get_uint32_le b 0;
    timestamp_mus_ns = get_uint32_le b 4;
    captured_packet_length;
    original_packet_length = get_uint16_le b 12;
    packet_data = read_bytes ch (UInt16.to_int captured_packet_length);
  }

type tcp_fields = {
  syn: bool;
  ack: bool;
  seqn: UInt32.t;
  ackn: UInt32.t;
}
[@@deriving show]

(* Reading from fixed offsets, since there are no IP options in the pcap file :-) *)
let parse_relevant_tcp_fields b =
  let flags = Bytes.get_uint8 b 37 in
  let syn =
    if flags land 0b0000_0010 = 2 then
      true
    else
      false
  in
  let ack =
    if flags land 0b0001_0000 = 16 then
      true
    else
      false
  in
  { syn; ack; seqn = get_uint32 b 28; ackn = get_uint32 b 32 }

let fold_pcap_packets filename f a =
  let rec process_packets ch a =
    match parse_packet_record ch with
    | packet -> process_packets ch (f a packet)
    | exception No_more_data -> a
  in
  In_channel.with_open_bin filename (fun ch ->
      let header_bytes = read_bytes ch 24 in
      let _ = parse_pcap_file_header header_bytes in
      process_packets ch a)

let process_packet (synpkt_map, packets, syn_packets, packets_acked) p =
  let packets = packets + 1 in
  let tcp_data = parse_relevant_tcp_fields p.packet_data in
  match tcp_data.syn, tcp_data.ack with
  | true, false ->
    let syn_packets = syn_packets + 1 in
    (* Store current sequence number / packet number in Map *)
    let synpkts = UInt32_map.add tcp_data.seqn syn_packets synpkt_map in
    synpkts, packets, syn_packets, packets_acked
  | true, true ->
    (* Sequence number is this segment is acknowledging *)
    let prev_seqn = UInt32.(sub tcp_data.ackn (of_int 1)) in
    (* Lookup Acknowledgement number - 1 in Map *)
    (match UInt32_map.find_opt prev_seqn synpkt_map with
    | None -> synpkt_map, packets, syn_packets, packets_acked
    | Some acked_packet -> synpkt_map, packets, syn_packets, packets_acked + 1)
  | false, false | false, true ->
    failwith "Example pcap only contains packets with syn, syn/ack"

let () =
  let filename = Sys.argv.(1) in
  let _, packets, syn_packets, packets_acked =
    fold_pcap_packets filename process_packet (UInt32_map.empty, 0, 0, 0)
  in
  Printf.printf
    "packets total: %i syn packets: %i syn packets acked: %i (%i %%)\n%!"
    packets syn_packets packets_acked
    (packets_acked / (syn_packets / 100))
