open Unsigned.UInt64
open Unsigned.UInt64.Infix

exception Malformed_data of string

let get_uint64_payload i = i land of_int 0b0111_1111
let get_payload i = Int.(logand i 0b0111_1111)
let get_cont_bit i = Int.(logand i 0b1000_0000) = 0b1000_0000
let set_cont_bit i = i lor of_int 0b1000_0000
let ( > ) x y = compare x y > 0

let encode i =
  let i = ref i in
  let b = Buffer.create 1 in
  while !i > zero do
    let payload = get_uint64_payload !i in
    i := !i lsr 7;
    let payload = if !i > zero then set_cont_bit payload else payload in
    Buffer.add_uint8 b (to_int payload)
  done;
  Buffer.to_bytes b

let decode b =
  let i = ref zero in
  if Int.equal (Bytes.length b) 0 then !i
  else
    let continuation_bit = ref true in
    let bi = ref 0 in
    let shift_i = ref 0 in
    while !continuation_bit do
      let data =
        try Bytes.get_uint8 b !bi
        with Invalid_argument _ -> raise (Malformed_data "need more data")
      in
      let payload = of_int (get_payload data) lsl !shift_i in
      continuation_bit := get_cont_bit data;
      i := !i lor payload;
      bi := Int.add !bi 1;
      shift_i := Int.add !shift_i 7
    done;
    if !continuation_bit && Int.sub (Bytes.length b) 1 = !bi then
      raise (Malformed_data "no more data, but continuation_bit is set")
    else !i
