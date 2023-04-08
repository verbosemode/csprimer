#!/usr/bin/env ocaml

(* kitty terminal: enable_audio_bell yes *)
#use "topfind"

#require "unix"

exception Ctrl_c

let read_input () =
  let rec read_loop () =
    let b_in = Bytes.make 1 '0' in
    let b_out = Bytes.make 1 '\x07' in
    let _ = Unix.read Unix.stdin b_in 0 1 in
    let _ = Unix.write Unix.stdout b_in 0 1 in
    let n = int_of_string (String.of_bytes b_in) in
    for i = 1 to n do
      let _ = Unix.write Unix.stdout b_out 0 1 in
      Unix.sleep 1
    done;
    Out_channel.flush stdout;
    read_loop ()
  in
  try read_loop ()
  with Ctrl_c ->
    print_endline "ctrl-c pressed. bye";
    exit (-1)

let () =
  let term_settings = Unix.tcgetattr Unix.stdin in
  Unix.tcsetattr Unix.stdin Unix.TCSADRAIN
    { term_settings with c_echo = false; c_icanon = false };
  Sys.signal Sys.sigint (Signal_handle (fun _ -> raise Ctrl_c));
  Fun.protect
    ~finally:(fun () -> Unix.tcsetattr Unix.stdin Unix.TCSADRAIN term_settings)
    (read_input ())
