open Unsigned

let test_uint64 = Alcotest.testable UInt64.pp UInt64.equal

let test_encode () =
  Alcotest.(check bytes)
    "encode 1" (Bytes.of_string "\x01")
    (Csprimer_varint.encode (UInt64.of_string "1"));
  Alcotest.(check bytes)
    "encode 150"
    (Bytes.of_string "\x96\x01")
    (Csprimer_varint.encode (UInt64.of_string "150"));
  Alcotest.(check bytes)
    "encode max_int"
    (Bytes.of_string "\xff\xff\xff\xff\xff\xff\xff\xff\xff\x01")
    (Csprimer_varint.encode UInt64.max_int)

let test_decode () =
  Alcotest.(check test_uint64)
    "decode 1" (UInt64.of_string "1")
    (Csprimer_varint.decode (Bytes.of_string "\x01"));
  Alcotest.(check test_uint64)
    "decode 150" (UInt64.of_string "150")
    (Csprimer_varint.decode (Bytes.of_string "\x96\x01"));
  Alcotest.(check test_uint64)
    "decode max_int" UInt64.max_int
    (Csprimer_varint.decode
       (Bytes.of_string "\xff\xff\xff\xff\xff\xff\xff\xff\xff\x01"))

let () =
  let open Alcotest in
  run "varint"
    [
      ("encode", [ test_case "encode" `Quick test_encode ]);
      ("decode", [ test_case "decode" `Quick test_decode ]);
    ]
