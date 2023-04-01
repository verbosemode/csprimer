# Protobuf varint - Base 128

* Variable width integers
* uint64 -> 1-10 bytes
* https://protobuf.dev/programming-guides/encoding/#varints
* TODO Qcheck tests

## Implementation

encode -> uint64 -> varint bytes
decode -> varint bytes -> uint64

## Examples

```
1 -> "\x01"
150 -> "\x96\x01"
18446744073709551615 -> "\xff\xff\xff\xff\xff\xffxff\xff\xff\x01"
```

### Decode 150

```
    0x96
Hex 1001 0110
 PB 1001 0110 00000001

10010110 00000001        // Original inputs.
 0010110  0000001        // Drop continuation bits.
 0000001  0010110        // Put into little-endian order.
 10010110                // Concatenate.
 128 + 16 + 4 + 2 = 150  // Interpret as integer.
```
