# sneaky_nan

## Task

* Store messages in IEEE 754's NaN encoding
* Switch from OCaml to Python due to weird issues with unboxed ints
  that I need to investigate.
* Stdlib.nan -> 0x7F_F8_00_00_00_00_00_01L
* but Float.equal Float.nan (Int64.to_float 0x7F_F8_00_00_00_00_00_01L) is false

## Interface

* val conceal : string -> float
* val extract : float -> string

## Planning

* NaN bit encoding
  - use infinity as "template bit pattern"
  0b1_1111_1111_111_0000, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00 -> inf
    S EEEE EEEE EEE FFFF....

### Open Questions

* Do we use single or double precision?
  - I guess double, since test input string is 6 bytes: hello!
  - double -> OCaml default
    - 1 bit s
    - 11 bits exponent
    - 52 bits fraction
      - 6 bytes
      - 4 bits left over. theoretically 2^4
* What is the input string length limit?
  - 6 bytes
* Encode length into left over 4 bits?
* How do we encode a zero length string?
  - fraction part must be non-zero
  - length 0, but add a 1 in the bit pattern
* How to extract a string?
  - Verify that input equals Float.nan
  - Extract string length n && check for max_len 6

## Functions

* extract
  - Verify that equals Float.nan
    - math.isnan in Python
  - Int64.bits_of_float : float -> int64
  - get_length
    - Assert on > 6 $max_string_len
* conceal
  - Verify string_len <= 6
  - Create inf float
  - Add length
  - if 0 -> add 1 to fraction to make the value a NaN
  - Int64.to_float?
