# utf8_truncate

Strategies:

1.) buffers

msb 0 -> add to text buffer
msb 10 -> add to scalar buffer
msb 11 ->
  - add scalar buffer to text buffer if truncate_length not reached, yet
  - initialize scalar buffer with new value
return text buffer when truncate_length is reached

2.) imperative version

msb 0 -> advance text_pointer if truncate_length not reached, yet
msb 11 -> advance text_pointer to pointer - 1, if we are not at beginning of text anymore
return data up to text_pointer
