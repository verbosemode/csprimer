Note:

- The "cases" file has one case per line. The first byte is an unsigned integer indicating the number of bytes to which to truncate; the remained is a utf-8 encoded string to truncate.
- The "expected" file has one expected result per line.
- Your program should read "cases", parse it, and write one correctly truncated string per line. This output can then be `diff`ed against "expected".
- Do not truncate the string in the middle of a single unicode codepoint! This is the purpose of the exercise.
- None of the cases cover grapheme clusters like 👨‍👩‍👧‍👦 (family of 4) ... this is a challenging stretch goal; see https://unicode.org/reports/tr29/
