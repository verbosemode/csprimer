#!/usr/bin/env python3

import math
import struct
import sys
from os import EX_OK

FLOAT_DOUBLE_PREC_MAXLEN = 8
FLOAT_DOUBLE_PREC_MSG_OFFSET = 2
MESSAGE_MAX_LEN = 6


def conceal(message: str) -> float:
    message_data = message.encode("utf-8")
    message_data_len = len(message_data)
    assert message_data_len <= MESSAGE_MAX_LEN

    flaot_double_prec = [0x00] * FLOAT_DOUBLE_PREC_MAXLEN
    flaot_double_prec[0] = 0b1_1111_111
    # encode string length into the 4 least significant bits
    flaot_double_prec[1] = 0b1111_0000 ^ message_data_len

    # Just set 1 bit in the fraction to make the infinite float a NaN
    # for strings of length 0
    if message_data_len == 0:
        flaot_double_prec[7] = 0b0000_0001
    else:
        for i, e in enumerate(message_data):
            flaot_double_prec[i + FLOAT_DOUBLE_PREC_MSG_OFFSET] = e

    return struct.unpack(">d", bytes(flaot_double_prec))[0]


def extract(float_data: float) -> str:
    assert math.isnan(float_data)

    data = struct.pack(">d", float_data)
    assert len(data) == FLOAT_DOUBLE_PREC_MAXLEN

    # extract message length
    message_len = data[1] & 0b0000_1111
    assert message_len <= MESSAGE_MAX_LEN

    message_data = data[
        FLOAT_DOUBLE_PREC_MSG_OFFSET : message_len + FLOAT_DOUBLE_PREC_MSG_OFFSET
    ]

    return message_data.decode("utf-8")


def main() -> int:
    assert extract(conceal("hello!")) == "hello!"
    assert extract(conceal("h")) == "h"
    assert extract(conceal("")) == ""
    assert extract(conceal("_🐫_")) == "_🐫_"
    try:
        assert extract(conceal("🐍🐫")) == "🐍🐫"
        print("[Error] encoded more than MESSAGE_MAX_LEN")
        return -1
    except AssertionError:
        pass

    return EX_OK


if __name__ == "__main__":
    sys.exit(main())
