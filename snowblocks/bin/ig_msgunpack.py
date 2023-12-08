#!/usr/bin/env python3

import sys
import msgpack
import datetime

def main():
    unpacker = msgpack.unpacker(sys.stdin.buffer, raw=False)
    for unpacked in unpacker:
        if isinstance(unpacked, list):
            unpacked[1] = datetime.datetime.fromtimestamp(unpacked[1]/1000).strftime('%F %T.%f')
        print(unpacked)
    return 0


if __name__ == '__main__':
    sys.exit(main())
