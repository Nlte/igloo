#!/usr/bin/env python3

import sys
import argparse


def info(s: str):
    print("INFO: %s" % s)

def error(s: str):
    print("ERROR: %s" % s)


def parse_args():

    class Formatter(
        argparse.ArgumentDefaultsHelpFormatter,
        argparse.RawTextHelpFormatter
    ):
        pass

    DESCRIPTION = """
    Script description
    """

    parser = argparse.ArgumentParser(description=DESCRIPTION,
                                     formatter_class=Formatter)
    parser.add_argument('--example-arg', type=str, required=True,
                        help='Help for this arg')
    return parser.parse_args()


def main():
    args = parse_args()
    return 0


if __name__ == '__main__':
    sys.exit(main())
