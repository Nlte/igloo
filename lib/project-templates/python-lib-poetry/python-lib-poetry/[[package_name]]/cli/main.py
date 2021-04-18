"""Command line application entrypoint."""

import argparse
from typing import List, Optional


def get_parser() -> argparse.ArgumentParser:
    """
    Return the CLI argument parser.

    Returns:
        An argparse parser.
    """
    return argparse.ArgumentParser(prog="[[ package_name ]]")


def main(args: Optional[List[str]] = None) -> int:
    """
    Run the main program.

    Arguments:
        args: Arguments passed from the command line.

    Returns:
        An return code.
    """
    parser = get_parser()
    opts = parser.parse_args(args=args)
    return 0
