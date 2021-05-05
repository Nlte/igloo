#!/usr/bin/env python3

import os
import sys
import urllib3
import argparse
import operator
from html.parser import HTMLParser
from distutils.version import StrictVersion
from packaging.version import parse as std_parse_version


DESCRIPTION = """
Fetch the url of a certain package version
"""

http = urllib3.PoolManager()


class WheelParser(HTMLParser):
    def __init__(self, wheel_list):
        self._wheel_list = wheel_list
        super().__init__()

    def handle_data(self, data):
        if data.endswith(".whl"):
            self._wheel_list.append(data)


def fetch_wheels(data):
    wheels = []
    parser = WheelParser(wheels)
    parser.feed(data)
    return wheels


def read_registry(url):
    resp = http.request("GET", url)
    return resp.data.decode("utf-8")


def parse_version(filename):
    return std_parse_version(filename.split("-")[1])


def find_version(wheels, version_str="latest"):
    wheel_map = {parse_version(p): p for p in wheels}
    if version_str == "latest":
        return max(wheel_map.items(), key=operator.itemgetter(1))[1]
    version = std_parse_version(version_str)
    if version in wheel_map.keys():
        return wheel_map[version]
    print("ERROR: version not found in registry", file=sys.stderr)
    return None


def print_output(url, pkg_name, version):
    out = os.path.join(url, pkg_name, version)
    print(out)


def parse_args():
    class Formatter(
        argparse.ArgumentDefaultsHelpFormatter, argparse.RawTextHelpFormatter
    ):
        pass

    parser = argparse.ArgumentParser(description=DESCRIPTION, formatter_class=Formatter)
    parser.add_argument(
        "--url", type=str, required=True, help="Url of the package registry"
    )
    parser.add_argument(
        "--pkgname", type=str, required=True, help="Name of the package registry"
    )
    parser.add_argument(
        "--version", type=str, required=False, default="latest", help="Version to fetch"
    )
    return parser.parse_args()


def main():
    args = parse_args()
    url = os.path.join(args.url, args.pkgname)
    wheels = fetch_wheels(read_registry(url))
    version = find_version(wheels, version_str=args.version)
    if version is None:
        return 1
    print_output(args.url, args.pkgname, version)
    return 0


if __name__ == "__main__":
    sys.exit(main())
