#!/usr/bin/env python3

import os
import re
import sys
import argparse
import subprocess
from typing import List, Optional, Pattern
from git_changelog.build import Changelog, Version
from jinja2.sandbox import SandboxedEnvironment


DESCRIPTION = """
Script to manage a markdown changelog from git logs.
"""

CHANGELOG_TEMPLATE = os.path.join(
    os.path.dirname(os.path.abspath(__file__)),
    "changelog.jinja"
)

VERSION_REGEX = r"^## \[v?(?P<version>[^\]]+)"

MARKER = "<!-- insertion marker -->"


def is_git_directory(path = '.'):
    rc = subprocess.call(['git', '-C', path, 'status'],
                         stderr=subprocess.STDOUT,
                         stdout = open(os.devnull, 'w'))
    return rc == 0

def is_git_remote_configured():
    rc = subprocess.call(['git', 'config', '--get', 'remote.origin.url'],
                         stderr=subprocess.STDOUT,
                         stdout = open(os.devnull, 'w'))
    return rc == 0



def read_template(filepath: str):
    with open(filepath, "r") as tf:
        return tf.read()


def read_changelog(filepath: str) -> List[str]:
    with open(filepath, "r") as cf:
        return cf.read().splitlines()


def write_changelog(filepath: str, lines: List[str]):
    with open(filepath, "w") as cf:
        cf.write("\n".join(lines).rstrip("\n"))
        cf.write("\n")


def latest_version(lines: List[str], regex: Pattern) -> Optional[str]:
    for line in lines:
        match = regex.search(line)
        if match:
            return match.groupdict()["version"]
    return None


def unreleased_versions(versions: List[Version], last_release: str) \
                        -> List[Version]:
    for index, version in enumerate(versions):
        if version.tag == last_release:
            return versions[:index]
    return versions


def update_changelog(
        inplace_file: str,
        marker: str = MARKER,
        version_regex: str = VERSION_REGEX,
        template_filepath: str = CHANGELOG_TEMPLATE,
        commit_style: str = "angular",
):
    env = SandboxedEnvironment(autoescape=False)
    changelog = Changelog(".", style=commit_style)
    template = env.from_string(read_template(template_filepath))

    if len(changelog.versions_list) == 1:
        last_version = changelog.versions_list[0]
        if last_version.planned_tag is None:
            planned_tag = "0.1.0"
            last_version.tag = planned_tag
            last_version.url += planned_tag
            last_version.compare_url = last_version.compare_url.replace("HEAD", planned_tag)

    lines = read_changelog(inplace_file)
    last_released = latest_version(lines, re.compile(version_regex))
    if last_released:
        changelog.versions_list = unreleased_versions(changelog.versions_list,
                                                      last_released)
    rendered = template.render(changelog=changelog, inplace=True)
    lines[lines.index(marker)] = rendered
    write_changelog(inplace_file, lines)


def parse_args():

    class Formatter(
        argparse.ArgumentDefaultsHelpFormatter,
        argparse.RawTextHelpFormatter
    ):
        pass

    parser = argparse.ArgumentParser(description=DESCRIPTION,
                                     formatter_class=Formatter)
    changelog_file = parser.add_argument('changelog_file', type=str,
                                         help='the changelog markdown file')
    return parser.parse_args()


def main():
    args = parse_args()
    if not is_git_directory():
        print("ERROR: not a git repository")
        return 1
    if not is_git_remote_configured():
        print("ERROR: git remote not configured, see `git remote -v`")
        return 1
    update_changelog(args.changelog_file)
    return 0


if __name__ == '__main__':
    sys.exit(main())
