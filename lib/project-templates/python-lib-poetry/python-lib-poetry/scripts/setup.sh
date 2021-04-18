#!/usr/bin/env bash
set -euo pipefail


install_poetry() {
    echo ">> installing poetry..."
    curl -sSL https://raw.githubusercontent.com/python-poetry/poetry/master/install-poetry.py | python -
}

install_poetry
