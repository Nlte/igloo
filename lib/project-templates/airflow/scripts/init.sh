#!/usr/bin/env bash
set -euo pipefail

mkdir -p ./dags ./logs ./plugins
if ! [ -f .env ]; then
	echo -e "AIRFLOW_UID=$(id -u)\nAIRFLOW_GID=0" > .env
fi
