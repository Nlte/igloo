LOCAL_DIR="$HOME/.local/share/igloo-dask" # must not end with /
SCHEDULER_PID="${LOCAL_DIR}/scheduler.pid"
SCHEDULER_INFO="${LOCAL_DIR}/scheduler.info"

if ! [ -d "$LOCAL_DIR" ]; then
    mkdir -p "$LOCAL_DIR"
fi
