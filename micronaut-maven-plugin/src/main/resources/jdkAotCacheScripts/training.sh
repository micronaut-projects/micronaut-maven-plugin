#!/usr/bin/env bash
#
# Trains a JDK AOT cache (JEP 483) for a Micronaut application, for micronaut.docker.jdkAotCache. It only needs bash:
# JRE images such as eclipse-temurin:25-jre have no curl, wget or jcmd, so the requests go through bash's /dev/tcp.
#
#   training.sh warm-up <port> <timeout> [<path>...]
#     Waits up to <timeout> seconds until the application answers HTTP requests on 127.0.0.1:<port>, then sends a
#     GET request to every <path> in order. Fails on an I/O error or a status of 400 or more. The docker goal runs it
#     with docker exec in the training container, whose entrypoint is the application.
#
#   training.sh train <cache> <port> <timeout> <switch|sigterm> [<path>...] -- <java> <argument>...
#     Runs the application with -XX:AOTCacheOutput=<cache> and checks that the cache was written. With "switch", the
#     Micronaut training-run switch warms the application up and exits. With "sigterm", the application runs in the
#     background, this script warms it up as above and stops it with SIGTERM. Generated Dockerfiles run it.

set -u

log() {
    printf '[jdk-aot-cache] %s\n' "$*"
}

fail() {
    printf '[jdk-aot-cache] %s\n' "$*" >&2
    exit 1
}

# Prints the status code of a GET request to 127.0.0.1:<port><path>. Fails when there is no HTTP response.
http_get() {
    local port=$1 path=$2 timeout=$3
    (
        printf 'GET %s HTTP/1.1\r\nHost: 127.0.0.1:%s\r\nConnection: close\r\n\r\n' "$path" "$port" >&3 || exit 1
        IFS= read -r -t "$timeout" status_line <&3 || exit 1
        # Read the whole response, as a client would
        while IFS= read -r -t "$timeout" _ <&3; do :; done
        [[ ${status_line%$'\r'} =~ ^HTTP/[0-9.]+\ ([0-9]{3}) ]] || exit 1
        printf '%s' "${BASH_REMATCH[1]}"
    ) 2>/dev/null 3<>"/dev/tcp/127.0.0.1/$port"
}

# warm_up <port> <timeout> <pid> [<path>...]: <pid> is the application process, or empty when it is not a child
warm_up() {
    local port=$1 timeout=$2 pid=$3 status path
    shift 3
    local probe=${1:-/} deadline=$((SECONDS + timeout))
    log "Waiting up to ${timeout}s for the application to answer on port $port"
    until status=$(http_get "$port" "$probe" 5); do
        if [ -n "$pid" ] && ! kill -0 "$pid" 2>/dev/null; then
            fail "The application exited before it answered on port $port"
        fi
        if [ "$SECONDS" -ge "$deadline" ]; then
            fail "The application did not answer on port $port within ${timeout}s"
        fi
        sleep 0.2 2>/dev/null || sleep 1
    done
    log "The application answers on port $port (GET $probe: $status)"
    for path in "$@"; do
        status=$(http_get "$port" "$path" "$timeout") || fail "GET $path failed: no HTTP response"
        if [ "$status" -ge 400 ]; then
            fail "GET $path answered $status"
        fi
        log "GET $path: $status"
    done
}

# await_exit <pid> <timeout>: waits for the application, kills it after <timeout> seconds, returns its exit status
await_exit() {
    local pid=$1 timeout=$2 status
    (
        for ((i = 0; i < timeout; i++)); do
            sleep 1
            kill -0 "$pid" 2>/dev/null || exit 0
        done
        printf '[jdk-aot-cache] The application did not exit within %ss, killing it\n' "$timeout" >&2
        kill -KILL "$pid" 2>/dev/null
    ) &
    local watchdog=$!
    wait "$pid"
    status=$?
    wait "$watchdog" 2>/dev/null
    return "$status"
}

train() {
    [ $# -ge 4 ] || fail "Usage: training.sh train <cache> <port> <timeout> <switch|sigterm> [<path>...] -- <java> <argument>..."
    local cache=$1 port=$2 timeout=$3 mode=$4 paths=() output major status
    shift 4
    while [ $# -gt 0 ] && [ "$1" != "--" ]; do
        paths+=("$1")
        shift
    done
    [ $# -ge 2 ] || fail "Missing the java command after --"
    shift

    output=$("$1" -XX:+UnlockDiagnosticVMOptions -XX:+PrintFlagsFinal -version 2>&1) || fail "$1 -version failed"
    [[ $output =~ \ version\ \"([0-9]+)(\.([0-9]+))? ]] || fail "Could not read the Java version of $1"
    major=${BASH_REMATCH[1]}
    if [ "$major" = 1 ]; then
        major=${BASH_REMATCH[3]}
    fi
    if [ "$major" -lt 25 ]; then
        fail "A JDK AOT cache needs Java 25 or later, but $1 is Java $major"
    fi
    if [[ $output =~ (^|$'\n')[[:space:]]*bool[[:space:]]+AOTCompatibleOopCompression[[:space:]] ]]; then
        # Lets the cache be used with a different compressed oops encoding than the one it was trained with
        export JDK_AOT_VM_OPTIONS="${JDK_AOT_VM_OPTIONS:+$JDK_AOT_VM_OPTIONS }-XX:+UnlockDiagnosticVMOptions -XX:+AOTCompatibleOopCompression"
    fi

    local options="-XX:AOTCacheOutput=$cache" i
    if [ "$mode" = switch ]; then
        options+=" -Dmicronaut.application.training.enabled=true"
        for ((i = 0; i < ${#paths[@]}; i++)); do
            options+=" -Dmicronaut.application.training.warmup.paths[$i]=${paths[$i]}"
        done
    fi
    log "Training with JDK_JAVA_OPTIONS=${JDK_JAVA_OPTIONS:+$JDK_JAVA_OPTIONS }$options"
    JDK_JAVA_OPTIONS="${JDK_JAVA_OPTIONS:+$JDK_JAVA_OPTIONS }$options" "$@" &
    application_pid=$!
    trap 'kill -KILL "$application_pid" 2>/dev/null' EXIT
    if [ "$mode" != switch ]; then
        warm_up "$port" "$timeout" "$application_pid" ${paths[@]+"${paths[@]}"}
        log "Stopping the application with SIGTERM"
        kill -TERM "$application_pid" 2>/dev/null
    fi
    await_exit "$application_pid" "$timeout"
    status=$?
    trap - EXIT
    case "$mode:$status" in
        switch:0 | sigterm:0 | sigterm:143) ;;
        *) fail "The training run exited with status $status" ;;
    esac
    [ -s "$cache" ] || fail "The training run did not write the JDK AOT cache $cache"
    log "Wrote the JDK AOT cache $cache"
}

command=${1:-}
shift || true
case "$command" in
    warm-up)
        [ $# -ge 2 ] || fail "Usage: training.sh warm-up <port> <timeout> [<path>...]"
        port=$1
        timeout=$2
        shift 2
        warm_up "$port" "$timeout" "" "$@"
        ;;
    train)
        train "$@"
        ;;
    *)
        fail "Usage: training.sh warm-up|train ..."
        ;;
esac
