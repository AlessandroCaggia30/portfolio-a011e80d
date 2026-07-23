#!/bin/bash
# auto-pull: polls origin/main and fast-forwards local main when ahead.
# Counterpart to auto-deploy.sh — keeps the local repo in sync with edits
# made from the deployed website.
#
# Usage:
#   ./auto-pull.sh                # foreground, Ctrl+C to stop
#   nohup ./auto-pull.sh > /tmp/auto-pull.log 2>&1 &   # background
#
# 2026-07-23 v2: coordinated with auto-deploy.sh via a shared mkdir-mutex,
# stale git-lock cleanup.

REPO_DIR="/Users/Alessandro/Repos/portfolio-a011e80d"
INTERVAL=15                                 # seconds between checks
LOCK_DIR="/tmp/mindnotes-git-sync.lock"    # mkdir mutex, shared with auto-deploy.sh
STALE_LOCK_SEC=300                          # 5 min → mutex considered stale

cd "$REPO_DIR" || { echo "Cannot cd to $REPO_DIR"; exit 1; }

echo "Watching origin/main for new commits in: $REPO_DIR"
echo "Coordination mutex:                    $LOCK_DIR"
echo "Checking every ${INTERVAL}s. Ctrl+C to stop."
echo "---"

# --- helpers ---------------------------------------------------------------

_acquire_lock() {
    local max_wait=${1:-30}
    local waited=0
    while ! mkdir "$LOCK_DIR" 2>/dev/null; do
        if [ -d "$LOCK_DIR" ]; then
            local mtime now
            mtime=$(stat -f "%m" "$LOCK_DIR" 2>/dev/null || echo 0)
            now=$(date +%s)
            if [ $((now - mtime)) -gt "$STALE_LOCK_SEC" ]; then
                rmdir "$LOCK_DIR" 2>/dev/null && \
                    echo "[$(date +%H:%M:%S)] wiped stale mutex ($((now - mtime))s old)"
            fi
        fi
        if [ "$waited" -ge "$max_wait" ]; then return 1; fi
        sleep 1
        waited=$((waited + 1))
    done
    return 0
}

_release_lock() { rmdir "$LOCK_DIR" 2>/dev/null; }

_clean_stale_git_locks() {
    local now cleaned=0
    now=$(date +%s)
    for lock in .git/HEAD.lock .git/index.lock .git/logs/HEAD.lock \
                .git/gc.log.lock .git/objects/maintenance.lock \
                .git/packed-refs.lock .git/refs/heads/main.lock; do
        [ -f "$lock" ] || continue
        local mtime age
        mtime=$(stat -f "%m" "$lock" 2>/dev/null || echo "$now")
        age=$((now - mtime))
        if [ "$age" -gt 60 ]; then
            rm -f "$lock" 2>/dev/null && cleaned=$((cleaned + 1))
        fi
    done
    if [ "$cleaned" -gt 0 ]; then
        echo "[$(date +%H:%M:%S)] cleared $cleaned stale git lock(s)"
    fi
}

trap _release_lock EXIT INT TERM

# --- main loop -------------------------------------------------------------

while true; do
    # Skip if there are local uncommitted edits (auto-deploy will handle them).
    if ! git diff --quiet --ignore-submodules HEAD 2>/dev/null; then
        echo "[$(date +%H:%M:%S)] Local uncommitted changes — skipping"
        sleep "$INTERVAL"
        continue
    fi

    # Acquire mutex; short timeout — pull cycles happen every 15s, don't block long.
    if ! _acquire_lock 20; then
        # auto-deploy is holding the lock (likely mid-push); we'll retry next tick.
        sleep "$INTERVAL"
        continue
    fi

    _clean_stale_git_locks

    if ! git fetch --quiet origin main 2>/dev/null; then
        echo "[$(date +%H:%M:%S)] Fetch failed (network?) — retrying"
        _release_lock
        sleep "$INTERVAL"
        continue
    fi

    LOCAL=$(git rev-parse HEAD)
    REMOTE=$(git rev-parse origin/main)

    if [ "$LOCAL" != "$REMOTE" ]; then
        BEHIND=$(git rev-list --count HEAD..origin/main)
        if git merge --ff-only origin/main > /dev/null 2>&1; then
            echo "[$(date +%H:%M:%S)] Pulled $BEHIND new commit(s)"
        else
            echo "[$(date +%H:%M:%S)] Non-fast-forward — diverged from origin/main, manual resolve needed"
        fi
    fi

    _release_lock
    sleep "$INTERVAL"
done
