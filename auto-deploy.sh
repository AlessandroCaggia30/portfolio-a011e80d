#!/bin/bash
# auto-deploy: watches the mindnotes app files (index.html + data_*.json)
# and pushes any changes to origin/main on every save.
# Usage: ./auto-deploy.sh           (Ctrl+C to stop)
#
# 2026-07-23 v2: coordinated with auto-pull.sh via a shared mkdir-mutex,
# stale git-lock cleanup, and retry-on-push-fail.

REPO_DIR="/Users/Alessandro/Repos/portfolio-a011e80d"
WATCH_DIR="$REPO_DIR/apps/mindnotes"
LOCK_DIR="/tmp/mindnotes-git-sync.lock"    # mkdir mutex, shared with auto-pull.sh
STALE_LOCK_SEC=300                          # 5 min → mutex considered stale

cd "$REPO_DIR" || { echo "Cannot cd to $REPO_DIR"; exit 1; }

echo "Watching for changes in: $WATCH_DIR"
echo "Coordination mutex:      $LOCK_DIR"
echo "Ctrl+C to stop."
echo "---"

# --- helpers ---------------------------------------------------------------

# mkdir-based mutex: atomic on any POSIX FS. Waits up to $1 seconds, cleaning
# a stale lock older than $STALE_LOCK_SEC.
_acquire_lock() {
    local max_wait=${1:-30}
    local waited=0
    while ! mkdir "$LOCK_DIR" 2>/dev/null; do
        # If lock is older than STALE_LOCK_SEC, wipe it (previous holder crashed).
        if [ -d "$LOCK_DIR" ]; then
            local mtime
            mtime=$(stat -f "%m" "$LOCK_DIR" 2>/dev/null || echo 0)
            local now
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

# Clean stale INTERNAL git locks (HEAD.lock, index.lock, etc.) left by crashed
# git operations. Only removes locks older than 60 seconds when no git process
# is running for this repo — mutex-safety net, not a substitute for it.
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

# Ensure mutex is released even on abnormal exit.
trap _release_lock EXIT INT TERM

# --- main loop -------------------------------------------------------------

fswatch -o "$WATCH_DIR/index.html" "$WATCH_DIR"/data*.json 2>/dev/null | while read -r _; do
    sleep 0.5   # coalesce rapid saves

    if ! _acquire_lock 60; then
        echo "[$(date +%H:%M:%S)] Could not acquire sync mutex within 60s — skipping event"
        continue
    fi

    _clean_stale_git_locks

    # Stage what we care about — includes the images/ folder now that data JSONs
    # reference external files (see 2026-07-25 extraction migration).
    git add apps/mindnotes/index.html apps/mindnotes/data*.json apps/mindnotes/images 2>/dev/null

    if git diff --cached --quiet -- apps/mindnotes/index.html apps/mindnotes/data*.json apps/mindnotes/images; then
        _release_lock
        continue
    fi

    # Retry pull-rebase → commit → push up to 3× with backoff.
    success=0
    for attempt in 1 2 3; do
        # Refresh: rebase local staged commit onto remote to avoid non-ff pushes.
        # If rebase fails on a genuine conflict, abort and let user resolve.
        if ! git pull --rebase --autostash --quiet origin main 2>/dev/null; then
            echo "[$(date +%H:%M:%S)] pull --rebase failed (attempt $attempt/3) — aborting rebase"
            git rebase --abort 2>/dev/null
            sleep $((attempt * 2))
            continue
        fi

        # Commit the staged changes (may already be committed after rebase autostash pop).
        if ! git diff --cached --quiet; then
            git commit -m "auto-deploy: mindnotes update $(date +%H:%M:%S)" > /dev/null 2>&1
        fi

        if git push --quiet origin main; then
            CHANGED=$(git diff --name-only HEAD~1 HEAD -- apps/mindnotes/ 2>/dev/null | tr '\n' ' ')
            echo "[$(date +%H:%M:%S)] Pushed: $CHANGED"
            success=1
            break
        fi

        echo "[$(date +%H:%M:%S)] push failed (attempt $attempt/3) — retrying"
        sleep $((attempt * 2))
    done

    if [ "$success" -eq 0 ]; then
        echo "[$(date +%H:%M:%S)] Push failed after 3 attempts — leaving commit local"
    fi

    _release_lock
done
