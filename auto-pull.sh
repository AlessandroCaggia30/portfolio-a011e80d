#!/bin/bash
# auto-pull: polls origin/main and fast-forwards local main when ahead.
# Counterpart to auto-deploy.sh, but in the other direction: keeps the local
# repo in sync with edits made from the deployed website.
#
# Usage:
#   ./auto-pull.sh                # foreground, Ctrl+C to stop
#   nohup ./auto-pull.sh > /tmp/auto-pull.log 2>&1 &   # background

REPO_DIR="/Users/Alessandro/Repos/portfolio-a011e80d"
INTERVAL=15  # seconds between checks

cd "$REPO_DIR" || { echo "Cannot cd to $REPO_DIR"; exit 1; }

echo "Watching origin/main for new commits in: $REPO_DIR"
echo "Checking every ${INTERVAL}s. Ctrl+C to stop."
echo "---"

while true; do
    # Skip pull if there are local uncommitted edits; we won't try to merge.
    if ! git diff --quiet --ignore-submodules HEAD 2>/dev/null; then
        echo "[$(date +%H:%M:%S)] Local uncommitted changes — skipping"
        sleep "$INTERVAL"
        continue
    fi

    if ! git fetch --quiet origin main 2>/dev/null; then
        echo "[$(date +%H:%M:%S)] Fetch failed (network?) — retrying"
        sleep "$INTERVAL"
        continue
    fi

    LOCAL=$(git rev-parse HEAD)
    REMOTE=$(git rev-parse origin/main)

    if [ "$LOCAL" != "$REMOTE" ]; then
        # Count how many new commits and pull fast-forward only.
        BEHIND=$(git rev-list --count HEAD..origin/main)
        if git merge --ff-only origin/main > /dev/null 2>&1; then
            echo "[$(date +%H:%M:%S)] Pulled $BEHIND new commit(s)"
        else
            echo "[$(date +%H:%M:%S)] Non-fast-forward — diverged from origin/main, manual resolve needed"
        fi
    fi

    sleep "$INTERVAL"
done
