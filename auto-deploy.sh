#!/bin/bash
# auto-deploy: watches the mindnotes app files (index.html + data_*.json)
# and pushes any changes to origin/main on every save.
# Usage: ./auto-deploy.sh           (Ctrl+C to stop)

REPO_DIR="/Users/Alessandro/Desktop/people/Alessandro/website/portfolio-a011e80d"
WATCH_DIR="$REPO_DIR/apps/mindnotes"

cd "$REPO_DIR" || { echo "Cannot cd to $REPO_DIR"; exit 1; }

echo "Watching for changes in: $WATCH_DIR"
echo "Will auto-commit + push index.html and data_*.json edits to origin/main."
echo "Ctrl+C to stop."
echo "---"

fswatch -o "$WATCH_DIR/index.html" "$WATCH_DIR"/data*.json 2>/dev/null | while read -r _; do
    # Coalesce rapid bursts (editors often save+touch multiple times).
    sleep 0.5

    # Stage only the tracked mindnotes app files we care about.
    git add apps/mindnotes/index.html apps/mindnotes/data*.json 2>/dev/null

    # Bail if nothing actually staged.
    if git diff --cached --quiet -- apps/mindnotes/index.html apps/mindnotes/data*.json; then
        continue
    fi

    # Pull first to reduce non-fast-forward push rejections.
    git pull --rebase --quiet origin main 2>/dev/null

    git commit -m "auto-deploy: mindnotes update $(date +%H:%M:%S)" > /dev/null

    if git push --quiet origin main; then
        CHANGED=$(git diff --name-only HEAD~1 HEAD -- apps/mindnotes/ | tr '\n' ' ')
        echo "[$(date +%H:%M:%S)] Pushed: $CHANGED"
    else
        echo "[$(date +%H:%M:%S)] Push failed — leaving the commit local."
    fi
done
