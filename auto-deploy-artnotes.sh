#!/bin/bash
# auto-deploy (artnotes): watches the artnotes app files (index.html + data_*.json)
# and pushes any changes to origin/main on every save.
# Usage: ./auto-deploy-artnotes.sh           (Ctrl+C to stop)

REPO_DIR="/Users/Alessandro/Repos/portfolio-a011e80d"
WATCH_DIR="$REPO_DIR/apps/artnotes"

cd "$REPO_DIR" || { echo "Cannot cd to $REPO_DIR"; exit 1; }

echo "Watching for changes in: $WATCH_DIR"
echo "Will auto-commit + push index.html and data_*.json edits to origin/main."
echo "Ctrl+C to stop."
echo "---"

fswatch -o "$WATCH_DIR/index.html" "$WATCH_DIR"/data*.json 2>/dev/null | while read -r _; do
    sleep 0.5

    git add apps/artnotes/index.html apps/artnotes/data*.json 2>/dev/null

    if git diff --cached --quiet -- apps/artnotes/index.html apps/artnotes/data*.json; then
        continue
    fi

    git pull --rebase --quiet origin main 2>/dev/null

    git commit -m "auto-deploy: artnotes update $(date +%H:%M:%S)" > /dev/null

    if git push --quiet origin main; then
        CHANGED=$(git diff --name-only HEAD~1 HEAD -- apps/artnotes/ | tr '\n' ' ')
        echo "[$(date +%H:%M:%S)] Pushed: $CHANGED"
    else
        echo "[$(date +%H:%M:%S)] Push failed — leaving the commit local."
    fi
done
