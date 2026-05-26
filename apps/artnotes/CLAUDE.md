# Artnotes - Development & Deployment Guide

## What it is

Artnotes is a parallel of mindnotes optimized for **music & literature mapping** —
huge graphs of genres, artists, authors, works. It is **totally separate** from
mindnotes: its own IndexedDB, its own GitHub Sync paths, its own auto-deploy
service. Editing mindnotes never touches artnotes data and vice versa.

## What was stripped (vs. mindnotes)

- KaTeX CDN + macros (no LaTeX rendering — katex calls are stubbed no-ops).
- Flashcards/FSRS UI (tab, review screen, rating buttons, badges).
- Quiz view.
- The mindnotes-specific one-time migrations and Maximization cleanup.

## What was added

- **Viewport culling** in `renderCanvas()` — only nodes whose bounding box
  intersects the visible viewport (+ 75% buffer) are rendered. This is the
  biggest speed win for enormous maps.
- A debounced re-render after pan/zoom (`_scheduleCullRefresh`) refreshes
  the culled set when the viewport moves enough to expose new nodes.
- Auto-select when only one subject exists (no picker click needed).

## Storage

- **IndexedDB** `artnotes_db` / store `data` / key `artnotes_arts`.
- **localStorage** prefix `artnotes_` for GitHub Sync token, SHAs, timestamps.

## Sync (totally separate from mindnotes)

- GitHub repo path: `apps/artnotes/data_arts.json` (configured in `SUBJECTS[0].repoPath`).
- Commits look like `artnotes: update Arts YYYY-MM-DD HH:MM:SS`.
- New launchd service `com.artnotes.autodeploy` runs `auto-deploy-artnotes.sh`
  which watches `apps/artnotes/index.html` + `apps/artnotes/data*.json`.
- The existing `auto-pull.sh` already pulls everything, so no separate pull
  service is needed.

To activate auto-deploy:
```bash
cp com.artnotes.autodeploy.plist ~/Library/LaunchAgents/
launchctl load ~/Library/LaunchAgents/com.artnotes.autodeploy.plist
```

## File Locations

| What | Path |
|------|------|
| App in repo | `apps/artnotes/` |
| Deployed app | Only `index.html` matters (everything inlined) |
| Auto-deploy script | `auto-deploy-artnotes.sh` (repo root) |
| Auto-deploy plist | `com.artnotes.autodeploy.plist` (repo root) |

## How to Deploy Changes

Edit `apps/artnotes/index.html`, commit, push. GitHub Pages rebuilds within
1-2 minutes. Or just let `com.artnotes.autodeploy` handle it.

## Adding seed data

Drop a `data_arts.json` next to `index.html`. On first load it'll seed
IndexedDB from the file. Subsequent edits live in IndexedDB; GitHub Sync
writes back to the repo file.
