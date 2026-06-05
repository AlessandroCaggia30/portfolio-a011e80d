# MindNotes — Development & Deployment Guide

## Architecture

MindNotes is a single-page vanilla JS app. The entire app runs from one `index.html` (~12 600 lines) with inlined CSS and JS. The separate `js/*.js` and `styles.css` files exist as reference/development copies but are NOT loaded by the deployed app.

## Deployment

GitHub Pages from `portfolio-a011e80d`.

| | |
|--|--|
| GitHub repo  | `AlessandroCaggia30/portfolio-a011e80d` |
| Live URL     | https://alessandrocaggia30.github.io/portfolio-a011e80d/apps/mindnotes/index.html |
| Local clone  | `/Users/Alessandro/Repos/portfolio-a011e80d/` |
| App path     | `apps/mindnotes/` |

GitHub Pages rebuilds 1–2 min after push to `main`.

## How to deploy changes

The source of truth is `apps/mindnotes/index.html`. Edit, commit, push:

```bash
cd /Users/Alessandro/Repos/portfolio-a011e80d
git add apps/mindnotes/index.html
git commit -m "..."
git push origin main
```

A launchd service (`com.mindnotes.autodeploy`) watches `apps/mindnotes/index.html` and `apps/mindnotes/data*.json` and auto-commits + pushes within seconds. Another (`com.mindnotes.autopull`) periodically `git pull --ff-only`.

## Subjects

`SUBJECTS` array (in `index.html`) drives the landing page. Each entry has `id`, `name`, `icon`, `dataKey` (localStorage), `seedFile` (apps/mindnotes/data_*.json), `repoPath` (apps/mindnotes/data_*.json).

| id | name | icon | seedFile |
|----|------|------|----------|
| `adv-math`    | Advanced Mathematics | ∑ | `data.json` |
| `adv-micro`   | Advanced Microeconometrics | 📊 | `data_advmicro.json` |
| `adv-macro`   | Advanced Macroeconomics | 🌐 | `data_advmacro.json` |
| `time-series` | Time Series Analysis | 📈 | `data_timeseries.json` |
| `statistics`  | Statistics | σ | `data_statistics.json` |

## Statistics — hyper-table mode (DIFFERENT from other subjects)

When `subject.id === 'statistics'`, the app **hides the canvas** and shows a full-page HTML **hyper-table** (`#statsTableView`):
- **Rows** = subtopics, grouped by topic with separator rows.
- **Columns** = `data.tableLayout.columns` (col 1 = Theory, cols 2..86 = exercises Ex0-9, cols 87..99 = past exams).
- **Cards** are clickable; click opens `#statsSnippetModal` showing the full content.
- **Yellow cards** (`stats-exam`) mark past-exam snippets. **Question text** is wrapped in `<span class="exam-question-text">` for blue color.

Build the Statistics data file from `apps/mindnotes/statistics/`:

```bash
cd apps/mindnotes/statistics
python3 build_snippets.py     # produces data_statistics.json
python3 build_ex0.py          # regenerates Ex0 plots
python3 build_ex1.py          # regenerates Ex1 plots
python3 build_ex2_plots.py    # Ex2 plots
python3 build_ex3_plots.py    # Ex3 plots
python3 build_ex8_9_plots.py  # Ex8/9 regression diagnostics
python3 build_past_exam_plots.py  # Past-exam plots
```

`plot_style.py` provides the unified visual style (navy + warm yellow palette, Helvetica/JetBrains Mono, subtle grid, no top/right spines).

Content sources:
- `ex0_data.py` through `ex9_content.py` — exercises 0–9
- `past_exams_content.py` — 13 past exams (2024–2026)

## LaTeX / Markdown / R rendering inside snippets

`processLatexCommands` (~5095 in index.html) does the heavy lifting. Supported:
- `\begin{tabular}{p{Wcm}|p{Wcm}|...}` tables (widths must sum to 38 cm)
- `\textbf`, `\textit`, `\texttt`, `\underline`, `\textcolor{red}{...}`
- Display/inline math via `$$...$$` and `$...$` (KaTeX)
- Markdown headings, **bold**, lists, inline `code`

Multi-line code fences (```` ``` ````) are NOT supported — instead, each R-command line gets wrapped in single backticks during build (see `code_blocks_to_inline` in `build_snippets.py`). Lines starting with `##` are auto-styled as soft-gray R output in the modal.

## App structure (inside `index.html`)

| Lines | What |
|-------|------|
| 1–2960 | CSS (incl. statistics hyper-table block ~2874–3110) |
| 2960–3000 | Subject screen + statistics view container |
| 3000–4900 | State, FSRS, sync, init |
| 4900–7500 | Renderer (formatContent, processLatexCommands, KaTeX bridge) |
| 7500–10000 | Editor, sidebar, search, modals |
| 11000+ | Statistics-specific: `renderStatsTable`, `openStatsSnippet`, `findStatsNode` |

## Important notes

- **DO NOT** edit the local working dir (`my note taking app/`) and push those files to the repo. The repo's index.html has features the local copy may lack.
- ALWAYS edit `portfolio-a011e80d/apps/mindnotes/index.html` and push from there.
- Standalone `js/*.js` files are reference only — NOT loaded by the deployed app.
- `statistics/__pycache__/` is gitignored (Python bytecode).
