# MindNotes - Development & Deployment Guide

## Architecture

MindNotes is a single-page vanilla JS app. Everything runs from one `index.html` file that inlines all CSS and JS. The separate `js/*.js` and `styles.css` files exist as reference/development copies but are NOT loaded by the deployed app.

## Deployment

The app is deployed via **GitHub Pages** from the `portfolio-a011e80d` repo.

- **GitHub repo**: `AlessandroCaggia30/portfolio-a011e80d`
- **Live URL**: https://alessandrocaggia30.github.io/portfolio-a011e80d/apps/mindnotes/index.html
- **Local clone**: `/Users/Alessandro/Desktop/people/Alessandro/website/portfolio-a011e80d/`
- **App path in repo**: `apps/mindnotes/`

## File Locations

| What | Path |
|------|------|
| Local working dir | `/Users/Alessandro/Desktop/people/Alessandro/website/my note taking app/` |
| Local git clone | `/Users/Alessandro/Desktop/people/Alessandro/website/portfolio-a011e80d/` |
| App in repo | `portfolio-a011e80d/apps/mindnotes/` |
| Deployed app | Only `index.html` matters (it inlines everything) |

## How to Deploy Changes

**The source of truth is `index.html` in the repo.** All CSS and JS are inlined in it. When making changes:

1. **Edit the repo copy directly**:
   ```
   /Users/Alessandro/Desktop/people/Alessandro/website/portfolio-a011e80d/apps/mindnotes/index.html
   ```

2. **Commit and push**:
   ```bash
   cd /Users/Alessandro/Desktop/people/Alessandro/website/portfolio-a011e80d
   git add apps/mindnotes/index.html
   git commit -m "description of changes"
   git push origin main
   ```

3. **Sync local working dir** (to keep it up to date):
   ```bash
   cp portfolio-a011e80d/apps/mindnotes/index.html "my note taking app/index.html"
   ```

GitHub Pages rebuilds within 1-2 minutes after push.

## Important Notes

- **DO NOT** edit the local working dir (`my note taking app/`) and push those files to the repo. The repo version has features the local copy may lack (subject selector landing page, FSRS flashcards, etc.).
- **ALWAYS** edit the repo copy at `portfolio-a011e80d/apps/mindnotes/index.html` and push from there.
- The standalone `js/*.js` files in both locations are **not used** by the deployed app — they exist as reference only. All code is inlined in `index.html`.
- The local working dir contains extra utility scripts (`*-layout.js`, `import-*.js`, `add-links.js`) that are dev tools, not part of the app.

## App Structure (inside index.html)

The `index.html` is ~8000 lines and contains everything:

1. **CSS** (lines 1-2450) — all styles inlined in `<style>` tags
2. **HTML** (lines 2450-3000) — subject screen, sidebar, canvas, modals
3. **JS** (lines 3000-8000) — all modules concatenated:
   - State management + SUBJECTS array (subject landing page config)
   - FSRS spaced repetition algorithm
   - Utility functions + KaTeX macros + LaTeX processing
   - Canvas rendering + node management
   - Editor + formatting + advanced macro dropdown
   - Sidebar + search
   - Cloud sync
   - Views + theme
   - Subject selection screen
   - App initialization

## Subject Landing Page

The app opens to a subject selector screen defined by the `SUBJECTS` array:

```javascript
const SUBJECTS = [
    { id: 'adv-math', name: 'Advanced Mathematics', icon: '∑', dataKey: 'mindnotes_advmath', seedFile: 'data.json' },
    { id: 'adv-micro', name: 'Advanced Microeconometrics', icon: '📊', dataKey: 'mindnotes_advmicro', seedFile: null },
    { id: 'adv-macro', name: 'Advanced Macroeconomics', icon: '🌐', dataKey: 'mindnotes_advmacro', seedFile: null }
];
```

Each subject stores its data independently in localStorage under its `dataKey`. To add a new subject, just add an entry to this array.

## LaTeX Support

KaTeX 0.16.9 with 120+ custom macros including:
- Number sets (`\R`, `\N`, `\Z`, etc.)
- 30+ named operators (`\Var`, `\Cov`, `\rank`, `\ker`, `\dom`, etc.)
- Calculus shortcuts (`\dv`, `\pdv`, `\dd`)
- Economics macros (`\Lagr`, `\Bellman`, `\gdp`, `\mc`, `\mr`, etc.)
- Probability (`\Prob{}`, `\Exp{}`, `\iid`, `\dto`, `\pto`)
- Auto-sized delimiters (`\norm{}`, `\abs{}`, `\set{}`, `\paren{}`)

The `processLatexCommands()` function converts LaTeX structural commands (\section, \begin{theorem}, \textbf, etc.) to markdown before rendering.
