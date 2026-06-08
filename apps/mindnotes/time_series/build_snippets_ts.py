"""
Build data_timeseries.json — Time Series Analysis hyper-table system.

Mirrors apps/mindnotes/statistics/build_snippets.py:
  TOPICS  ->  SUBTOPICS  ->  CANVAS NODES (theory + master + exams in columns)
  Col 1   = Theory   (merged from existing canvas nodes for that sub-topic)
  Col 2   = Master Exam Ready (TODO placeholder; filled by Prompt 6)
  Col 3..11 = past-exam questions for each sitting

Sub-topic taxonomy and question-to-sub-topic mapping come from
topics_proposal.py (13 topics, 23 sub-topics, 61 questions). The existing
canvas nodes in data_timeseries.json are mapped into Col-1 cells via
NODE_TO_SUBTOPIC (every existing canvas node lands in at least one Col-1
cell so no theory is lost). The two coral nodes ("SSM filtering and
prediction" and "Exam gaps") are kept intact.
"""
import json, os, re, time

HERE = os.path.dirname(os.path.abspath(__file__))
ROOT = os.path.dirname(HERE)
SRC_JSON = os.path.join(ROOT, "data_timeseries.json")
OUT      = os.path.join(ROOT, "data_timeseries.json")

# ---------------------------------------------------------------------
# Imports
# ---------------------------------------------------------------------
import sys
sys.path.insert(0, HERE)

from past_exams_content import past_exams_ts
from topics_proposal import TOPICS


# ---------------------------------------------------------------------
# Renderer helpers (mirrors statistics/build_snippets.py)
# ---------------------------------------------------------------------
def _md_cell_to_latex(s):
    s = s.strip()
    s = re.sub(r"\*\*(.+?)\*\*", r"\\textbf{\1}", s)
    s = re.sub(r"(?<!\*)\*([^*\n]+)\*(?!\*)", r"\\textit{\1}", s)
    return s


_FENCE_RE = re.compile(r"```[a-zA-Z_]*\n(.*?)\n```", re.DOTALL)
def _fence_replacer(match):
    body = match.group(1).rstrip("\n")
    out_lines = []
    for line in body.split("\n"):
        if line.strip() == "":
            out_lines.append("")
        else:
            safe = line.replace("`", "'")
            out_lines.append("`" + safe + "`")
    return "\n%%RBLOCK%%\n" + "\n".join(out_lines) + "\n%%/RBLOCK%%\n"

def _wrap_consecutive_inline_code_runs(text):
    lines = text.split("\n")
    out = []
    i = 0
    inline_only = re.compile(r"^\s*`[^`]+`\s*$")
    inside_block = False
    while i < len(lines):
        ln = lines[i]
        if "%%RBLOCK%%" in ln:
            inside_block = True
            out.append(ln); i += 1; continue
        if "%%/RBLOCK%%" in ln:
            inside_block = False
            out.append(ln); i += 1; continue
        if (not inside_block) and inline_only.match(ln):
            j = i
            run = []
            while j < len(lines):
                if inline_only.match(lines[j]):
                    run.append(lines[j]); j += 1
                elif lines[j].strip() == "" and j + 1 < len(lines) and inline_only.match(lines[j + 1]):
                    run.append(""); j += 1
                else:
                    break
            while run and run[-1].strip() == "":
                run.pop()
            non_blank = [r for r in run if r.strip()]
            if len(non_blank) >= 2:
                out.append("%%RBLOCK%%")
                out.extend(non_blank)
                out.append("%%/RBLOCK%%")
                i = j
                continue
        out.append(ln)
        i += 1
    return "\n".join(out)

def code_blocks_to_inline(text):
    text = _FENCE_RE.sub(_fence_replacer, text)
    return _wrap_consecutive_inline_code_runs(text)


def md_tables_to_latex(text):
    lines = text.split("\n")
    out_lines = []
    i = 0
    n = len(lines)
    pipe_row = re.compile(r"^\s*\|.*\|\s*$")
    sep_row  = re.compile(r"^\s*\|[\s\-:|]+\|\s*$")
    while i < n:
        if i + 1 < n and pipe_row.match(lines[i]) and sep_row.match(lines[i + 1]):
            header = [c.strip() for c in lines[i].strip().strip("|").split("|")]
            j = i + 2
            body = []
            while j < n and pipe_row.match(lines[j]):
                body.append([c.strip() for c in lines[j].strip().strip("|").split("|")])
                j += 1
            ncols = len(header)
            w = round(38.0 / ncols, 2)
            col_spec = "|".join([f"p{{{w}cm}}"] * ncols)
            out_lines.append(f"\\begin{{tabular}}{{{col_spec}}}")
            out_lines.append("\\hline")
            out_lines.append(" & ".join(_md_cell_to_latex(c) for c in header) + " \\\\")
            out_lines.append("\\hline")
            for row in body:
                row = row + [""] * (ncols - len(row))
                out_lines.append(" & ".join(_md_cell_to_latex(c) for c in row[:ncols]) + " \\\\")
            out_lines.append("\\hline")
            out_lines.append("\\end{tabular}")
            i = j
        else:
            out_lines.append(lines[i])
            i += 1
    return "\n".join(out_lines)


# ---------------------------------------------------------------------
# Canvas layout helpers (mirrors stats build)
# ---------------------------------------------------------------------
COL_X = [200 + 800*i for i in range(20)]
TOP_Y = 200
SNIPPET_W = 600
SNIPPET_GAP = 40
H_NORMAL, H_IMG, H_THEORY = 360, 540, 720

def H(d): return H_IMG if d.get("images") else H_NORMAL

def node(node_id, title, content, x, y, color, w=SNIPPET_W, h=H_NORMAL,
         links=None, images=None, flashcard=False):
    return {"id": node_id, "title": title, "content": content,
            "flashcard": flashcard, "images": images or [], "links": links or [],
            "x": x, "y": y, "width": w, "height": h, "color": color}


# ---------------------------------------------------------------------
# COLUMN HEADERS — 11 columns
# ---------------------------------------------------------------------
COLUMN_HEADERS = [
    {"col":  1, "label": "Theory"},
    {"col":  2, "label": "Master Exam Ready"},
    {"col":  3, "label": "Sep 2025"},
    {"col":  4, "label": "Jun 2025"},
    {"col":  5, "label": "May 2025"},
    {"col":  6, "label": "Jun 2024"},
    {"col":  7, "label": "May 2024"},
    {"col":  8, "label": "May 2023"},
    {"col":  9, "label": "Jun 2022"},
    {"col": 10, "label": "May 2022"},
    {"col": 11, "label": "May 2021"},
]

EXAM_PREFIX_TO_COL = {
    "exam_sep_2025_": 3,
    "exam_jun_2025_": 4,
    "exam_may_2025_": 5,
    "exam_jun_2024_": 6,
    "exam_may_2024_": 7,
    "exam_may_2023_": 8,
    "exam_jun_2022_": 9,
    "exam_may_2022_": 10,
    "exam_may_2021_": 11,
}

def _col_for_exam_id(eid):
    for prefix, col in EXAM_PREFIX_TO_COL.items():
        if eid.startswith(prefix):
            return col
    return None


# ---------------------------------------------------------------------
# Map every existing canvas node title -> a sub-topic id (sid).
# Each existing node ends up in at least one Col-1 cell so no theory is
# lost. Some nodes go to >1 sub-topic when they back several sub-topics.
# Keys must match the EXACT titles in data_timeseries.json.
# ---------------------------------------------------------------------
NODE_TITLE_TO_SUBTOPICS = {
    # T1 — foundations
    "Time series as stochastic processes":              ["t1a"],
    "Aims of time series analysis":                     ["t1a"],
    # T2 — stationarity / ACVF / sample mean
    "weak Stationarity":                                ["t2a"],
    "Strict stationarity":                              ["t2a"],
    "Stationarity does not imply strict stat.":         ["t2a"],
    "Summaries of a stochastic process":                ["t2b"],
    # T3 — Markov theory
    "Markov Property":                                  ["t3a"],
    "Example 2: categorical time series (state is observed)": ["t3a"],
    "Conditions of Theorem 2.1 — recurrent / aperiodic / irreducible": ["t3b"],
    "Ergodic properties of a Markov chain (Theorem 2.1)": ["t3b"],
    # T4 — Markov estimation
    "Estimation":                                       ["t4a"],
    "Proof: the counts ratio is the MLE":               ["t4a"],
    "Asymptotics":                                      ["t4a"],
    "Bernoulli vs categorical":                         ["t4a"],
    # T5 — HMM
    "Decoding / filtering":                             ["t5a", "t8a"],  # shared
    "Exercise — weather/activity filtering (state is unobserved)": ["t5a"],
    # T6 — SSM / DLM
    "State space models (SSM)":                         ["t6a"],
    "Example SSM":                                      ["t6a"],
    "SSM flexibility — no stationarity required":       ["t6b"],
    # T7 — DLM building blocks
    "Example 1: RW":                                    ["t7a"],
    "Fitting a trend":                                  ["t7b"],
    "Fitting a seasonal component":                     ["t7b"],
    "Time series decomposition":                        ["t7b"],
    "Example 3: AR(1) (A and B)":                       ["t7e"],
    # T8 — Kalman filter
    "SSM filtering and prediction":                     ["t8a", "t8b"],  # coral, shared
    # T9 — Smoothing
    "Smoothing and decoding":                           ["t9a"],
    # T10 — Forecast / SES
    "Forecasting algorithms — Cases 0, 1, 2 (SES / Holt / Holt-Winters)": ["t10b"],
    "tuning alfa in SES":                               ["t10b"],
    "How to assign the Probability law of the process": ["t10b"],
    # T11 — Innovations
    "Model checking: forecast errors / innovations":    ["t11a"],
    # T12 — DLM MLE
    "Estimation of unknown parameters --- DLM (prediction-error decomposition)": ["t12a"],
    "Parameter Estimation":                             ["t12a"],
    "Probability vs Likelihood":                        ["t12a"],
    "Examples":                                         ["t12a"],
    # T13 — Bayesian
    "Bayesian statistics":                              ["t13a"],
    "Bayesian vs frequentist inference":                ["t13a"],
    "Posterior mean and precision":                     ["t13a"],
    "Proof for the closed form versions of Ct and mt in the fixed theta case": ["t13a"],
    "Bayesian point estimation: loss functions and the posterior mean": ["t13a"],
    # Special — keep coral, route to t13b as an open bucket
    "Exam gaps":                                        ["t13b"],
}


# ---------------------------------------------------------------------
# Per-sub-topic colour (so the table card style matches a topic group).
# ---------------------------------------------------------------------
SUBTOPIC_COLOR = {
    "T1": "coral", "T2": "orange", "T3": "skyblue", "T4": "purple",
    "T5": "green", "T6": "yellow", "T7": "lavender", "T8": "pink",
    "T9": "salmon", "T10": "lightblue", "T11": "teal", "T12": "gold",
    "T13": "red",
}


# ---------------------------------------------------------------------
# Master-exam content (filled by Prompt 6). If a real master exists in
# master_exercises_content.py for `sid`, use it; otherwise fall back to a
# TODO placeholder so the column is never empty.
# ---------------------------------------------------------------------
try:
    from master_exercises_content import master_exercises_ts as MASTERS_TS
except Exception:
    MASTERS_TS = {}

def make_master_placeholder(sid, sname):
    if sid in MASTERS_TS:
        m = MASTERS_TS[sid]
        return {
            "title": m.get("title", f"Master Exam Ready — {sname}"),
            "content": m["content"],
            "is_master": True,
            "is_exam": False,
            "topic_hint": sid,
            "images": m.get("images", []),
        }
    return {
        "title": f"Master Exam Ready — {sname}",
        "content": (
            f"## Master Exam Ready — {sname}\n\n"
            f"_TODO: consolidated answer for sub-topic **{sid}**._"
        ),
        "is_master": True,
        "is_exam": False,
        "topic_hint": sid,
        "images": [],
    }


# ---------------------------------------------------------------------
# Load existing canvas nodes from current data_timeseries.json and build
# a lookup keyed by title.
# ---------------------------------------------------------------------
with open(SRC_JSON, "r", encoding="utf-8") as f:
    existing_data = json.load(f)

# Pull ALL canvas nodes from ALL topics/subtopics so that re-running the
# builder against its own previous output still finds the original
# theory nodes. Skip nodes that look like generated table content:
#   - id starts with "th_" or "master_"
#   - title starts with "Theory — " or "Master Exam Ready — "
#   - id matches a past-exam id (would be re-emitted from past_exams_ts)
existing_topics = existing_data["data"]["topics"]
existing_by_title = {}
existing_node_ids = set()

def _is_generated_node(n):
    nid = (n.get("id") or "")
    title = (n.get("title") or "")
    if nid.startswith("th_") or nid.startswith("master_"):
        return True
    if nid in past_exams_ts:
        return True
    if title.startswith("Theory — ") or title.startswith("Master Exam Ready — "):
        return True
    return False

for t in existing_topics:
    for s in t.get("subtopics", []):
        for n in s.get("nodes", []):
            if _is_generated_node(n):
                continue
            existing_by_title.setdefault(n["title"], []).append(n)
            existing_node_ids.add(n["id"])

# Sanity check: every node title must be in NODE_TITLE_TO_SUBTOPICS
unmapped_titles = []
for title in existing_by_title.keys():
    if title not in NODE_TITLE_TO_SUBTOPICS:
        unmapped_titles.append(title)
if unmapped_titles:
    print("[WARN] These canvas titles have no sub-topic mapping:")
    for t in unmapped_titles:
        print(f"   - {t!r}")


# ---------------------------------------------------------------------
# Build per-sub-topic: theory_nodes (list of canvas nodes), columns dict.
# ---------------------------------------------------------------------
SUBTOPICS = []
for topic in TOPICS:
    tid = topic["id"]
    tname = topic["name"]
    for st in topic["subtopics"]:
        sid = st["sid"]
        sname = st["name"]
        # Theory nodes belonging to this sub-topic
        theory_nodes = []
        for title, nodes in existing_by_title.items():
            sids = NODE_TITLE_TO_SUBTOPICS.get(title, [])
            if sid in sids:
                theory_nodes.extend(nodes)
        # Columns 3..11: past-exam questions (sub-topic via topic_hint)
        columns = {}
        for q in st["questions"]:
            d = past_exams_ts.get(q)
            if d is None:
                print(f"[WARN] unknown past_exams_ts id {q} in sub-topic {sid}")
                continue
            col = _col_for_exam_id(q)
            if col is None:
                print(f"[WARN] no column for exam id {q}")
                continue
            columns.setdefault(col, []).append(q)
        # Column 1 (theory) and column 2 (master) handled below
        SUBTOPICS.append({
            "topic_id": tid,
            "topic_name": tname,
            "sid": sid,
            "sname": sname,
            "theory_nodes": theory_nodes,
            "columns": columns,
        })


# ---------------------------------------------------------------------
# Build the canvas-node list for each sub-topic row.
# ---------------------------------------------------------------------
SUBTOPIC_GROUP_OF = {sid: t["id"] for t in TOPICS for sid in [s["sid"] for s in t["subtopics"]]}

# Group sub-topic rows by their parent topic so we can emit one canvas
# topic per parent (mirrors how Stats groups rows under topics_out).
topics_out = {}
total_nodes_count = 0
absorbed_titles_per_sid = {}  # diagnostic — what theory nodes went where
empty_cells = []  # (sid, col) — for the final report

for stm in SUBTOPICS:
    tid = stm["topic_id"]
    tname = stm["topic_name"]
    sid = stm["sid"]
    sname = stm["sname"]
    columns = stm["columns"]
    theory_nodes = stm["theory_nodes"]
    color = SUBTOPIC_COLOR.get(tid, "yellow")

    if tid not in topics_out:
        topics_out[tid] = {"id": f"topic_{tid.lower()}", "name": f"{tid} — {tname}", "subtopics": []}

    nodes_in_subtopic = []

    # ---- Column 1: theory ----
    # Use the `th_` prefix so the table renderer applies stats-theory styling.
    theory_id = f"th_{sid}"
    theory_title = f"Theory — {sname}"
    merged_links = []
    merged_images = []
    seen_link_targets = set()
    seen_imgs = set()
    # Concatenate the canvas nodes' content with bold title prefix.
    if theory_nodes:
        parts = []
        for n in theory_nodes:
            chunk_title = n.get("title", "(untitled)")
            chunk = f"**{chunk_title}**\n\n{n.get('content','').strip()}".strip()
            parts.append(chunk)
            for lk in n.get("links", []) or []:
                t = lk.get("target") if isinstance(lk, dict) else None
                if t and t not in seen_link_targets:
                    seen_link_targets.add(t)
                    merged_links.append(lk)
            for img in n.get("images", []) or []:
                if img not in seen_imgs:
                    seen_imgs.add(img)
                    merged_images.append(img)
        merged_content = "\n\n---\n\n".join(parts)
        absorbed_titles_per_sid[sid] = [n.get("title") for n in theory_nodes]
    else:
        merged_content = (
            f"## Theory — {sname}\n\n"
            f"_(no canvas theory node currently mapped to **{sid}**; "
            "add one in time_series/build_snippets_ts.py NODE_TITLE_TO_SUBTOPICS.)_"
        )
        absorbed_titles_per_sid[sid] = []
        empty_cells.append((sid, 1))

    converted_th_content = md_tables_to_latex(code_blocks_to_inline(merged_content))
    # Also link to all exam questions in this row for cross-nav.
    for col_idx, items in columns.items():
        for eid in items:
            if eid not in seen_link_targets:
                merged_links.append({"target": eid, "type": "forward"})
                seen_link_targets.add(eid)

    th_node = node(theory_id, theory_title, converted_th_content,
                   COL_X[0], TOP_Y, "yellow", w=SNIPPET_W, h=H_THEORY,
                   links=merged_links, images=merged_images)
    th_node["column"] = 1
    nodes_in_subtopic.append(th_node)

    # ---- Column 2: master (TODO placeholder) ----
    master_id = f"master_{sid}"
    master_data = make_master_placeholder(sid, sname)
    converted_master_content = md_tables_to_latex(code_blocks_to_inline(master_data["content"]))
    m_node = node(master_id, master_data["title"], converted_master_content,
                  COL_X[1], TOP_Y, "lightblue", w=SNIPPET_W, h=H_NORMAL,
                  links=[theory_id], images=[])
    m_node["column"] = 2
    m_node["isMaster"] = True
    nodes_in_subtopic.append(m_node)
    # We deliberately do NOT add column 2 to columns dict so that "empty
    # cells" reporting still flags it as TODO. (But we keep the node.)

    # ---- Columns 3..11: exam questions ----
    for col_idx in sorted(columns.keys()):
        items = columns[col_idx]
        if not items:
            continue
        x = COL_X[col_idx - 1]
        cy = TOP_Y
        for ex_id in items:
            d = past_exams_ts[ex_id]
            converted_content = md_tables_to_latex(code_blocks_to_inline(d["content"]))
            n_node = node(ex_id, d["title"], converted_content,
                          x, cy, color, w=SNIPPET_W, h=H(d),
                          links=[theory_id], images=d.get("images", []))
            n_node["column"] = col_idx
            n_node["isExam"] = True
            nodes_in_subtopic.append(n_node)
            cy += H(d) + SNIPPET_GAP

    # Identify empty exam-column cells for the report (cols 3..11 not in row)
    for c in range(3, 12):
        if c not in columns:
            empty_cells.append((sid, c))

    topics_out[tid]["subtopics"].append({
        "id": sid, "name": sname, "nodes": nodes_in_subtopic,
    })
    total_nodes_count += len(nodes_in_subtopic)


# ---------------------------------------------------------------------
# Coalesce into the existing top-level shape. We replace the FIRST topic
# (the legacy "Lecture 1 & Stationarity" canvas) with the 13 new sub-
# topic-row topics. Any other topics that existed are preserved as-is.
# ---------------------------------------------------------------------
topic_id_order = [t["id"] for t in TOPICS]
topics_list = [topics_out[tid] for tid in topic_id_order if tid in topics_out]

# Wrap rows under a single virtual "topic 0" container the way the
# Statistics table renderer expects (data.topics[0] holds all rows). We
# keep the rows split across topics list so the table groups them, but
# the renderer reads ALL topics' subtopics as rows.

output = {
    "version": existing_data.get("version", "2.0"),
    "exportedAt": int(time.time() * 1000),
    "data": {
        "topics": topics_list,
        "trash": existing_data.get("data", {}).get("trash", []),
        "tableLayout": {
            "subject": "time-series",
            "columns": COLUMN_HEADERS,
        },
    },
}

# Sanity: every existing canvas node must appear in at least one Col-1
# theory aggregation. Compute the set of titles we absorbed and compare.
absorbed_set = set()
for v in absorbed_titles_per_sid.values():
    absorbed_set.update(v)
not_absorbed = [t for t in existing_by_title.keys() if t not in absorbed_set]
if not_absorbed:
    print("[WARN] These canvas-node titles ended up in NO Col-1 cell:")
    for t in not_absorbed:
        print(f"   - {t!r}")

# Write output
with open(OUT, "w", encoding="utf-8") as f:
    json.dump(output, f, ensure_ascii=False, indent=2)

# Post-write parse-back check
with open(OUT, "r", encoding="utf-8") as f:
    _raw = f.read()
import re as _re
if _re.search(r"^(?:<{7} |={7}$|>{7} )", _raw, _re.M):
    raise SystemExit(f"build_snippets_ts: {OUT} contains merge-conflict markers — aborting")
try:
    _check = json.loads(_raw)
    assert _check.get("data", {}).get("topics"), "no topics array"
    assert _check.get("data", {}).get("tableLayout"), "no tableLayout"
except Exception as _e:
    raise SystemExit(f"build_snippets_ts: {OUT} did not round-trip as JSON: {_e}")

# ---------------------------------------------------------------------
# Build report
# ---------------------------------------------------------------------
print(f"Wrote {OUT}")
print(f"Total nodes: {total_nodes_count}")
print(f"Topics (groups): {len(topics_list)}; sub-topic rows: {sum(len(t['subtopics']) for t in topics_list)}")
print(f"Columns: {len(COLUMN_HEADERS)}")
print()
print("=== Row coverage (cols 3..11 = exam sittings) ===")
rows = 0; filled_cells = 0; total_exam_cells = 0
for t in topics_list:
    print(f"--- {t['name']} ---")
    for s in t["subtopics"]:
        cols_used = sorted({n.get("column", 1) for n in s["nodes"]})
        exam_cols_used = [c for c in cols_used if 3 <= c <= 11]
        n_exam_qs = sum(1 for n in s["nodes"] if n.get("isExam"))
        print(f"  {s['id']:6s} {s['name'][:55]:55s} "
              f"nodes={len(s['nodes']):2d}  cols={cols_used}  exams={n_exam_qs}")
        rows += 1
        filled_cells += len(exam_cols_used)
        total_exam_cells += 9
print()
print(f"Row × col exam cells filled: {filled_cells}/{rows*9}  "
      f"(theory: {rows}/{rows} col-1 cells; master: {rows} TODO placeholders in col-2)")
print()
print("=== Canvas-node absorption (existing -> Col-1) ===")
for sid, titles in absorbed_titles_per_sid.items():
    if titles:
        print(f"  {sid}: {titles}")
print()
unmapped = [t for t in existing_by_title.keys() if t not in absorbed_set]
print(f"Canvas titles NOT absorbed into any sub-topic Col-1: {len(unmapped)}")
for t in unmapped:
    print(f"   - {t!r}")
