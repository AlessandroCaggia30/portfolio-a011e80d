"""
Build data_statistics.json — Statistics subject for MindNotes.

Structure (rev 2026-06-05):
  TOPICS  →  SUBTOPICS  →  CANVAS NODES (theory + exercises in columns)
  G1 Plotting     → Pie / Bar / Histogram / Spike / Cumulative
  G2 Proportions  → Exact / Uniform-on-interval
  G3 Derived vars → single subtopic
  G4 Central tendency → By-type / Mean-vs-median skew / Approx-grouped / Subgroup-compare

Each subtopic canvas has col 1 = focused theory snippet, col 2+ = exercises (one column per
full exercise from any exam set, sub-parts stacked vertically inside the column).
"""
import json, os, re, time

HERE = os.path.dirname(os.path.abspath(__file__))
OUT  = os.path.join(os.path.dirname(HERE), "data_statistics.json")


# ---------------------------------------------------------------------
# Markdown pipe-table → LaTeX \begin{tabular}{...} converter
# (The MindNotes renderer handles \begin{tabular} but not markdown pipe
# tables. Widths sum to 38 cm per the project's rule for snippet pages.)
# ---------------------------------------------------------------------
def _md_cell_to_latex(s):
    s = s.strip()
    # **bold**  →  \textbf{bold}
    s = re.sub(r"\*\*(.+?)\*\*", r"\\textbf{\1}", s)
    # *italic*  →  \textit{italic}    (avoid double-star already handled)
    s = re.sub(r"(?<!\*)\*([^*\n]+)\*(?!\*)", r"\\textit{\1}", s)
    return s


# ---------------------------------------------------------------------
# Fenced code-block (```lang … ```) → inline-backtick-per-line.
# The MindNotes renderer only handles INLINE code (single backticks →
# <code>), not fenced blocks — triple backticks were rendering as
# literal characters. Strategy: strip the fences, wrap each non-empty
# line in inline backticks (so each line becomes a <code> element on
# its own paragraph).
# ---------------------------------------------------------------------
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
    # Marker pair survives escapeHtml in the renderer; the renderer then
    # collapses everything between %%RBLOCK%% / %%/RBLOCK%% into a single
    # unified <pre class="stats-r-block"> so the R command and ## output
    # lines render as one continuous code-console block (no pill gaps).
    return "\n%%RBLOCK%%\n" + "\n".join(out_lines) + "\n%%/RBLOCK%%\n"

def code_blocks_to_inline(text):
    text = _FENCE_RE.sub(_fence_replacer, text)
    return _wrap_consecutive_inline_code_runs(text)


def _wrap_consecutive_inline_code_runs(text):
    """Wrap a run of 2+ consecutive lines that are entirely a single inline
    code span (`...`) into a %%RBLOCK%%...%%/RBLOCK%% pair so the renderer
    can collapse them into a single unified code/console box. Skips text
    already inside an existing %%RBLOCK%% block."""
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
            # Accept blank lines between inline-code lines as part of the run
            while j < len(lines):
                if inline_only.match(lines[j]):
                    run.append(lines[j])
                    j += 1
                elif lines[j].strip() == "" and j + 1 < len(lines) and inline_only.match(lines[j + 1]):
                    run.append("")
                    j += 1
                else:
                    break
            # Strip trailing blanks within the captured run
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


def md_tables_to_latex(text):
    """Find markdown pipe tables in `text` and replace each with a LaTeX tabular."""
    lines = text.split("\n")
    out_lines = []
    i = 0
    n = len(lines)
    pipe_row = re.compile(r"^\s*\|.*\|\s*$")
    sep_row  = re.compile(r"^\s*\|[\s\-:|]+\|\s*$")
    while i < n:
        if i + 1 < n and pipe_row.match(lines[i]) and sep_row.match(lines[i + 1]):
            header = [c.strip() for c in lines[i].strip().strip("|").split("|")]
            sep    = [c.strip() for c in lines[i + 1].strip().strip("|").split("|")]
            j = i + 2
            body = []
            while j < n and pipe_row.match(lines[j]):
                body.append([c.strip() for c in lines[j].strip().strip("|").split("|")])
                j += 1
            ncols = len(header)
            # Width per column: 38 cm divided evenly (rule from project memory).
            w = round(38.0 / ncols, 2)
            col_spec = "|".join([f"p{{{w}cm}}"] * ncols)
            out_lines.append(f"\\begin{{tabular}}{{{col_spec}}}")
            out_lines.append("\\hline")
            out_lines.append(" & ".join(_md_cell_to_latex(c) for c in header) + " \\\\")
            out_lines.append("\\hline")
            for row in body:
                # Pad short rows to header length (defensive)
                row = row + [""] * (ncols - len(row))
                out_lines.append(" & ".join(_md_cell_to_latex(c) for c in row[:ncols]) + " \\\\")
            out_lines.append("\\hline")
            out_lines.append("\\end{tabular}")
            i = j
        else:
            out_lines.append(lines[i])
            i += 1
    return "\n".join(out_lines)

COL_X = [200 + 800*i for i in range(100)]  # supports columns 1..100
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

# =====================================================================
# FOCUSED THEORY SNIPPETS — one per subtopic
# =====================================================================

T_G1A_PIE = """## Pie chart

A **pie chart** displays the **relative frequencies** of a **qualitative variable** as proportional slices of a disk. The angle (equivalently, the area) of each slice equals the share of the corresponding category in the sample.

### Definition
Given a qualitative variable $X$ with categories $c_1,\\dots,c_K$ and absolute frequencies $n_1,\\dots,n_K$ with $n=\\sum_k n_k$, the slice for category $c_k$ has:
$$
\\text{angle}_k = 2\\pi\\,\\frac{n_k}{n}, \\qquad \\text{area}_k \\propto \\frac{n_k}{n} = f_k .
$$
Because slices depend only on the *shares* $f_k$, the pie looks **identical** whether you pass absolute counts, relative frequencies, or percentages.

![Theory illustration](statistics/images/theory/th_g1a_ai.png)

### When to use
- Variable is **qualitative** (categorical).
- Especially well-suited to **nominal** variables — the disk has no axis, so no ordering is implied.
- **Few categories** (rule of thumb $K\\le 5$): humans compare angles poorly, so too many slices become unreadable.
- The goal is to show **relative share** of each class, not absolute counts.

### When NOT to use
- **Ordinal** variables — the natural order of categories is lost on a disk. Prefer a **bar plot**, which keeps categories on an ordered x-axis.
- **Many categories** ($K>5$) — slices become small and indistinguishable; a bar plot is more readable.
- **Numerical** variables (continuous or discrete) — use a **histogram** or a **spike plot** instead.

### Reading a pie chart
- Two pies are comparable only after fixing the same legend / category ordering; the absolute sample sizes $n$ are not visible from the pie alone (state $n$ in the caption).
- Slice **labels** should report the percentage (and optionally the count) — otherwise small slices are hard to rank.

**R commands:**
```r
distr.plot.x(x=Var, plot.type="pie", freq="perc", data=DF)
distr.table.x(x=Var, freq=c("count","perc"), data=DF)
```
"""

T_G1B_BAR = """## Bar plot

A **bar plot** uses bars of **equal width** whose **height** is proportional to the (absolute, relative, or percentage) frequency of each category. The categorical variable goes on the x-axis; the frequency on the y-axis.

**When to use:**
- **Qualitative** variables, especially **ordinal** ones — categories are plotted along the x-axis in their natural order, allowing direct visual comparison of magnitudes.
- **Nominal** variables when you want to compare frequencies precisely (the disk shape of a pie chart makes magnitude comparison harder).
- **Discrete numerical** variables only when consecutive integer values are present with no gaps and uniform spacing (otherwise use a **spike plot**).

![Theory illustration](statistics/images/theory/th_g1b_ai.png)

**How to read it:**
- Each bar = one category. Bar **width** is fixed and meaningless; only **height** encodes information.
- The y-axis can be **counts** $n_i$, **relative frequencies** $f_i = n_i/n$, or **percentages** $100 \\cdot f_i$ — the *shape* of the chart is identical; only the y-axis label and scale change.

**Cautions:**
- For **nominal** variables, R orders bars **alphabetically by default** — do not draw conclusions from bar position; only heights are interpretable.
- For **ordinal** variables, define the factor explicitly with `levels=` so categories appear in the correct sequence (e.g. `None < Low < Medium < High`).
- Do **not** use a bar plot for a **continuous** variable (use a histogram) or for a discrete numerical variable with gaps (use a spike plot).
- Keep the y-axis starting at **zero**; truncated axes exaggerate differences.

**R commands:**
```r
distr.plot.x(x=Var, plot.type="bars", data=DF)
# Ordering an ordinal variable first:
DF$Var_recode <- factor(DF$Var, levels=c("None","Low","Medium","High"))
distr.plot.x(x=DF$Var_recode, plot.type="bars")
# Percentage instead of count on the y-axis:
distr.plot.x(x=Var, plot.type="bars", freq="perc", data=DF)
```
"""

T_G1C_HIST = """## Histogram (continuous variables)

A **histogram** represents the distribution of a **continuous numerical** variable. Each bar spans a class interval $[a_i, b_i)$, and its **area** — not its height — encodes information. The defining invariant is:
$$
\\text{area}_i \\;=\\; d_i \\cdot w_i \\;=\\; f_i, \\qquad \\sum_i f_i = 1,
$$
where $w_i = b_i - a_i$ is the class width, $d_i$ is the density on the y-axis, and $f_i$ is the relative frequency.

### Equal-width classes
With a constant width $w$, plotting **counts** $n_i$ or **frequencies** $f_i$ on the y-axis is fine: every bar shares the same width, so bar areas are proportional to bar heights and the visual ordering is preserved.

### Unequal-width classes — the density rule (compulsory)
Whenever class widths differ ($w_i \\ne w_j$), the y-axis **must** be **density**:
$$
d_i = \\frac{f_i}{w_i}.
$$
Without this correction, wider classes look more important than they are — the *area-as-frequency* principle is what makes a histogram honest across bins of different sizes.

The figure below contrasts the two choices on the same dataset with unequal widths. The left panel plots **counts**: the wide tail bar visually competes with the bulk despite holding only 12% of the sample. The right panel plots **density**: each bar's area equals its relative frequency $f_i$, restoring honest proportions (annotation shows $d_3 \\cdot w_3 = 0.25$).

### Choice of number / boundaries of classes
- **Too few classes** (e.g. 5) hide structure — peaks, skewness, multimodality and tail behaviour all blur together.
- **Too many classes** (e.g. 20–30) introduce sampling noise, especially in sparse tails where each bar carries only one or two observations.
- A **custom binning** — narrow widths in the bulk, broad widths in the tail — is often the best compromise. The moment widths become unequal, density on the y-axis is mandatory.

### Comparing distributions across subgroups
Plot the histograms on the **same x-axis** and use **densities** (not counts). This way subsamples of unequal size become visually comparable, since areas integrate to 1 in both groups regardless of their absolute sample sizes.

**R commands:**
```r
distr.plot.x(x=Var, plot.type="hist", breaks=15, data=DF)
distr.plot.x(x=Var, plot.type="hist", breaks=c(0,10,20,40,60,100), data=DF)
distr.plot.x(x=Var[group=="A"], plot.type="hist", breaks=B, data=DF)
distr.plot.x(x=Var[group=="B"], plot.type="hist", breaks=B, data=DF)
```
"""

T_G1D_SPIKE = """## Spike plot (discrete numerical variables)

A **spike plot** ("spike diagram" / "lollipop") represents the distribution of a **discrete numerical** variable by drawing a **zero-width vertical stick** at each observed value $x_i$ on a **numerical axis**, with **height** proportional to the absolute or relative frequency $n_i$ (or $f_i$).

### Definition
Given a discrete numerical variable $X$ with observed values $x_1 < x_2 < \\dots < x_K$ and frequencies $n_k$ (or $f_k = n_k/n$), the spike at $x_k$ has:
$$
\\text{position}_k = x_k \\;\\;\\text{on the numerical axis}, \\qquad \\text{height}_k = n_k \\;\\;\\text{(or }f_k\\text{)}.
$$
Two features distinguish the spike plot from a bar plot:
1. The x-axis is **numerical**, not categorical — distances between values are real distances.
2. Sticks have **zero width** — there is no claim of mass between consecutive integers.

![Theory illustration](statistics/images/theory/th_g1d_ai.png)

### Why not a histogram or a bar plot?
- A **histogram** is wrong for a discrete numerical variable: values strictly between two consecutive integers are impossible, so a bar of positive width would *visually claim* mass that does not exist (see the right panel above — the bar covering $[2, 3)$ wrongly suggests values like $2.3$ occur).
- A **bar plot** treats categories as equally spaced and orderable but not metric: it cannot show a **gap** where a value is absent (e.g. nobody has exactly 5 children). The spike plot keeps the integer spacing, so a missing value is visible as empty space on the axis.
- The spike plot honours both constraints: **zero-width sticks** at the actual values, and **empty gaps** for values with zero frequency.

### When to use
- **Discrete numerical** variables: counts of children, number of visits, number of rooms, etc.
- Whenever the *spacing* between values is meaningful (one unit apart, two units apart, etc.).
- Both for absolute frequencies (counts) and relative frequencies / percentages — only the y-axis label changes.

### When a bar plot is an acceptable substitute
Only when **all consecutive integer values are present with no gaps** and the spacing is uniform. In that case sticks and equal-width bars carry the same information. If even one integer is missing in the observed range, use a spike plot to preserve the gap.

### Reading a spike plot
- The y-axis can be **counts** $n_i$ or **relative frequencies** $f_i$; the shape is identical — only the scale changes.
- An empty integer position with no stick means **zero observations** at that value (it is *not* a missing category, it is a meaningful zero).
- The **mode** is the value with the tallest stick; **symmetry / skewness** are read from the stick profile just as with a histogram.

**R commands:**
```r
distr.plot.x(x=Var, plot.type="spike", freq=c("counts"), data=DF)
distr.plot.x(x=Var, plot.type="spike", freq=c("perc"),  data=DF)
distr.table.x(x=Var, freq=c("counts","perc"), data=DF)
```
"""

T_G1E_CUM = """## Cumulative plots: ogive and step diagram

The **cumulative distribution** $F(x)$ at a point $x$ is the proportion of observations $\\le x$:
$$
F(x) \\;=\\; \\frac{\\#\\{i : x_i \\le x\\}}{n} \\;=\\; \\sum_{x_i \\le x} f_i.
$$
$F$ is non-decreasing, starts at $0$ and ends at $1$. Its graphical form depends on whether the variable is **discrete** (only jumps) or **continuous in classes** (piecewise linear).

### Ogive — continuous variable presented in classes
Plot the points $(b_i, F_i)$ where $b_i$ is the upper bound of class $i$ and $F_i$ the cumulative relative frequency at that bound, then connect with **straight lines** starting at $(a_1, 0)$. Between two kinks the curve is straight, and its slope on class $i$ equals the **density** $d_i = f_i / w_i$ (under the implicit **uniform-on-interval** assumption that the $f_i$ mass is evenly spread across $[a_i, b_i)$).

**Quantile read-off.** Find the class $i$ where $F_{i-1} \\le q < F_i$, then interpolate linearly:
$$
p_q \\;\\approx\\; a_i + \\frac{q - F_{i-1}}{d_i}
\\;=\\; a_i + \\frac{q - F_{i-1}}{F_i - F_{i-1}}\\,(b_i - a_i), \\qquad q = 0.5\\text{ for the median}.
$$

### Step diagram — discrete numerical variable
$F$ is **constant between consecutive observed values** and **jumps by $f_i$** at each observed value $x_i$ (no interpolation). The curve has horizontal flats and vertical jumps; a closed dot marks the value of $F$ at each $x_i$ (right-continuous convention).

### Illustration (image below)

- **Left panel — step diagram** (discrete family-size data): each jump equals the relative frequency $f_i$ of value $x_i$; flats in between, closed dots at the top of each jump (right-continuous convention).
- **Right panel — ogive** (continuous time-in-class data): slope on each segment is the density $d_i = f_i / w_i$; the dashed yellow guides show the **median read-off** at $F = 0.5$ — drop a horizontal at $F=0.5$, intersect the ogive, then drop a vertical to the x-axis.

**R commands:**
```r
distr.plot.x(x=Var, plot.type="cumfreq", data=DF)    # ogive (continuous) or step (discrete)
distr.table.x(x=Var, freq="cum", data=DF)            # numerical cumulative table
```
"""

T_G2A_EXACT = """## Exact proportions from frequency tables

When a frequency table is given, computing the proportion of cases in a query subset is **exact** in two situations only:

1. **Discrete** or **categorical** variable: just sum the relative frequencies of the values / categories in the query set,
$$
P(X \\in A) = \\sum_{k:\\, c_k \\in A} f_k .
$$
2. **Continuous** variable grouped in classes, **and** the query interval $[L, U)$ is a **union of full classes** — i.e. $L$ and $U$ coincide with class boundaries. Then
$$
P(L \\le X < U) = \\sum_{i:\\,[a_i,b_i)\\subseteq[L,U)} f_i .
$$

In any other case (continuous variable with $L$ or $U$ falling **inside** a class), the table alone is not enough — see uniform-on-interval (G2.B).

![Theory illustration](statistics/images/theory/th_g2a_ai.png)

### Why exact?
A frequency table gives us the count in each full class **exactly**. Summing whole-class frequencies is just arithmetic on known counts — no assumption is made about how cases are distributed *inside* a class. The moment we need a slice that does not align with class boundaries, we must interpolate, and the answer becomes approximate.

### Computational shortcuts from raw data
When the raw data are available (not just the table), two equivalent routes:
```r
# 1) Boolean vector + mean -- counts TRUEs / n
mean(DF$Var >= L & DF$Var < U)
# 2) Read from the cumulative distribution table
distr.table.x(x=Var, freq="cum", data=DF)
```

### Cumulative table primitives
```r
distr.table.x(x=Var, freq=c("counts","prop","cum"), data=DF)
```
The cumulative proportion $F_i = F(b_i)$ at the boundary $b_i$ is the `Cum.Prop` column. For a union-of-classes interval $[L, U) = [b_j, b_k)$,
$$
P(L \\le X < U) = F(b_k) - F(b_j) ,
$$
which is the standard CDF-difference identity (no interpolation involved).

### Worked numerical example (mirrors the figure)
Classes $[0,10),\\,[10,20),\\,[20,40),\\,[40,60),\\,[60,100)$ with $f = (0.08,\\,0.20,\\,0.32,\\,0.22,\\,0.18)$. Query $[20, 60)$ aligns with boundaries, so
$$
P(20 \\le X < 60) = f_{[20,40)} + f_{[40,60)} = 0.32 + 0.22 = 0.54 \\quad (\\text{exact}).
$$

> **Always report whether your answer is exact or approximate.** Exact = the query is a union of full classes (or you are dealing with a discrete / categorical variable).
"""

T_G2B_APPROX = """## Approximate proportions: uniform-on-interval

When the query interval $[L, U)$ has endpoints that fall **inside** a class $[a_i, b_i)$ (not at a class boundary), the count in $[L, U) \\cap [a_i, b_i)$ is **unknown** from the table alone. The standard assumption is:

> **Uniform on the interval:** the $f_i$ units of mass are evenly distributed across $[a_i, b_i)$, so the density is constant and equal to $d_i = f_i / w_i$.

### Two equivalent views

**(1) Histogram view — density × overlap.**
$$
P\\big(X \\in [L, U) \\cap [a_i, b_i)\\big) \\;\\approx\\; d_i \\cdot \\text{length}\\big([L, U) \\cap [a_i, b_i)\\big) \\;=\\; f_i \\cdot \\frac{\\text{overlap}}{w_i}.
$$

**(2) Cumulative (ogive) view — linear interpolation of $F$ inside the class.**
$$
F(x) \\;\\approx\\; F(a_i) + \\frac{x - a_i}{b_i - a_i}\\,\\big(F(b_i) - F(a_i)\\big), \\qquad x \\in [a_i, b_i),
$$
so for a query in $[L, U) \\subset [a_i, b_i)$ one reads $P([L,U)) \\approx F(U) - F(L)$ off the **straight segment** of the ogive.

The two views are the **same assumption**: constant density on $[a_i, b_i)$ $\\Leftrightarrow$ linear $F$ on $[a_i, b_i)$.

![Theory illustration](statistics/images/theory/th_g2b_ai.png)

### Working recipe
Split the query into **full classes** $\\cup$ **partial classes**; for each full class add $f_i$; for each partial piece apply density $\\times$ overlap; sum.

**Example.** $P(15 \\le \\text{Time} < 50)$ when classes are $[10,20),\\,[20,30),\\,[30,60)$:
$$
\\underbrace{\\tfrac{1}{2}\\cdot f_{[10,20)}}_{\\text{half of }[10,20)} \\;+\\; \\underbrace{f_{[20,30)}}_{\\text{full}} \\;+\\; \\underbrace{\\tfrac{20}{30}\\cdot f_{[30,60)}}_{\\text{2/3 of }[30,60)}.
$$

### Edge cases & sanity checks
- If $L$ **and** $U$ are both class boundaries, the result is **exact** — no interpolation is used (see Theory — Exact proportions).
- Reliable when each class is narrow enough that the within-class shape is nearly uniform; for very wide or strongly skewed classes the error grows.
- Each partial coefficient lies in $[0, 1]$; the total cannot exceed $1$.

**R checks:** the source-of-truth `distr.table.x` gives the table; the arithmetic above is then done by hand or verified in R.

```r
distr.table.x(x=Var, breaks=brks, data=DF)
```

> Always **report whether your answer is exact or approximate**, and which assumption was used.
"""

T_G3_DERIVED = """## Constructing derived variables for meaningful comparison

Raw counts almost always **mislead** when compared across units of different sizes — a city with 10 000 burglaries is not necessarily more dangerous than one with 1 000 if it has 100× the population. The fix is to **normalize**: build a *derived variable* whose values are comparable across units.

![Theory illustration](statistics/images/theory/th_g3_ai.png)

### Rate = count / exposure (× scale)
$$
\\text{Rate} = \\frac{\\text{count}}{\\text{exposure}} \\cdot k.
$$
- **Count** = the raw number of events for the unit (crimes, cases, sales, ...).
- **Exposure** = the substantive denominator — the quantity *at risk* of producing the count (population, area, person-years, etc.).
- **Scale $k$** = chosen so the rate is on a human-readable range (e.g. *"per 100 000 inhabitants"* → $k = 10^5$).

The hard part is choosing the **denominator**; the scale $k$ is just cosmetic.

| Phenomenon | Substantive denominator | Resulting derived variable |
|------------|------------------------|----------------------------|
| Crime / property crime | Population | Crime rate per 100 k inhabitants |
| Disease incidence | At-risk population (or person-years) | Incidence rate |
| Population concentration | Area (km$^2$) | Population density |
| Births / deaths | Total population | Crude birth / death rate |

> **Worked example.** Region $A$: 8.5 M people, 170 000 km$^2$ → density $= 50$ /km$^2$. Region $D$: 1.4 M people, 3 500 km$^2$ → density $= 400$ /km$^2$. Raw counts put $A$ ahead 6×; the derived variable density flips the ranking and reveals $D$ is **8× more crowded**.

### Categorical splits from a continuous variable
A second kind of derived variable: *bin* a continuous variable at a meaningful threshold to compare distributions across groups. The question shifts from "what is the distribution of $X$?" to "do high-$X$ and low-$X$ units behave differently on $Y$?".

Recipe:
1. Choose a threshold $\\tau$ on substantive grounds (e.g. Density $\\ge 100$ = "urbanized").
2. Build `Group <- ifelse(X >= τ, "Hi", "Lo")`.
3. Compare the distributions of the response on the two subsets (histogram, summary, boxplot).

**R commands:**
```r
DF$Rate <- 100000 * (DF$count_var) / DF$Population
DF$Density <- DF$Population / DF$Area
DF$Group <- ifelse(DF$Density >= 100, "Hi", "Lo")
distr.plot.x(x=Var, plot.type="hist", data=subset(DF, Group=="Hi"))
distr.plot.x(x=Var, plot.type="hist", data=subset(DF, Group=="Lo"))
distr.summary.x(x=Var, stats="summary", data=subset(DF, Group=="Hi"))
```

### Checklist before reporting a derived variable
- **Denominator is substantively right** — what is actually at risk?
- **Denominator is non-zero for every unit** (otherwise the derived value is undefined).
- **Scale $k$ is documented** ("per 100 k", "per km$^2$", etc.).
- **New variable is comparable across units** — same definition everywhere.
- **State explicitly** whether subsequent statistics are computed on the *raw* or the *derived* variable.
"""

T_G4A_BYTYPE = """## Mode, median, mean — choosing by variable type

The choice of central-tendency measure is **dictated by the variable's scale of measurement**. Each measure requires a stronger mathematical structure than the previous one: only frequencies for the mode, an order for the median, and a meaningful notion of distance for the mean.

### Decision table

| Variable type | Mode | Median | Mean |
|---------------|:----:|:------:|:----:|
| Nominal qualitative | ✔ | ✘ | ✘ |
| Ordinal qualitative | ✔ | ✔ | ✘ |
| Discrete numerical  | ✔ | ✔ | ✔ |
| Continuous numerical | (modal *class*) | ✔ | ✔ |

![Theory illustration](statistics/images/theory/th_g4a_ai.png)

### The three measures — definition and minimum structure required

- **Mode** = the value (or category) with the highest frequency. Needs only a frequency table, so it works for **any** variable type — including pure labels like *colour* or *brand*. Caveat: when the modal share is small (e.g. 30–35%) the mode is *not* a representative summary of the distribution; report it together with its share.
- **Median** = the value that splits the ordered distribution into two halves of equal weight (the $0.50$ quantile). Requires an **order** on the values, so it works from **ordinal** variables onward. For an even sample size on numerical data the median is conventionally taken as the average of the two central order statistics.
- **Mean** = $\\bar x = \\dfrac{1}{n}\\sum_{i=1}^{n} x_i = \\sum_k f_k \\cdot x_k$ (the second form when the data are summarised by a frequency table over distinct values $x_k$). Requires that **distances** between values be meaningful — so it works only on **numerical** variables (interval or ratio scale). Computing a mean of ordinal codes like *low=1, medium=2, high=3* is misleading because the gap between codes has no metric meaning.

### Modal class (continuous variable in classes with **unequal widths**)
For a continuous variable summarised by a frequency table with **unequal class widths**, the modal class is **not** the class with the highest count: it is the class with the **highest density**
$$
d_i = \\frac{f_i}{w_i} = \\frac{\\text{relative frequency}}{\\text{class width}}.
$$
A wider class can collect many observations and still have a low density — it is not the modal class. When all widths are equal, densities are proportional to counts and the two criteria coincide.

### Worked micro-example
- *Brand of phone* (nominal): only the **mode** is meaningful — there is no order, so no median or mean.
- *Satisfaction* on a 5-point scale (ordinal): **mode** and **median** are meaningful; the **mean** of the scores is conventional but not strictly valid.
- *Number of children per household* (discrete numerical): all three measures are valid.
- *Income in classes* $[0,10)$, $[10,20)$, $[20,50)$ thousand euro (continuous, unequal widths): the modal class is the one with the largest **density** $f_i/w_i$, not the largest count.

**R commands:**
```r
distr.summary.x(x=Var, stats="mode", data=DF)
distr.summary.x(x=Var, stats=c("mean","median"), data=DF)
distr.summary.x(x=Var, stats="central tendency measures", data=DF)
```
"""

T_G4B_SKEW = """## Mean vs median under skewness

### The rule of thumb
- **Symmetric** distribution: $\\bar x \\approx \\text{median}$.
- **Right-skewed** (long right tail): $\\bar x > \\text{median}$ — the long right tail pulls the mean **above** the median.
- **Left-skewed** (long left tail): $\\bar x < \\text{median}$ — the long left tail pulls the mean **below** the median.

The mean is a **sum** — every observation contributes its value, so extreme values exert leverage. The median is a **rank** statistic — only the position of values matters, not their magnitude. That is why the median is **robust** to outliers and heavy tails, while the mean is not.

![Theory illustration](statistics/images/theory/th_g4b_ai.png)

### Practical decision: which to report?
- Distribution *concentrated near a central value with a long tail* (income, house prices, durations) → report the **median**: it represents a *typical* case, not pulled by a handful of extremes.
- Distribution *symmetric* with no outliers → either is fine; the mean is slightly more efficient and combines additively across subgroups.

### Diagnosing skewness without a plot
From a numerical summary alone:
- $\\bar x > \\text{median}$ $\\Rightarrow$ **right-skewed**.
- $\\bar x \\approx \\text{median}$ $\\Rightarrow$ **symmetric** (approximately).
- $\\bar x < \\text{median}$ $\\Rightarrow$ **left-skewed**.

The *size* of the gap $\\bar x - \\text{median}$, relative to the spread (e.g. IQR or sd), measures how strong the skew is.

### Quick worked example
For wages $\\{18,\\,20,\\,22,\\,24,\\,26,\\,200\\}$ (thousand euro):
$$\\text{median} = 23, \\qquad \\bar x = 51.7.$$
The single value 200 drags the mean far above the median — **mean > median**, right-skewed, and the median (23) is clearly more representative.

**R commands:**
```r
distr.summary.x(x=Var, stats="summary", digits=2, data=DF)
# Returns: min, q1, median, mean, q3, max, sd, ...
# Compare mean and median to read skewness directly.
```
"""

T_G4C_GROUPED = """## Approximate mean and median from grouped data

When raw data are unavailable and we only have a frequency table (or a histogram), we **approximate** mean and median under the **uniform-on-interval** assumption — the mass of each class is uniformly spread across that class.

### Approximate mean
Take the midpoint $m_i = (a_i + b_i)/2$ as the representative value of class $i$:
$$
\\bar x \\approx \\sum_i f_i \\cdot m_i = \\frac{\\sum_i n_i \\cdot m_i}{n}.
$$

### Approximate median
1. Identify the **median class** $[a_k, b_k)$ — the first class where the cumulative relative frequency reaches $0.50$.
2. Inside that class, interpolate linearly under uniform-on-interval:
$$
p_{50} \\approx a_k + \\frac{0.5 - F_{k-1}}{d_k} = a_k + \\frac{0.5 - F_{k-1}}{f_k} \\cdot w_k.
$$

Where $F_{k-1}$ is the cumulative relative frequency at the lower bound of the median class, $f_k$ the median class's relative frequency, $w_k$ its width, $d_k = f_k/w_k$ its density.

![Theory illustration](statistics/images/theory/th_g4c_ai.png)

### Why approximate?
We do not know how the $f_k$ mass is distributed inside the class — the uniform assumption is just the simplest.

**Reading from a histogram:** densities → frequencies (density × width) → use the same recipe.
"""

T_G4D_COMPARE = """## Comparing central tendency across subgroups or periods

![Cross-subgroup comparison of mode, median, mean](statistics/images/theory/th_g4d_ai.png)

When a population breaks down into meaningful subgroups (e.g. customers with Time < 30 vs > 30 minutes; survey years 2015–2016 vs 2022–2023), compute **mode, median, and mean for each subgroup** and compare.

### Subgroup recipe
1. Split the dataset by the grouping variable.
2. Within each subgroup, build the frequency / cumulative table.
3. Compute mode / median / mean (exact if raw data are available; approximate otherwise — see grouped-data theory).

### Interpretation
- A shift in **mode** signals a change in the most frequent value.
- A shift in **median** signals a change in the typical value.
- A shift in **mean** without a corresponding shift in median signals a change in the **tails** (extreme cases) — heavy-tail vs light-tail.

### Modal class with unequal-width intervals
Always compare **densities** $d_i = f_i/w_i$, not counts: a wide class may have many cases but a low density, and therefore *not* be the modal class.

**R commands:**
```r
distr.summary.x(x=Var, stats="summary", data=subset(DF, Time<=30))
distr.summary.x(x=Var, stats="summary", data=subset(DF, Time> 30))
```
"""


# =====================================================================
# EX 0 — exercise contents (from earlier session, slightly trimmed)
# =====================================================================

EX1_HEADER = ("The dataframe `Data_USA` in `Exe0_Data.Rdata` contains data on the states of "
              "the USA — Population, Area (sq mi), Frost (interval classes), property crimes, "
              "violent crimes, and an ordinal Happyness.Level.")

ex0 = {}

ex0["ex1a"] = {"title": "Ex 0 / 1a — Plot for Frost (unequal widths)",
"content": """**Question.** Would you use a barplot to display the frequency distribution of the variable `Frost`? Why? What plot would you use?

*(Context: """ + EX1_HEADER + """)*

---

**Answer.** A barplot is **inappropriate** because the classes of `Frost` have **different widths**. The visual impression must be carried by **areas, not heights**.

The correct plot is a **histogram with density on the vertical axis** $d_i = f_i/w_i$.

```r
distr.plot.x(x=Frost, plot.type="bars", data=Data_USA)            # WRONG: barplot
distr.plot.x(x=Frost, interval=T, plot.type="hist", data=Data_USA) # CORRECT: density histogram
```

Frost table:

| Frost          | Count | Prop  |
|----------------|------:|------:|
| [0, 60)        | 10    | 0.20  |
| [60, 90)       | 8     | 0.16  |
| [90, 120)      | 8     | 0.16  |
| [120, 150)     | 13    | 0.26  |
| [150, 180)     | 9     | 0.18  |
| [180, 200]     | 2     | 0.04  |
| **TOTAL**      | **50**| **1.00** |

The first class has a relatively high frequency (0.20) but spans the **widest** lower interval (60), so the *density* is only 0.20/60 ≈ 0.0033 — not the highest. The barplot would mislead by hiding the width information.
""",
"images": ["statistics/images/ex1a-frost-bar-vs-hist.png"]}

ex0["ex1b"] = {"title": "Ex 0 / 1b — Build Density variable",
"content": """**Question.** Add to the dataframe a new variable `Density` defined as the ratio between a state's population size and its area.

---

**Answer.**
```r
Data_USA$Density <- Data_USA$Population / Data_USA$Area
```
This creates a new column with units *people per square mile*. From now on per-area comparisons among states can be made on `Density` directly instead of `Population` (which depends on state size).
""",
"images": []}

ex0["ex1c"] = {"title": "Ex 0 / 1c — Choosing class widths for the Density histogram",
"content": """**Question.** To display the distribution of `Density`, would you use 6 intervals of equal width, the intervals $0$-$30,\\,30$-$60,\\,60$-$100,\\,100$-$200,\\,200$-$300,\\,300$-$1200$, or 20 intervals of equal width?

---

**Answer.** The three histograms are reported below:
```r
distr.plot.x(x=Density, plot.type="hist", breaks=6, data=Data_USA)
distr.plot.x(x=Density, plot.type="hist", breaks=c(0,30,60,100,200,300,1200), data=Data_USA)
distr.plot.x(x=Density, plot.type="hist", breaks=20, data=Data_USA)
```

The histogram with **6 classes** does not detail the behaviour of the distribution on the interval including the highest proportion of data; on the other side, the interval with **20 classes** offers a detailed but not particularly useful description of the distribution's right tail. The histogram built with **intervals of different widths** seems suitable: it allows both to describe quite accurately the distribution of the variable between $0$ and $200$, and to simplify the description of the tail using a unique low-density class.

Whenever class widths differ, **density on the y-axis is compulsory**.
""",
"images": ["statistics/images/ex1c-density-hist3.png"]}

ex0["ex1d"] = {"title": "Ex 0 / 1d — Build a rate variable for violent crimes",
"content": """**Question.** Suppose you would like to study the different levels of violent crimes across states. Would you consider one variable in the database or build a new variable? Which?

---

**Answer.** No single variable in the database captures *overall violent crime* per state. We need a **preliminary build**: first sum the four violent-crime counts (assault, murder, rape, robbery), then **divide by population** so states are comparable.

- Raw counts depend on state size — a big state can have many crimes simply because many people live there.
- For **density in histograms**, the variable must also allow fair comparison between states.

To avoid tiny decimals, multiply by **100 000** → rate is *violent crimes per 100 000 inhabitants*:

```r
Data_USA$Rate.Violent <- 100000 *
  (Data_USA$Violent.Assault + Data_USA$Violent.Murder +
   Data_USA$Violent.Rape    + Data_USA$Violent.Robbery) /
  Data_USA$Population
```
""",
"images": []}

ex0["ex1e"] = {"title": "Ex 0 / 1e — Histogram of Rate.Violent",
"content": """**Question.** Describe the distribution of the variable obtained at point d using a suitable plot.

---

**Answer.** `Rate.Violent` is **numeric continuous** → use a **histogram**. Start with a relatively large number of classes (15 or 20) to get a first impression, then simplify with a reduced number of classes of **different widths**:
```r
distr.plot.x(x=Rate.Violent, plot.type="hist", breaks=15, data=Data_USA)
distr.plot.x(x=Rate.Violent, plot.type="hist", breaks=20, data=Data_USA)
distr.plot.x(x=Rate.Violent, plot.type="hist",
             breaks=c(110,200,250,300,350,400,450,500,550,600,1050),
             data=Data_USA)
```
**Distribution:** right-skewed, concentrated between ~200 and ~500, with a thin upper tail reaching ~1000 (a few high-violence states).

**Equal vs unequal widths.** With **equal-width** bins the y-axis can show **counts**. With **unequal widths** the y-axis must show **density** (= count / (n · width)): areas — not heights — represent the proportion of states. Plotting counts with unequal widths is misleading: a wide bin looks tall just because it spans more values.
""",
"images": ["statistics/images/ex1e-rateviolent-hist.png"]}

ex0["ex1f"] = {"title": "Ex 0 / 1f — % states with Property.Rates > 3000",
"content": """**Question.** Add `Property.Rates` (per 100 000 population). What is the percentage of states with rate higher than 3000?

---

**Answer.**
```r
Data_USA$Property.Rates <- 100000 *
  (Data_USA$Property.Burglary + Data_USA$Property.Larceny +
   Data_USA$Property.Motor) / Data_USA$Population

is.higher30 <- (Data_USA$Property.Rates > 3000)
distr.table.x(is.higher30, freq=c("counts","perc","cum"), p.digits=2)
##  is.higher30  Count  Percent  Cum.Count  Cum.Percent
##  FALSE         48     94.12     48        94.12
##  TRUE           3      5.88     51       100.00
##  TOTAL         51    100.00

# alternative
mean(is.higher30)              # [1] 0.05882353
sum(is.higher30)               # [1] 3
100*sum(is.higher30)/length(is.higher30)  # [1] 5.882353
```

**Result: 3 of 51 states ≈ 5.88%**. (Computation is **exact** — discrete count over a known total.)
""",
"images": ["statistics/images/ex1f-propertyrates-hist.png"]}

ex0["ex1g"] = {"title": "Ex 0 / 1g — Approx proportion Frost < 80 (uniform-on-interval)",
"content": """**Question.** What is the proportion of states with `Frost` < 80?

---

**Answer.** Since `Frost` is measured in classes, this proportion can only be **approximately** determined, assuming the frequency in each interval is **uniformly distributed** on the interval.
```r
distr.table.x(x=Frost, freq=c("counts","prop","dens","cum"), data=Data_USA)
```
```
##           Frost Count Prop Cum.Count Cum.Prop
##         [0,60)    10 0.20        10     0.20
##        [60,90)     8 0.16        18     0.36
##       [90,120)     8 0.16        26     0.52
##      [120,150)    13 0.26        39     0.78
##      [150,180)     9 0.18        48     0.96
##      [180,200)     2 0.04        50     1.00
##          TOTAL    50 1.00
```

Sum the frequency of $[0,60)$ and the **portion** of $[60,90)$ attributed to $[60,80)$ under uniform-on-interval. Class $[60,90)$ has proportion 0.16 and width 30; 80 lies $20/30 = 2/3$ of the way through, so the attributed share is $(2/3)\\cdot 0.16$.

$$
P(\\text{Frost} < 80) \\approx 0.20 + 0.16 \\cdot \\frac{2}{3} = 0.3066667.
$$
```r
0.20 + 0.16 * 2/3
## [1] 0.3066667
(10 + 8 * 2/3) / 50
## [1] 0.3066667
```

*Note: rounding may yield slightly different results; at the exam, state the procedure you followed.*
""",
"images": []}

ex0["ex1h"] = {"title": "Ex 0 / 1h — Cumulative for Happyness.Level (ordinal)",
"content": """**Question.** The variable `Happyness.Level` is a factor and its levels have been properly ordered. Obtain the frequency distribution of `Happyness.Level`. What % of citizens are *happiest or quite happy*? What is the frequency cumulated at *So and so*?

---

**Answer.** Tabulate with `freq="cum"`:
```r
distr.table.x(x=Happyness.Level, freq="cum", data=Data_USA)
##  Happyness.Level  Count   Prop   Cum.Count  Cum.Prop
##  Unhappiest         10   0.2041     10       0.2041
##  Quite unhappy      12   0.2449     22       0.4490
##  So and so          12   0.2449     34       0.6939
##  Quite happy         8   0.1633     42       0.8571
##  Happiest            7   0.1429     49       1.0000
##  TOTAL              49   1.0000
```
*(Two states have NA in `Happyness.Level`, so N = 49 not 51.)*
The % *happiest or quite happy* is the sum of the two top relative frequencies (equivalently, sum of absolute frequencies / N, ×100):
```r
0.1429 + 0.1633        # 0.3062
(8 + 7) / 49           # 0.3061224
```
The proportion cumulated at *So and so* is **0.6939** (the value of `Cum.Prop` at that level).

*[At the exam: don't worry about rounding — use the digits you need but explain your procedure.]*
""",
"images": []}

ex0["ex1i"] = {"title": "Ex 0 / 1i — Compare Rate.murders histograms by Density",
"content": """**Question.** Build `Rate.murders` (per 100 000), extract Density<100 and Density≥100 subgroups, plot histograms with breaks 0, 2, 4, 8, 12. Comment on differences.

---

**Answer.**
```r
Rate.murders <- 100000 * Data_USA$Violent.Murder / Data_USA$Population
Rate.murders_lt_100  <- Rate.murders[Data_USA$Density <  100]
Rate.murders_ge_100  <- Rate.murders[Data_USA$Density >= 100]
distr.plot.x(x=Rate.murders_lt_100, plot.type="hist", breaks=c(0,2,4,8,12), data=Data_USA)
distr.plot.x(x=Rate.murders_ge_100, plot.type="hist", breaks=c(0,2,4,8,12), data=Data_USA)
```

Frequency tables:

| Rate.murders | <100 Count | <100 Prop | ≥100 Count | ≥100 Prop |
|--------------|-----------:|----------:|-----------:|----------:|
| [0, 2)       | 5  | 0.21 | 0  | 0.00 |
| [2, 4)       | 9  | 0.38 | 9  | 0.35 |
| [4, 8)       | 5  | 0.21 | 14 | 0.54 |
| [8, 12)      | 5  | 0.21 | 3  | 0.12 |
| **TOTAL**    | 24 | 1.00 | 26 | 1.00 |

Higher-Density (more urbanised) states tend to have **higher murder rates in general** — most mass sits in $[2,4)$ and $[4,8)$ (54% in the middle bin) and none in $[0,2)$. Lower-Density states are more concentrated on the **first intervals** but, interestingly, the **proportion of states in the highest bin $[8,12)$ is larger for lower-Density states** (21% vs 12%). (Use **density**, not counts, when comparing histograms of different sample sizes.)
""",
"images": ["statistics/images/ex1i-rate-murders-split.png"]}

EX2_FARE_TABLE = """*(Fare distribution given in the prompt, fares ≤ 270.)*

| fare interval | density |
|---------------|--------:|
| $[0, 10)$     | 0.03765 |
| $[10, 20)$    | 0.02002 |
| $[20, 30)$    | 0.01580 |
| $[30, 60)$    | 0.00419 |
| $[60, 100)$   | 0.00196 |
| $[100, 180)$  | 0.00044 |
| $[180, 270)$  | 0.00029 |
"""

EX2_FAM = """*(Family-size distribution given in the prompt.)*

| size.family | Count |
|------------:|------:|
| 0           | 891   |
| 1           | 319   |
| 2           | 42    |
| 3           | 20    |
| 4           | 22    |
| 5           | 6     |
| 8           | 9     |
| **TOTAL**   | **1309** |
"""

ex0["ex2a1"] = {"title": "Ex 0 / 2a1 — P(10 ≤ fare < 60) exact",
"content": """**Question.** Can you assess **exactly** the proportion of passengers with fare in $[10, 60)$?

""" + EX2_FARE_TABLE + """

---

**Answer.** Exact, since both endpoints align with class boundaries. Each class contributes density × width:
$$
(20-10)\\cdot 0.02002 + (30-20)\\cdot 0.01580 + (60-30)\\cdot 0.00419 = 0.4839.
$$
```r
(20-10)*0.02002 + (30-20)*0.01580 + (60-30)*0.00419
## [1] 0.4839
```
**Result: 48.39%.**
""",
"images": []}

ex0["ex2a2"] = {"title": "Ex 0 / 2a2 — P(50 ≤ fare < 100) by uniform-on-interval",
"content": """**Question.** Can you assess **exactly** the proportion of passengers who paid a fare $\\ge 50$ and $< 100$ (limiting attention to fares $\\le 270$)? If not, can you obtain at least an approximation of the required frequency? Report your results.

""" + EX2_FARE_TABLE + """

---

**Answer.** Only **approximately**: 50 is a value *within* one of the considered intervals and not an endpoint. We need the frequency of $[60, 100)$ in full, plus the frequency of $[50, 60) \\subset [30, 60)$.

Under the **assumption of uniform distribution** over class $[30, 60)$, the share of that class lying in $[50, 60)$ is $(60-50)/(60-30) = 1/3$ of its frequency (length of full class is 30, length of sub-interval is 10).
$$
\\underbrace{(60-30)\\cdot 0.00419 \\cdot \\tfrac{1}{3}}_{\\text{partial } [50,60)} + \\underbrace{(100-60)\\cdot 0.00196}_{\\text{full } [60,100)} = 0.1203.
$$
```r
(60-30)*0.00419*(1/3) + (100-60)*0.00196
## [1] 0.1203
# or equivalently (density times sub-interval width)
(60-50)*0.00419 + (100-60)*0.00196
## [1] 0.1203
```
**Result: ~12.03%** (approximate).
""",
"images": []}

ex0["ex2a3"] = {"title": "Ex 0 / 2a3 — Ogive of fare",
"content": """**Question.** Sketch the ogive of the fare distribution using the provided classes.

""" + EX2_FARE_TABLE + """

---

**Answer.** Proportions per class (density × width) and their cumulative:

| fare interval | Prop | Cum.Prop |
|---------------|-----:|---------:|
| $[0, 10)$     | 0.3765 | 0.3765 |
| $[10, 20)$    | 0.2002 | 0.5767 |
| $[20, 30)$    | 0.1580 | 0.7347 |
| $[30, 60)$    | 0.1257 | 0.8604 |
| $[60, 100)$   | 0.0784 | 0.9388 |
| $[100, 180)$  | 0.0352 | 0.9740 |
| $[180, 270)$  | 0.0261 | 1.0001 |

**Ogive:** horizontal axis = `fare` (with class boundaries as kinks); vertical axis = cumulative relative frequency. Plot the points $(a_1, 0), (b_1, F_1), (b_2, F_2), \\ldots$ and connect with straight segments. Slope on class $i$ = density $d_i = f_i / w_i$.
""",
"images": ["statistics/images/ex2a3-fare-ogive.png"]}

ex0["ex2b1"] = {"title": "Ex 0 / 2b1 — P(size.family ≥ 4) exact",
"content": """**Question.** What proportion of families had 4 or more components travelling together?

""" + EX2_FAM + """

---

**Answer.** Discrete variable → exact:
$$
\\frac{22 + 6 + 9}{1309} = \\frac{37}{1309} = 0.02826585.
$$
```r
(22 + 6 + 9) / 1309
## [1] 0.02826585
```
**Result: ~2.83%.**
""",
"images": []}

ex0["ex2b2"] = {"title": "Ex 0 / 2b2 — Spike plot of size.family",
"content": """**Question.** Report a suitable plot to display the relative frequencies of `size.family`.

""" + EX2_FAM + """

---

**Answer.** Discrete numerical → **spike plot** (compulsory: values 6 and 7 don't appear in the data; an honest plot must show that gap).

| size.family | Prop  |
|------------:|------:|
| 0           | 0.681 |
| 1           | 0.244 |
| 2           | 0.032 |
| 3           | 0.015 |
| 4           | 0.017 |
| 5           | 0.005 |
| 8           | 0.007 |
""",
"images": ["statistics/images/ex2b2-size-family-spike.png"]}

ex0["ex2b3"] = {"title": "Ex 0 / 2b3 — Step diagram for size.family",
"content": """**Question.** Sketch a plot for the cumulative frequencies of `size.family`.

""" + EX2_FAM + """

---

**Answer.** Cumulative table:

| size.family | Count | Prop  | Cum.Count | Cum.Prop |
|------------:|------:|------:|----------:|---------:|
| 0           | 891   | 0.681 | 891       | 0.681    |
| 1           | 319   | 0.244 | 1210      | 0.924    |
| 2           | 42    | 0.032 | 1252      | 0.956    |
| 3           | 20    | 0.015 | 1272      | 0.972    |
| 4           | 22    | 0.017 | 1294      | 0.989    |
| 5           | 6     | 0.005 | 1300      | 0.993    |
| 8           | 9     | 0.007 | 1309      | 1.000    |

**Step diagram** (no interpolation): the cumulative proportion is constant between observed values and jumps by $f_i$ at each observed value $x_i$.
""",
"images": ["statistics/images/ex2b3-size-family-step.png"]}


# =====================================================================
# EX 1 — exercise contents
# =====================================================================

ex1 = {}

# ----- 1.1 (pizzerie) -----
ex1["1_1a"] = {"title": "Ex 1.1a — SmokingArea pie chart",
"content": """**Question.** Sample has similar percentages of pizzerias with and without smoking areas (`SmokingArea`). What simple graphical representation can be produced?

---

**Answer.** `SmokingArea` is a **nominal categorical (binary)** variable. The relative-frequency distribution:
```r
distr.table.x(SmokingArea, freq=c("count","perc"), data=pizzerie)
##  SmokingArea Count Percent
##  No           51    51
##  Yes          49    49
##  TOTAL       100   100
```
A **pie chart** displays the relative frequencies; the chart looks identical whether built on frequencies or percentages.
```r
distr.plot.x(SmokingArea, freq="perc", plot.type="pie", data=pizzerie)
```
The sample includes roughly the same percentage of pizzerias with/without smoking area (only 1 pizzeria of difference, since $n=100$).
""",
"images": ["statistics/images/ex1_1a-smoking-pie.png"]}

ex1["1_1b"] = {"title": "Ex 1.1b — District bar plot",
"content": """**Question.** A bar chart represents the frequency distribution of pizzerias in the three areas (`District`). What can you infer? Would it change with absolute frequencies? Is this the only possible representation?

---

**Answer.** The bar chart visualizes the frequency distribution (absolute/relative/percentage) of a qualitative variable. The three categories (Lodi, Milano, Pavia) have approximately the same relevance in the sample. The pattern is the same whether absolute or relative frequencies are used, because they are proportional.

The bar plot is particularly suitable for **ordinal** qualitative variables (categories in their natural order). For **nominal** `District`, the R default order is alphabetical, so do not draw conclusions from bar **position**. A **pie chart** is a possible alternative.
```r
distr.plot.x(District, plot.type="bars", data=pizzerie)
```
""",
"images": ["statistics/images/ex1_1b-district-bar.png"]}

ex1["1_1c"] = {"title": "Ex 1.1c — Plot for Sales",
"content": """**Question.** Which graph to represent the distribution of `Sales` (turnover)?

---

**Answer.** `Sales` is continuous quantitative → **histogram**.
```r
distr.plot.x(Sales, plot.type="hist", data=pizzerie)
```
""",
"images": []}

ex1["1_1d"] = {"title": "Ex 1.1d — Choice of intervals for Sales histogram",
"content": """**Question.** Is a Sales histogram with 15 equal-width intervals more effective than one with intervals 0–10, 10–15, 15–20, 20–25, 25–35, 35–50, 50–70 (×1000)? Why? Comment on "absolute or relative frequencies can be reported on the y-axis."

---

**Answer.**
```r
distr.plot.x(Sales, plot.type="hist", breaks=15, data=pizzerie)
distr.plot.x(Sales, plot.type="hist",
             breaks=c(10000,15000,20000,25000,35000,50000,70000), data=pizzerie)
```
Both histograms show high concentration in low–medium sales classes, with extreme values in the right tail (also observed with very few frequencies, defining a tail to the right). The custom-width histogram is more effective at summarising the distribution by using fewer classes **without excessive loss of information**, making the shape of the distribution clear.

With reference to the comment "absolute or relative frequencies can be reported on the y-axis": this is **only correct when all classes have equal widths**. When widths differ (as in the custom binning), the y-axis must be **density** $f_i/w_i$; otherwise areas mislead. In the second histogram above, the y-axis is therefore *density*, and it is the **area** (not the height) of each bar that represents the proportion of pizzerias in that class.
""",
"images": ["statistics/images/ex1_1d-sales-hists.png"]}

ex1["1_1e"] = {"title": "Ex 1.1e — % pizzerias with Sales in [15000, 30000)",
"content": """**Question.** Pizzerias with sales between 15000 and 30000 are considered "medium turnover". What percentage of the sample falls into this category?

---

**Answer.** Build a logical vector and take the mean (= proportion of TRUEs):
```r
mean(pizzerie$Sales >= 15000 & pizzerie$Sales < 30000)
## [1] 0.59
head(pizzerie$Sales >= 15000 & pizzerie$Sales < 30000)
## [1] TRUE FALSE TRUE  TRUE  TRUE TRUE
```
The sum of logical values returns the count of TRUE entries, and the mean returns the proportion. **Result: 59% of pizzerias have medium turnover.**
""",
"images": []}

ex1["1_1f"] = {"title": "Ex 1.1f — Central tendency for District (mode)",
"content": """**Question.** What measure of central tendency would you use for `District`? For what variable types is it possible/appropriate?

---

**Answer.** `District` is qualitative **nominal**; the only summary measure that can be calculated is the **mode**.
```r
distr.summary.x(x=District, stats="mode", data=pizzerie)
##  n   n.a   mode    n.modes   mode%
## 100    0   Lodi       1       0.35

distr.table.x(District, freq="percentages", data=pizzerie)
##  District   Percent
##  Lodi          35
##  Milano        33
##  Pavia         32
##  TOTAL        100
```
Mode = **Lodi** (35%). However it accounts for only 35% of observations → **poorly representative** of the sample.

**NB.** The mode can be calculated for any variable type, but it is not particularly appropriate when the variable takes many values (as for continuous numerical variables) or when the observed values all have very low frequencies similar to each other.
""",
"images": ["statistics/images/ex1_1f-district-pie.png"]}

ex1["1_1g"] = {"title": "Ex 1.1g — % pizzerias with fewer than 5 employees",
"content": """**Question.** Pizzerias with less than 5 employees are considered small/medium sized. What percentage of the sample falls into this group?

---

**Answer.**
```r
mean(pizzerie$Employees < 5)
## [1] 0.45
```
**Result: 45%.** Alternatively from the cumulative table:
```r
distr.table.x(x=Employees, freq=c("cum"), data=pizzerie)
##  Employees  Count  Prop  Cum.Count  Cum.Prop
##   1           2    0.02     2        0.02
##   2           3    0.03     5        0.05
##   3          12    0.12    17        0.17
##   4          28    0.28    45        0.45  <- cum at 4 = 45%
##   5          23    0.23    68        0.68
##  ...
```
""",
"images": []}

ex1["1_1h"] = {"title": "Ex 1.1h — Mean vs median of Sales",
"content": """**Question.** Calculate the mean and median of `Sales`. How do you explain any differences? Which is more representative of typical turnover?

---

**Answer.**
```r
distr.summary.x(x=Sales, stats=c("mean","median"), data=pizzerie)
##  n  n.a    mean    median
## 100   0  23946.99  22349.5
```
- **mean = 23 946.99**
- **median = 22 349.5**

The histograms at point d) show **right skewness** of Sales: a long right tail. As expected, **mean > median**: the mean is pulled up by the right-tail extreme values. The mean is more sensitive to outliers; the median is more robust.

Since the distribution exhibits concentration toward lower-middle values, to describe the "centre" of the data — i.e. the value around which data are most concentrated — **the median is more appropriate** for summarising typical pizzeria turnover.
""",
"images": []}

ex1["1_1i"] = {"title": "Ex 1.1i — Approximate mean & median from grouped Brescia data",
"content": """**Question.** A survey on 88 pizzerias in Brescia gave the following summary:

| Sales in Brescia | Percentage |
|------------------|-----------:|
| [0, 15000)       | 21         |
| [15000, 30000)   | 63         |
| [30000, 90000)   | 16         |

Calculate the approximate mean and median, and compare. Why are we talking about *approximate* values?

---

**Answer.** Build midpoints and cumulative percentages:

| Sales            | Percentage | Midpoint | Cum. Percent |
|------------------|-----------:|---------:|-------------:|
| [0, 15000)       | 21         | 7500     | 21           |
| [15000, 30000)   | 63         | 22500    | 84           |
| [30000, 90000)   | 16         | 60000    | 100          |

**Approximate mean** (midpoint × proportion):
```r
7500*0.21 + 22500*0.63 + 60000*0.16
## [1] 25350
```
$\\bar x \\approx 25\\,350$.

**Approximate median.** Cumulative reaches 50% in the second class $[15000, 30000)$ — the **median class**. Under uniform-on-interval, the median is at the value $p_{50}$ where the rectangle area = 0.50. The area up to 15000 is 0.21, so the median class must contribute 0.29 (out of 0.63):
$$
(p_{50} - 15000) \\cdot \\frac{0.63}{15000} = 0.29 \\quad \\Rightarrow \\quad p_{50} = \\frac{0.29 \\cdot 15000}{0.63} + 15000.
$$
```r
29 * 15000 / 63 + 15000
## [1] 21904.76
```
$p_{50} \\approx 21\\,904.76$.

**Mean > median**, consistent with the right-skewed shape of the histogram.

Both values are **approximate** because they are obtained from the *grouped* data (the table), not the raw data — we approximate under the **uniform distribution within each class** assumption.
""",
"images": []}

# ----- 1.2 (DS) -----
ex1["1_2a"] = {"title": "Ex 1.2a — % of customers with Age ≤ Middle",
"content": """**Question.** After specifying the type of variable `Age`, indicate the percentage of customers with age at most "Middle".

---

**Answer.** `Age` is **ordinal**, but in the dataset is coded as a "character" variable. To analyse it properly, define a factor with explicit order:
```r
DS$Age_recode <- factor(DS$Age, levels=c("Young","Middle","Senior"))
distr.table.x(x=DS$Age_recode, freq=c("Perc","Cum"))
##  DS$Age_recode  Percent  Cum.Percent
##  Young            29        29
##  Middle           52        81
##  Senior           19       100
##  TOTAL           100
```
**The percentage of customers with age at most Middle = 81%.**
""",
"images": []}

ex1["1_2b"] = {"title": "Ex 1.2b — Summary measures for Age (mode + median)",
"content": """**Question.** What summary measures can be computed for `Age`? Indicate the values and give a brief description.

---

**Answer.** `Age_recode` is **ordinal qualitative**. Two measures are available:
- **Mode** — the category with the highest frequency. Here mode = **"Middle" (52%)**, considered representative.
- **Median** — the category whose cumulative frequency first exceeds 50%. From point a) the cumulative at *Middle* is 81% (> 50%), so **median = "Middle"**.

The mean is **not defined** for a qualitative variable (no meaningful arithmetic on the labels).
```r
distr.summary.x(x=DS$Age_recode, stats=c("mode","median"))
```
""",
"images": []}

ex1["1_2c"] = {"title": "Ex 1.2c — Spike plot for Children",
"content": """**Question.** After specifying the type of the variable `Children`, propose an appropriate graphical representation of its frequency distribution.

---

**Answer.** `Children` is **quantitative discrete**.
```r
distr.table.x(x=DS$Children, freq=c("Counts","Perc"))
##  DS$Children  Count  Percent
##     0         360     48
##     1         184     25
##     2         111     15
##     3          95     13
##  TOTAL        750    100
```
The number of unique values is small; the distribution is well represented by a **spike plot**:
```r
distr.plot.x(x=DS$Children, freq="Counts", plot.type="spike")
```
Since the observed values are equispaced consecutive integers, a barplot would also be acceptable.
""",
"images": ["statistics/images/ex1_2c-children-spike.png"]}

ex1["1_2d"] = {"title": "Ex 1.2d — Pie chart for History + bar plot alternative",
"content": """**Question.** Given a pie chart of `History` (categories *None, Low, Medium, High*), what are the characteristics of this graph? Does it change with relative vs absolute frequencies? Propose an alternative.

---

**Answer.** The pie chart is a graph in which the categories of a variable are displayed by slices of different sizes, proportional to the relative frequencies (or absolute frequencies, the representation would be similar). The slices suggest *None* and *High* are more frequent.

However, **History is ordinal** (`None < Low < Medium < High`), and the pie chart **does not preserve order**. A **bar plot** is preferable: it visualises the relative magnitude of categories and respects their ordering when the factor is defined with explicit levels:
```r
DS$History_recode <- factor(x=DS$History, levels=c("None","Low","Medium","High"))
distr.plot.x(DS$History_recode, plot.type="bars")
```
""",
"images": ["statistics/images/ex1_2d-history-pie-bar.png"]}

# ----- 1.3 (customer_habits) -----
ex1["1_3a"] = {"title": "Ex 1.3a — Variable inventory of customer_habits",
"content": """**Question.** How many variables in the dataset? How many qualitative vs quantitative?

---

**Answer.**
```r
str(customer_habits)
## 'data.frame': 34866 obs. of 15 variables:
##  $ index            : num    (identifier, not a statistical variable)
##  $ Date             : chr
##  $ Year             : num
##  $ Product_Category : chr
##  $ Sub_Category     : chr
##  $ Unit_Cost        : num
##  $ Unit_Price       : num
##  $ Month_ord        : Factor w/ 12 levels
##  $ Month            : chr
##  $ Quantity         : num
##  $ Cost             : num
##  $ Revenue          : num
##  $ Country          : chr
##  $ Age              : num
##  $ Sex              : chr
```
The dataset has **15 variables in total** (index is an identifier — not a statistical variable, so **14 statistical variables**):
- **Qualitative (categorical):** Date, Month, Month_ord, Sex, Country, Product_Category, Sub_Category (7 variables).
- **Quantitative:** Year, Age, Quantity, Unit_Cost, Unit_Price, Cost, Revenue (7 variables) — Year/Age/Quantity discrete, the rest continuous.
""",
"images": []}

ex1["1_3b"] = {"title": "Ex 1.3b — Sex composition of the sample",
"content": """**Question.** What is the sex composition of the sample?

---

**Answer.**
```r
distr.table.x(x=customer_habits$Sex)
##  customer_habits$Sex  Count  Prop
##  F                    15235  0.44
##  M                    19631  0.56
##  TOTAL                34866  1.00
```
We observe an approximate balance between male and female customers, with a slight majority of male customers (**56%**) compared to female (**44%**).
""",
"images": []}

ex1["1_3c"] = {"title": "Ex 1.3c — Plot the Sex distribution",
"content": """**Question.** Graphically represent the distribution of the two sexes in the sample.

---

**Answer.** **Pie chart** or **bar plot** — both are appropriate for the binary nominal variable `Sex`.
```r
distr.plot.x(x=customer_habits$Sex, freq=c("perc"), plot.type="pie")
distr.plot.x(x=customer_habits$Sex,                  plot.type="bar")
```
""",
"images": ["statistics/images/ex1_3c-sex-pie-bar.png"]}

ex1["1_3d"] = {"title": "Ex 1.3d — Country: majority European? Mode?",
"content": """**Question.** Is most of the data from European customers? What is the mode of `Country`?

---

**Answer.**
```r
distr.table.x(x=customer_habits$Country, freq=c("counts","percentage"), p.digits=1)
##  customer_habits$Country  Count  Percent
##  France                   6603    18.9
##  Germany                  9896    28.4
##  United Kingdom           8119    23.3
##  United States           10248    29.4
##  TOTAL                  34866   100.0
```
Four countries of origin. The **three European countries (France + Germany + UK) sum to about 70.6%**, vs 29.4% US — so **yes, European customers are more represented** in the dataset; the composition is skewed toward European buyers.

The **mode of `Country` is United States (29.4%)**. However the second-most-represented country, Germany, has a very similar frequency (28.4%), making the **mode a poorly representative measure** here. The weight of each country varies only between 18.9% and 29.4% — the distribution is not far from uniform.
""",
"images": ["statistics/images/ex1_3d-country-pie-bar.png"]}

ex1["1_3e"] = {"title": "Ex 1.3e — Spike plot of Age",
"content": """**Question.** Propose a suitable graphical representation of `Age`. Can you guess the median age from the graph alone?

---

**Answer.** `Age` is a discrete variable with many unique values. Customers in the dataset are **aged between 15 and 90 years**. Either a **spike plot** or a histogram can be used; the histogram is to be avoided when frequencies of the values are very similar to each other — in this case the spike plot represents the distribution in a more satisfactory manner:
```r
distr.table.x(x=customer_habits$Age, freq=c("percentage"), p.digits=2)
distr.plot.x(x=customer_habits$Age, freq=c("percentage"), plot.type="spike")
```
The median is the value that divides the distribution into two subgroups of approximately equal frequency. Each spike represents the percentage of customers of a given age. Noting the **low percentages of customers aged 75 or older and 25 or younger**, a value that splits the sample into two halves of equal weight will be **between 40 and 50**.
""",
"images": ["statistics/images/ex1_3e-age-spike.png"]}

ex1["1_3f"] = {"title": "Ex 1.3f — Mean vs median for Age (skewness)",
"content": """**Question.** Given the age distribution, would you expect the mean to be higher or lower than the median? Why?

---

**Answer.** The Age distribution appears slightly **skewed to the right** (long right tail beyond 60). In such a situation, **mean > median**, since the mean is pulled up by the right-tail extreme values.
```r
distr.summary.x(Age, stats="summary", digits=2, data=customer_habits)
##   n     n.a min q1 median mean    q3  max   sd   var
## 34866   0    16 40   46  47.13   54   96  10.53 110.87
```
- **median = 46**
- **mean = 47.13** — slightly higher, as predicted.
""",
"images": []}

ex1["1_3g"] = {"title": "Ex 1.3g — Revenue histogram (10 equal-width breaks)",
"content": """**Question.** Represent `Revenue` using a histogram with 10 intervals of equal width.

---

**Answer.**
```r
distr.plot.x(Revenue, plot.type="histogram", breaks=10, data=customer_habits)
```
The histogram shows a sharply **right-skewed** distribution: a dominant first class near zero and progressively smaller densities up to the right tail near 15 000.
""",
"images": ["statistics/images/ex1_3g-revenue-hist.png"]}

ex1["1_3h"] = {"title": "Ex 1.3h — Revenue: two custom interval choices",
"content": """**Question.** Two alternative choices: (i) intervals of width 1000 from 0 to 5000, then two extra intervals of width 1000 and 5000; (ii) intervals of width 500 from 0 to 5000, then two extra. Which is more informative?

---

**Answer.**
```r
distr.plot.x(Revenue, plot.type="histogram",
             breaks=1000*c(0,1,2,3,4,5,9,16), data=customer_habits)
distr.plot.x(Revenue, plot.type="histogram",
             breaks= 500*c(0,1,2,3,4,5,6,7,8,9,10,18,32), data=customer_habits)
```
Although the choice of intervals with **width 500** allows a more detailed description of the distribution between 0 and 5000, **it does not bring out any more features than the choice with width 1000** — both clearly show the strong right-skewness. The width-1000 binning is simpler and equally informative.
""",
"images": ["statistics/images/ex1_3h-revenue-hist-comparison.png"]}

ex1["1_3i"] = {"title": "Ex 1.3i — Revenue: mean vs median",
"content": """**Question.** Since the distribution is heavily right-skewed, do you expect a median lower or higher than the mean?

---

**Answer.** **Median much lower than mean**: the right tail contains few but very large revenues that drag the mean upward.
```r
distr.summary.x(Revenue, stats="summary", digits=2, data=customer_habits)
##  n     n.a min   q1  median  mean    q3     max    sd      var
## 34866   0   0.67 140  428    909.72 1150.5 15548  1286.83  1655919
```
- **median = 428**, **mean = 909.72** — mean is more than twice the median.

Comparison with the most recent survey: the median is substantially the same — at most slightly increased — indicating that 50% of cases have revenues lower than ~430/450. The mean instead increased, suggesting that the distribution shifted to the right and the highest revenues increased even more. **The mean is more sensitive to changes in the extreme values.**
""",
"images": []}

# ----- 1.4 (Quantity / Quantity_New) -----
QN_TABLE = """*(2022-2023 survey — `Quantity_New` table given in prompt.)*

| Quantity_New | Count |
|-------------:|------:|
| 1            | 5401  |
| 2            | 7340  |
| 3            | 8238  |
| 4            | 2561  |
| 6            | 700   |
| **TOTAL**    | **24 240** |
"""

ex1["1_4a1"] = {"title": "Ex 1.4 a1 — Spike plot for Quantity_New",
"content": """**Question.** Propose a correct graphical representation for `Quantity_New`.

""" + QN_TABLE + """

---

**Answer.** `Quantity_New` is **quantitative discrete**. The correct plot is the **spike diagram**. Absolute or relative frequencies (or percentages) can be plotted on the y-axis without misleading.
```r
distr.plot.x(x=Quantity_New, freq="Counts", plot.type="spike")
distr.plot.x(x=Quantity_New, freq="perc",   plot.type="spike")
```
Frequencies:

| Quantity_New | Count | Percent |
|-------------:|------:|--------:|
| 1            | 5401  | 22.3 |
| 2            | 7340  | 30.3 |
| 3            | 8238  | 34.0 |
| 4            | 2561  | 10.6 |
| 6            | 700   |  2.9 |
| **TOTAL**    | 24 240 | 100.0 |

**Note** the value 5 is never observed in the data. Since the variable is numerical, the distances between observed values must be properly accounted for — a **barplot would be wrong** because it would close the gap between 4 and 6 (the spike plot leaves the value 5 visibly empty).
""",
"images": ["statistics/images/ex1_4a1-quantity-spike-bar.png"]}

ex1["1_4a2"] = {"title": "Ex 1.4 a2 — Cumulative percentage and step plot",
"content": """**Question.** Build the cumulative percentage distribution and a suitable plot for cumulative frequencies.

""" + QN_TABLE + """

---

**Answer.** The $i$-th cumulative percentage represents the overall relevance of the first $i$ categories. From the frequency distribution:

| Quantity_New | Count | Percent | Cum. Percent |
|-------------:|------:|--------:|-------------:|
| 1 | 5401 | 22.3 | 22.3 |
| 2 | 7340 | 30.3 | 52.6 |
| 3 | 8238 | 34.0 | 86.6 |
| 4 | 2561 | 10.6 | 97.1 |
| 6 |  700 |  2.9 | 100.0 |

The appropriate graphical representation is the **cumulative frequency curve** (step diagram for a discrete variable):
```r
distr.plot.x(x=Quantity_New, freq="cum", plot.type="cumfreq")
```
""",
"images": ["statistics/images/ex1_4a2-quantity-cumulative.png"]}

ex1["1_4a3"] = {"title": "Ex 1.4 a3 — Mode, median, mean of Quantity_New",
"content": """**Question.** Determine mode, median, and mean for `Quantity_New`.

""" + QN_TABLE + """

---

**Answer.**
- **Mode = 3.** The absolute frequency at value 3 is 8238 (relative percentage 34%), the highest.
- **Median = 2.** From the cumulative frequencies, the first value where cum. percent exceeds 50% is **2** (cum at 1 is 22.3%, at 2 is 52.6% — the first to cross 50%).
- **Mean** (weighted average using counts as weights):
$$
\\bar x = \\frac{\\sum_i n_i \\cdot x_i}{n} = \\frac{5401 \\cdot 1 + 7340 \\cdot 2 + 8238 \\cdot 3 + 2561 \\cdot 4 + 700 \\cdot 6}{24\\,240} = \\frac{59\\,239}{24\\,240} \\approx 2.444.
$$
```r
x      <- 1:6
counts <- c(5401, 7340, 8238, 2561, 0, 700)
xbar   <- sum(counts*x) / sum(counts)
xbar
## [1] 2.443853
```
""",
"images": []}

ex1["1_4b"] = {"title": "Ex 1.4 b — Compare central tendency 2015-16 vs 2022-23",
"content": """**Question.** Compare mode, median and mean of `Quantity` for the two periods 2015–2016 and 2022–2023.

---

**Answer.**
```r
distr.summary.x(Quantity, stats="central tendency measures",
                digits=2, data=customer_habits)
##  Central tendency measures
##  n     n.a   mode  n.modes  mode%  median  mean
## 34866   0     3       1     0.4035    3    2.64
```
Side-by-side (percentages):

|   | 2015–16 | 2022–23 |
|--:|--------:|--------:|
| Mode   | 3    | 3 (source says 2 — **likely a typo**; the highest 2022–23 share is value 3 at 34.0% > value 2 at 30.3%) |
| Median | 3    | 2 |
| Mean   | 2.64 | 2.45 |

| value | 2015–16 % | 2022–23 % |
|-----:|----------:|----------:|
| 1 | 18.2 | 22.3 |
| 2 | 20.4 | 30.3 |
| 3 | 40.4 | 34.0 |
| 4 | 21.0 | 10.6 |
| 5 | 0    | 0    |
| 6 | 0    | 2.9  |

Neglecting the (rare) group of customers who jointly purchase more than 4 products (only ~3% in 2022–23 vs 0% in 2015–16), the **number of products jointly purchased is essentially between 1 and 4** in both periods. The percentage weight on values 1 and 2 has grown (22.3% + 30.3% = 52.6% in 2022–23 vs 18.2% + 20.4% = 38.6% in 2015–16); on values 3 and 4 it has fallen (44.6% vs 61.4%). The change is reflected in the summary measures (lower median, lower mean), confirming a shift in customers' purchasing habits toward smaller bundles.

> **Source discrepancy flagged.** The source answer table says *Mode 3 / 2*, but recomputation gives mode = 3 for both periods (value 3 has the highest share in 2022–23 as well: 34.0% > 30.3%). I leave the recomputed value here; the source likely contains a typo.
""",
"images": ["statistics/images/ex1_4b-quantity-periods.png"]}

# ----- 1.5 (Time — 1800 customers, table given) -----
TIME_TABLE = """*(Frequency distribution of `Time`, $n = 1800$ customers — given in the prompt.)*

| Time class   | freq.ass | width |
|--------------|---------:|------:|
| $[0,10)$     | 122      | 10    |
| $[10,20)$    | 420      | 10    |
| $[20,30)$    | 294      | 10    |
| $[30,60)$    | 176      | 30    |
| $[60,90)$    | 571      | 30    |
| $[90,150]$   | 217      | 60    |
| **TOTAL**    | **1800** |       |
"""

ex1["1_5a"] = {"title": "Ex 1.5a — P(Time ≤ 5): impulsive-buyer share",
"content": """**Question.** <span class="exam-question-text">It has been observed that customers who make a purchase in 5 minutes or less often buy impulsively, not choosing the most appropriate product for their needs, and expressing dissatisfaction afterwards. What is the percentage of such customers in the available sample?</span>

""" + TIME_TABLE + """

---

**Answer.** Assuming the 122 customers in $[0,10)$ are *uniformly distributed* over the interval, half of them — 61 — will have completed their purchase in 5 minutes or less:
$$
\\text{Freq}\\{Time \\le 5\\} = \\frac{61}{1800} \\cdot 100\\% = 3.39\\%.
$$
Apparently a rather low percentage.
""",
"images": []}

ex1["1_5b"] = {"title": "Ex 1.5b — P(Time ≤ 30): testing the manager's 60% claim",
"content": """**Question.** <span class="exam-question-text">The sales manager of one store claims that 60% of customers complete their purchase within half an hour of entering the store. Based on the data, would you agree with the manager?</span>

""" + TIME_TABLE + """

---

**Answer.** The total number of customers in the sample who completed their purchase within 30 minutes is $122 + 420 + 294 = 836$ — large in number, but the percentage is
$$
\\text{Freq}\\{Time \\le 30\\} = \\frac{836}{1800} \\cdot 100\\% = 46.44\\%,
$$
**quite lower than 60% as expected by the manager** — so the data do *not* support the manager's claim.
""",
"images": []}

ex1["1_5c"] = {"title": "Ex 1.5c — P(15 ≤ Time ≤ 50): relevance of this subgroup",
"content": """**Question.** <span class="exam-question-text">How many customers in the sample completed their purchase within 15 to 50 minutes? Is this group of customers the most relevant group in the sample?</span>

""" + TIME_TABLE + """

---

**Answer.** From the table we cannot determine the exact number, because the extremes 15 and 50 do not coincide with the class boundaries. Assuming **uniform within each class**:

- $[15,20)$: half of $[10,20)$ → $420/2 = 210$ customers;
- $[20,30)$: fully included → $294$ customers;
- $[30,50)$: $20/30 = 2/3$ of $[30,60)$ → $20/30 \\cdot 176 = 117.33$ customers.

$$
\\text{Freq}\\{15 \\le Time \\le 50\\} \\approx \\frac{210 + 294 + 117.33}{1800} \\cdot 100\\% = 34.52\\%.
$$

About **one third of customers** — considerable, but **still not a majority**, so this is *not* the most relevant group in the sample.
""",
"images": []}

ex1["1_5d"] = {"title": "Ex 1.5d — Ogive of Time and median estimation",
"content": """**Question.** Plot the cumulative distribution of `Time`. Based on the graphical analysis alone, can you determine the median time? Provide a more accurate assessment based on the data collected.

""" + TIME_TABLE + """

---

**Answer.** Since data are grouped into classes, exact cumulative frequency is known **only at the class extremes**; under uniform-within-interval, the curve is **piecewise linear** between them — this is the ogive. Cumulative frequencies (relative + cumulative):

| Time      | rel.freq | cum.freq |
|-----------|---------:|---------:|
| $[0,10)$   | 0.068    | 0.068 |
| $[10,20)$  | 0.233    | 0.301 |
| $[20,30)$  | 0.163    | 0.464 |
| $[30,60)$  | 0.098    | 0.562 |
| $[60,90)$  | 0.317    | 0.879 |
| $[90,150]$ | 0.121    | 1.000 |

The ogive joins points $(a_1, 0)$, $(b_i, F_i)$ with straight lines:
```r
distr.plot.x(x=Time, freq="cum", plot.type="cumfreq")
```
**Graphical estimate.** Cumulative at 30 is 0.464 (< 0.5); at 60 is 0.562 (≥ 0.5) → median class $[30,60)$. Reading the ogive at $F = 0.5$ gives roughly $p_{50} \\approx 40$ min.

**More accurate value.** Linear interpolation inside the median class:
$$
p_{50} \\approx 30 + \\frac{0.5 - 0.464}{d_4} = 30 + \\frac{0.036}{0.00327} \\approx 41.01 \\text{ min}.
$$
where $d_4 = 0.098/30 = 0.00327$ is the frequency density of the median class. The graphical guess (~40) does not differ significantly from this more precise value.
""",
"images": ["statistics/images/ex1_5d-time-ogive.png"]}

ex1["1_5e"] = {"title": "Ex 1.5e — Typical time spent (modal class via densities)",
"content": """**Question.** How much time does a customer typically spend in the store? Suggest the time (or rather, a range of times) that appears most relevant in terms of frequency.

""" + TIME_TABLE + """

---

**Answer.** We need to find the **modal class** — the class with the highest frequency. The class with maximum *absolute frequency* is $[60,90)$ with 571 cases, **but classes have different widths**: larger classes can collect more observations simply because they include more values. A correct comparison must be based on the **density** $d_i = f_i / w_i$:

| Time      | abs.freq | rel.freq | width | density |
|-----------|---------:|---------:|------:|--------:|
| $[0,10)$   | 122  | 0.068 | 10 | 0.00680 |
| $[10,20)$  | 420  | 0.233 | 10 | **0.02330** |
| $[20,30)$  | 294  | 0.163 | 10 | 0.01630 |
| $[30,60)$  | 176  | 0.098 | 30 | 0.00327 |
| $[60,90)$  | 571  | 0.317 | 30 | 0.01057 |
| $[90,150]$ | 217  | 0.121 | 60 | 0.00202 |

The denser class is the **second** one: **modal class $= [10, 20)$** with density 0.0233 — this identifies the set of "typical" times most frequently spent in the shop before purchase. Note its density is **more than twice** that of $[60,90)$ (0.01057), the absolute-frequency leader — which in fact ranks only **third** by density. The class $[20,30)$, adjacent to the modal class, has density (0.0163) higher than $[60,90)$.
""",
"images": []}

ex1["1_5f"] = {"title": "Ex 1.5f — Approximate mean of Time (midpoints)",
"content": """**Question.** Determine the mean time before completing a purchase. Is this exact or approximate?

""" + TIME_TABLE + """

---

**Answer.** From grouped data, the mean is **approximate** — use class midpoints as representative values:
$$
\\bar x \\approx \\frac{122 \\cdot 5 + 420 \\cdot 15 + 294 \\cdot 25 + 176 \\cdot 45 + 571 \\cdot 75 + 217 \\cdot 120}{1800} = 50.58 \\text{ min}.
$$
Approximate value, because we don't know how observations are distributed *within* each class.
""",
"images": []}

ex1["1_5g"] = {"title": "Ex 1.5g — Histogram of Time (densities)",
"content": """**Question.** Graphically represent the frequency distribution of `Time`.

""" + TIME_TABLE + """

---

**Answer.** Continuous variable with unequal class widths → histogram with **densities** on the y-axis.
```r
distr.plot.x(x=Time, plot.type="hist",
             breaks=c(0,10,20,30,60,90,150), data=...)
```
The mean (50.58) and median (41.01) both fall in a **low-density class**: they do not lie at the centre of mass of the distribution. The histogram shows **two sub-intervals of local high-density** — $[10,20)$ (the modal class) and $[60,90)$ — suggesting **two typical customer behaviours**: a short stay in the store (~10–30 min) and a longer one (~60–90 min). Mean and median therefore represent a *compromise* between these two profiles.
""",
"images": ["statistics/images/ex1_5g-time-hist.png"]}

ex1["1_5h"] = {"title": "Ex 1.5h — Approximate mean & median for two subgroups",
"content": """**Question.** Determine the approximate mean and median for the two subgroups: `Time` ≤ 30 minutes and `Time` > 30 minutes.

""" + TIME_TABLE + """

---

**Answer.** Recompute relative frequencies within each subgroup.

**Group A — Time ≤ 30** ($n_A = 836$):

| Time | freq | rel.freq | midpoint |
|------|----:|--------:|---------:|
| $[0,10)$ | 122 | 0.146 | 5 |
| $[10,20)$ | 420 | 0.502 | 15 |
| $[20,30)$ | 294 | 0.352 | 25 |

Median in class $[10,20)$ (cum: 0.146 → 0.648):
$$
p_{50}^A = 10 + \\frac{0.5 - 0.146}{0.502} \\cdot 10 = 17.05.
$$
Mean:
$$
\\bar x_A = \\frac{122 \\cdot 5 + 420 \\cdot 15 + 294 \\cdot 25}{836} = 17.06.
$$

**Group B — Time > 30** ($n_B = 964$):

| Time | freq | rel.freq | midpoint |
|------|----:|--------:|---------:|
| $[30,60)$ | 176 | 0.183 | 45 |
| $[60,90)$ | 571 | 0.592 | 75 |
| $[90,150]$ | 217 | 0.225 | 120 |

Median in class $[60,90)$ (cum: 0.183 → 0.775):
$$
p_{50}^B = 60 + \\frac{0.5 - 0.183}{0.592} \\cdot 30 = 76.06.
$$
Mean:
$$
\\bar x_B = \\frac{176 \\cdot 45 + 571 \\cdot 75 + 217 \\cdot 120}{964} = 79.65.
$$

The summary measures are now much **more representative** of each subgroup: each median and each mean falls in the corresponding **modal class** (the class with maximum density), which is not the case for the overall distribution.
""",
"images": []}

# ----- 1.6 (Expenses histogram) -----
EXP_BLOCK = """*(Histogram of `Expenses` for a web-company sample is given in the prompt; visible densities: 0.022 at $[0,20)$, 0.01 at $[20,40)$, 0.005 at $[40,60)$, 0.003 at $[60,80)$, 0.002 at $[80,120)$, 0.001 at $[120,160)$, 0.0006 at $[160,300]$.)*
"""

ex1["1_6a"] = {"title": "Ex 1.6a — Evaluate statements about mean/median",
"content": """**Question.** Based on the histogram, which of the following statements about the distribution of expenses is plausible?
1) About 50% of clients spend at most 40, with an average expense of approximately 20.
2) About 50% of clients spend 40 or less, with an average expense around 40.
3) About 50% of clients spend more than 40, with an average expense around 80.
4) About 50% of clients spend less than 28, with an average amount around 50.

""" + EXP_BLOCK + """

---

**Answer.** The statements essentially concern the **median** and the **mean** of the distribution.

The histogram shows a **strongly positively-skewed** distribution. In such a condition, **mean > median**.

- **Statement 1.** mean = 20 < median = 40. **False** (it would require left-skew or negative skewness).
- **Statement 2.** mean ≈ median = 40 → symmetric. **False** (the distribution is clearly not symmetric).
- **Statement 3.** Frequencies in $[0,20)$: $f_1 = w_1 \\cdot d_1 = 20 \\cdot 0.022 = 0.44$; in $[20,40)$: $f_2 = 20 \\cdot 0.01 = 0.20$. So $\\Pr(\\text{Expenses} > 40) = 1 - 0.44 - 0.20 = 0.36$ — *not* 50%. **False**.
- **Statement 4.** 22% spend < 20; 54% spend < 30 (≈ 0.44 + 0.2 · 0.5 = 0.54); so 50% spend < ~28. The approximated median is included between 20 and 30. As for the mean: most clients have expenses < 60, but the strong right-skewness inflates the mean — an average around 50 is plausible. **Statement 4 is the one that is not clearly wrong.**
""",
"images": []}

ex1["1_6b"] = {"title": "Ex 1.6b — Approximate median and mean from the histogram",
"content": """**Question.** Is it possible to obtain the median and/or the mean of the distribution from the plot?

""" + EXP_BLOCK + """

---

**Answer.** Yes — frequencies of all intervals are obtained as density × width (small rounding errors aside; any unclear class can be recovered as $1 - \\sum(\\text{other frequencies})$):

| expenses     | width | Density | Frequency |
|--------------|------:|--------:|----------:|
| $[0,20)$     | 20    | 0.022   | 0.44 |
| $[20,40)$    | 20    | 0.010   | 0.20 |
| $[40,60)$    | 20    | 0.005   | 0.10 |
| $[60,80)$    | 20    | 0.003   | 0.06 |
| $[80,120)$   | 40    | 0.002   | 0.08 |
| $[120,160)$  | 40    | 0.001   | 0.04 |
| $[160,300]$  | 140   | 0.0006  | 0.084 |

**Approximate median.** Cumulative reaches 50% in class $[20,40)$ (cum at 20 = 0.44, at 40 = 0.64):
$$
p_{50} = 20 + \\frac{0.5 - 0.44}{0.01} = 26.
$$

**Approximate mean.** Midpoints × frequencies:
$$
\\bar x = 10 \\cdot 0.44 + 30 \\cdot 0.20 + 50 \\cdot 0.10 + 70 \\cdot 0.06 + 100 \\cdot 0.08 + 140 \\cdot 0.04 + 230 \\cdot 0.084 \\approx 52.52.
$$

The median (~26) is much smaller than the mean (~52.52), confirming the strong right-skewness.
""",
"images": []}


# =====================================================================
# TOPIC / SUBTOPIC STRUCTURE
# =====================================================================

# Each subtopic: (subtopic_id, subtopic_name, theory_id, theory_title, theory_content,
#                 columns) where columns is a dict {col_index: [(node_id, content_dict)]}.

from ex2_content import ex2
from ex3_content import ex3
from ex4_content import ex4
from ex5_content import ex5
from ex6_content import ex6
from ex7_content import ex7
from ex8_content import ex8
from ex9_content import ex9
from past_exams_content import past_exams  # 13 past exams (2024-2026)
from master_exercises_content import master_exercises  # one per subtopic, col 100

# Tag master exercises so the table can give them a distinct style
for _mid, _m in master_exercises.items():
    _m.setdefault("is_master", True)

ALL = {**ex0, **ex1, **ex2, **ex3, **ex4, **ex5, **ex6, **ex7, **ex8, **ex9, **past_exams}
# Master exercises get keyed by `master_<sid>` so they never clash with snippet ids
for _sid, _m in master_exercises.items():
    ALL[f"master_{_sid}"] = _m

def th(tid, ttitle, tcontent):
    return {"id": tid, "title": ttitle, "content": tcontent}

SUBTOPIC_COLOR = {
    "G1": "coral", "G2": "orange", "G3": "purple", "G4": "skyblue",
    "G5": "green", "G6": "pink",
    "G7": "yellow", "G8": "lavender", "G9": "salmon",
    "G10": "lightblue", "G11": "teal", "G12": "gold",
    "G13": "navy", "G14": "crimson", "G15": "forest",
}

# G13 / G14 / G15 — Inferential statistics theory snippets

T_G13_CI = """## G13 — Confidence intervals

A confidence interval (CI) is a random interval $[L, U]$ for an unknown population parameter $\\theta$, constructed from data so that, before observing the sample,
$$
\\Pr([L, U] \\ni \\theta) = 1 - \\alpha.
$$
$1-\\alpha$ is the **confidence level** (often 0.90, 0.95, 0.99). The endpoints $L, U$ are random; $\\theta$ is fixed. Once data are observed and a specific interval is computed, the interval either does or does not contain $\\theta$ — the $1-\\alpha$ refers to the long-run coverage of the procedure.

### Key formulas
- **CI for the mean (known $\\sigma$):** $\\bar X \\pm z_{1-\\alpha/2} \\cdot \\sigma/\\sqrt{n}$.
- **CI for the mean (unknown $\\sigma$):** $\\bar X \\pm t_{n-1,\\;1-\\alpha/2} \\cdot s/\\sqrt{n}$.
- **CI for a proportion:** $\\hat p \\pm z_{1-\\alpha/2} \\cdot \\sqrt{\\hat p(1-\\hat p)/n}$.
- **CI for difference of means** (independent samples, equal variances, pooled SE):
$$
(\\bar X - \\bar Y) \\pm t_{n_x+n_y-2,\\;1-\\alpha/2} \\cdot \\sqrt{s_p^2\\big(\\tfrac{1}{n_x}+\\tfrac{1}{n_y}\\big)},
\\quad s_p^2 = \\frac{(n_x-1)s_x^2 + (n_y-1)s_y^2}{n_x+n_y-2}.
$$
- **CI for difference (Welch, unequal variances):** $\\text{SE} = \\sqrt{s_x^2/n_x + s_y^2/n_y}$.
- **CI for difference (paired):** $\\bar D \\pm t_{n-1,\\;1-\\alpha/2} \\cdot s_D/\\sqrt{n}$.
- **CI for difference of proportions:** $(\\hat p_1 - \\hat p_2) \\pm z \\cdot \\sqrt{\\hat p_1(1-\\hat p_1)/n_1 + \\hat p_2(1-\\hat p_2)/n_2}$.

### Sample-size rules
- Mean: $n \\ge (z\\sigma/ME)^2$.
- Proportion: $n \\ge (z/ME)^2 \\cdot p(1-p)$; use $p=0.5$ for worst-case.

**R commands:**
```r
CI.mean(x, sigma=..., conf.level=0.95, data=DF)        # known sigma
CI.mean(x, conf.level=0.95, data=DF)                   # unknown sigma (t)
CI.prop(x, success="Yes", conf.level=0.95, data=DF)
CI.diffmean(x, y, type=c("independent","paired"), var.test=TRUE,
            conf.level=0.95, data=DF)
CI.diffprop(x, y, conf.level=0.95, data=DF)
qnorm(0.975); qt(0.975, df=n-1)                        # critical values
```

![Confidence interval band and long-run coverage](statistics/images/theory/th_g13_ai.png)
"""

T_G14_HT = """## G14 — Hypothesis tests

Test $H_0: \\theta = \\theta_0$ vs $H_1$. Compute a test statistic; compare to a critical value or compute a $p$-value.

### Decision rule
- Reject $H_0$ at level $\\alpha$ if **p-value < $\\alpha$**.
- Equivalent: reject if the observed statistic is outside the $1-\\alpha$ acceptance region.

### Two-sided p-value
For a $z$- or $t$-statistic, the two-sided p-value is
$$
p\\text{-value} = 2\\cdot\\Pr\\!\\big(T > |t_{\\text{obs}}|\\big),
$$
where $T$ is the reference distribution under $H_0$ (standard normal for $z$, $t_{\\nu}$ for $t$).

### Common tests
- **One-sample mean test** (known $\\sigma$): $z = (\\bar X - \\mu_0)/(\\sigma/\\sqrt{n})$.
- **One-sample mean test** (unknown $\\sigma$): $t = (\\bar X - \\mu_0)/(s/\\sqrt{n})$, df $= n-1$.
- **Two-sample mean test** (independent, equal variances): pooled $s_p^2$; df $= n_x + n_y - 2$.
- **Two-sample mean test** (Welch): separate variances.
- **Paired test**: one-sample test on differences $D_i$.
- **One-proportion z-test**: $z = (\\hat p - p_0)/\\sqrt{p_0(1-p_0)/n}$.
- **Two-proportion z-test**: $z = (\\hat p_1 - \\hat p_2)/\\sqrt{\\hat p_{\\text{pool}}(1-\\hat p_{\\text{pool}})(1/n_1+1/n_2)}$.
- **Chi-squared goodness of fit:** $\\sum_i (O_i - E_i)^2/E_i \\sim \\chi^2_{k-1}$.
- **Chi-squared independence:** same formula on a contingency table, df $= (r-1)(c-1)$.
- **Fisher's exact test:** for 2×2 tables with small samples.

### Duality with confidence intervals
A two-sided $\\alpha$-test rejects $H_0: \\theta = \\theta_0$ **iff $\\theta_0$ is outside the $(1-\\alpha)$ CI**.

**R commands:**
```r
TEST.diffmean(x, by=group, type="independent", mdiff0=0,
              alternative="two.sided", var.test=TRUE, data=DF)
TEST.diffprop(x, y, success.x="Yes", pdiff=0, alternative="two.sided")
chisq.test(table_)               # goodness of fit / independence
fisher.test(table_)              # exact test for 2x2
2*(1 - pnorm(abs(z)))            # two-sided p-value (z-test)
2*(1 - pt(abs(t), df=n-1))       # two-sided p-value (t-test)
```

![Hypothesis testing: rejection regions, p-value, Type I/II errors](statistics/images/theory/th_g14_ai.png)
"""

T_G15_REG = """## G15 — Linear regression (simple and multiple)

The linear regression model:
$$
Y_i = \\beta_0 + \\beta_1 X_{1i} + \\ldots + \\beta_k X_{ki} + \\varepsilon_i, \\quad \\varepsilon_i \\sim N(0, \\sigma^2)\\text{ iid}.
$$

### Estimation (OLS)
The least-squares estimates $\\hat\\beta$ minimize $\\sum (Y_i - \\hat Y_i)^2$. Closed-form for simple regression:
$$
\\hat\\beta_1 = \\frac{\\sum (x_i-\\bar x)(y_i-\\bar y)}{\\sum (x_i-\\bar x)^2}, \\qquad \\hat\\beta_0 = \\bar y - \\hat\\beta_1 \\bar x.
$$

### Inference
- **t-test for each $\\beta_j$:** $t = \\hat\\beta_j / \\text{SE}(\\hat\\beta_j)$, df $= n-k-1$.
- **CI for $\\beta_j$:** $\\hat\\beta_j \\pm t_{n-k-1,\\;1-\\alpha/2}\\cdot \\text{SE}(\\hat\\beta_j)$.
- **R² = SS$_{\\text{reg}}$/SST = 1 − SSE/SST**: fraction of variance of $Y$ explained by the model, where SST $= \\sum (Y_i - \\bar Y)^2$, SS$_{\\text{reg}} = \\sum (\\hat Y_i - \\bar Y)^2$, SSE $= \\sum (Y_i - \\hat Y_i)^2$.
- **Adjusted R²:** $R^2_{\\text{adj}} = 1 - \\dfrac{SSE/(n-k-1)}{SST/(n-1)}$ — penalizes adding predictors.
- **Confidence interval** for the mean response: narrower; **prediction interval** for a single new observation: wider (adds $\\sigma_\\epsilon$).

### Assumptions (and diagnostics)
1. **Linearity** — fitted vs residuals plot, no curvature.
2. **Homoscedasticity** — constant variance: scale-location plot, no fanning.
3. **Independence of errors** — residual lag-1 plot, time-order plot.
4. **Normality of errors** — Q-Q plot, histogram of standardized residuals.
5. **No high-leverage / influential points** — Cook's distance, leverage plot.

### Categorical predictors
R automatically converts factors into indicator variables, taking one level as the reference. Each coefficient = deviation from the reference, holding other predictors constant.

**R commands:**
```r
mod <- lm(y ~ x1 + x2 + factor_var, data=DF)
summary(mod); confint(mod, level=0.95)
predict(mod, newdata=..., interval="confidence")
predict(mod, newdata=..., interval="prediction")
plot(mod, which=1)   # residuals vs fitted
plot(mod, which=3)   # scale-location
plot(mod, which=4)   # Cook's distance
distr.plot.x(rstandard(mod), plot.type="histogram")
```

![OLS fit with residuals, R² decomposition, and multivariate regression plane](statistics/images/theory/th_g15_ai.png)
"""

# G10 / G11 / G12 — Probability theory snippets

T_G10_NORMAL = """## G10 — Normal distribution and probability calculations

A continuous random variable $X$ is **normally distributed** with mean $\\mu$ and standard deviation $\\sigma$ — written $X \\sim N(\\mu, \\sigma^2)$ — if its density is
$$
f(x) = \\frac{1}{\\sigma\\sqrt{2\\pi}} \\exp\\!\\left[-\\frac{(x-\\mu)^2}{2\\sigma^2}\\right].
$$

### Standardization
If $X \\sim N(\\mu, \\sigma^2)$, then $Z = (X-\\mu)/\\sigma \\sim N(0, 1)$ — the **standard normal**.

### Probability calculations
- $\\Pr(X \\le x)$: cumulative distribution at $x$ — read via $z$-table or computed in R with `pnorm`.
- $\\Pr(X > x) = 1 - \\Pr(X \\le x)$.
- $\\Pr(a < X < b) = \\Pr(X \\le b) - \\Pr(X \\le a)$.

### Quantile (percentile) computation
The $q$-quantile $x_q$ satisfies $\\Pr(X \\le x_q) = q$ — invert the CDF, computed in R with `qnorm`.

**R commands:**
```r
pnorm(x, mean=mu, sd=sigma)              # P(X <= x)
1 - pnorm(x, mean=mu, sd=sigma)          # P(X > x)
qnorm(q, mean=mu, sd=sigma)              # x such that P(X <= x) = q

# Standard normal
pnorm(z); qnorm(q)
```

### Reading a normal in context
- "what is the probability the value exceeds threshold $T$" → $1 - \\Pr(X \\le T)$.
- "what is the minimum value of the top $\\alpha\\%$" → $(1 - \\alpha)$-th quantile.
- "what is the 90th percentile" → `qnorm(0.9, mu, sigma)`.

![Normal density: 68/95/99.7 rule and standardization](statistics/images/theory/th_g10_ai.png)
"""

T_G11_SAMP = """## G11 — Sampling distributions and the Central Limit Theorem

![Theory illustration](statistics/images/theory/th_g11_ai.png)

When we draw a sample of size $n$ from a population and compute a statistic (e.g. sample mean $\\bar X$, sample proportion $\\bar P$), the statistic itself has a **distribution** — the *sampling distribution*.

### Sample mean distribution
For independent $X_1, \\ldots, X_n$ from a population with mean $\\mu$ and variance $\\sigma^2$:
$$
\\bar X = \\frac{1}{n}\\sum_{i=1}^n X_i, \\qquad E[\\bar X] = \\mu, \\qquad \\mathrm{Var}(\\bar X) = \\sigma^2/n.
$$

If the population is normal: $\\bar X \\sim N(\\mu, \\sigma^2/n)$ exactly.

**Central Limit Theorem (CLT):** for $n$ large (rule of thumb $n \\ge 30$, or $n \\ge 40$ if heavy-tailed), $\\bar X$ is *approximately* normal **regardless of the population distribution**.

### Sample proportion distribution
For $X_i \\sim \\text{Bernoulli}(p)$, the sample proportion $\\bar P = \\frac{1}{n}\\sum X_i$ has
$$
E[\\bar P] = p, \\qquad \\mathrm{Var}(\\bar P) = \\frac{p(1-p)}{n}.
$$
For $n$ large (typically $np \\ge 10$ and $n(1-p) \\ge 10$), by CLT:
$$
\\bar P \\approx N\\!\\left(p,\\; \\frac{p(1-p)}{n}\\right).
$$

### Probability calculations
Same R commands as for any normal — just plug in the **sampling-distribution parameters**:
```r
pnorm(value, mean=mu_population, sd=sigma_population/sqrt(n))    # P(X-bar <= value)
pnorm(value, mean=p,             sd=sqrt(p*(1-p)/n))             # P(P-bar <= value)
```

### When can we apply CLT vs need normality?
- Large sample → CLT applies → no need to assume population normal.
- Small sample → must assume population is normal to use the same formulas.
"""

T_G12_LINCOMB = """## G12 — Linear combinations of independent random variables

![X+Y vs X-Y: variance depends on Cov(X,Y)](statistics/images/theory/th_g12_ai.png)

Given $X_1, \\ldots, X_n$ independent random variables (not necessarily normal) and constants $a_0, a_1, \\ldots, a_n$, define
$$
Y = a_0 + \\sum_{i=1}^n a_i X_i.
$$

### Expectation (always linear)
$$
E[Y] = a_0 + \\sum_{i=1}^n a_i\\, E[X_i].
$$
This holds **regardless** of dependence between the $X_i$.

### Variance (requires independence or covariance terms)
$$
\\mathrm{Var}(Y) = \\sum_{i=1}^n a_i^2\\,\\mathrm{Var}(X_i) + 2\\sum_{i<j} a_i a_j \\mathrm{Cov}(X_i, X_j).
$$
If the $X_i$ are independent (so all covariances are zero), the cross terms vanish.

### If the $X_i$ are normal
Any linear combination of independent normals is again normal:
$$
Y \\sim N\\!\\left(a_0 + \\sum_i a_i \\mu_i,\\; \\sum_i a_i^2 \\sigma_i^2\\right).
$$
This is the key result behind weighted-average grades, total spending across customers, etc.

### Common applications
- **Sum** $S = X_1 + \\ldots + X_n$ of $n$ iid normals $X_i \\sim N(\\mu, \\sigma^2)$: $S \\sim N(n\\mu, n\\sigma^2)$.
- **Weighted average** of grades: e.g. $G = 0.4 X + 0.6 Y$ → $E[G] = 0.4 \\mu_X + 0.6 \\mu_Y$, etc.
- **Difference** $D = X - Y$: $E[D] = \\mu_X - \\mu_Y$, $\\mathrm{Var}(D) = \\mathrm{Var}(X) + \\mathrm{Var}(Y) - 2\\mathrm{Cov}(X, Y)$.

### Bivariate normal
$(X, Y)$ jointly normal with means $\\mu_X, \\mu_Y$, variances $\\sigma_X^2, \\sigma_Y^2$ and correlation $\\rho$:
- Marginals: $X \\sim N(\\mu_X, \\sigma_X^2)$, $Y \\sim N(\\mu_Y, \\sigma_Y^2)$.
- Linear combination $aX + bY \\sim N(a\\mu_X + b\\mu_Y, a^2\\sigma_X^2 + b^2\\sigma_Y^2 + 2ab\\rho\\sigma_X\\sigma_Y)$.
"""

# G7 / G8 / G9 — Bivariate-statistics theory snippets

T_G7_BIVQUAL = """## G7 — Two-way tables: joint, marginal, conditional distributions

Bivariate analysis of **two qualitative variables** $X, Y$.

![Theory illustration](statistics/images/theory/th_g7_ai.png)

### Joint, marginal, conditional frequencies
For each cell $(i, j)$:
- **Joint count** $n_{ij}$; **joint relative frequency** $f_{ij} = n_{ij}/n$.
- **Marginal of $X$:** $n_{i\\cdot} = \\sum_j n_{ij}$; $f_{i\\cdot} = n_{i\\cdot}/n$. Similarly for $Y$.
- **Row-conditional** ($Y$ given $X = x_i$): $f_{j|i} = n_{ij}/n_{i\\cdot}$.
- **Column-conditional** ($X$ given $Y = y_j$): $f_{i|j} = n_{ij}/n_{\\cdot j}$.

### Independence
$X$ and $Y$ are **independent** iff conditional distributions equal the marginals:
$$
f_{j|i} = f_{j\\cdot} \\quad \\forall i, j, \\quad \\text{equivalently} \\quad f_{ij} = f_{i\\cdot}\\,f_{\\cdot j}.
$$
If conditional distributions look **different** across categories of the conditioning variable → **associated**.

### Plots
- **Stacked bar** of one variable's conditional distribution within each level of the other.
- **Side-by-side bar** (`bar.type="beside"`) for direct visual comparison.

**R commands:**
```r
distr.table.xy(X, Y, freq=c("counts","percentages"), data=DF)
distr.table.xy(X, Y, freq="perc", freq.type="y|x", data=DF)   # row-conditional
distr.table.xy(X, Y, freq="perc", freq.type="x|y", data=DF)   # col-conditional
distr.plot.xy (X, Y, freq="perc", plot.type="bars", bar.type="xy",   data=DF)
distr.plot.xy (X, Y, freq="perc", plot.type="bars", bar.type="beside", data=DF)
```
"""

T_G8_CONDSUMM = """## G8 — Conditional summary measures (mixed-type bivariate)

When **one variable is qualitative** and the other is **numerical**, we compare the *distribution* of the numerical variable across categories of the qualitative variable.

![Theory illustration](statistics/images/theory/th_g8_ai.png)

### Tools
- **Side-by-side boxplots**: the most effective single graph for comparing many groups at once. Each box shows the conditional median, quartiles and whiskers; outliers are visible.
- **Conditional five-number summary, mean, variance, SD, CV.** Computed within each subgroup.
- **Coefficient of variation** is essential when subgroup *means* differ, since SD by itself is hard to interpret in relative terms.

### Reading the comparison
- Difference in **medians** → location shift between groups.
- Difference in **IQR / SD** → spread differs.
- Difference in **CV** → spread differs *relative to the mean*.
- Difference in **shape** (skew, outliers) → conditional distributions differ in form, not just position.

### Independence test (qualitative ↔ numerical)
Even visually: if all subgroup boxplots are nearly identical, the qualitative variable does not "explain" the numerical one — likely independent. If they differ in any aspect (location/spread/shape) → associated.

**R commands:**
```r
distr.plot.xy(x=NumVar, y=QualVar, plot.type="boxplot", data=DF)
distr.summary.x(NumVar, by=QualVar, stats="summary", data=DF)
distr.summary.x(NumVar, by=QualVar, stats=c("mean","dispersion"), data=DF)
```
"""

T_G9_COVCOR = """## G9 — Covariance, correlation and scatter for two numerical variables

![Theory illustration](statistics/images/theory/th_g9_ai.png)

### Scatter plot
First step: a **scatterplot** to see the shape of the joint variation. The relationship may be:
- linear (positive or negative),
- non-linear (e.g. quadratic — a U/inverted-U shape),
- absent (cloud of points with no pattern).

### Covariance
$$
\\mathrm{Cov}(X, Y) = \\frac{1}{n-1}\\sum_{i=1}^{n}(x_i - \\bar x)(y_i - \\bar y).
$$
- Sign reveals **direction**: positive → $X$ and $Y$ tend to move together; negative → opposite.
- **Magnitude** is unit-dependent — cannot judge "strength" by raw covariance alone.

### Pearson correlation
$$
r = \\rho_{X,Y} = \\frac{\\mathrm{Cov}(X,Y)}{\\sigma_X \\sigma_Y} \\in [-1, +1].
$$
- Unit-free, scale-free measure of **linear** association.
- $|r| \\to 1$: data tightly clustered around a straight line.
- $r = 0$: no *linear* association (could still be non-linearly related).

### Interpretation guidelines
- $|r| < 0.3$ — weak / negligible linear association.
- $0.3 \\le |r| < 0.7$ — moderate.
- $|r| \\ge 0.7$ — strong.
- These are conventional thresholds; always confirm with the scatterplot.

### Limitations
- **Non-linear** relationships can have $r \\approx 0$ (e.g. quadratic), so always plot.
- **Outliers** can inflate or deflate $r$ — examine the scatter for influential points.
- Correlation $\\ne$ causation.

**R commands:**
```r
distr.plot.xy(x=Xvar, y=Yvar, plot.type="scatter", data=DF, fitline=TRUE)
cov(DF$Xvar, DF$Yvar)
cor(DF$Xvar, DF$Yvar)
```
"""

# ===== G5 / G6 focused theory snippets =====

T_G5_DISP = """## G5 — Dispersion measures (range, IQR, variance, SD, CV)

Dispersion quantifies how far the data are spread around (or away from) a central reference.

![Theory illustration](statistics/images/theory/th_g5_ai.png)

### Range and interquartile range
- **Range** $R = \\max - \\min$. Simple but driven by the most extreme observation, so sensitive to outliers.
- **Interquartile range** $IQR = Q_3 - Q_1$. Width of the interval containing the middle 50% of data. Robust to tail outliers.

### Variance and standard deviation (around the mean)
With $n$ observations and sample mean $\\bar x$:
$$
\\sigma^2 = \\frac{1}{n-1}\\sum_{i=1}^{n} (x_i - \\bar x)^2, \\qquad \\sigma = \\sqrt{\\sigma^2}.
$$

For a frequency table with $k$ classes (midpoints $m_k$, relative freq $f_k$):
$$
\\sigma^2 \\approx \\frac{n}{n-1}\\left[\\sum_k f_k \\cdot m_k^2 - \\bar x^2\\right]
= \\frac{n}{n-1}\\sum_k f_k(m_k-\\bar x)^2.
$$

Variance has squared units; SD has the same units as the data (interpret SD as the "average distance from the mean").

### Coefficient of variation
$$
cv = \\frac{\\sigma}{|\\bar x|}.
$$
A unit-free relative measure. Comparable across distributions with **different means** or units. Useful when comparing two variables/groups whose averages differ substantially.

### Reading the values
- $cv < 0.30$ → relatively low variability.
- $0.30 \\le cv < 1$ → moderate.
- $cv \\approx 1$ → SD comparable in size to the mean (high variability).
- $cv > 1$ → very dispersed; SD exceeds the mean.

**R commands:**
```r
distr.summary.x(x=Var, stats="dispersion", data=DF)
##   n n.a  range  IQrange   sd   var   cv
distr.summary.x(x=Var, stats=c("mean","dispersion"), data=DF)
```
"""

T_G6A_QUANT = """## G6.A — Quantiles, percentiles, deciles

A **quantile of order** $q \\in (0,1)$ is a value $p_q$ such that $\\text{Freq}\\{X \\le p_q\\} = q$.

- **Quartiles:** $Q_1 = p_{0.25}$, median $= p_{0.50}$, $Q_3 = p_{0.75}$.
- **Deciles:** $p_{0.1}, p_{0.2}, \\ldots, p_{0.9}$ — split the data into 10 equally weighted groups.
- **Percentiles** $p_{0.05}, p_{0.95}, p_{0.99}$ — useful for characterising the tails.

### Reading from a cumulative-frequency table or an ogive
$$
p_q \\approx a_k + \\frac{q - F_{k-1}}{f_k} \\cdot w_k = a_k + \\frac{q - F_{k-1}}{d_k},
$$
where the median class is the first $[a_k, b_k)$ with cumulative relative frequency reaching $q$, $w_k$ is its width and $d_k = f_k/w_k$ its density.

### Discrete numerical variable
The $q$-quantile is the **smallest observed value** for which the cumulative relative frequency reaches or exceeds $q$.

**R commands:**
```r
distr.summary.x(x=Var, stats="p90", data=DF)
distr.summary.x(x=Var, stats=c("p5","p95"), data=DF)
distr.summary.x(x=Var, stats="deciles", data=DF)
```

![Ogive read-off and rank rule](statistics/images/theory/th_g6a_ai.png)
"""

T_G6B_BOX = """## G6.B — Boxplots and the five-number summary

The **five-number summary** is $(\\min, Q_1, \\text{median}, Q_3, \\max)$ — a compact picture of both the center and the tails.

### Boxplot anatomy
- The **box** spans $[Q_1, Q_3]$; the **median** is drawn inside it.
- **Whiskers** extend from the box to the most extreme regular (non-outlier) values, i.e. up to $Q_3 + 1.5\\cdot IQR$ on the right and down to $Q_1 - 1.5\\cdot IQR$ on the left.
- Observations beyond the whiskers are flagged as **outliers** (small circles).

![Theory illustration](statistics/images/theory/th_g6b_ai.png)

### Reading skewness from the box
- The median **inside the box**: if it sits closer to $Q_1$ than to $Q_3$ → right skew; closer to $Q_3$ → left skew; centered → symmetric.
- **Whisker lengths**: a longer right whisker (or more right-side outliers) reinforces a right-skew diagnosis.
- The **histogram** is the complementary tool to confirm.

**R commands:**
```r
distr.summary.x(x=Var, stats="fivenumber", data=DF)
distr.plot.x(x=Var, plot.type="boxplot", data=DF)
```
"""

T_G6C_OUT = """## G6.C — Outliers and extreme values

A value $x$ is flagged as an **outlier** with respect to a distribution if it lies far enough from the bulk of the data:

- **Upper outlier:** $x > Q_3 + 1.5\\cdot IQR$.
- **Lower outlier:** $x < Q_1 - 1.5\\cdot IQR$.

These thresholds are computed from the **quartiles** of the distribution; the multiplier $1.5$ is the standard convention used by the boxplot.

### Counting outliers
Once $Q_1, Q_3$ are computed (e.g. with `distr.summary.x(..., stats="fivenumber")` or `quantile(DF$Var, c(.25,.75))`), count outliers with a boolean mask:
```r
Q1 <- quantile(DF$Var, 0.25); Q3 <- quantile(DF$Var, 0.75)
IQR <- Q3 - Q1
sum(DF$Var > Q3 + 1.5*IQR)                                   # upper outliers
sum(DF$Var < Q1 - 1.5*IQR)                                   # lower outliers
mean(DF$Var < Q1 - 1.5*IQR | DF$Var > Q3 + 1.5*IQR)          # % outliers
```

### Why care
Outliers can **inflate the mean** and the SD without changing the median or IQR much. When a distribution has many outliers, prefer **robust summaries** (median, IQR) over mean and SD; flag the outliers separately and investigate them if material.

### Limitations
- The 1.5·IQR rule is a convention — not a probabilistic threshold. For very large or very small samples, alternative criteria (e.g. 3·IQR for "extreme" outliers) may be more informative.
- For grouped data given as a frequency table, the same logic applies once $Q_1$ and $Q_3$ are computed from the ogive.

![IQR fences on boxplot and number line](statistics/images/theory/th_g6c_ai.png)
"""

# For each subtopic: theory + dict {col_idx: [ex_ids]} (col 2 = first exercise of set 0 = Ex1 of set 0; etc.)
# Columns indexing: col 2 = "Ex 0 / 1", col 3 = "Ex 0 / 2", col 4 = "Ex 1.1", col 5 = "Ex 1.2",
# col 6 = "Ex 1.3", col 7 = "Ex 1.4", col 8 = "Ex 1.5", col 9 = "Ex 1.6".

# Helper to build subtopic content dicts.
def sub(group, sid, sname, theory_id, theory_title, theory_content, columns,
        theory_images=None):
    return {"group": group, "sid": sid, "sname": sname,
            "theory": (theory_id, theory_title, theory_content),
            "theory_images": theory_images or [],
            "columns": columns}

SUBTOPICS = [
  # ===== G1 — Plotting =====
  sub("G1", "g1a_pie", "Pie chart", "th_g1a", "Theory — Pie chart", T_G1A_PIE, {
        4: ["1_1a"], 5: ["1_2d"], 6: ["1_3c"],
  }),
  sub("G1", "g1b_bar", "Bar plot", "th_g1b", "Theory — Bar plot", T_G1B_BAR, {
        4: ["1_1b"],
  }),
  sub("G1", "g1c_hist", "Histogram", "th_g1c", "Theory — Histogram", T_G1C_HIST, {
        2: ["ex1a", "ex1c", "ex1e", "ex1i"],
        4: ["1_1c", "1_1d"],
        6: ["1_3a", "1_3g", "1_3h"],
        8: ["1_5g"],
        12: ["2_3c"],
        14: ["2_5g"],
        15: ["2_6a3"],
  }, theory_images=["statistics/images/theory/th_g1c_ai.png"]),
  sub("G1", "g1d_spike", "Spike plot", "th_g1d", "Theory — Spike plot", T_G1D_SPIKE, {
        3: ["ex2b2"],
        5: ["1_2c"],
        6: ["1_3e"],
        7: ["1_4a1"],
  }),
  sub("G1", "g1e_cum", "Cumulative plots (ogive + step)",
      "th_g1e", "Theory — Cumulative plots", T_G1E_CUM, {
        3: ["ex2a3", "ex2b3"],
        7: ["1_4a2"],
        8: ["1_5d"],
        13: ["2_4a"],         # Ex 2.4a — identify the ogive
        16: ["2_7a"],         # Ex 2.7a — reading ogive of Nr_visits
  }, theory_images=["statistics/images/theory/th_g1e_ai.png"]),
  # ===== G2 — Proportions =====
  sub("G2", "g2a_exact", "Exact proportions",
      "th_g2a", "Theory — Exact proportions", T_G2A_EXACT, {
        2: ["ex1f", "ex1h"],
        3: ["ex2a1", "ex2b1"],
        4: ["1_1e", "1_1g"],
        5: ["1_2a"],
        6: ["1_3b"],
        8: ["1_5b"],
        14: ["2_5c"],         # Ex 2.5c — proportions in overlapping age intervals
        15: ["2_6b"],         # Ex 2.6b — products sold below cost
  }, theory_images=["statistics/images/theory/th_g2a_ai.png"]),
  sub("G2", "g2b_approx", "Uniform-on-interval approximation",
      "th_g2b", "Theory — Uniform-on-interval", T_G2B_APPROX, {
        2: ["ex1g"],
        3: ["ex2a2"],
        8: ["1_5a", "1_5c"],
        11: ["2_2a", "2_2a1"], # Ex 2.2a and 2.2a1
  }, theory_images=["statistics/images/theory/th_g2b_ai.png"]),
  # ===== G3 — Derived variables =====
  sub("G3", "g3_main", "Constructing derived variables",
      "th_g3", "Theory — Constructing derived variables", T_G3_DERIVED, {
        2: ["ex1b", "ex1d"],
        17: ["2_8a"],         # Ex 2.8a — Margin_perc
  }, theory_images=["statistics/images/theory/th_g3_ai.png"]),
  # ===== G4 — Central tendency =====
  sub("G4", "g4a_bytype", "Mode, median, mean by variable type",
      "th_g4a", "Theory — Choosing mode/median/mean by variable type", T_G4A_BYTYPE, {
        4: ["1_1f"],
        5: ["1_2b"],
        6: ["1_3d"],
        7: ["1_4a3"],
        8: ["1_5e"],
        11: ["2_2b"],  # Ex 2.2b modal class
  }),
  sub("G4", "g4b_skew", "Mean vs median under skewness",
      "th_g4b", "Theory — Mean vs median under skewness", T_G4B_SKEW, {
        4: ["1_1h"],
        6: ["1_3f", "1_3i"],
        9: ["1_6a"],
        14: ["2_5f"],         # Ex 2.5f — Age mean vs median
  }, theory_images=["statistics/images/theory/th_g4b_ai.png"]),
  sub("G4", "g4c_grouped", "Approximate mean & median from grouped data",
      "th_g4c", "Theory — Approximate mean & median (grouped data)", T_G4C_GROUPED, {
        4: ["1_1i"],
        8: ["1_5f"],
        9: ["1_6b"],
  }, theory_images=["statistics/images/theory/th_g4c_ai.png"]),
  sub("G4", "g4d_compare", "Cross-subgroup / period comparison",
      "th_g4d", "Theory — Cross-subgroup comparison", T_G4D_COMPARE, {
        7: ["1_4b"],
        8: ["1_5h"],
  }, theory_images=["statistics/images/theory/th_g4d_ai.png"]),
  # ===== G5 — Dispersion =====
  sub("G5", "g5_disp", "Dispersion measures (range, IQR, var, SD, CV)",
      "th_g5", "Theory — Dispersion measures", T_G5_DISP, {
        10: ["2_1a", "2_1g"],
        11: ["2_2c", "2_2f"],
        12: ["2_3a"],
        13: ["2_4d"],
        15: ["2_6c"],
        16: ["2_7d", "2_7f"],
        17: ["2_8c"],
  }, theory_images=["statistics/images/theory/th_g5_ai.png"]),
  # ===== G6 — Quantiles, boxplots, outliers =====
  sub("G6", "g6a_quant", "Quantiles, percentiles, deciles",
      "th_g6a", "Theory — Quantiles and percentiles", T_G6A_QUANT, {
        10: ["2_1f"],
        11: ["2_2d", "2_2e"],
        13: ["2_4c"],
        14: ["2_5b"],
        15: ["2_6a1"],
        16: ["2_7b", "2_7c"],
        17: ["2_8d"],
  }, theory_images=["statistics/images/theory/th_g6a_ai.png"]),
  sub("G6", "g6b_box", "Boxplots and the 5-number summary",
      "th_g6b", "Theory — Boxplots and 5-number summary", T_G6B_BOX, {
        10: ["2_1b", "2_1c", "2_1e"],
        14: ["2_5a", "2_5d"],
        15: ["2_6a2"],
        16: ["2_7e"],
        17: ["2_8b"],
  }, theory_images=["statistics/images/theory/th_g6b_ai.png"]),
  sub("G6", "g6c_outliers", "Outliers and extreme values",
      "th_g6c", "Theory — Outliers", T_G6C_OUT, {
        10: ["2_1d"],
        12: ["2_3b", "2_3c"],
        13: ["2_4b"],
  }, theory_images=["statistics/images/theory/th_g6c_ai.png"]),
  # ===== G7 — Two-way tables / conditional distributions (qualitative × qualitative) =====
  sub("G7", "g7_twoway", "Two-way tables: joint, marginal, conditional",
      "th_g7", "Theory — Two-way tables and independence", T_G7_BIVQUAL, {
        18: ["3_1b"],
        19: ["3_2c", "3_2e", "3_2g", "3_2h"],
        23: ["3_6a", "3_6b", "3_6c", "3_6d", "3_6e", "3_6f", "3_6g"],
        24: ["3_7a1", "3_7a3"],
        26: ["3_9a1"],
        27: ["3_10a1", "3_10a2"],
        29: ["3_12a", "3_12b", "3_12c"],
  }, theory_images=["statistics/images/theory/th_g7_ai.png"]),
  # ===== G8 — Conditional summary measures (qualitative × numerical) =====
  sub("G8", "g8_condsumm", "Conditional summary measures",
      "th_g8", "Theory — Conditional summary measures", T_G8_CONDSUMM, {
        18: ["3_1a", "3_1c", "3_1d"],
        19: ["3_2a", "3_2b", "3_2d", "3_2f", "3_2i", "3_2m"],
        21: ["3_4a1", "3_4a2", "3_4b"],
        22: ["3_5a1", "3_5a2"],
        24: ["3_7b1"],
        25: ["3_8a"],
        26: ["3_9b", "3_9c"],
        28: ["3_11a", "3_11b"],
  }, theory_images=["statistics/images/theory/th_g8_ai.png"]),
  # ===== G9 — Covariance, correlation, scatter =====
  sub("G9", "g9_corr", "Covariance, correlation and scatter",
      "th_g9", "Theory — Covariance and correlation", T_G9_COVCOR, {
        18: ["3_1e"],
        19: ["3_2l"],
        20: ["3_3a"],
        28: ["3_11c"],
  }, theory_images=["statistics/images/theory/th_g9_ai.png"]),
  # ===== G10 — Normal distribution =====
  sub("G10", "g10_normal", "Normal distribution",
      "th_g10", "Theory — Normal distribution", T_G10_NORMAL, {
        30: ["4_1a", "4_1b"],         # Ex 4.1
        31: ["4_2a", "4_2b"],         # Ex 4.2
        32: ["4_3a", "4_3b", "4_3c"], # Ex 4.3
  }, theory_images=["statistics/images/theory/th_g10_ai.png"]),
  # ===== G11 — Sampling distributions and CLT =====
  sub("G11", "g11_clt", "Sampling distributions and CLT",
      "th_g11", "Theory — Sampling distributions / CLT", T_G11_SAMP, {
        32: ["4_3d"],
        33: ["4_4a1", "4_4b"],   # Ex 4.4
        38: ["4_9a", "4_9b", "4_9c"],
        39: ["4_10a", "4_10b"],
        41: ["4_12a", "4_12b"],
        42: ["4_13a"],
  }, theory_images=["statistics/images/theory/th_g11_ai.png"]),
  # ===== G12 — Linear combinations of random variables =====
  sub("G12", "g12_lincomb", "Linear combinations of normals",
      "th_g12", "Theory — Linear combinations", T_G12_LINCOMB, {
        34: ["4_5"],
        35: ["4_6"],
        36: ["4_7"],
        37: ["4_8a"],
        40: ["4_11a"],
  }, theory_images=["statistics/images/theory/th_g12_ai.png"]),
  # ===== G13 — Confidence intervals (split into 5 subtopics) =====
  sub("G13", "g13a_ci_one_mean", "CI for one mean (known/unknown σ)",
      "th_g13a", "Theory — Confidence intervals", T_G13_CI, {
        43: ["5_1a","5_1b","5_2a","5_2b"],
        45: ["5_8a","5_8b","5_10b"],
        46: ["6_1a","6_1b","6_1d"],
        48: ["6_3a"],
        52: ["6_8a1"],
        57: ["6_13d"],
        61: ["5_3a","5_3b"],
  }, theory_images=["statistics/images/theory/th_g13_ai.png"]),
  sub("G13", "g13b_ci_one_prop", "CI for one proportion + sample-size planning",
      "th_g13a", "Theory — Confidence intervals", T_G13_CI, {
        44: ["5_6b"],
        45: ["5_13a3"],
        46: ["6_1c"],
        48: ["6_3b1","6_3b2"],
        50: ["6_6a","6_6b","6_6c","6_6d"],
        51: ["6_7a"],
        56: ["6_12a","6_12b"],
        57: ["6_13a"],
  }),
  sub("G13", "g13c_ci_diff_means", "CI for difference of two means (independent)",
      "th_g13a", "Theory — Confidence intervals", T_G13_CI, {
        43: ["5_1f"],
        44: ["5_6a","5_7a","5_7b"],
        48: ["6_3d"],
        49: ["6_4a"],
        52: ["6_8a2","6_8b","6_8d"],
        54: ["6_10a"],
        59: ["6_15a","6_15b"],
  }),
  sub("G13", "g13d_ci_diff_prop", "CI for difference of two proportions",
      "th_g13a", "Theory — Confidence intervals", T_G13_CI, {
        43: ["5_5a","5_5b"],
        48: ["6_3c"],
        50: ["6_6e"],
        53: ["6_9a"],
        58: ["6_14a"],
        70: ["5_13b"],
  }),
  sub("G13", "g13e_ci_paired", "Paired CI (matched observations)",
      "th_g13a", "Theory — Confidence intervals", T_G13_CI, {
        44: ["5_4","5_6d"],
        47: ["6_2a","6_2b"],
        52: ["6_8c1"],
        55: ["6_11a"],
        60: ["6_17a","6_18b"],
  }),
  sub("G13", "g13f_estimation", "Unbiased estimators and sampling SE",
      "th_g13a", "Theory — Confidence intervals", T_G13_CI, {
        45: ["5_13a1","5_13a2"],
  }),
  # ===== G14 — Hypothesis tests (split into 5 subtopics) =====
  sub("G14", "g14a_one_sample", "One-sample tests (mean & proportion)",
      "th_g14", "Theory — Hypothesis tests", T_G14_HT, {
        62: ["7_1a","7_1c"],
        67: ["7_8a"],
  }, theory_images=["statistics/images/theory/th_g14_ai.png"]),
  sub("G14", "g14b_two_sample", "Two-sample tests (means & proportions, independent)",
      "th_g14", "Theory — Hypothesis tests", T_G14_HT, {
        62: ["7_3a","7_3b"],
        64: ["7_5a"],
        66: ["7_7a"],
        69: ["7_10a"],
  }),
  sub("G14", "g14c_paired", "Paired tests",
      "th_g14", "Theory — Hypothesis tests", T_G14_HT, {
        65: ["7_6a","7_6b"],
  }),
  sub("G14", "g14d_chi_squared", "Chi-squared (goodness-of-fit + independence)",
      "th_g14", "Theory — Hypothesis tests", T_G14_HT, {
        63: ["7_4a","7_4b"],
        66: ["7_7b"],
        68: ["7_9a","7_9b"],
  }),
  sub("G14", "g14e_power", "Power, Type II error & sample-size effects",
      "th_g14", "Theory — Hypothesis tests", T_G14_HT, {
        62: ["7_1b"],
  }),
  # ===== Past-exam sub-parts: distribute by topic_hint =====
  # ===== G15 — Linear regression (split into 5 subtopics) =====
  sub("G15", "g15a_simple_reg", "Simple regression: estimation, R², slope test",
      "th_g15", "Theory — Linear regression", T_G15_REG, {
        71: ["8_1a","8_1b"],
        72: ["8_2a"],
        73: ["8_3a"],
        75: ["8_5a","8_8a"],
  }, theory_images=["statistics/images/theory/th_g15_ai.png"]),
  sub("G15", "g15b_prediction", "Prediction intervals & CI for the mean response",
      "th_g15", "Theory — Linear regression", T_G15_REG, {
        71: ["8_1c"],
        72: ["8_2b"],
        75: ["8_10a"],
  }),
  sub("G15", "g15c_multi_reg", "Multiple regression",
      "th_g15", "Theory — Linear regression", T_G15_REG, {
        76: ["9_1","9_2"],
        77: ["9_3"],
        78: ["9_4"],
        81: ["9_8"],
        83: ["9_10"],
        84: ["9_11"],
        85: ["9_12"],
        86: ["9_13"],
  }),
  sub("G15", "g15d_categorical", "Categorical predictors / dummies / interactions",
      "th_g15", "Theory — Linear regression", T_G15_REG, {
        79: ["9_5"],
        80: ["9_6","9_7"],
        82: ["9_9"],
  }),
  sub("G15", "g15e_diagnostics", "Residual diagnostics & multicollinearity",
      "th_g15", "Theory — Linear regression", T_G15_REG, {
        74: ["8_4a"],
  }),
]

TOPIC_META = {
    "G13": ("t_g13_ci",         "G13 — Confidence intervals"),
    "G14": ("t_g14_tests",      "G14 — Hypothesis tests"),
    "G15": ("t_g15_regression", "G15 — Linear regression"),
    "G10": ("t_g10_normal", "G10 — Normal distribution"),
    "G11": ("t_g11_clt",    "G11 — Sampling distributions and CLT"),
    "G12": ("t_g12_lincomb","G12 — Linear combinations of RVs"),
    "G7": ("t_g7_twoway",   "G7 — Two-way tables (bivariate qualitative)"),
    "G8": ("t_g8_condsumm", "G8 — Conditional summary measures"),
    "G9": ("t_g9_corr",     "G9 — Covariance, correlation, scatter"),
    "G5": ("t_g5_dispersion", "G5 — Dispersion measures"),
    "G6": ("t_g6_quantiles_box", "G6 — Quantiles, boxplots, outliers"),
    "G1": ("t_g1_plots", "G1 — Graphical representation of distributions"),
    "G2": ("t_g2_proportions", "G2 — Proportions from frequency tables"),
    "G3": ("t_g3_derived_vars", "G3 — Constructing derived variables"),
    "G4": ("t_g4_central_tendency", "G4 — Central tendency measures"),
}

# =====================================================================
# Assemble — multi-topic / multi-subtopic + explicit table-layout metadata.
# The Statistics subject in the app uses a custom TABLE renderer (not the
# canvas), keyed off:
#   - data.topics[].subtopics[] = ROWS (one per subtopic; topics group the rows)
#   - node.column field        = COLUMN index (1=Theory, 2=Ex0/Q1, 3=Ex0/Q2,
#                                  4=Ex1.1, 5=Ex1.2, ..., 9=Ex1.6)
#   - data.tableLayout         = column header labels (for the table thead)
# Multiple nodes with the same (subtopic, column) stack vertically in the cell.
# =====================================================================

COLUMN_HEADERS = [
    {"col": 1, "label": "Theory"},
    {"col": 100, "label": "Master Exam Ready (consolidated)"},
    {"col": 2, "label": "Ex 0 / Q1 (USA states)"},
    {"col": 3, "label": "Ex 0 / Q2 (Titanic)"},
    {"col": 4, "label": "Ex 1.1 (pizzerie)"},
    {"col": 5, "label": "Ex 1.2 (DS)"},
    {"col": 6, "label": "Ex 1.3 (customer_habits)"},
    {"col": 7, "label": "Ex 1.4 (Quantity_New)"},
    {"col": 8, "label": "Ex 1.5 (Time)"},
    {"col": 9, "label": "Ex 1.6 (Expenses)"},
    {"col": 10, "label": "Ex 2.1 (pizzerie)"},
    {"col": 11, "label": "Ex 2.2 (pupils hours)"},
    {"col": 12, "label": "Ex 2.3 (DS amount)"},
    {"col": 13, "label": "Ex 2.4 (insurance ogive)"},
    {"col": 14, "label": "Ex 2.5 (customer Age)"},
    {"col": 15, "label": "Ex 2.6 (Revenue)"},
    {"col": 16, "label": "Ex 2.7 (Nr_visits)"},
    {"col": 17, "label": "Ex 2.8 (Margin_perc)"},
    {"col": 18, "label": "Ex 3.1 (pizzerie SmokingArea)"},
    {"col": 19, "label": "Ex 3.2 (DS AmountSpent)"},
    {"col": 20, "label": "Ex 3.3 (Satisfaction corr.)"},
    {"col": 21, "label": "Ex 3.4 (Services EXPENSES)"},
    {"col": 22, "label": "Ex 3.5 (TotUsers × Weather)"},
    {"col": 23, "label": "Ex 3.6 (Country × Sex)"},
    {"col": 24, "label": "Ex 3.7 (Product × Sex)"},
    {"col": 25, "label": "Ex 3.8 (Quantity | Product)"},
    {"col": 26, "label": "Ex 3.9 (LoL tier × class)"},
    {"col": 27, "label": "Ex 3.10 (Company Prod × Channel)"},
    {"col": 28, "label": "Ex 3.11 (Campaign Loyalty)"},
    {"col": 29, "label": "Ex 3.12 (Effectiveness × Channel)"},
    {"col": 30, "label": "Ex 4.1 (tea X~N)"},
    {"col": 31, "label": "Ex 4.2 (battery X~N)"},
    {"col": 32, "label": "Ex 4.3 (private label X~N)"},
    {"col": 33, "label": "Ex 4.4 (delivery + CLT)"},
    {"col": 34, "label": "Ex 4.5 (sum normals)"},
    {"col": 35, "label": "Ex 4.6 (sum/diff normals)"},
    {"col": 36, "label": "Ex 4.7 (covariance, joint)"},
    {"col": 37, "label": "Ex 4.8 (bivariate normal G)"},
    {"col": 38, "label": "Ex 4.9 (pizzeria sample mean)"},
    {"col": 39, "label": "Ex 4.10 (AmountSpent CLT)"},
    {"col": 40, "label": "Ex 4.11 (ad cost linear)"},
    {"col": 41, "label": "Ex 4.12 (sample mean + prop)"},
    {"col": 42, "label": "Ex 4.13 (lincomb + CLT prop)"},
    {"col": 43, "label": "Ex 5.1-5.2 (CI mean / SE)"},
    {"col": 44, "label": "Ex 5.4-5.7 (paired/diff CI)"},
    {"col": 45, "label": "Ex 5.8-5.10 (Salary, Profitability)"},
    {"col": 46, "label": "Ex 6.1 (restocking)"},
    {"col": 47, "label": "Ex 6.2 (vgsales NA-EU paired)"},
    {"col": 48, "label": "Ex 6.3 (Salary, Female prop)"},
    {"col": 49, "label": "Ex 6.4 (pooled CI)"},
    {"col": 50, "label": "Ex 6.6 (proportion CIs)"},
    {"col": 51, "label": "Ex 6.7 (DS platform prop)"},
    {"col": 52, "label": "Ex 6.8 (Developers_ITA skills)"},
    {"col": 53, "label": "Ex 6.9 (diff prop)"},
    {"col": 54, "label": "Ex 6.10 (JP_Sales genre)"},
    {"col": 55, "label": "Ex 6.11 (paired summary)"},
    {"col": 56, "label": "Ex 6.12 (Adventure / Shooter)"},
    {"col": 57, "label": "Ex 6.13 (CI for proportion 108/140)"},
    {"col": 58, "label": "Ex 6.14 (EA vs Activision)"},
    {"col": 59, "label": "Ex 6.15 (pooled vs Welch)"},
    {"col": 60, "label": "Ex 6.17-6.18 (NA-EU paired)"},
    {"col": 61, "label": "Ex 5.3-5.5 (proportion CI/test)"},
    {"col": 62, "label": "Ex 7.1 (regression NewHired)"},
    {"col": 63, "label": "Ex 7.4 (two-prop test vgsales)"},
    {"col": 64, "label": "Ex 7.5 (chi-squared)"},
    {"col": 65, "label": "Ex 7.6 (one-prop test + p-value)"},
    {"col": 66, "label": "Ex 7.7 (Developers AI)"},
    {"col": 67, "label": "Ex 7.8 (stratified t-test)"},
    {"col": 68, "label": "Ex 7.9 (chi-sq Children / Age)"},
    {"col": 69, "label": "Ex 7.10 (pooled t-stat)"},
    {"col": 70, "label": "Ex 7.3 (AmountSpent by Sex)"},
    {"col": 71, "label": "Ex 8.1 (Debt~TV)"},
    {"col": 72, "label": "Ex 8.2 (AmountSpent~Salary)"},
    {"col": 73, "label": "Ex 8.3 (Weeks~Age)"},
    {"col": 74, "label": "Ex 8.4 (restaurants~surface)"},
    {"col": 75, "label": "Ex 8.5-8.10 (manual regression)"},
    {"col": 76, "label": "Ex 9.1-9.2 (Baseball)"},
    {"col": 77, "label": "Ex 9.3 (Competition)"},
    {"col": 78, "label": "Ex 9.4 (superstore)"},
    {"col": 79, "label": "Ex 9.5 (restaurants multi)"},
    {"col": 80, "label": "Ex 9.6-9.7 (MBA)"},
    {"col": 81, "label": "Ex 9.8 (Lotteries)"},
    {"col": 82, "label": "Ex 9.9 (GS salary)"},
    {"col": 83, "label": "Ex 9.10 (Severance)"},
    {"col": 84, "label": "Ex 9.11 (Absence)"},
    {"col": 85, "label": "Ex 9.12 (Visitors time series)"},
    {"col": 86, "label": "Ex 9.13 (Loans credit)"},
    {"col": 87, "label": "Past exam — 1st partial 2024"},
    {"col": 88, "label": "Past exam — 1st partial 2025"},
    {"col": 89, "label": "Past exam — 1st partial 2026"},
    {"col": 90, "label": "Past exam — general 1 2024"},
    {"col": 91, "label": "Past exam — general 1 2025"},
    {"col": 92, "label": "Past exam — general 1 2026"},
    {"col": 93, "label": "Past exam — general 2 2024"},
    {"col": 94, "label": "Past exam — general 2 2025"},
    {"col": 95, "label": "Past exam — general 2 2026"},
    {"col": 96, "label": "Past exam — July 2024"},
    {"col": 97, "label": "Past exam — July 2025"},
    {"col": 98, "label": "Past exam — September 2024"},
    {"col": 99, "label": "Past exam — September 2025"},
]

# ---------------------------------------------------------------------
# Inject past-exam sub-parts into the SUBTOPICS list (yellow cards).
# ---------------------------------------------------------------------
PAST_EXAM_COL = {
    "exam_p1_2024_": 87, "exam_p1_2025_": 88, "exam_p1_2026_": 89,
    "exam_g1_2024_": 90, "exam_g1_2025_": 91, "exam_g1_2026_": 92,
    "exam_g2_2024_": 93, "exam_g2_2025_": 94, "exam_g2_2026_": 95,
    "exam_july_2024_": 96, "exam_july_2025_": 97,
    "exam_sep_2024_": 98,  "exam_sep_2025_":  99,
}
TOPIC_HINT_MAP = {
    "G1":  "g1c_hist",  "G2":  "g2a_exact",   "G3":  "g3_main",
    "G4":  "g4a_bytype","G5":  "g5_disp",     "G6":  "g6b_box",
    "G7":  "g7_twoway", "G8":  "g8_condsumm", "G9":  "g9_corr",
    "G10": "g10_normal","G11": "g11_clt",     "G12": "g12_lincomb",
    "G13": "g13a_ci_one_mean", "G14": "g14a_one_sample", "G15": "g15a_simple_reg",
}
def _col_for_exam_id(eid):
    for prefix, col in PAST_EXAM_COL.items():
        if eid.startswith(prefix):
            return col
    return None
for eid, d in past_exams.items():
    col = _col_for_exam_id(eid)
    if col is None: continue
    hint = d.get("topic_hint", "G1")
    sub_target = TOPIC_HINT_MAP.get(hint, "g1c_hist")
    for stm in SUBTOPICS:
        if stm["sid"] == sub_target:
            stm["columns"].setdefault(col, []).append(eid)
            break

# Inject master exercises into column 100 of their owning subtopic.
for stm in SUBTOPICS:
    if stm["sid"] in master_exercises:
        stm["columns"].setdefault(100, []).append(f"master_{stm['sid']}")

topics_out = {}
total_nodes_count = 0
md_table_count = 0  # how many tables we converted (for the build summary)
for stm in SUBTOPICS:
    group = stm["group"]
    if group not in topics_out:
        tid, tname = TOPIC_META[group]
        topics_out[group] = {"id": tid, "name": tname, "subtopics": []}

    sid = stm["sid"]; sname = stm["sname"]
    th_id, th_title, th_content = stm["theory"]
    columns = stm["columns"]
    color = SUBTOPIC_COLOR[group]

    nodes_in_subtopic = []
    # Theory node (column 1)
    theory_links = [eid for col_items in columns.values() for eid in col_items]
    converted_th_content = md_tables_to_latex(code_blocks_to_inline(th_content))
    if "\\begin{tabular}" in converted_th_content and "\\begin{tabular}" not in th_content:
        md_table_count += converted_th_content.count("\\begin{tabular}")
    th_node = node(th_id, th_title, converted_th_content,
                   COL_X[0], TOP_Y, "yellow", w=SNIPPET_W, h=H_THEORY,
                   links=theory_links, images=stm.get("theory_images", []))
    th_node["column"] = 1
    nodes_in_subtopic.append(th_node)

    # Exercise nodes — column N comes from the column index used in TOPIC_COLUMNS
    for col_idx in sorted(columns.keys()):
        items = columns[col_idx]
        if not items:
            continue
        x = COL_X[col_idx - 1]
        cy = TOP_Y
        for ex_id in items:
            d = ALL[ex_id]
            converted_content = md_tables_to_latex(code_blocks_to_inline(d["content"]))
            new_tables = (converted_content.count("\\begin{tabular}")
                          - d["content"].count("\\begin{tabular}"))
            md_table_count += max(0, new_tables)
            n_node = node(ex_id, d["title"], converted_content,
                          x, cy, color, w=SNIPPET_W, h=H(d),
                          links=[th_id], images=d.get("images", []))
            n_node["column"] = col_idx
            if d.get("is_exam"):
                n_node["isExam"] = True   # consumed by renderStatsTable for yellow class
            nodes_in_subtopic.append(n_node)
            cy += H(d) + SNIPPET_GAP

    topics_out[group]["subtopics"].append({
        "id": sid, "name": sname, "nodes": nodes_in_subtopic,
    })
    total_nodes_count += len(nodes_in_subtopic)

topics_list = [topics_out[g] for g in ("G1", "G2", "G3", "G4", "G5", "G6",
                                       "G7", "G8", "G9", "G10", "G11", "G12",
                                       "G13", "G14", "G15") if g in topics_out]

output = {
    "version": "2.0",
    "exportedAt": int(time.time() * 1000),
    "data": {
        "topics": topics_list,
        "trash": [],
        "tableLayout": {
            "subject": "statistics",
            "columns": COLUMN_HEADERS,
        },
    },
}

with open(OUT, "w", encoding="utf-8") as f:
    json.dump(output, f, ensure_ascii=False, indent=2)

# Post-write sanity: re-read and confirm it parses + has no merge-conflict
# markers. Catches accidental corruption before the file ships to the website.
with open(OUT, "r", encoding="utf-8") as f:
    _raw = f.read()
import re as _re
if _re.search(r"^(?:<{7} |={7}$|>{7} )", _raw, _re.M):
    raise SystemExit(f"build_snippets: {OUT} contains merge-conflict markers — aborting")
try:
    _check = json.loads(_raw)
    assert _check.get("data", {}).get("topics"), "no topics array"
except Exception as _e:
    raise SystemExit(f"build_snippets: {OUT} did not round-trip as JSON: {_e}")

print(f"Wrote {OUT}")
print(f"Total nodes: {total_nodes_count}")
print(f"Topics: {len(topics_list)}; subtopics (= rows): {sum(len(t['subtopics']) for t in topics_list)}")
print(f"Columns: {len(COLUMN_HEADERS)}")
print()
for t in topics_list:
    print(f"=== {t['name']} ===")
    for s in t["subtopics"]:
        cols = sorted({n.get('column', 1) for n in s['nodes']})
        print(f"  - {s['name']}: {len(s['nodes'])} nodes, columns used = {cols}")
