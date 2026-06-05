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

COL_X = [200, 900, 1700, 2500, 3300, 4100, 4900, 5700, 6500, 7300]
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

A **pie chart** displays the relative frequencies of a **qualitative variable** as proportional slices of a disk.

**When to use:**
- Qualitative (categorical) variables, especially **nominal** ones with few categories (≤5).
- Highlights relative weight of categories; absolute and relative frequency representations look identical.
- Useful when the *ordering* of categories should not be implied (the disk has no axis).

**When NOT to use:**
- Variables with many categories — slices become unreadable.
- **Ordinal** variables — pie loses the natural order; prefer a bar plot.
- Continuous or discrete numerical variables — use a histogram or spike plot.

**R commands:**
```r
distr.plot.x(x=Var, plot.type="pie", freq="perc", data=DF)
distr.table.x(x=Var, freq=c("count","perc"), data=DF)
```

The pie does not depend on whether absolute or relative frequencies are used — slice areas are the same.
"""

T_G1B_BAR = """## Bar plot

A **bar plot** uses bars of equal width whose **height** is proportional to the (absolute, relative or percentage) frequency of each category.

**When to use:**
- Qualitative variables, especially **ordinal** ones — categories are plotted along the x-axis in their natural order, allowing direct visual comparison.
- Nominal variables when you want to compare magnitudes precisely (the disk shape of a pie chart makes comparison harder).

**Cautions:**
- For **nominal** variables, the default R ordering is alphabetical; do not draw conclusions from bar position.
- Bar plot heights all carry the same meaning whether absolute, relative, or percentage frequencies are used — the *shape* of the chart is identical.
- For **ordered** variables, define the factor explicitly with `levels=` so categories appear in the correct sequence.

**R commands:**
```r
distr.plot.x(x=Var, plot.type="bars", data=DF)
# Ordering an ordinal variable first:
DF$Var_recode <- factor(DF$Var, levels=c("None","Low","Medium","High"))
distr.plot.x(x=DF$Var_recode, plot.type="bars")
```
"""

T_G1C_HIST = """## Histogram (continuous variables)

A **histogram** represents the distribution of a **continuous numerical** variable. Each bar spans a class interval $[a_i, b_i)$, and its **area** equals the relative frequency $f_i$ of that class.

### Equal-width classes
With constant width $w$, plotting *frequency on the y-axis* is fine because bar areas are proportional to bar heights.

### Unequal-width classes — the density rule (compulsory)
Whenever class widths differ ($w_i \\ne w_j$), the y-axis must be **density**:
$$
d_i = \\frac{f_i}{w_i}, \\qquad \\text{area of bar }= f_i.
$$
Without this correction, wider classes look more important than they are.

### Choice of number / boundaries of classes
- **Too few classes** (e.g. 5) hide structure (peaks, skewness, tails blur).
- **Too many classes** (e.g. 20–30) introduce noise, especially in sparse tails.
- A **custom binning** — narrow widths in the bulk, broad widths in the tail — is often the best compromise.

### Comparing distributions across subgroups
Plot two histograms on the same x-axis and use **densities** (so that subsamples of unequal size are visually comparable).

**R commands:**
```r
distr.plot.x(x=Var, plot.type="hist", breaks=15, data=DF)
distr.plot.x(x=Var, plot.type="hist", breaks=c(0,10,20,40,60,100), data=DF)
distr.plot.x(x=Var[group=="A"], plot.type="hist", breaks=B, data=DF)
distr.plot.x(x=Var[group=="B"], plot.type="hist", breaks=B, data=DF)
```
"""

T_G1D_SPIKE = """## Spike plot (discrete numerical variables)

A **spike plot** ("spike diagram" / "lollipop") draws a vertical stick at each observed integer (or each discrete value), with height proportional to the absolute or relative frequency.

**Why not a histogram or a bar plot?**
- A **histogram** is wrong for a discrete numerical variable — values between two consecutive integers are not possible, so a continuous bar of positive width would suggest mass that does not exist.
- A **bar plot** is wrong in general because it treats all categories as equally spaced — but for a discrete numerical variable the distance between values is meaningful (skipping a value should leave a visible gap).
- The spike plot honours both constraints: zero-width sticks at the actual values, with empty gaps for missing integers.

**Acceptable substitute:** a bar plot is acceptable only when consecutive integer values are present with no gaps and the spacing is uniform.

**R commands:**
```r
distr.plot.x(x=Var, plot.type="spike", freq=c("counts"), data=DF)
distr.plot.x(x=Var, plot.type="spike", freq=c("perc"),  data=DF)
distr.table.x(x=Var, freq=c("counts","perc"), data=DF)
```
"""

T_G1E_CUM = """## Cumulative plots: ogive and step diagram

The **cumulative distribution** $F(x)$ at a point $x$ is the proportion of observations $\\le x$. Its graphical form depends on whether the variable is continuous or discrete.

### Ogive — continuous variable presented in classes
Plot the points $(b_i, F_i)$ where $b_i$ is the upper bound of class $i$ and $F_i$ the cumulative relative frequency at that bound. Connect with **straight lines** starting at $(a_1, 0)$. Between two kinks the curve is straight: its slope on class $i$ equals the **density** $d_i = f_i/w_i$.

Use the ogive to **estimate quantiles** (median, percentiles) — under the uniform-on-interval assumption,
$$
p_q \\approx a_i + \\frac{q - F_{i-1}}{d_i}, \\qquad q = 0.5\\text{ for the median}.
$$

### Step diagram — discrete numerical variable
Cumulative frequency is **constant between consecutive observed values** and **jumps by $f_i$** at each observed value $x_i$. No interpolation: the curve has horizontal flats and vertical jumps.

**R commands:**
```r
distr.plot.x(x=Var, plot.type="cumfreq", data=DF)    # ogive (continuous)
distr.plot.x(x=Var, plot.type="cumfreq", data=DF)    # step diagram (discrete)
distr.table.x(x=Var, freq="cum", data=DF)
```
"""

T_G2A_EXACT = """## Exact proportions from frequency tables

When a frequency table is given, computing a proportion of a subset is **exact** in two cases:

1. **Discrete** or **categorical** variable: simply sum the relative frequencies of the values/categories in the query set.
2. **Continuous** variable in classes, and the query interval $[L, U)$ is a **union of full classes** (i.e. $L, U$ coincide with class boundaries). Then $P(L \\le X < U) = \\sum_i f_i$ over the included classes.

Two equivalent computational shortcuts when answering from raw data instead of a table:
```r
# 1) Boolean vector + mean (counts TRUEs / total)
mean(DF$Var >= L & DF$Var < U)
# 2) Use the cumulative distribution table
distr.table.x(x=Var, freq="cum", data=DF)
```

**Cumulative table primitives:**
```r
distr.table.x(x=Var, freq=c("counts","prop","cum"), data=DF)
```
Cumulative proportion $F_i$ at the boundary $b_i$ is read directly from the `Cum.Prop` column.

**Why exact?** Because we know the count in each full class exactly — we are not interpolating inside any class.
"""

T_G2B_APPROX = """## Approximate proportions: uniform-on-interval

When the query interval $[L, U)$ has endpoints that fall **inside** a class $[a_i, b_i)$ (not at a boundary), the proportion of cases in $[L, U) \\cap [a_i, b_i)$ is unknown from the table alone. The standard assumption is:

> **Uniform on the interval:** the $f_i$ units of mass are evenly distributed across $[a_i, b_i)$, so the density is constant and equal to $d_i = f_i / w_i$.

Under this assumption:
$$
P\\big(X \\in [L, U) \\cap [a_i, b_i)\\big) \\approx d_i \\cdot \\text{length}\\big([L, U) \\cap [a_i, b_i)\\big) = f_i \\cdot \\frac{\\text{overlap}}{w_i}.
$$

**Working recipe.** Split the query into full classes ∪ partial classes; for each partial piece apply density × overlap; sum.

**Example.** $P(15 \\le \\text{Time} \\le 50)$ when classes are $[10,20),\\,[20,30),\\,[30,60)$:
$$
\\underbrace{\\tfrac{1}{2}\\cdot f_{[10,20)}}_{\\text{half of }[10,20)} \\;+\\; \\underbrace{f_{[20,30)}}_{\\text{full}} \\;+\\; \\underbrace{\\tfrac{20}{30}\\cdot f_{[30,60)}}_{\\text{2/3 of }[30,60)}.
$$

**R checks:** the source-of-truth `distr.table.x` gives the table; the arithmetic above is then done by hand or with R as a verification.

> Always **report whether your answer is exact or approximate**, and which assumption was used.
"""

T_G3_DERIVED = """## Constructing derived variables for meaningful comparison

Raw counts almost always mislead when compared across units of different sizes. The fix is to **normalize**.

### Rate = count / exposure (× scale)
$$
\\text{Rate} = \\frac{\\text{count}}{\\text{exposure}} \\cdot k.
$$
The scaling $k$ chooses units (e.g. "per 100 000 inhabitants" → $k=10^5$); the **denominator** is the substantive choice and depends on what is at risk.

- Crime → population.
- Disease incidence → at-risk population.
- Population density → area.
- Property crimes → population.

### Categorical splits from a continuous variable
Often the goal is "do dense and sparse states behave differently?" — threshold a continuous variable at a meaningful value (e.g. Density $\\ge 100$) and compare distributions of the two subsets.

**R commands:**
```r
DF$Rate <- 100000 * (DF$count_var) / DF$Population
DF$Density <- DF$Population / DF$Area
DF$Group <- ifelse(DF$Density >= 100, "Hi", "Lo")
distr.plot.x(x=Var, plot.type="hist", data=subset(DF, Group=="Hi"))
distr.plot.x(x=Var, plot.type="hist", data=subset(DF, Group=="Lo"))
```

**Checks:** correct denominator, non-zero for every unit, intended scale, comparability of the new variable.
"""

T_G4A_BYTYPE = """## Mode, median, mean — choosing by variable type

| Variable type | Mode | Median | Mean |
|---------------|:----:|:------:|:----:|
| Nominal qualitative | ✔ | ✘ | ✘ |
| Ordinal qualitative | ✔ | ✔ | ✘ |
| Discrete numerical  | ✔ | ✔ | ✔ |
| Continuous numerical | (modal *class*) | ✔ | ✔ |

- **Mode** = the value (or category) with highest frequency. Works for any variable type. Beware: when the mode's share is small (e.g. 35%) it is *not* a representative summary.
- **Median** = the value that splits the ordered distribution in two halves of equal weight. Requires an order on the values — works from ordinal onward.
- **Mean** = $\\bar x = \\frac{1}{n}\\sum_i x_i = \\sum_i f_i \\cdot x_i$ when the data are summarised by a frequency table. Requires that the *distances* between values are meaningful — works from interval-scale numerical variables onward.

### Modal class (continuous variable in classes with **unequal widths**)
Use **densities**, not counts: the modal class is the class with **highest density** $d_i = f_i / w_i$. A wider class can have more cases but a lower density — it is not the modal class.

**R commands:**
```r
distr.summary.x(x=Var, stats="mode", data=DF)
distr.summary.x(x=Var, stats=c("mean","median"), data=DF)
distr.summary.x(x=Var, stats="central tendency measures", data=DF)
```
"""

T_G4B_SKEW = """## Mean vs median under skewness

- For a **symmetric** distribution, mean ≈ median.
- For a **right-skewed** distribution (long right tail), **mean > median** — the mean is pulled up by extreme values.
- For a **left-skewed** distribution, **mean < median**.

The mean is **more sensitive** to outliers and to changes in the extreme values; the median is **robust**.

**Practical decision:**
- If the distribution is *concentrated near a central value with a long tail* → report the **median** (more representative of typical cases).
- If the distribution is *symmetric* and there are no outliers → either is fine; the mean is slightly more efficient.

**Reading skewness from a histogram or summary:**
- `mean > median`  ⇒ right-skewed.
- `mean ≈ median`  ⇒ symmetric.
- `mean < median`  ⇒ left-skewed.

**R commands:**
```r
distr.summary.x(x=Var, stats="summary", digits=2, data=DF)
# Returns: min, q1, median, mean, q3, max, sd, ...
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

### Why approximate?
We do not know how the $f_k$ mass is distributed inside the class — the uniform assumption is just the simplest.

**Reading from a histogram:** densities → frequencies (density × width) → use the same recipe.
"""

T_G4D_COMPARE = """## Comparing central tendency across subgroups or periods

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

**Answer.** Three histograms:
```r
distr.plot.x(x=Density, plot.type="hist", breaks=6, data=Data_USA)
distr.plot.x(x=Density, plot.type="hist", breaks=c(0,30,60,100,200,300,1200), data=Data_USA)
distr.plot.x(x=Density, plot.type="hist", breaks=20, data=Data_USA)
```

- **6 classes** — hides the detail in the bulk of the distribution.
- **20 classes** — too detailed in the right tail (noise).
- **Custom widths** — best: narrow in the bulk (0–200), broad in the sparse tail (300–1200), with the y-axis in **density**.

Whenever class widths differ, **density on the y-axis is compulsory**.
""",
"images": ["statistics/images/ex1c-density-hist3.png"]}

ex0["ex1d"] = {"title": "Ex 0 / 1d — Build a rate variable for violent crimes",
"content": """**Question.** You are interested in emphasizing the different levels of violent crimes across states. Would you consider one variable in the database or build a new variable? Which?

---

**Answer.** Build a **rate per 100 000 population** — raw counts depend on state size and are not comparable.

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

**Answer.** Continuous numerical → **histogram**. Start with many classes (15–20), then optionally use custom widths:
```r
distr.plot.x(x=Rate.Violent, plot.type="hist", breaks=15, data=Data_USA)
distr.plot.x(x=Rate.Violent, plot.type="hist", breaks=20, data=Data_USA)
distr.plot.x(x=Rate.Violent, plot.type="hist",
             breaks=c(110,200,250,300,350,400,450,500,550,600,1050),
             data=Data_USA)
```
With unequal widths the y-axis must be **density** — areas (not heights) represent the proportion of states.
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
"images": []}

ex0["ex1g"] = {"title": "Ex 0 / 1g — Approx proportion Frost < 80 (uniform-on-interval)",
"content": """**Question.** What is the proportion of states with `Frost` < 80?

---

**Answer.** Frost is in classes → only **approximate**, under uniform-on-interval.
```r
distr.table.x(x=Frost, freq=c("counts","prop","dens","cum"), data=Data_USA)
```
Class $[60,90)$ has proportion 0.16 and width 30; 80 lies $20/30 = 2/3$ of the way through.

$$
P(\\text{Frost} < 80) \\approx 0.20 + 0.16 \\cdot \\frac{2}{3} = 0.3066667.
$$
```r
0.20 + 0.16 * 2/3
## [1] 0.3066667
(10 + 8 * 2/3) / 50
## [1] 0.3066667
```
""",
"images": []}

ex0["ex1h"] = {"title": "Ex 0 / 1h — Cumulative for Happyness.Level (ordinal)",
"content": """**Question.** Obtain the frequency distribution of `Happyness.Level`. What % are *quite happy or happiest*? What is the frequency cumulated at *So and so*?

---

**Answer.**
```r
distr.table.x(x=Happyness.Level, freq="cum", data=Data_USA)
##  Happyness.Level  Count  Prop   Cum.Count  Cum.Prop
##  Unhappiest        10    0.20    10         0.20
##  Quite unhappy     12    0.24    22         0.45
##  So and so         12    0.24    34         0.69
##  Quite happy        8    0.16    42         0.86
##  Happiest           7    0.14    49         1.00
##  TOTAL             49    1.00
```
% quite happy or happiest:
```r
0.14 + 0.16            # 0.30
(8 + 7) / 49           # 0.3061224
```
Cumulated at *So and so*: **0.69**.

(DC has missing `Happyness.Level`, so denominator = 49 not 51.)
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

Higher-Density states concentrate around moderate murder rates $[4,8)$; lower-Density states are more dispersed and have a larger proportion in the highest bin $[8,12)$. (Use **density**, not counts, when comparing histograms of different sample sizes.)
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
"content": """**Question.** Can you assess **exactly** P(fare ∈ [50, 100))? If not, approximate it.

""" + EX2_FARE_TABLE + """

---

**Answer.** Only **approximately**, since 50 lies inside the class $[30, 60)$. Under uniform-on-interval, the share of $[30, 60)$ that lies in $[50, 60)$ is $(60-50)/(60-30) = 1/3$ of the class's proportion.
$$
\\underbrace{(60-30)\\cdot 0.00419 \\cdot \\tfrac{1}{3}}_{\\text{partial } [30,60)} + \\underbrace{(100-60)\\cdot 0.00196}_{\\text{full } [60,100)} = 0.1203.
$$
```r
(60-30)*0.00419*(1/3) + (100-60)*0.00196
## [1] 0.1203
# or equivalently
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
| $[0, 10)$     | 0.38 | 0.38 |
| $[10, 20)$    | 0.20 | 0.58 |
| $[20, 30)$    | 0.16 | 0.74 |
| $[30, 60)$    | 0.13 | 0.86 |
| $[60, 100)$   | 0.08 | 0.94 |
| $[100, 180)$  | 0.04 | 0.97 |
| $[180, 270)$  | 0.03 | 1.00 |

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
Both histograms show high concentration in low–medium sales classes, with extreme values in the right tail (also observed with very few frequencies, defining a tail to the right). The custom-width histogram is more effective to summarize the distribution by using fewer classes and excessive loss of information, **making the shape of the distribution clear**.

With reference to the comment "absolute or relative frequencies can be reported on the y-axis": this is **only correct when all classes have equal widths**. When widths differ (as in the custom binning), the y-axis must be **density** $f_i/w_i$; otherwise areas mislead. In the second histogram above, the y-axis is therefore *density*, and it is the **area** (not the height) of each bar that represents the proportion of states in that class.
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
- **Qualitative (categorical):** Date, Year, Month, Month_ord, Sex, Country, Product_Category, Sub_Category (8 variables).
- **Quantitative:** Age, Quantity, Unit_Cost, Unit_Price, Cost, Revenue (6 variables) — all continuous except Age and Quantity, which are discrete.
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

**Answer.** `Age` is discrete with many unique values (16 to 90). Use a **spike plot** (a histogram would dilute the high-frequency-level detail; the spike plot represents each integer age honestly):
```r
distr.table.x(x=customer_habits$Age, freq=c("percentage"), p.digits=2)
distr.plot.x(x=customer_habits$Age, freq=c("percentage"), plot.type="spike")
```
From the plot: customer ages span 15–90; the median is the value that splits the sample into two equal-weight halves. The low percentages above 75 and below 25 suggest the median is between **40 and 50**.
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
\\bar x = \\frac{\\sum_i n_i \\cdot x_i}{n} = \\frac{5401 \\cdot 1 + 7340 \\cdot 2 + 8238 \\cdot 3 + 2561 \\cdot 4 + 700 \\cdot 6}{24\\,240} \\approx 2.447.
$$
```r
x      <- 1:6
counts <- c(5401, 7340, 8238, 2561, 0, 700)
xbar   <- sum(counts*x) / sum(counts)
xbar
## [1] 2.443853
```
*(Tiny discrepancy from 2.447 — comes from the source's higher-precision use of `Quantity_New`.)*
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

ex1["1_5a"] = {"title": "Ex 1.5a — P(Time ≤ 5) under uniform-on-interval",
"content": """**Question.** Of 122 customers in $[0,10)$, what is the percentage with `Time` ≤ 5 minutes?

""" + TIME_TABLE + """

---

**Answer.** Under the uniform-on-interval assumption, half of the 122 customers in $[0,10)$ have $Time \\le 5$:
$$
P(Time \\le 5) = \\frac{61}{1800} \\cdot 100\\% = 3.39\\%.
$$
Apparently a rather low percentage.
""",
"images": []}

ex1["1_5b"] = {"title": "Ex 1.5b — P(Time ≤ 30) exact",
"content": """**Question.** Total number / percentage of customers with `Time` ≤ 30 minutes?

""" + TIME_TABLE + """

---

**Answer.** Sum the first three (fully aligned) classes:
$$
\\frac{122 + 420 + 294}{1800} = \\frac{836}{1800} \\cdot 100\\% = 46.44\\%.
$$
""",
"images": []}

ex1["1_5c"] = {"title": "Ex 1.5c — P(15 ≤ Time ≤ 50) approx",
"content": """**Question.** Estimate the number of customers with `Time` between 15 and 50 minutes.

""" + TIME_TABLE + """

---

**Answer.** Under uniform-on-interval, $[15,20)$ is half of $[10,20)$ (210 customers); $[20,30)$ is fully included (294); $[30,50)$ is $20/30 = 2/3$ of $[30,60)$ — about $20/30 \\cdot 176 = 117.33$ customers.
$$
P(15 \\le Time \\le 50) \\approx \\frac{210 + 294 + 117.33}{1800} \\cdot 100\\% = 34.52\\%.
$$
About one third of customers — considerable but not a majority.
""",
"images": []}

ex1["1_5d"] = {"title": "Ex 1.5d — Ogive of Time and median estimation",
"content": """**Question.** Build the ogive of `Time`; estimate the median.

""" + TIME_TABLE + """

---

**Answer.** Cumulative frequencies (relative + cumulative), given the table:

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
**Median.** Cumulative at 30 is 0.464 (< 0.5); at 60 is 0.562 (≥ 0.5) → median class $[30,60)$. Under uniform-on-interval:
$$
p_{50} \\approx 30 + \\frac{0.5 - 0.464}{d_4} = 30 + \\frac{0.036}{0.00327} \\approx 41.01 \\text{ min}.
$$
where $d_4 = 0.098/30 = 0.00327$ is the frequency density of the median class.
""",
"images": ["statistics/images/ex1_5d-time-ogive.png"]}

ex1["1_5e"] = {"title": "Ex 1.5e — Modal class via densities (unequal widths)",
"content": """**Question.** Identify the modal class of `Time`.

""" + TIME_TABLE + """

---

**Answer.** The class with maximum *frequency* is $[60,90)$ with 571 cases — **but classes have different widths**, so this is misleading. Compute the **density** $d_i = f_i / w_i$:

| Time      | abs.freq | rel.freq | width | density |
|-----------|---------:|---------:|------:|--------:|
| $[0,10)$   | 122  | 0.068 | 10 | 0.00680 |
| $[10,20)$  | 420  | 0.233 | 10 | **0.02330** |
| $[20,30)$  | 294  | 0.163 | 10 | 0.01630 |
| $[30,60)$  | 176  | 0.098 | 30 | 0.00327 |
| $[60,90)$  | 571  | 0.317 | 30 | 0.01057 |
| $[90,150]$ | 217  | 0.121 | 60 | 0.00202 |

**Modal class = $[10, 20)$** with density 0.0233, the highest in the table. (Note the density of $[10,20)$ is more than twice the density of $[60,90)$, the absolute-frequency leader.)
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

**Answer.** Yes — frequencies of all intervals can be obtained as density × width. The frequency of the unclear $[120,160)$ class can be inferred as $1 - \\sum (\\text{other frequencies})$:

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

ALL = {**ex0, **ex1}

def th(tid, ttitle, tcontent):
    return {"id": tid, "title": ttitle, "content": tcontent}

SUBTOPIC_COLOR = {
    "G1": "coral", "G2": "orange", "G3": "purple", "G4": "skyblue"
}

# For each subtopic: theory + dict {col_idx: [ex_ids]} (col 2 = first exercise of set 0 = Ex1 of set 0; etc.)
# Columns indexing: col 2 = "Ex 0 / 1", col 3 = "Ex 0 / 2", col 4 = "Ex 1.1", col 5 = "Ex 1.2",
# col 6 = "Ex 1.3", col 7 = "Ex 1.4", col 8 = "Ex 1.5", col 9 = "Ex 1.6".

# Helper to build subtopic content dicts.
def sub(group, sid, sname, theory_id, theory_title, theory_content, columns):
    return {"group": group, "sid": sid, "sname": sname,
            "theory": (theory_id, theory_title, theory_content),
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
  }),
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
  }),
  # ===== G2 — Proportions =====
  sub("G2", "g2a_exact", "Exact proportions",
      "th_g2a", "Theory — Exact proportions", T_G2A_EXACT, {
        2: ["ex1f", "ex1h"],
        3: ["ex2a1", "ex2b1"],
        4: ["1_1e", "1_1g"],
        5: ["1_2a"],
        6: ["1_3b"],
        8: ["1_5b"],
  }),
  sub("G2", "g2b_approx", "Uniform-on-interval approximation",
      "th_g2b", "Theory — Uniform-on-interval", T_G2B_APPROX, {
        2: ["ex1g"],
        3: ["ex2a2"],
        8: ["1_5a", "1_5c"],
  }),
  # ===== G3 — Derived variables =====
  sub("G3", "g3_main", "Constructing derived variables",
      "th_g3", "Theory — Constructing derived variables", T_G3_DERIVED, {
        2: ["ex1b", "ex1d"],
  }),
  # ===== G4 — Central tendency =====
  sub("G4", "g4a_bytype", "Mode, median, mean by variable type",
      "th_g4a", "Theory — Choosing mode/median/mean by variable type", T_G4A_BYTYPE, {
        4: ["1_1f"],
        5: ["1_2b"],
        6: ["1_3d"],
        7: ["1_4a3"],
        8: ["1_5e"],
  }),
  sub("G4", "g4b_skew", "Mean vs median under skewness",
      "th_g4b", "Theory — Mean vs median under skewness", T_G4B_SKEW, {
        4: ["1_1h"],
        6: ["1_3f", "1_3i"],
        9: ["1_6a"],
  }),
  sub("G4", "g4c_grouped", "Approximate mean & median from grouped data",
      "th_g4c", "Theory — Approximate mean & median (grouped data)", T_G4C_GROUPED, {
        4: ["1_1i"],
        8: ["1_5f"],
        9: ["1_6b"],
  }),
  sub("G4", "g4d_compare", "Cross-subgroup / period comparison",
      "th_g4d", "Theory — Cross-subgroup comparison", T_G4D_COMPARE, {
        7: ["1_4b"],
        8: ["1_5h"],
  }),
]

TOPIC_META = {
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
    {"col": 2, "label": "Ex 0 / Q1 (USA states)"},
    {"col": 3, "label": "Ex 0 / Q2 (Titanic)"},
    {"col": 4, "label": "Ex 1.1 (pizzerie)"},
    {"col": 5, "label": "Ex 1.2 (DS)"},
    {"col": 6, "label": "Ex 1.3 (customer_habits)"},
    {"col": 7, "label": "Ex 1.4 (Quantity_New)"},
    {"col": 8, "label": "Ex 1.5 (Time)"},
    {"col": 9, "label": "Ex 1.6 (Expenses)"},
]

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
    converted_th_content = md_tables_to_latex(th_content)
    if "\\begin{tabular}" in converted_th_content and "\\begin{tabular}" not in th_content:
        md_table_count += converted_th_content.count("\\begin{tabular}")
    th_node = node(th_id, th_title, converted_th_content,
                   COL_X[0], TOP_Y, "yellow", w=SNIPPET_W, h=H_THEORY,
                   links=theory_links)
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
            converted_content = md_tables_to_latex(d["content"])
            new_tables = (converted_content.count("\\begin{tabular}")
                          - d["content"].count("\\begin{tabular}"))
            md_table_count += max(0, new_tables)
            n_node = node(ex_id, d["title"], converted_content,
                          x, cy, color, w=SNIPPET_W, h=H(d),
                          links=[th_id], images=d.get("images", []))
            n_node["column"] = col_idx
            nodes_in_subtopic.append(n_node)
            cy += H(d) + SNIPPET_GAP

    topics_out[group]["subtopics"].append({
        "id": sid, "name": sname, "nodes": nodes_in_subtopic,
    })
    total_nodes_count += len(nodes_in_subtopic)

topics_list = [topics_out[g] for g in ("G1", "G2", "G3", "G4") if g in topics_out]

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
