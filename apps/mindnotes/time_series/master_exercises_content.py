"""
Master Exam Ready snippets — Time Series Analysis.

One consolidated exercise per sub-topic, covering every UNIQUE sub-question
seen across the linked past-exam snippets, deduplicated.

Color coding by frequency (count of distinct exam appearances):
  - red                  : appears in exactly 1 exam   (mark   ▼ From: <ID> / ▲)
  - dark yellow (#B8860B): appears in 2-3 exams       (mark   ▼ 2-3x: <IDs> / ▲)
  - orange               : appears in 4+ exams        (mark   ▼ 4+x: <IDs> / ▲)

Each tagged block opens with a downward triangle ▼ on the colored label and closes
with an upward triangle ▲ in the same color so the boundary is visible even when
the content is long.
"""

master_exercises_ts = {}


# ============================================================================
# t1a — What is a stochastic process / time series
# Linked: exam_sep_2025_q1, exam_may_2023_q1   (2 exams → DARK YELLOW tier)
# ============================================================================
master_exercises_ts["t1a"] = {
    "title": "Master — Stochastic process / time series (foundations)",
    "content": r"""\textbf{\textcolor{red}{MASTER --- Stochastic process / time series (foundations).}}

\emph{Canonical setup.} Take the monthly UK lung-deaths series \texttt{ldeaths} ($T=72$, Jan 1974 -- Dec 1979). This is one observed path of a stochastic process; we use it only to ground the definitions in something concrete.

```R
data(ldeaths); y <- as.numeric(ldeaths); T <- length(y)  # T = 72
plot.ts(ldeaths, ylab="UK monthly lung deaths", main="One observed path of the stochastic process Y_t")
```

\textbf{\textcolor[HTML]{B8860B}{▼ 2-3x: Sep 2025 Q1(a), May 2023 Q1(a) --- DEFINE STOCHASTIC PROCESS.}}

\textbf{(a) Definition.} A \emph{stochastic process} is a family of random variables (or random vectors) $(Y_t)_{t\in T}$ indexed by a set $T$ (the index set, typically $T=\mathbb{N}$ or $\mathbb{Z}$ for discrete time). Each $Y_t$ takes values in a state space $\mathcal Y$ (e.g.\ $\mathbb{R}$, $\mathbb{R}^m$, or a finite alphabet).

It is fully specified by its \emph{finite-dimensional distributions}
\[ \bigl\{\,f_{Y_{t_1},\dots,Y_{t_k}}(y_1,\dots,y_k)\;:\; k\ge 1,\;(t_1,\dots,t_k)\in T^k\,\bigr\}, \]
which must satisfy Kolmogorov's consistency conditions (symmetry under permutation + marginalisation). By Kolmogorov's extension theorem, any consistent family determines a unique process on $T$.

\textbf{Why the f.d.d.s matter.} For Gaussian processes the f.d.d.s reduce to mean function $\mu(t)$ + covariance $\gamma(s,t)$ --- two summaries fully describe everything.

\textcolor[HTML]{B8860B}{▲}

\textbf{\textcolor[HTML]{B8860B}{▼ 2-3x: Sep 2025 Q1(b), May 2023 Q1(b) --- TIME SERIES AS A STOCHASTIC PROCESS.}}

\textbf{(b) Definition.} A \emph{time series} is a stochastic process $(Y_t)_{t\in T}$ with $T\subseteq\mathbb{Z}$ (discrete-time indexing) and $Y_t\in\mathcal Y$. The \textbf{observed data} $(y_1,\dots,y_T)$ are a \emph{single finite realisation} of \emph{one} path.

```R
# the ldeaths series is one realisation y_{1:T}; we do NOT see other paths
length(y); head(y); tail(y)
```

\emph{Implication.} Inference from one path is possible \emph{only} under structural assumptions that link different time points:
\begin{itemize}
\item \textbf{Stationarity} (weak or strict): different time slices have the same distribution / second moments $\Rightarrow$ averaging over $t$ approximates averaging over realisations.
\item \textbf{Parametric dynamics}: ARMA, DLM, HMM impose a low-dimensional model that ties $Y_t$ to past $Y_{s<t}$ or latent states.
\item \textbf{Ergodicity}: time-averages converge to ensemble-averages (precondition for sample-mean consistency, see t2c).
\end{itemize}

Without such assumptions a time series is \emph{not} a sample of size $T$ from a population --- it is a sample of size 1 from a $T$-dimensional joint distribution.

\textcolor[HTML]{B8860B}{▲}

\textbf{\textcolor{red}{▼ From: Sep 2025 Q1(a) --- KOLMOGOROV EXTENSION (advanced).}}

\textbf{(c) Kolmogorov's extension theorem.} Given a family of f.d.d.s indexed by all finite tuples $(t_1,\dots,t_k)\subset T$, if they satisfy
\begin{itemize}
\item \emph{Permutation symmetry:} $f_{t_{\pi(1)},\dots,t_{\pi(k)}}(y_{\pi(1)},\dots,y_{\pi(k)}) = f_{t_1,\dots,t_k}(y_1,\dots,y_k)$
\item \emph{Marginal consistency:} $\int f_{t_1,\dots,t_k,t_{k+1}}(y_1,\dots,y_k,y_{k+1})\,dy_{k+1} = f_{t_1,\dots,t_k}(y_1,\dots,y_k)$
\end{itemize}
then there exists a unique probability measure on $(\mathcal{Y}^T,\mathcal{B}(\mathcal{Y}^T))$ producing those f.d.d.s. This is what makes "specify all the joint distributions" a legitimate way to define a process.

\textcolor{red}{▲}

\textbf{Closing.} Linked snippets: \texttt{exam\_sep\_2025\_q1}, \texttt{exam\_may\_2023\_q1}.

\emph{Image:} \texttt{images/master/master\_t1a\_ai.png} (the \texttt{ldeaths} path + a stylised cartoon of \emph{several} alternative paths from the same process, to ground the "single realisation" point).""",
    "images": ["images/master/master_t1a_ai.png"]
}


# ============================================================================
# t2a — Weak stationarity — definition & examples
# Linked: exam_sep_2025_q2, exam_jun_2024_q1   (2 exams → DARK YELLOW tier)
# ============================================================================
master_exercises_ts["t2a"] = {
    "title": 'Master — Weak stationarity — definition & examples',
    "content": r"""\textbf{\textcolor{red}{MASTER --- Weak stationarity — definition & examples \;[Topic: Stationarity, ACVF & sample mean]}}

\emph{Canonical dataset.} annual Lake Huron level (\texttt{LakeHuron}). One series; we use it to ground all sub-parts.

```R
data(LakeHuron); y <- as.numeric(LakeHuron)
```

\textbf{\textcolor{[HTML]{B8860B}}{▼ 2-3x: Sep 2025 Q2, Jun 2024 Q1 ---}} CONSOLIDATED CONTENT FROM LINKED EXAM(S).

\textbf{\emph{From Sep 2025 Q2.}}

\textbf{(a)} $\mu(t)=\mathbb{E}[Y_t]$; $\gamma(s,t)=\operatorname{Cov}(Y_s,Y_t)$, a function of \emph{two} arguments.

\textbf{(b)} Weakly stationary iff $\mathbb{E}[Y_t]$ and $\mathbb{E}[Y_tY_{t+h}]$ are finite and do not depend
on $t$, for every $h$:
$\mu(t)=\mu$, $\sigma^2(t)=\sigma^2$, $\gamma(t,t+h)=\tilde\gamma(h)$.

\textbf{(c) NO}, because ARMA has \emph{constant} coefficients $(\phi_i,\theta_j,\sigma^2)$, hence
constant mean/variance and lag-only ACVF; it cannot describe a non-stationary mean/variance.
\textbf{YES, but only if} the non-stationarity is first removed: differencing $(1-B)^d$ for
unit roots (ARIMA), $(1-B^s)$ for seasonality (SARIMA), or remove trend/seasonality and fit
ARMA to the stationary residual.

\textbf{\emph{From Jun 2024 Q1.}}

\textbf{(a)} $\mathbb{E}[Y_t]$ and $\mathbb{E}[Y_tY_{t+h}]$ finite and independent of $t$, $\forall h$:
$\mu(t)=\mu$, $\sigma^2(t)=\sigma^2$, $\gamma(t,t+h)=\tilde\gamma(h)$.

\textbf{(b)} \emph{White noise} $Y_t\overset{\text{iid}}{\sim}\mathcal{N}(0,\sigma^2)$: $\mu=0$, $\gamma(h)=\sigma^2\mathbf{1}\{h=0\}$.
Or \emph{causal AR(1)} $Y_t=\phi Y_{t-1}+\varepsilon_t$, $|\phi|<1$, started in stationary
distribution: $\mu=0$, $\gamma(h)=\sigma^2\phi^{|h|}/(1-\phi^2)$ (Example 3.A; the
``$|\varphi|<1\Rightarrow$ stationary'' proof in the notes).

\textcolor{[HTML]{B8860B}}{▲}

\textbf{Closing.} Linked snippets: \texttt{exam_sep_2025_q2, exam_jun_2024_q1}.

\emph{Image:} \texttt{images/master/master\_t2a\_ai.png} (canonical plot for this topic).""",
    "images": ["images/master/master_t2a_ai.png"]
}


# ============================================================================
# t2b — ACVF / correlogram — when defined?
# Linked: exam_may_2025_q1, exam_may_2022_q2, exam_may_2021_q2   (3 exams → DARK YELLOW tier)
# ============================================================================
master_exercises_ts["t2b"] = {
    "title": 'Master — ACVF / correlogram — when defined?',
    "content": r"""\textbf{\textcolor{red}{MASTER --- ACVF / correlogram — when defined? \;[Topic: Stationarity, ACVF & sample mean]}}

\emph{Canonical dataset.} annual Lake Huron level (\texttt{LakeHuron}). One series; we use it to ground all sub-parts.

```R
data(LakeHuron); y <- as.numeric(LakeHuron)
```

\textbf{\textcolor{[HTML]{B8860B}}{▼ 2-3x: May 2025 Q1, May 2022 Q2, May 2021 Q2 ---}} CONSOLIDATED CONTENT FROM LINKED EXAM(S).

\textbf{\emph{From May 2025 Q1.}}

\textbf{(a) NO.} The ACF is defined as $\gamma(s,t)=\operatorname{Cov}(Y_s,Y_t)$ whenever
$\mathbb{E}[Y_t^2]<\infty$. Stationarity merely allows reducing it to a 1-arg function $\gamma(h)$
of the lag.

\textbf{(b) YES} (essentially). The correlogram
$\hat\gamma(h)=T^{-1}\sum_{t}(Y_t-\bar Y)(Y_{t+|h|}-\bar Y)$ \emph{pools} across $t$ to
estimate one function: meaningful only if the true ACVF depends solely on the lag. Without
(weak) stationarity + ergodicity it has no clean interpretation.

\textbf{\emph{From May 2022 Q2.}}

\textbf{NO}, same as Exam 3 Q1a. ACVF $\gamma(s,t)=\operatorname{Cov}(Y_s,Y_t)$ is defined whenever
$\mathbb{E}[Y_t^2]<\infty$.

\textbf{\emph{From May 2021 Q2.}}

\textbf{NO}, see Exam 3 Q1a / Exam 8 Q2.

\textcolor{[HTML]{B8860B}}{▲}

\textbf{Closing.} Linked snippets: \texttt{exam_may_2025_q1, exam_may_2022_q2, exam_may_2021_q2}.

\emph{Image:} \texttt{images/master/master\_t2b\_ai.png} (canonical plot for this topic).""",
    "images": ["images/master/master_t2b_ai.png"]
}


# ============================================================================
# t2c — Sample-mean estimator under stationarity + ergodicity
# Linked: exam_may_2024_q1, exam_jun_2022_q1   (2 exams → DARK YELLOW tier)
# ============================================================================
master_exercises_ts["t2c"] = {
    "title": 'Master — Sample-mean estimator under stationarity + ergodicity',
    "content": r"""\textbf{\textcolor{red}{MASTER --- Sample-mean estimator under stationarity + ergodicity \;[Topic: Stationarity, ACVF & sample mean]}}

\emph{Canonical dataset.} annual Lake Huron level (\texttt{LakeHuron}). One series; we use it to ground all sub-parts.

```R
data(LakeHuron); y <- as.numeric(LakeHuron)
```

\textbf{\textcolor{[HTML]{B8860B}}{▼ 2-3x: May 2024 Q1, Jun 2022 Q1 ---}} CONSOLIDATED CONTENT FROM LINKED EXAM(S).

\textbf{\emph{From May 2024 Q1.}}

By stationarity $\mu(t)\equiv\mu$. Use the sample mean
$\boxed{\hat\mu_T=\bar Y_T=T^{-1}\sum_t Y_t}$. Unbiased under stationarity
($\mathbb{E}[\bar Y_T]=\mu$). Consistent under stationarity + ergodicity (ergodic theorem).
Asymptotic variance involves the long-run variance $\sum_h\gamma(h)$ (use HAC for SE).

\textbf{\emph{From Jun 2022 Q1.}}

\textbf{YES, under conditions}: \emph{stationarity} (so $\mathbb{E}[Y_t]=\mu$ is constant and pooling
across $t$ is meaningful) + \emph{ergodicity} (so $\bar Y_n\asto\mu$). Without stationarity
$\bar Y_n$ targets a mixture $\bar\mu_n=n^{-1}\sum_t\mathbb{E}[Y_t]$, generically different from
$\mathbb{E}[Y_n]$.

\textcolor{[HTML]{B8860B}}{▲}

\textbf{Closing.} Linked snippets: \texttt{exam_may_2024_q1, exam_jun_2022_q1}.

\emph{Image:} \texttt{images/master/master\_t2c\_ai.png} (canonical plot for this topic).""",
    "images": ["images/master/master_t2c_ai.png"]
}


# ============================================================================
# t3a — Markov property, DAG & conditional independence
# Linked: exam_jun_2025_q1, exam_may_2025_q2, exam_may_2021_q1   (3 exams → DARK YELLOW tier)
# ============================================================================
master_exercises_ts["t3a"] = {
    "title": 'Master — Markov property, DAG & conditional independence',
    "content": r"""\textbf{\textcolor{red}{MASTER --- Markov property, DAG & conditional independence \;[Topic: Markov chains — theory (DAG, Markov property, ergodicity)]}}

\emph{Canonical dataset.} a 3-state pollution chain low/med/high (\texttt{synthetic 3-state MC}). One series; we use it to ground all sub-parts.

```R
# simulated 3-state Markov chain (see below)
```

\textbf{\textcolor{[HTML]{B8860B}}{▼ 2-3x: Jun 2025 Q1, May 2025 Q2, May 2021 Q1 ---}} CONSOLIDATED CONTENT FROM LINKED EXAM(S).

\textbf{\emph{From Jun 2025 Q1.}}

\textbf{(a) Not in general.} The state space and initial law alone do not imply Markovianity.
$(Y_t)$ is a MC (keydef \textbf{9a}) iff $p(y_t\mid y_{0:t-1})=p(y_t\mid y_{t-1})$ for every $t$.

\textbf{(b)} The MC DAG is the path $Y_0\to Y_1\to\cdots\to Y_t\to\cdots$. Every directed path
from $Y_{1:t-2}$ to $Y_t$ goes through $Y_{t-1}$. Conditioning on $Y_{t-1}$ blocks all such
paths ($Y_{t-1}$ is a serial/chain node), so $Y_t\perp(Y_{1:t-2})\mid Y_{t-1}$ by
$d$-separation. (Algebraically: the Markov property.)

\textbf{\emph{From May 2025 Q2.}}

\textbf{YES.} $Y_t=Y_{t-1}+Z_t$ with $Z_t$ independent of $Y_{0:t-1}$. Hence
$\Prob(Y_t=y_t\mid Y_{0:t-1})=\Prob(Z_t=y_t-Y_{t-1})=\Prob(Y_t\mid Y_{t-1})$
(Markov property; cf.\ Example 1 RW). \emph{It is also non-stationary} ($\operatorname{Var}(Y_t)=4p(1-p)\,t$).

\textbf{\emph{From May 2021 Q1.}}

\textbf{NO.} Homogeneous $\Rightarrow$ $\Prob(X_{n+1}=j\mid X_n=i)=p_{ij}$ independent of $n$.
The hypothesis would force $p_{3,1}=0.4=0.5$, contradiction. (Could be inhomogeneous Markov,
or not Markov.)

\textcolor{[HTML]{B8860B}}{▲}

\textbf{Closing.} Linked snippets: \texttt{exam_jun_2025_q1, exam_may_2025_q2, exam_may_2021_q1}.

\emph{Image:} \texttt{images/master/master\_t3a\_ai.png} (canonical plot for this topic).""",
    "images": ["images/master/master_t3a_ai.png"]
}


# ============================================================================
# t3b — Transition-matrix arithmetic & ergodic convergence (Thm 2.1)
# Linked: exam_jun_2024_q3, exam_may_2023_q2, exam_may_2022_q3, exam_jun_2022_q5   (4 exams → ORANGE tier)
# ============================================================================
master_exercises_ts["t3b"] = {
    "title": 'Master — Transition-matrix arithmetic & ergodic convergence (Thm 2.1)',
    "content": r"""\textbf{\textcolor{red}{MASTER --- Transition-matrix arithmetic & ergodic convergence (Thm 2.1) \;[Topic: Markov chains — theory (DAG, Markov property, ergodicity)]}}

\emph{Canonical dataset.} a 3-state pollution chain low/med/high (\texttt{synthetic 3-state MC}). One series; we use it to ground all sub-parts.

```R
# simulated 3-state Markov chain (see below)
```

\textbf{\textcolor{orange}{▼ 4+x: Jun 2024 Q3, May 2023 Q2, May 2022 Q3, Jun 2022 Q5 ---}} CONSOLIDATED CONTENT FROM LINKED EXAM(S).

\textbf{\emph{From Jun 2024 Q3.}}

\textbf{(a)} Rows sum to 1: $P=\bigl(\begin{smallmatrix}.6&0&.4\\.1&.6&.3\\.3&.1&.6\end{smallmatrix}\bigr)$.

\textbf{(b) YES.} $P$ is irreducible on $\{1,2,3\}$ ($1\to3\to2$, $2\to1$, $3\to1$ reach every
state) and aperiodic ($p_{11}>0$). By Theorem 2.1 (keydef \textbf{11d}), there is a unique
stationary $\pi$ and $\Prob(Y_n=j\mid Y_0=i)\to\pi_j$ for every $i,j$.
Solving $\pi P=\pi$: $\pi\approx(0.394,\,0.121,\,0.485)$.

\textbf{(c) YES.} $\Prob(Y_n=j)=\sum_i\nu_i\Prob(Y_n=j\mid Y_0=i)\to\pi_j$ regardless of the
initial law $\nu$.

\textbf{\emph{From May 2023 Q2.}}

$\mathbf P=\tfrac13\mathbf 1\mathbf 1^{\top}$ (all entries $1/3$). Then $\mathbf P^k=\mathbf P$ for $k\ge 1$,
so $\Prob(Y_2=2\mid Y_0=1)=(\mathbf P^2)_{12}=1/3$.

\textbf{\emph{From May 2022 Q3.}}

\textbf{YES.} Identical to Exam 4 Q3b: irreducible + aperiodic on a finite state space $\Rightarrow$
ergodic (Theorem 2.1); $\Prob(Y_n=j\mid Y_0=1)\to\pi_j$, with $\pi\approx(0.394,0.121,0.485)$.

\textbf{\emph{From Jun 2022 Q5.}}

Only the latent chain enters (no $Y$). By the Markov property,
$\Prob(\cdot\mid S_0=1)=p_{11}\cdot p_{11}\cdot p_{12}=p_{11}^2 p_{12}$.

\textcolor{orange}{▲}

\textbf{Closing.} Linked snippets: \texttt{exam_jun_2024_q3, exam_may_2023_q2, exam_may_2022_q3, exam_jun_2022_q5}.

\emph{Image:} \texttt{images/master/master\_t3b\_ai.png} (canonical plot for this topic).""",
    "images": ["images/master/master_t3b_ai.png"]
}


# ============================================================================
# t4a — Panel transition-count likelihood & MLE
# Linked: exam_sep_2025_q6, exam_may_2024_q2   (2 exams → DARK YELLOW tier)
# ============================================================================
master_exercises_ts["t4a"] = {
    "title": 'Master — Panel transition-count likelihood & MLE',
    "content": r"""\textbf{\textcolor{red}{MASTER --- Panel transition-count likelihood & MLE \;[Topic: Markov chains — likelihood, MLE & Anderson–Goodman CI]}}

\emph{Canonical dataset.} panel of n=20 locations × T=100 obs from a 3-state MC (\texttt{panel MC counts}). One series; we use it to ground all sub-parts.

```R
# panel simulation, see below
```

\textbf{\textcolor{[HTML]{B8860B}}{▼ 2-3x: Sep 2025 Q6, May 2024 Q2 ---}} CONSOLIDATED CONTENT FROM LINKED EXAM(S).

\textbf{\emph{From Sep 2025 Q6.}}

\textbf{Likelihood} (cond.\ on $y_{i,0}$; keydef \textbf{12}):
$L(\mathbf P)=\prod_{i,j}p_{ij}^{n_{ij}}$, with $n_{ij}=\sum_{k,t}\mathbf{1}\{Y_{k,t-1}=i,Y_{k,t}=j\}$ the
pooled transition counts and $n_{i,+}=\sum_j n_{ij}$.

\textbf{MLE} (row-by-row Lagrangian, sufficient statistic $\{n_{ij}\}$):
$$
\boxed{\;\widehat p_{ij}=\frac{n_{ij}}{n_{i,+}}.\;}
$$

\textbf{Asymptotic CI} (keydef \textbf{13}, Anderson--Goodman). Conditional on $n_{i,+}$,
$(n_{i,1},n_{i,2},n_{i,3})\sim\mathrm{Multinom}(n_{i,+};p_{i,:})$, so
$\sqrt{n_{i,+}}(\widehat p_{ij}-p_{ij})\xrightarrow{d}\mathcal{N}(0,p_{ij}(1-p_{ij}))$. Wald $(1-\alpha)$ CI:
$$
\boxed{\;\widehat p_{ij}\pm z_{\alpha/2}\sqrt{\widehat p_{ij}(1-\widehat p_{ij})/n_{i,+}}.\;}
$$

\textbf{\emph{From May 2024 Q2.}}

\textbf{(a)} $L(\mathbf P)=\prod_{ij}p_{ij}^{n_{ij}}$ (conditioned on fixed $y_{i,0}$).

\textbf{(b)} Row-by-row Lagrangian gives $\widehat p_{ij}=n_{ij}/n_{i,+}$.

\textbf{(c)} Required: $\widehat p_{3,1}=30/100=0.30$. Wald 90\% CI:
$0.30\pm 1.65\sqrt{0.30\cdot 0.70/100}=0.30\pm 0.076$, i.e.\ $\boxed{[0.224,\,0.376]}$.

\textcolor{[HTML]{B8860B}}{▲}

\textbf{Closing.} Linked snippets: \texttt{exam_sep_2025_q6, exam_may_2024_q2}.

\emph{Image:} \texttt{images/master/master\_t4a\_ai.png} (canonical plot for this topic).""",
    "images": ["images/master/master_t4a_ai.png"]
}


# ============================================================================
# t4b — Wald CI for p_ij + forecasting future percentages
# Linked: exam_jun_2025_q2, exam_may_2022_q5   (2 exams → DARK YELLOW tier)
# ============================================================================
master_exercises_ts["t4b"] = {
    "title": 'Master — Wald CI for p_ij + forecasting future percentages',
    "content": r"""\textbf{\textcolor{red}{MASTER --- Wald CI for p_ij + forecasting future percentages \;[Topic: Markov chains — likelihood, MLE & Anderson–Goodman CI]}}

\emph{Canonical dataset.} panel of n=20 locations × T=100 obs from a 3-state MC (\texttt{panel MC counts}). One series; we use it to ground all sub-parts.

```R
# panel simulation, see below
```

\textbf{\textcolor{[HTML]{B8860B}}{▼ 2-3x: Jun 2025 Q2, May 2022 Q5 ---}} CONSOLIDATED CONTENT FROM LINKED EXAM(S).

\textbf{\emph{From Jun 2025 Q2.}}

\textbf{(a)} Free parameters of $\mathbf P$: $3\cdot 2=6$ (rows sum to 1).

\textbf{(b)} $L(\mathbf P)=\prod_{i,j}p_{ij}^{n_{ij}}$.

\textbf{(c)} $\widehat p_{1,1}=70/150\approx 0.467$.
$0.467\pm 1.65\sqrt{0.467\cdot 0.533/150}=0.467\pm 1.65\cdot 0.0407=0.467\pm 0.067$,
i.e.\ $\boxed{[0.400,\,0.534]}$.

\textbf{(d)} See keydef \textbf{13}: given visit count $n_{1,+}=150$, the next-state outcomes
are an i.i.d.\ sample from $\mathrm{Multinom}(1;p_{1,:})$, so
$(n_{1,1},n_{1,2},n_{1,3})\mid n_{1,+}\sim\mathrm{Multinom}(n_{1,+};p_{1,:})$. CLT applied to
$\widehat p_{1,1}=n_{11}/n_{1,+}$ gives
$\sqrt{n_{1,+}}(\widehat p_{1,1}-p_{1,1})\xrightarrow{d}\mathcal{N}(0,p_{1,1}(1-p_{1,1}))$.

\textbf{(e) Point estimate of $q$.}
Step 1: estimated July YES probability via plug-in
$\widehat p_{\text{YES}}=\sum_i\widehat\pi_i^{(5)}\,\widehat p_{i,1}$, with
$\widehat\pi^{(5)}\propto(150,250,100)$ the empirical state distribution in June.
This gives $\widehat p_{\text{YES}}=(150\cdot.467+250\cdot.300+100\cdot.300)/500=175/500=0.35$.
Step 2: $\bar Y_{\text{July}}\approx\mathcal{N}(0.35,\,0.35\cdot 0.65/n)$ (panel proportion CLT).
$\sqrt{0.35\cdot 0.65/1000}\approx 0.015$.
$\widehat q=1-\Phi((0.5-0.35)/0.015)=1-\Phi(10)\approx 0$.

\textbf{\emph{From May 2022 Q5.}}

\textbf{(a)} \emph{Estimated:} the formula
$=p_{13}p_{33}^2 p_{31}$ is known, but its numerical value depends on the unknown $\mathbf P$.

\textbf{(b)} $\widehat p_{3,1}=30/170\approx 0.176$. 95\% CI:
$0.176\pm 1.96\sqrt{0.176\cdot 0.824/170}=0.176\pm 0.057=\boxed{[0.119,0.234]}$.
\emph{MLE properties used:} consistency (plug-in $\widehat p_{3,1}$ in the SE is valid by
Slutsky) and asymptotic normality (Anderson--Goodman, keydef \textbf{13}).

\textcolor{[HTML]{B8860B}}{▲}

\textbf{Closing.} Linked snippets: \texttt{exam_jun_2025_q2, exam_may_2022_q5}.

\emph{Image:} \texttt{images/master/master\_t4b\_ai.png} (canonical plot for this topic).""",
    "images": ["images/master/master_t4b_ai.png"]
}


# ============================================================================
# t5a — HMM definition, parameters & forward-algorithm likelihood
# Linked: exam_may_2025_q3, exam_may_2023_q3, exam_may_2022_q6, exam_may_2021_q4   (4 exams → ORANGE tier)
# ============================================================================
master_exercises_ts["t5a"] = {
    "title": 'Master — HMM definition, parameters & forward-algorithm likelihood',
    "content": r"""\textbf{\textcolor{red}{MASTER --- HMM definition, parameters & forward-algorithm likelihood \;[Topic: Hidden Markov Models (HMM) — model, likelihood, decoding]}}

\emph{Canonical dataset.} 2-state HMM with Gaussian emissions (\texttt{synthetic HMM}). One series; we use it to ground all sub-parts.

```R
# simulated HMM (see below)
```

\textbf{\textcolor{orange}{▼ 4+x: May 2025 Q3, May 2023 Q3, May 2022 Q6, May 2021 Q4 ---}} CONSOLIDATED CONTENT FROM LINKED EXAM(S).

\textbf{\emph{From May 2025 Q3.}}

\textbf{(a) HMM} (keydef \textbf{15b}, HMM variant).
Latent topic $(S_t)\in\{1,\dots,k\}$ homogeneous Markov, $S_0\sim\pi$, transition $\mathbf P=[p_{ij}]$.
Observed word $Y_t\in\{1,\dots,M\}$, emission
$e_{i,w}=\Prob(Y_t=w\mid S_t=i)$, $\sum_w e_{i,w}=1$. Conditional indep.\ of obs.\ given the state.

\textbf{(b)} $\phi=(\pi,\mathbf P,\mathbf E)$: $(k-1)+k(k-1)+k(M-1)$ free parameters.

\textbf{(c) Likelihood (forward algorithm).} Define $\alpha_t(i)=\Prob(Y_{1:t}=y_{1:t},S_t=i;\phi)$:
$\alpha_1(i)=\pi_i e_{i,y_1}$; $\alpha_t(j)=\bigl(\sum_i\alpha_{t-1}(i)p_{ij}\bigr)e_{j,y_t}$.
Then
$$
L(\phi;y_{1:t})=\sum_{i=1}^k\alpha_t(i),
$$
computed in $O(k^2 t)$ instead of $O(k^t)$. MLE by EM / Baum--Welch.

\textbf{\emph{From May 2023 Q3.}}

\textbf{(a)} $S_0=1$;
$S_t\mid S_{t-1}=i\sim\mathrm{Cat}(p_{i,1},p_{i,2},p_{i,3})$;
$Y_t\mid S_t=i\sim\mathcal{N}(0,\sigma_i^2)$, $i=1,2,3$.

\textbf{(b)} $\phi=(\mathbf P,\sigma_1^2,\sigma_2^2,\sigma_3^2)$, $6+3=9$ free
parameters. ($\pi$ degenerate at 1.) Identification up to relabelling of states (enforce
$\sigma_1^2<\sigma_2^2<\sigma_3^2$).

\textbf{\emph{From May 2022 Q6.}}

\textbf{(a)} $\pi=(\tfrac13,\tfrac13,\tfrac13)$; $S_t\mid S_{t-1}=i\sim\mathrm{Cat}(p_{i,:})$;
$Y_t\mid S_t=i\sim\mathcal{N}(0,\sigma_i^2)$, $i=1,2,3$.

\textbf{(b)} $\phi=(\mathbf P,\sigma_1^2,\sigma_2^2,\sigma_3^2)$, $6+3=9$ free parameters.

\textbf{\emph{From May 2021 Q4.}}

\textbf{(a)} $(S_t)\in\{1,2,3\}$ Markov, $S_1\sim\pi$, transition $\mathbf P$;
$Y_t\mid S_t=i\sim\mathrm{Poisson}(\lambda_i)$, $i=1,2,3$.

\textbf{(b)} $\phi=(\pi,\mathbf P,\lambda_1,\lambda_2,\lambda_3)$: $2+6+3=11$ free.

\textbf{(c)} Forward algorithm:
$\alpha_1(i)=\pi_i e^{-\lambda_i}\lambda_i^{y_1}/y_1!$,\;\;
$\alpha_t(j)=(\sum_i\alpha_{t-1}(i)p_{ij})e^{-\lambda_j}\lambda_j^{y_t}/y_t!$;
$L(\phi)=\sum_i\alpha_T(i)$.

\textcolor{orange}{▲}

\textbf{Closing.} Linked snippets: \texttt{exam_may_2025_q3, exam_may_2023_q3, exam_may_2022_q6, exam_may_2021_q4}.

\emph{Image:} \texttt{images/master/master\_t5a\_ai.png} (canonical plot for this topic).""",
    "images": ["images/master/master_t5a_ai.png"]
}


# ============================================================================
# t5b — Decoding (Viterbi / forward–backward) + path-probability
# Linked: exam_jun_2024_q4   (1 exams → RED tier)
# ============================================================================
master_exercises_ts["t5b"] = {
    "title": 'Master — Decoding (Viterbi / forward–backward) + path-probability',
    "content": r"""\textbf{\textcolor{red}{MASTER --- Decoding (Viterbi / forward–backward) + path-probability \;[Topic: Hidden Markov Models (HMM) — model, likelihood, decoding]}}

\emph{Canonical dataset.} 2-state HMM with Gaussian emissions (\texttt{synthetic HMM}). One series; we use it to ground all sub-parts.

```R
# simulated HMM (see below)
```

\textbf{\textcolor{red}{▼ 1x: Jun 2024 Q4 ---}} CONSOLIDATED CONTENT FROM LINKED EXAM(S).

\textbf{\emph{From Jun 2024 Q4.}}

\textbf{(a)} Transition counts in the path: $n_{11}=3,n_{12}=3,n_{21}=2,n_{23}=2,n_{32}=1,n_{33}=1$.
$\Prob(y_{1:12}\mid Y_0=1)=\prod p_{y_{t-1},y_t}=p_{11}^3 p_{12}^3 p_{21}^2 p_{23}^2 p_{32}p_{33}$.

\textbf{(b.i) HMM, $k=2$.} $(S_t)\in\{1,2\}$, $\pi\in\Delta^1$, $\mathbf P\in\mathbb{R}^{2\times 2}$;
emission over $\{1,2,3\}$, $\mathbf E\in\mathbb{R}^{2\times 3}$. Parameters $\phi=(\pi,\mathbf P,\mathbf E)$,
$1+2+4=7$ free.

\textbf{(b.ii) Likelihood.} Forward algorithm:
$\alpha_1(i)=\pi_i e_{i,y_1}$, $\alpha_t(j)=(\sum_i\alpha_{t-1}(i)p_{ij})e_{j,y_t}$;
$L(\phi)=\sum_i\alpha_T(i)$.

\textbf{(b.iii) Decoding.} \emph{Global:} Viterbi
$\hat s_{1:T}=\argmax\Prob(S_{1:T}=s_{1:T}\mid y_{1:T})$, via
$\delta_t(j)=[\max_i\delta_{t-1}(i)p_{ij}]e_{j,y_t}$ + back-pointers.
\emph{Local:} $\hat s_t=\argmax\gamma_t(s)=\argmax\Prob(S_t=s\mid y_{1:T})$ from forward--backward.

\textcolor{red}{▲}

\textbf{Closing.} Linked snippets: \texttt{exam_jun_2024_q4}.

\emph{Image:} \texttt{images/master/master\_t5b\_ai.png} (canonical plot for this topic).""",
    "images": ["images/master/master_t5b_ai.png"]
}


# ============================================================================
# t6a — SSM / DLM general definition (univariate & multivariate)
# Linked: exam_sep_2025_q4, exam_may_2024_q5   (2 exams → DARK YELLOW tier)
# ============================================================================
master_exercises_ts["t6a"] = {
    "title": 'Master — SSM / DLM general definition (univariate & multivariate)',
    "content": r"""\textbf{\textcolor{red}{MASTER --- SSM / DLM general definition (univariate & multivariate) \;[Topic: State-space models & DLMs — general definition]}}

\emph{Canonical dataset.} annual flow of the river Nile (\texttt{Nile}). One series; we use it to ground all sub-parts.

```R
data(Nile); y <- as.numeric(Nile)
```

\textbf{\textcolor{[HTML]{B8860B}}{▼ 2-3x: Sep 2025 Q4, May 2024 Q5 ---}} CONSOLIDATED CONTENT FROM LINKED EXAM(S).

\textbf{\emph{From Sep 2025 Q4.}}

\textbf{(a) DLM} (DLMwR \S 2.3; keydef \textbf{16a}).
$\theta_t=G_t\theta_{t-1}+w_t$, $w_t\overset{\text{iid}}{\sim}\Nd_p(0,W_t)$;
$Y_t=F_t\theta_t+v_t$, $v_t\overset{\text{iid}}{\sim}\Nd_q(0,V_t)$;
$\theta_0\sim\Nd_p(m_0,C_0)$; $\{w_t\},\{v_s\},\theta_0$ mutually independent.

\textbf{(b)} $Y_t\perp Y_{1:t-1}\mid\theta_t$ (conditional indep.\ of obs.), so
$Y_t\mid(\theta_t,Y_{1:t-1})\sim\Nd_q(F_t\theta_t,V_t)$.

\textbf{(c)} $(\theta_t)$ Markov, so
$\theta_t\mid\theta_{0:t-1}\sim\Nd_p(G_t\theta_{t-1},W_t)$.

\textbf{(d) Prediction step (DLMwR \S 2.7.2, Prop.\ 2.2).}
Given $\theta_{t-1}\mid y_{1:t-1}\sim\Nd_p(m_{t-1},C_{t-1})$:
$$
\theta_t\mid y_{1:t-1}\sim\Nd_p(a_t,R_t),\quad a_t=G_t m_{t-1},\;R_t=G_t C_{t-1}G_t^{\top}+W_t,
$$
$$
\boxed{\;Y_t\mid y_{1:t-1}\sim\Nd_q(f_t,Q_t),\quad f_t=F_t a_t,\;Q_t=F_t R_t F_t^{\top}+V_t.\;}
$$
Linear combination of independent Gaussians is Gaussian.

\textbf{\emph{From May 2024 Q5.}}

\textbf{(a)} As Exam 1 Q4a (keydef \textbf{16a}).

\textbf{(b)} $\theta_t=G_t\theta_{t-1}+w_t$, $w_t\perp\theta_{t-1}\mid y_{1:t-1}$. So
$$
\boxed{\;\theta_t\mid y_{1:t-1}\sim\Nd_p(a_t,R_t),\quad a_t=G_t m_{t-1},\;R_t=G_t C_{t-1}G_t^{\top}+W_t.\;}
$$

\textcolor{[HTML]{B8860B}}{▲}

\textbf{Closing.} Linked snippets: \texttt{exam_sep_2025_q4, exam_may_2024_q5}.

\emph{Image:} \texttt{images/master/master\_t6a\_ai.png} (canonical plot for this topic).""",
    "images": ["images/master/master_t6a_ai.png"]
}


# ============================================================================
# t6b — SSM flexibility — non-stationarity & SV models (is it a DLM?)
# Linked: exam_sep_2025_q3, exam_may_2022_q4   (2 exams → DARK YELLOW tier)
# ============================================================================
master_exercises_ts["t6b"] = {
    "title": 'Master — SSM flexibility — non-stationarity & SV models (is it a DLM?)',
    "content": r"""\textbf{\textcolor{red}{MASTER --- SSM flexibility — non-stationarity & SV models (is it a DLM?) \;[Topic: State-space models & DLMs — general definition]}}

\emph{Canonical dataset.} annual flow of the river Nile (\texttt{Nile}). One series; we use it to ground all sub-parts.

```R
data(Nile); y <- as.numeric(Nile)
```

\textbf{\textcolor{[HTML]{B8860B}}{▼ 2-3x: Sep 2025 Q3, May 2022 Q4 ---}} CONSOLIDATED CONTENT FROM LINKED EXAM(S).

\textbf{\emph{From Sep 2025 Q3.}}

\textbf{YES, because} SSMs do not require $(Y_t)$ to be stationary --- the latent state
$(\theta_t)$ can be non-stationary (random walk, integrated, regime-switching) and the
observations inherit it. The Kalman filter/smoother is derived from Markovianity +
conditional Gaussianity, not from stationarity. Local level $\theta_t=\theta_{t-1}+w_t$,
$Y_t=\theta_t+v_t$ is the canonical SSM for $I(1)$ asset prices.

\textbf{\emph{From May 2022 Q4.}}

\textbf{(a) YES, SSM.} $(\theta_t)$ is a Markov chain (Gaussian AR(1)) and
$Y_t\mid\theta_t\sim\mathcal{N}(0,e^{\theta_t})$ depends only on $\theta_t$ (conditional indep.\ of
obs.).

\textbf{(b) NO, not a DLM.} The observation equation is non-linear in $\theta_t$ (multiplicative
$e^{\theta_t/2}$); variance depends on the state. Outside the linear-Gaussian template, KF
does not apply. \emph{Standard linearisation:} $\log Y_t^2=\theta_t+\log v_t^2$ (with
$\log v_t^2\sim\log\chi^2_1$ non-Gaussian) --- then approximated by a Gaussian mixture for
MCMC.

\textcolor{[HTML]{B8860B}}{▲}

\textbf{Closing.} Linked snippets: \texttt{exam_sep_2025_q3, exam_may_2022_q4}.

\emph{Image:} \texttt{images/master/master\_t6b\_ai.png} (canonical plot for this topic).""",
    "images": ["images/master/master_t6b_ai.png"]
}


# ============================================================================
# t7a — Random-walk + noise model — definition & independence proofs
# Linked: exam_may_2024_q3, exam_may_2023_q4   (2 exams → DARK YELLOW tier)
# ============================================================================
master_exercises_ts["t7a"] = {
    "title": 'Master — Random-walk + noise model — definition & independence proofs',
    "content": r"""\textbf{\textcolor{red}{MASTER --- Random-walk + noise model — definition & independence proofs \;[Topic: DLM building blocks — RW+noise, structural, regression, AR-as-DLM]}}

\emph{Canonical dataset.} Nile (RW+noise) + simulated LLT (\texttt{Nile + synthetic}). One series; we use it to ground all sub-parts.

```R
data(Nile); y <- as.numeric(Nile)
```

\textbf{\textcolor{[HTML]{B8860B}}{▼ 2-3x: May 2024 Q3, May 2023 Q4 ---}} CONSOLIDATED CONTENT FROM LINKED EXAM(S).

\textbf{\emph{From May 2024 Q3.}}

\textbf{(a)} $\theta_t=\theta_{t-1}+w_t$, $v_t\overset{\text{iid}}{\sim}\mathcal{N}(0,V)$, $w_t\overset{\text{iid}}{\sim}\mathcal{N}(0,W)$,
$\theta_0\sim\mathcal{N}(m_0,C_0)$, all mutually independent.

\textbf{(b)} $\theta_s=\theta_0+\sum_{u=1}^s w_u$, so $(\theta_1,\dots,\theta_t)$ is a function
of $(\theta_0,w_1,\dots,w_t)$. By the mutual independence in (a),
$v_t\perp(\theta_0,w_1,\dots,w_t)$, hence $v_t\perp(\theta_1,\dots,\theta_t)$.

\textbf{\emph{From May 2023 Q4.}}

\textbf{(a)} As Exam 5 Q3a.

\textbf{(b)} $\theta_s=\theta_0+\sum_{u=1}^s w_u$ for $s<t$, so $(\theta_1,\dots,\theta_{t-1})$
is a function of $(\theta_0,w_1,\dots,w_{t-1})$. By the mutual indep.\ assumption,
$w_t\perp(\theta_0,w_1,\dots,w_{t-1})$, hence $w_t\perp(\theta_1,\dots,\theta_{t-1})$.

\textcolor{[HTML]{B8860B}}{▲}

\textbf{Closing.} Linked snippets: \texttt{exam_may_2024_q3, exam_may_2023_q4}.

\emph{Image:} \texttt{images/master/master\_t7a\_ai.png} (canonical plot for this topic).""",
    "images": ["images/master/master_t7a_ai.png"]
}


# ============================================================================
# t7b — Local linear trend / structural BSM (trend + seasonality)
# Linked: exam_sep_2025_q5, exam_may_2021_q3   (2 exams → DARK YELLOW tier)
# ============================================================================
master_exercises_ts["t7b"] = {
    "title": 'Master — Local linear trend / structural BSM (trend + seasonality)',
    "content": r"""\textbf{\textcolor{red}{MASTER --- Local linear trend / structural BSM (trend + seasonality) \;[Topic: DLM building blocks — RW+noise, structural, regression, AR-as-DLM]}}

\emph{Canonical dataset.} Nile (RW+noise) + simulated LLT (\texttt{Nile + synthetic}). One series; we use it to ground all sub-parts.

```R
data(Nile); y <- as.numeric(Nile)
```

\textbf{\textcolor{[HTML]{B8860B}}{▼ 2-3x: Sep 2025 Q5, May 2021 Q3 ---}} CONSOLIDATED CONTENT FROM LINKED EXAM(S).

\textbf{\emph{From Sep 2025 Q5.}}

Local linear trend (DLMwR \S 3.2.2). Take $\theta_t=(\mu_t,\beta_t)^{\top}$:
$$
\boxed{\;
F=(1,\,0),\;V=\sigma^2,\quad
G=\begin{pmatrix}1&1\\0&1\end{pmatrix},\;W=\begin{pmatrix}\sigma_{w_1}^2&0\\0&\sigma_{w_2}^2\end{pmatrix},\;\;
\theta_0=\begin{pmatrix}\mu_0\\\beta_0\end{pmatrix}\sim\Nd_2(m_0,C_0).\;}
$$

\textbf{\emph{From May 2021 Q3.}}

\textbf{YES.} Use a \emph{structural} DLM = local linear trend + seasonal component (DLMwR
\S 3.2.2--3.2.3, Harvey's BSM):
$Y_t=\mu_t+\gamma_t+v_t$,
$\mu_t=\mu_{t-1}+\beta_{t-1}+w_{1,t}$, $\beta_t=\beta_{t-1}+w_{2,t}$,
$\gamma_t=-\sum_{j=1}^{s-1}\gamma_{t-j}+w_{3,t}$ ($s=12$).
The latent state carries the non-stationary components; KF + diffuse prior on
non-stationary parts handles them directly. (Recall: SSMs do not need stationarity.)

\textcolor{[HTML]{B8860B}}{▲}

\textbf{Closing.} Linked snippets: \texttt{exam_sep_2025_q5, exam_may_2021_q3}.

\emph{Image:} \texttt{images/master/master\_t7b\_ai.png} (canonical plot for this topic).""",
    "images": ["images/master/master_t7b_ai.png"]
}


# ============================================================================
# t7c — Time-varying-coefficient regression DLM
# Linked: exam_may_2025_q4, exam_jun_2022_q6   (2 exams → DARK YELLOW tier)
# ============================================================================
master_exercises_ts["t7c"] = {
    "title": 'Master — Time-varying-coefficient regression DLM',
    "content": r"""\textbf{\textcolor{red}{MASTER --- Time-varying-coefficient regression DLM \;[Topic: DLM building blocks — RW+noise, structural, regression, AR-as-DLM]}}

\emph{Canonical dataset.} Nile (RW+noise) + simulated LLT (\texttt{Nile + synthetic}). One series; we use it to ground all sub-parts.

```R
data(Nile); y <- as.numeric(Nile)
```

\textbf{\textcolor{[HTML]{B8860B}}{▼ 2-3x: May 2025 Q4, Jun 2022 Q6 ---}} CONSOLIDATED CONTENT FROM LINKED EXAM(S).

\textbf{\emph{From May 2025 Q4.}}

\textbf{(a)} State $\theta_t=(\alpha_t,\beta_t)^{\top}$; $F_t=(1,x_t)$:
$$
Y_t=F_t\theta_t+v_t,\;v_t\sim\mathcal{N}(0,\sigma^2);\quad
\theta_t=\theta_{t-1}+w_t,\;w_t\sim\Nd_2(0,W).
$$

\textbf{(b)} Smoothing distribution $=\pi(\theta_{0:n}\mid y_{1:n})$ (joint) or
$\pi(\theta_t\mid y_{1:n})=\Nd_2(s_t,S_t)$ (marginal at $t$, by RTS).
Pools \emph{past and future} data, more informative than filtering.

\textbf{(c) $\sigma^2$ unknown.} Either (i) MLE: maximise the prediction-error likelihood
$\ell(\sigma^2,W)$ (Exam 1 Q7a) and plug $\hat\sigma^2$ into the KF; or (ii) Bayesian
conjugate: $\sigma^2\sim\mathrm{IG}$, $W\sim\mathrm{IW}$, FFBS Gibbs on $(\theta_{0:n},\sigma^2,W)$.

\textbf{(d) Hierarchical DLM.} For hospitals $h=1,2$, one DLM each
$\theta_t^{(h)}$, tied via a shared population mean $\theta_0^{(h)}\sim\Nd_2(\mu,T)$,
hyperprior $\mu\sim\Nd_2(0,\Sigma_0)$. Inference shrinks each hospital's coefficient toward
$\mu$; degree of shrinkage learned from data.

\textbf{\emph{From Jun 2022 Q6.}}

\emph{Time-varying-coefficients DLM:}
$Y_t=\alpha_t+\beta_t x_t+v_t$; $(\alpha_t,\beta_t)^{\top}=(\alpha_{t-1},\beta_{t-1})^{\top}+w_t$,
$w_t\sim\Nd_2(0,W)$.
State $\theta_t=(\alpha_t,\beta_t)^{\top}$, $F_t=(1,x_t)$, $G=I_2$. \emph{Motivation:} no
parametric non-linear form; coefficients drift smoothly (RW), with $W$ controlling adaptation
speed.

\textcolor{[HTML]{B8860B}}{▲}

\textbf{Closing.} Linked snippets: \texttt{exam_may_2025_q4, exam_jun_2022_q6}.

\emph{Image:} \texttt{images/master/master\_t7c\_ai.png} (canonical plot for this topic).""",
    "images": ["images/master/master_t7c_ai.png"]
}


# ============================================================================
# t7d — Multivariate DLM & dependence between latent series
# Linked: exam_jun_2025_q4   (1 exams → RED tier)
# ============================================================================
master_exercises_ts["t7d"] = {
    "title": 'Master — Multivariate DLM & dependence between latent series',
    "content": r"""\textbf{\textcolor{red}{MASTER --- Multivariate DLM & dependence between latent series \;[Topic: DLM building blocks — RW+noise, structural, regression, AR-as-DLM]}}

\emph{Canonical dataset.} Nile (RW+noise) + simulated LLT (\texttt{Nile + synthetic}). One series; we use it to ground all sub-parts.

```R
data(Nile); y <- as.numeric(Nile)
```

\textbf{\textcolor{red}{▼ 1x: Jun 2025 Q4 ---}} CONSOLIDATED CONTENT FROM LINKED EXAM(S).

\textbf{\emph{From Jun 2025 Q4.}}

\textbf{(a)} Same as Q4(a) Exam 1.

\textbf{(b)} $\theta_{j,t}=\theta_{j,t-1}+w_{j,t}$, $Y_{j,t}=\theta_{j,t}+v_{j,t}$ for $j=1,2$:
$$
\boxed{\;F=I_2,\;G=I_2,\;V=\mathrm{diag}(\sigma_{v_1}^2,\sigma_{v_2}^2),\;
W=\mathrm{diag}(\sigma_{w_1}^2,\sigma_{w_2}^2).\;}
$$

\textbf{(c)} Three options.
(1) \emph{Correlated state noise:} make $W$ non-diagonal, $W_{12}=\rho_w\sigma_{w_1}\sigma_{w_2}$
--- contemporaneous correlation of increments.
(2) \emph{Common latent factor:} $\theta_t=Af_t$ with $f_t$ a 1-dim.\ RW
(common stochastic trend, factor / cointegration).
(3) \emph{Cross terms in $G$:} $G=\bigl(\begin{smallmatrix}1&\delta\\0&1\end{smallmatrix}\bigr)$
or full $G$ (states evolve as a VAR(1)).

\textcolor{red}{▲}

\textbf{Closing.} Linked snippets: \texttt{exam_jun_2025_q4}.

\emph{Image:} \texttt{images/master/master\_t7d\_ai.png} (canonical plot for this topic).""",
    "images": ["images/master/master_t7d_ai.png"]
}


# ============================================================================
# t7e — AR(p) as DLM (companion form)
# Linked: exam_jun_2024_q2   (1 exams → RED tier)
# ============================================================================
master_exercises_ts["t7e"] = {
    "title": 'Master — AR(p) as DLM (companion form)',
    "content": r"""\textbf{\textcolor{red}{MASTER --- AR(p) as DLM (companion form) \;[Topic: DLM building blocks — RW+noise, structural, regression, AR-as-DLM]}}

\emph{Canonical dataset.} Nile (RW+noise) + simulated LLT (\texttt{Nile + synthetic}). One series; we use it to ground all sub-parts.

```R
data(Nile); y <- as.numeric(Nile)
```

\textbf{\textcolor{red}{▼ 1x: Jun 2024 Q2 ---}} CONSOLIDATED CONTENT FROM LINKED EXAM(S).

\textbf{\emph{From Jun 2024 Q2.}}

\textbf{NO.} In a DLM $\theta_t$ is the \emph{latent / unobserved} process with its own
dynamics. The proposal puts (constant) coefficients as a non-evolving state and \emph{past
observations} into $F_t$ --- so there is no state innovation $w_t$, and
$F_t=(Y_{t-1},Y_{t-2})$ depends on past data, breaking the conditional indep.\
$Y_t\perp Y_{1:t-1}\mid\theta_t$.

\textbf{Correct DLM (companion form, DLMwR \S 3.2.5).} $\theta_t=(Y_t,Y_{t-1})^{\top}$,
$G=\bigl(\begin{smallmatrix}\alpha_1&\alpha_2\\1&0\end{smallmatrix}\bigr)$, $F=(1,0)$,
$V=0$, $W=\bigl(\begin{smallmatrix}\sigma^2&0\\0&0\end{smallmatrix}\bigr)$.

\textcolor{red}{▲}

\textbf{Closing.} Linked snippets: \texttt{exam_jun_2024_q2}.

\emph{Image:} \texttt{images/master/master\_t7e\_ai.png} (canonical plot for this topic).""",
    "images": ["images/master/master_t7e_ai.png"]
}


# ============================================================================
# t8a — Filtering distribution: definition, not just a point estimate
# Linked: exam_may_2024_q4, exam_jun_2022_q2, exam_may_2023_q6   (3 exams → DARK YELLOW tier)
# ============================================================================
master_exercises_ts["t8a"] = {
    "title": 'Master — Filtering distribution: definition, not just a point estimate',
    "content": r"""\textbf{\textcolor{red}{MASTER --- Filtering distribution: definition, not just a point estimate \;[Topic: Kalman filter — filtering distribution & update derivation]}}

\emph{Canonical dataset.} annual Nile flow (local-level DLM) (\texttt{Nile}). One series; we use it to ground all sub-parts.

```R
library(dlm); data(Nile)
```

\textbf{\textcolor{[HTML]{B8860B}}{▼ 2-3x: May 2024 Q4, Jun 2022 Q2, May 2023 Q6 ---}} CONSOLIDATED CONTENT FROM LINKED EXAM(S).

\textbf{\emph{From May 2024 Q4.}}

\textbf{NO}: filtering targets the conditional \emph{distribution} $\pi(\theta_t\mid y_{1:t})$,
not just a point estimate; and it concerns $\theta_t$ specifically (current state), not
$\theta_i$ for arbitrary $i$ (different problem: predicting $i>t$ or smoothing $i<t$).

\textbf{\emph{From Jun 2022 Q2.}}

\textbf{YES} in a DLM: the filtering distribution is Gaussian (by induction), so $(m_t,C_t)$
suffice. \emph{Not} in general SSMs (non-Gaussian / non-linear): the distribution can be
multimodal/skewed; need particle filters.

\textbf{\emph{From May 2023 Q6.}}

\textbf{(a)} As Exam 1 Q4a.

\textbf{(b) YES} in a DLM. By induction, $\theta_t\mid y_{1:t}$ is Gaussian (KF preserves
Gaussianity); $(m_t,C_t)$ then fully characterise the distribution. Marginal CI for component $j$:
$(m_t)_j\pm z_{1-\alpha/2}\sqrt{(C_t)_{jj}}$; joint ellipsoid via $C_t^{-1}$.
\emph{Caveat.} Fails for non-Gaussian SSMs (need particle filter) and for plug-in at $\widehat\phi$
(parameter uncertainty understated).

\textcolor{[HTML]{B8860B}}{▲}

\textbf{Closing.} Linked snippets: \texttt{exam_may_2024_q4, exam_jun_2022_q2, exam_may_2023_q6}.

\emph{Image:} \texttt{images/master/master\_t8a\_ai.png} (canonical plot for this topic).""",
    "images": ["images/master/master_t8a_ai.png"]
}


# ============================================================================
# t8b — KF predict + update derivation (with Bayes step)
# Linked: exam_jun_2025_q3, exam_may_2025_q5, exam_may_2021_q5   (3 exams → DARK YELLOW tier)
# ============================================================================
master_exercises_ts["t8b"] = {
    "title": 'Master — KF predict + update derivation (with Bayes step)',
    "content": r"""\textbf{\textcolor{red}{MASTER --- KF predict + update derivation (with Bayes step) \;[Topic: Kalman filter — filtering distribution & update derivation]}}

\emph{Canonical dataset.} annual Nile flow (local-level DLM) (\texttt{Nile}). One series; we use it to ground all sub-parts.

```R
library(dlm); data(Nile)
```

\textbf{\textcolor{[HTML]{B8860B}}{▼ 2-3x: Jun 2025 Q3, May 2025 Q5, May 2021 Q5 ---}} CONSOLIDATED CONTENT FROM LINKED EXAM(S).

\textbf{\emph{From Jun 2025 Q3.}}

\textbf{(a)} Keydef \textbf{15b}. SSM: $\bigl(Y_t,\theta_t\bigr)_{t\ge 1}$, $\theta_0\sim\pi$,
$\theta_t\mid\theta_{t-1}\sim f(\theta_t\mid\theta_{t-1})$ (Markov state),
$Y_t\mid\theta_t\sim f(y_t\mid\theta_t)$ (conditional indep.\ of obs.\ given state).

\textbf{(b)} \textbf{NO}: filtering is the \emph{distribution} $\pi(\theta_t\mid y_{1:t})$, not
a point estimate. The conditional mean is one summary; uncertainty (variance, credible
region) requires the full distribution.

\textbf{(c) DLM filtering update.} \emph{Predict} (Q4d of Exam 1):
$\theta_t\mid y_{1:t-1}\sim\mathcal{N}(a_t,R_t)$, $a_t=Gm_{t-1}$, $R_t=GC_{t-1}G^{\top}+W$;
$Y_t\mid y_{1:t-1}\sim\mathcal{N}(f_t,Q_t)$, $f_t=Fa_t$, $Q_t=FR_tF^{\top}+V$.
\emph{Update} via Bayes: $p(\theta_t\mid y_{1:t})\propto p(y_t\mid\theta_t)p(\theta_t\mid y_{1:t-1})$;
Gaussian $\times$ Gaussian $=$ Gaussian. The joint $(\theta_t,Y_t)\mid y_{1:t-1}$ is Gaussian
with cross-covariance $R_tF^{\top}$; conditioning on $Y_t=y_t$ gives
$$
\boxed{\;\theta_t\mid y_{1:t}\sim\mathcal{N}(m_t,C_t),\quad
m_t=a_t+K_t(y_t-f_t),\;C_t=R_t-K_tQ_tK_t^{\top},\;K_t=R_tF^{\top} Q_t^{-1}.\;}
$$

\textbf{\emph{From May 2025 Q5.}}

\textbf{(a)} $Y_t=\theta_t+v_t$, $\theta_t=\theta_{t-1}+w_t$, $v_t\overset{\text{iid}}{\sim}\mathcal{N}(0,V)$, $w_t\overset{\text{iid}}{\sim}\mathcal{N}(0,W)$,
$\theta_0\sim\mathcal{N}(m_0,C_0)$, indep. (DLMwR \S 2.3.2.)

\textbf{(b) KF steps.}
\emph{Predict}: $a_t=m_{t-1}$, $R_t=C_{t-1}+W$; $f_t=a_t$, $Q_t=R_t+V$.
\emph{Gain}: $K_t=R_t/Q_t$.
\emph{Update}: $m_t=a_t+K_t(y_t-f_t)$, $C_t=(1-K_t)R_t=V R_t/Q_t$.

\textbf{(c) NO} (unless $W=0$). The fixed point of $C^*=V(C^*+W)/(C^*+W+V)$ is
$C^*=\tfrac12(-W+\sqrt{W^2+4VW})>0$. The injection of fresh noise $W$ at every step prevents
$C_t\to 0$. (If $W=0$ the state is static and $C_t=VC_0/(V+tC_0)\to 0$ as $1/t$.)

\textbf{\emph{From May 2021 Q5.}}

\textbf{(a)} Keydef \textbf{16a}.

\textbf{(b)} 3 steps. \emph{Step 1 (state prediction)}: $\theta_t\mid y_{1:t-1}\sim\Nd_p(a_t,R_t)$,
$a_t=G_t m_{t-1}$, $R_t=G_t C_{t-1}G_t^{\top}+W_t$ (affine + Gaussian closure).
\emph{Step 2 (obs prediction)}: $Y_t\mid y_{1:t-1}\sim\Nd_q(f_t,Q_t)$, $f_t=F_t a_t$,
$Q_t=F_t R_t F_t^{\top}+V_t$.
\emph{Step 3 (filtering update)}: \textbf{Bayes' rule} on the conditionally Gaussian model
gives $p(\theta_t\mid y_{1:t})\propto p(y_t\mid\theta_t)p(\theta_t\mid y_{1:t-1})$;
Gaussian conditioning on the joint $(\theta_t,Y_t)\mid y_{1:t-1}$ yields
$m_t=a_t+K_t(y_t-f_t)$, $C_t=R_t-K_t Q_t K_t^{\top}$, $K_t=R_t F_t^{\top} Q_t^{-1}$.

\textbf{(c) Proof of Step 1.} $\theta_t=G_t\theta_{t-1}+w_t$ with $w_t\perp\sigma(\theta_{t-1},y_{1:t-1})$.
Conditional moments:
$\mathbb{E}[\theta_t\mid y_{1:t-1}]=G_t m_{t-1}=a_t$;
$\operatorname{Var}(\theta_t\mid y_{1:t-1})=G_t C_{t-1}G_t^{\top}+W_t=R_t$ (independence kills cross terms).
$(\theta_{t-1},w_t)\mid y_{1:t-1}$ jointly Gaussian $\Rightarrow$ linear combination
$\theta_t\mid y_{1:t-1}\sim\Nd_p(a_t,R_t)$. \hfill$\Box$

\textcolor{[HTML]{B8860B}}{▲}

\textbf{Closing.} Linked snippets: \texttt{exam_jun_2025_q3, exam_may_2025_q5, exam_may_2021_q5}.

\emph{Image:} \texttt{images/master/master\_t8b\_ai.png} (canonical plot for this topic).""",
    "images": ["images/master/master_t8b_ai.png"]
}


# ============================================================================
# t9a — Filtering vs smoothing — definitions & DAG-based proofs
# Linked: exam_sep_2025_q7, exam_jun_2022_q3, exam_may_2023_q5, exam_may_2022_q7   (4 exams → ORANGE tier)
# ============================================================================
master_exercises_ts["t9a"] = {
    "title": 'Master — Filtering vs smoothing — definitions & DAG-based proofs',
    "content": r"""\textbf{\textcolor{red}{MASTER --- Filtering vs smoothing — definitions & DAG-based proofs \;[Topic: Kalman smoother (RTS) & filtering vs smoothing]}}

\emph{Canonical dataset.} annual Nile flow (filter + smoother) (\texttt{Nile}). One series; we use it to ground all sub-parts.

```R
library(dlm); data(Nile)
```

\textbf{\textcolor{orange}{▼ 4+x: Sep 2025 Q7, Jun 2022 Q3, May 2023 Q5, May 2022 Q7 ---}} CONSOLIDATED CONTENT FROM LINKED EXAM(S).

\textbf{\emph{From Sep 2025 Q7.}}

\textbf{(a)} Prediction-error decomposition (DLMwR \S 4.1, eq.\ 4.1, p.\ 144):
$$
L(\phi\mid y_{1:T})=\prod_{t=1}^T \Nd_q\bigl(y_t;f_t(\phi),Q_t(\phi)\bigr),
$$
with $(f_t,Q_t)$ from the Kalman filter at $\phi$. Equivalently
$\ell(\phi)=-\tfrac12\sum_t[\,q\log(2\pi)+\log|Q_t|+e_t^{\top} Q_t^{-1}e_t\,]$.
$\widehat\phi$ numerically (BFGS / EM).

\textbf{(b) Definitions.}
\emph{Filtering:} $\pi(\theta_t\mid y_{1:t})$ --- state given data \emph{up to now}.
\emph{Joint smoothing:} $\pi(\theta_{0:T}\mid y_{1:T})$ --- full latent path given \emph{all} data.
\emph{Marginal smoothing:} $\pi(\theta_t\mid y_{1:T})$, $t<T$ --- past state given all data
(including \emph{after} $t$).

\textbf{(c) RTS smoother} (DLMwR Prop.\ 2.4, p.\ 61). Run KF forward, store $(m_t,C_t)$ and
$(a_{t+1},R_{t+1})$. Backward, with $\theta_t\mid y_{1:T}\sim\Nd_p(s_t,S_t)$:
$$
\boxed{\;
s_T=m_T,\,S_T=C_T;\quad
J_t=C_t G_{t+1}^{\top} R_{t+1}^{-1};\quad
s_t=m_t+J_t(s_{t+1}-a_{t+1}),\;\;
S_t=C_t-J_t(R_{t+1}-S_{t+1})J_t^{\top}.
\;}
$$
\emph{Proof hint.} Backward Markov: $\theta_t\mid(\theta_{t+1},y_{1:T})=\theta_t\mid(\theta_{t+1},y_{1:t})$
(future data add no info on $\theta_t$ once $\theta_{t+1}$ is known). Gaussian conditioning on
the joint $(\theta_t,\theta_{t+1})\mid y_{1:t}$ gives a Gaussian backward kernel with
regression coefficient $J_t$; marginalize against
$p(\theta_{t+1}\mid y_{1:T})=\mathcal{N}(s_{t+1},S_{t+1})$.

\textbf{\emph{From Jun 2022 Q3.}}

\textbf{NO}: same point as for filtering. Smoothing targets the \emph{distribution}
$\pi(\theta_t\mid y_{1:T})$ (or joint $\pi(\theta_{0:T}\mid y_{1:T})$); the mean is just one
summary. In a DLM, $(s_t,S_t)$ from RTS together describe the (Gaussian) marginal smoothing
distribution.

\textbf{\emph{From May 2023 Q5.}}

DLM DAG: $\theta_0\to\theta_1\to\cdots\to\theta_T$ with $\theta_s\to Y_s$ at each $s$. Every
directed path from $\theta_t$ to any $Y_s$ ($s\ge t+1$) passes through $\theta_{t+1}$, which
is a serial/chain node. Conditioning on $\theta_{t+1}$ blocks every such path; by $d$-separation,
$\theta_t\perp(Y_{t+1},\dots,Y_T)\mid\theta_{t+1}$. (This is the backward-Markov property used
in the RTS smoother.)

\textbf{\emph{From May 2022 Q7.}}

\emph{Filtering:} $\pi(\theta_t\mid y_{1:t})$ --- state given data \emph{up to now} (online).
\emph{Smoothing:} $\pi(\theta_{0:T}\mid y_{1:T})$ (joint) or $\pi(\theta_t\mid y_{1:T})$
(marginal) --- state given \emph{all} sampled data, including \emph{future} of $t$ (offline).
Smoothing is more informative: $\operatorname{Var}(\theta_t\mid y_{1:T})\preceq\operatorname{Var}(\theta_t\mid y_{1:t})$.
For DLM: filtering = forward KF; smoothing = RTS backward sweep.

\textcolor{orange}{▲}

\textbf{Closing.} Linked snippets: \texttt{exam_sep_2025_q7, exam_jun_2022_q3, exam_may_2023_q5, exam_may_2022_q7}.

\emph{Image:} \texttt{images/master/master\_t9a\_ai.png} (canonical plot for this topic).""",
    "images": ["images/master/master_t9a_ai.png"]
}


# ============================================================================
# t10a — Predictive distribution N(f_t,Q_t) — derivation
# Linked: exam_jun_2024_q6   (1 exams → RED tier)
# ============================================================================
master_exercises_ts["t10a"] = {
    "title": 'Master — Predictive distribution N(f_t,Q_t) — derivation',
    "content": r"""\textbf{\textcolor{red}{MASTER --- Predictive distribution N(f_t,Q_t) — derivation \;[Topic: One-step-ahead prediction & forecast function]}}

\emph{Canonical dataset.} annual Nile flow (k-step forecast) (\texttt{Nile}). One series; we use it to ground all sub-parts.

```R
library(dlm); data(Nile)
```

\textbf{\textcolor{red}{▼ 1x: Jun 2024 Q6 ---}} CONSOLIDATED CONTENT FROM LINKED EXAM(S).

\textbf{\emph{From Jun 2024 Q6.}}

\textbf{(a) NO.} $Y_t\in\mathbb{R}^q$ general; $F_t$ is $q\times p$.

\textbf{(b)} $Y_t=F_t\theta_t+v_t$, $v_t\perp\theta_t$, $v_t\sim\Nd_q(0,V_t)$. Linear combo:
$\boxed{Y_t\mid y_{1:t-1}\sim\Nd_q(F_t a_t,\,F_t R_t F_t^{\top}+V_t)=\Nd_q(f_t,Q_t)}$.

\textcolor{red}{▲}

\textbf{Closing.} Linked snippets: \texttt{exam_jun_2024_q6}.

\emph{Image:} \texttt{images/master/master\_t10a\_ai.png} (canonical plot for this topic).""",
    "images": ["images/master/master_t10a_ai.png"]
}


# ============================================================================
# t10b — Forecast function, k-step intervals, SES & loss functions
# Linked: exam_jun_2024_q5, exam_jun_2022_q4, exam_may_2022_q1   (3 exams → DARK YELLOW tier)
# ============================================================================
master_exercises_ts["t10b"] = {
    "title": 'Master — Forecast function, k-step intervals, SES & loss functions',
    "content": r"""\textbf{\textcolor{red}{MASTER --- Forecast function, k-step intervals, SES & loss functions \;[Topic: One-step-ahead prediction & forecast function]}}

\emph{Canonical dataset.} annual Nile flow (k-step forecast) (\texttt{Nile}). One series; we use it to ground all sub-parts.

```R
library(dlm); data(Nile)
```

\textbf{\textcolor{[HTML]{B8860B}}{▼ 2-3x: Jun 2024 Q5, Jun 2022 Q4, May 2022 Q1 ---}} CONSOLIDATED CONTENT FROM LINKED EXAM(S).

\textbf{\emph{From Jun 2024 Q5.}}

\textbf{(a)} $\kappa=W/V$.

\textbf{(b)} From filter, $\theta_t\mid y_{1:t}\sim\mathcal{N}(m_t,C_t)$. Iterate state $k$ times:
$\theta_{t+k}=\theta_t+\sum_{j=1}^k w_{t+j}$, $Y_{t+k}=\theta_{t+k}+v_{t+k}$. Hence
$$
\boxed{\;Y_{t+k}\mid y_{1:t}\sim\mathcal{N}(m_t,C_t+kW+V).\;}
$$
Forecast function is \emph{flat}: $\hat y_{t+k\mid t}=m_t$ for every $k$.
For $k=2$, CI: $m_t\pm z_{1-\alpha/2}\sqrt{C_t+2W+V}$.

\textbf{\emph{From Jun 2022 Q4.}}

\textbf{(a)} As Exam 5 Q3a.

\textbf{(b)} KF predict step: $a_t=m_{t-1}$, $R_t=C_{t-1}+W$;
$f_t=a_t=m_{t-1}$, $Q_t=R_t+V=C_{t-1}+W+V$.

\textbf{(c.i) Quadratic loss.} Bayes estimator = conditional mean:
$\hat y_t=\mathbb{E}[Y_t\mid y_{1:t-1}]=f_t=m_{t-1}$.

\textbf{(c.ii) Absolute loss.} Bayes estimator = conditional median.
$Y_t\mid y_{1:t-1}\sim\mathcal{N}(f_t,Q_t)$ symmetric $\Rightarrow$ median $=$ mean,
so $\hat y_t=f_t=m_{t-1}$ as well. (Differ in non-Gaussian / asymmetric predictives.)

\textbf{\emph{From May 2022 Q1.}}

\textbf{NO}, as a stand-alone recursion --- it returns only a point forecast.
\emph{However}, SES is the steady-state KF of the local-level DLM (Case 0 in the
forecasting-algorithms note), so embedding SES in that DLM gives
$Y_{t+1}\mid y_{1:t}\sim\mathcal{N}(\hat y_{t+1\mid t},Q_{t+1})$ with $Q_{t+1}=C_t+W+V$. The DLM
\emph{has} the intervals; the algorithm does not.

\textcolor{[HTML]{B8860B}}{▲}

\textbf{Closing.} Linked snippets: \texttt{exam_jun_2024_q5, exam_jun_2022_q4, exam_may_2022_q1}.

\emph{Image:} \texttt{images/master/master\_t10b\_ai.png} (canonical plot for this topic).""",
    "images": ["images/master/master_t10b_ai.png"]
}


# ============================================================================
# t11a — Innovations: zero-mean, orthogonality, standardisation
# Linked: exam_jun_2025_q5, exam_may_2024_q6, exam_may_2021_q6   (3 exams → DARK YELLOW tier)
# ============================================================================
master_exercises_ts["t11a"] = {
    "title": 'Master — Innovations: zero-mean, orthogonality, standardisation',
    "content": r"""\textbf{\textcolor{red}{MASTER --- Innovations: zero-mean, orthogonality, standardisation \;[Topic: Forecast errors / innovations & model checking]}}

\emph{Canonical dataset.} annual Nile flow (innovations) (\texttt{Nile}). One series; we use it to ground all sub-parts.

```R
library(dlm); data(Nile)
```

\textbf{\textcolor{[HTML]{B8860B}}{▼ 2-3x: Jun 2025 Q5, May 2024 Q6, May 2021 Q6 ---}} CONSOLIDATED CONTENT FROM LINKED EXAM(S).

\textbf{\emph{From Jun 2025 Q5.}}

By definition of conditional expectation and the tower property:
$\mathbb{E}[e_t]=\mathbb{E}[Y_t-\mathbb{E}(Y_t\mid Y_{1:t-1})]=\mathbb{E}[Y_t]-\mathbb{E}[\mathbb{E}(Y_t\mid Y_{1:t-1})]=\mathbb{E}[Y_t]-\mathbb{E}[Y_t]=0$.
(One line: $\mathbb{E}[e_t\mid\mathcal F_{t-1}]=0$, so unconditionally $\mathbb{E}[e_t]=0$.)

\textbf{\emph{From May 2024 Q6.}}

\textbf{NO.} The innovations are: zero-mean ($\mathbb{E} e_t=0$), \emph{uncorrelated} across $t$
(orthogonality), Gaussian \emph{conditionally on $y_{1:t-1}$} with variance $Q_t$ \emph{depending
on $t$}. So $e_t\sim\mathcal{N}(0,Q_t)$, not unit variance. \emph{Standardised innovations}
$\tilde e_t=Q_t^{-1/2}e_t\overset{\text{iid}}{\sim}\mathcal{N}(0,1)$ --- that is the object used in model checking
(QQ-plot, Ljung--Box on $\tilde e_t$; see keydef on model checking).

\textbf{\emph{From May 2021 Q6.}}

\emph{Mean.} $\mathbb{E}[e_t\mid\mathcal F_{t-1}]=f_t-f_t=0\Rightarrow\mathbb{E}[e_t]=0$.

\emph{Orthogonality} for $t>s$: $e_s$ is $\mathcal F_{t-1}$-measurable (since $s\le t-1$).
By tower + pull out:
$\mathbb{E}[e_t e_s]=\mathbb{E}[\mathbb{E}[e_t\mid\mathcal F_{t-1}]e_s]=\mathbb{E}[0\cdot e_s]=0$.
Hence $\operatorname{Cov}(e_t,e_s)=0$. $(e_t)$ is a martingale-difference sequence; standardised
$\tilde e_t=Q_t^{-1/2}e_t$ are i.i.d.\ $\mathcal{N}(0,1)$ under correct specification --- used in
model checking (QQ plot, Ljung--Box).

\textcolor{[HTML]{B8860B}}{▲}

\textbf{Closing.} Linked snippets: \texttt{exam_jun_2025_q5, exam_may_2024_q6, exam_may_2021_q6}.

\emph{Image:} \texttt{images/master/master\_t11a\_ai.png} (canonical plot for this topic).""",
    "images": ["images/master/master_t11a_ai.png"]
}


# ============================================================================
# t12a — Likelihood of phi via prediction-error decomposition (MLE)
# Linked: exam_jun_2024_q7, exam_may_2024_q7   (2 exams → DARK YELLOW tier)
# ============================================================================
master_exercises_ts["t12a"] = {
    "title": 'Master — Likelihood of phi via prediction-error decomposition (MLE)',
    "content": r"""\textbf{\textcolor{red}{MASTER --- Likelihood of phi via prediction-error decomposition (MLE) \;[Topic: Parameter estimation in DLM — MLE & prediction-error decomp.]}}

\emph{Canonical dataset.} annual Nile flow (MLE of V, W) (\texttt{Nile}). One series; we use it to ground all sub-parts.

```R
library(dlm); data(Nile)
```

\textbf{\textcolor{[HTML]{B8860B}}{▼ 2-3x: Jun 2024 Q7, May 2024 Q7 ---}} CONSOLIDATED CONTENT FROM LINKED EXAM(S).

\textbf{\emph{From Jun 2024 Q7.}}

\textbf{(a)} Frequentist: $p(y_{1:n}\mid\phi)=\prod_t\Nd_q(y_t;f_t(\phi),Q_t(\phi))$
(prediction-error decomposition, DLMwR eq.\ 4.1). \emph{Yes, depends on $\phi$.}

\textbf{(b)} Bayesian: prior predictive (evidence)
$$
p(y_{1:n})=\int p(y_{1:n}\mid\phi)\pi(\phi)d\phi,
$$
does \emph{not} depend on $\phi$ (integrated out). Typically intractable; used for Bayes
factors.

\textbf{\emph{From May 2024 Q7.}}

\textbf{(a)} Prediction-error decomposition (DLMwR \S 4.1):
$L(\phi)=\prod_t\Nd_q(y_t;f_t(\phi),Q_t(\phi))$.
$\widehat\phi=\argmax_\phi\ell(\phi)$ by numerical optimization (BFGS), each evaluation = one
KF pass.

\textbf{(b)} Run KF at $\widehat\phi$; one-step ahead
$Y_t\mid y_{1:t-1}\sim\Nd_q(f_t(\widehat\phi),Q_t(\widehat\phi))$. \emph{Ignores parameter
uncertainty}; intervals too narrow.

\textbf{(c)} Bayesian: prior $\pi(\phi)$, posterior by MCMC (Gibbs / FFBS), predictive
$p(y_t\mid y_{1:t-1})=\int p(y_t\mid y_{1:t-1},\phi)p(\phi\mid y_{1:t-1})d\phi$ ---
mixture of Gaussians, properly inflated for parameter uncertainty.

\textcolor{[HTML]{B8860B}}{▲}

\textbf{Closing.} Linked snippets: \texttt{exam_jun_2024_q7, exam_may_2024_q7}.

\emph{Image:} \texttt{images/master/master\_t12a\_ai.png} (canonical plot for this topic).""",
    "images": ["images/master/master_t12a_ai.png"]
}


# ============================================================================
# t13a — Conjugate Normal–Normal posterior (static theta / Case A)
# Linked: exam_sep_2025_q8, exam_jun_2022_q7   (2 exams → DARK YELLOW tier)
# ============================================================================
master_exercises_ts["t13a"] = {
    "title": 'Master — Conjugate Normal–Normal posterior (static theta / Case A)',
    "content": r"""\textbf{\textcolor{red}{MASTER --- Conjugate Normal–Normal posterior (static theta / Case A) \;[Topic: Bayesian inference, conjugate updates & MCMC]}}

\emph{Canonical dataset.} monthly atmospheric CO2 (Bayesian DLM) (\texttt{co2}). One series; we use it to ground all sub-parts.

```R
data(co2); y <- as.numeric(co2)
```

\textbf{\textcolor{[HTML]{B8860B}}{▼ 2-3x: Sep 2025 Q8, Jun 2022 Q7 ---}} CONSOLIDATED CONTENT FROM LINKED EXAM(S).

\textbf{\emph{From Sep 2025 Q8.}}

Conjugate prior $\theta\sim\mathcal{N}(m_0,C_0)$ (flat prior $\Leftrightarrow C_0\to\infty$).

\textbf{(a)} Conditional on $\theta$, $Y_t$ are i.i.d.\ $\mathcal{N}(\theta,1)$. So
$$
p(y_{1:t},\theta)=\mathcal{N}(\theta;m_0,C_0)\prod_{s=1}^t\mathcal{N}(y_s;\theta,1).
$$

\textbf{(b) Static-$\theta$ (Case A) update} --- conjugate Normal-Normal:
$$
\boxed{\;\theta\mid y_{1:n}\sim\mathcal{N}(m_n,C_n),\quad
\frac{1}{C_n}=\frac{1}{C_0}+\frac{n}{\sigma^2},\quad m_n=C_n\Bigl(\tfrac{m_0}{C_0}+\tfrac{n\bar y_n}{\sigma^2}\Bigr).\;}
$$
(Precision adds; $m_n$ = precision-weighted average of $m_0$ and $\bar y_n$.)
\emph{Numerics ($\sigma^2=1$, $n=20$, $\bar y_n=4$, flat prior):} $C_n=1/20=0.05$, $m_n=4$, so
$\theta\mid y_{1:20}\sim\mathcal{N}(4,0.05)$; 95\% CI $4\pm 1.96\sqrt{0.05}\approx[3.56,4.44]$.

\textbf{\emph{From Jun 2022 Q7.}}

$W=0\Rightarrow\theta_t=\theta_0=:\theta$ constant. Conjugate static-$\theta$ model
(Case A; ``Posterior mean and precision''):
$$
\boxed{\;\theta_n\mid y_{1:n}\sim\mathcal{N}(m_n,C_n),\quad
\tfrac{1}{C_n}=\tfrac{1}{C_0}+\tfrac{n}{V},\quad
m_n=C_n\bigl(\tfrac{m_0}{C_0}+\tfrac{n\bar y_n}{V}\bigr).\;}
$$
(Closed form: $m_n=(Vm_0+nC_0\bar y_n)/(V+nC_0)$, $C_n=C_0V/(V+nC_0)$.)

\textcolor{[HTML]{B8860B}}{▲}

\textbf{Closing.} Linked snippets: \texttt{exam_sep_2025_q8, exam_jun_2022_q7}.

\emph{Image:} \texttt{images/master/master\_t13a\_ai.png} (canonical plot for this topic).""",
    "images": ["images/master/master_t13a_ai.png"]
}


# ============================================================================
# t13b — Bayesian predictive distribution integrating out phi (+ MCMC)
# Linked: exam_jun_2025_q6, exam_may_2025_q6, exam_may_2022_q8   (3 exams → DARK YELLOW tier)
# ============================================================================
master_exercises_ts["t13b"] = {
    "title": 'Master — Bayesian predictive distribution integrating out phi (+ MCMC)',
    "content": r"""\textbf{\textcolor{red}{MASTER --- Bayesian predictive distribution integrating out phi (+ MCMC) \;[Topic: Bayesian inference, conjugate updates & MCMC]}}

\emph{Canonical dataset.} monthly atmospheric CO2 (Bayesian DLM) (\texttt{co2}). One series; we use it to ground all sub-parts.

```R
data(co2); y <- as.numeric(co2)
```

\textbf{\textcolor{[HTML]{B8860B}}{▼ 2-3x: Jun 2025 Q6, May 2025 Q6, May 2022 Q8 ---}} CONSOLIDATED CONTENT FROM LINKED EXAM(S).

\textbf{\emph{From Jun 2025 Q6.}}

Put a prior $\pi(\phi)$. The predictive marginalises both the state \emph{and} the parameters:
$$
\boxed{\;p(y_{t+1}\mid y_{1:t})=\int p(y_{t+1}\mid y_{1:t},\phi)\,p(\phi\mid y_{1:t})\,d\phi.\;}
$$
For each fixed $\phi$, $p(y_{t+1}\mid y_{1:t},\phi)=\mathcal{N}(f_{t+1}(\phi),Q_{t+1}(\phi))$ from the KF.
$p(\phi\mid y_{1:t})\propto L(\phi)\pi(\phi)$ is explored by MCMC. The integral is a
mixture of Gaussians: properly inflated for parameter uncertainty (\emph{wider} than the
plug-in $\mathcal{N}(f_{t+1}(\widehat\phi),Q_{t+1}(\widehat\phi))$).

\textbf{\emph{From May 2025 Q6.}}

\textbf{(a)} Prior $\pi(\phi)$; $\theta_0\sim\Nd_p(m_0,C_0)$;
$Y_t=F_t\theta_t+v_t$, $v_t\sim\Nd_m(0,V_t(\phi))$;
$\theta_t=G_t\theta_{t-1}+w_t$, $w_t\sim\Nd_p(0,W_t(\phi))$.

\textbf{(b)} $p(\theta_t\mid y_{1:t})=\int p(\theta_t\mid y_{1:t},\phi)p(\phi\mid y_{1:t})d\phi$.
For each $\phi$ fixed, $p(\theta_t\mid y_{1:t},\phi)=\Nd_p(m_t(\phi),C_t(\phi))$ (KF). The
marginal is a \emph{mixture of Gaussians}, generally non-Gaussian. Approximated by MCMC
(draw $\phi^{(s)}$, run KF, average). Wider intervals than the plug-in at $\widehat\phi$
(honest uncertainty).

\textbf{\emph{From May 2022 Q8.}}

\textbf{(a)} Prior $\pi(\phi)$, posterior $p(\phi\mid y_{1:t-1})\propto L(\phi)\pi(\phi)$ via
MCMC. Predictive:
$$
p(y_t\mid y_{1:t-1})=\int p(y_t\mid y_{1:t-1},\phi)p(\phi\mid y_{1:t-1})d\phi
\approx\tfrac{1}{S}\sum_s\Nd_q(y_t;f_t(\phi^{(s)}),Q_t(\phi^{(s)})),
$$
mixture of Gaussians, properly inflated for $\phi$-uncertainty.

\textbf{(b)} MCMC constructs an ergodic Markov chain on the parameter/latent space whose
\emph{stationary distribution} is the target posterior. Detailed balance
$\pi(x)K(x,y)=\pi(y)K(y,x)$ ensures $\pi$-invariance; ergodicity (irreducibility + aperiodicity)
gives the LLN $\tfrac1S\sum_s h(X^{(s)})\to\Exp_\pi[h(X)]$ (Theorem 2.1 applied to the
sampler's chain). So MCMC \emph{is} a Markov chain, designed to converge to the right limit
distribution.

\textcolor{[HTML]{B8860B}}{▲}

\textbf{Closing.} Linked snippets: \texttt{exam_jun_2025_q6, exam_may_2025_q6, exam_may_2022_q8}.

\emph{Image:} \texttt{images/master/master\_t13b\_ai.png} (canonical plot for this topic).""",
    "images": ["images/master/master_t13b_ai.png"]
}
