"""Masters wave A — T2 + T3."""
master_exercises_ts = {}

master_exercises_ts["t2a"] = {
    "title": "Master — Weak stationarity — definition & examples",
    "content": r"""\textbf{\textcolor{red}{MASTER --- Weak stationarity — definition & examples}}

\emph{Canonical framework:} take a univariate time series $(Y_t)_{t\ge 1}$ — for concreteness picture the \texttt{LakeHuron} annual-levels series. Two functional summaries describe its first two moments: the \textbf{mean function} $\mu(t)=\mathbb{E}[Y_t]$ and the \textbf{autocovariance function} $\gamma(s,t)=\operatorname{Cov}(Y_s,Y_t)$, both of which are well-defined whenever $\mathbb{E}[Y_t^2]<\infty$. \textbf{Weak (covariance) stationarity} requires that both first and second moments be \emph{time-invariant}, so that pooling across $t$ is meaningful for inference. The questions below cover (i) the formal definition, (ii) the two functional summaries that show up in it, (iii) canonical stationary examples, and (iv) what happens when stationarity fails and an ARMA fit is attempted.

---

<details class="master-subpart" open>
<summary><span class="tag tag-2plus">≥2 ex</span> (Q1) <em>When is it (weakly) stationary?</em>  [Sep 2025 Q2b, Jun 2024 Q1a]</summary>

The series $(Y_t)_{t\ge 1}$ is \textbf{weakly (covariance) stationary} iff $\mathbb{E}[Y_t]$ and $\mathbb{E}[Y_t Y_{t+h}]$ are finite and do \emph{not} depend on $t$, for every lag $h\in\mathbb{Z}$:
\[
\mu(t)=\mu,\qquad \sigma^2(t)=\sigma^2,\qquad \gamma(t,t+h)=\tilde\gamma(h).
\]

Equivalently:

- \textbf{Constant mean} — $\mathbb{E}[Y_t]=\mu$ for all $t$.
- \textbf{Constant variance} — $\operatorname{Var}(Y_t)=\sigma^2<\infty$ for all $t$.
- \textbf{Lag-only autocovariance} — $\gamma(s,t)$ depends only on $|s-t|=h$.

\emph{Why it matters.} Stationarity is what makes a single observed path $(y_1,\dots,y_T)$ informative about the underlying distribution: time-averaging substitutes for ensemble-averaging, the correlogram pools across $t$ to estimate one function of the lag, and ARMA-type models with constant coefficients are sensible.

\emph{Strict vs.\ weak.} Strict stationarity asks that the entire joint law of $(Y_{t_1},\dots,Y_{t_k})$ be invariant under time-shifts; weak stationarity asks only that the first two moments be invariant. For Gaussian processes, weak $\Leftrightarrow$ strict.

</details>

<details class="master-subpart" open>
<summary><span class="tag tag-2plus">≥2 ex</span> (Q2) <em>Define its mean function and autocovariance function.</em>  [Sep 2025 Q2a]</summary>

Two functions of time index $t$ (and lag) summarise the first-two-moment structure of $(Y_t)$:

- \textbf{Mean function} — $\mu(t)=\mathbb{E}[Y_t]$, a function of \emph{one} argument $t$. Records how the centring evolves over time.
- \textbf{Autocovariance function (ACVF)} — $\gamma(s,t)=\operatorname{Cov}(Y_s,Y_t)=\mathbb{E}\big[(Y_s-\mu(s))(Y_t-\mu(t))\big]$, a function of \emph{two} arguments $(s,t)$.

\emph{Existence.} Both are well-defined whenever $\mathbb{E}[Y_t^2]<\infty$ — no stationarity assumption needed.

\emph{Reduction under stationarity.} If $(Y_t)$ is weakly stationary, $\mu(t)\equiv\mu$ collapses to a scalar, and $\gamma(s,t)$ depends only on the lag $h=t-s$, giving a 1-argument function $\tilde\gamma(h)$. The \textbf{autocorrelation function (ACF)} is the normalised version $\rho(h)=\tilde\gamma(h)/\tilde\gamma(0)$.

\emph{R sample analogues:}

`mean(y)                          ## sample mean estimating mu`
`acf(y, type="covariance")        ## sample ACVF`
`acf(y, type="correlation")       ## sample ACF`

</details>

<details class="master-subpart" open>
<summary><span class="tag tag-2plus">≥2 ex</span> (Q3) <em>Provide an example of a stationary time series.</em>  [Jun 2024 Q1b]</summary>

Two canonical examples — the workhorses of intro time-series courses.

\textbf{(i) White noise.} $Y_t\overset{\mathrm{iid}}{\sim}\mathcal{N}(0,\sigma^2)$. Trivially:
\[
\mu(t)=0,\qquad \gamma(t,t+h)=\sigma^2\,\mathbf{1}\{h=0\}.
\]
Both are $t$-free, so $(Y_t)$ is weakly stationary (and in fact strictly stationary by iid-ness).

\textbf{(ii) Causal AR(1).} $Y_t=\phi\,Y_{t-1}+\varepsilon_t$ with $\varepsilon_t\overset{\mathrm{iid}}{\sim}\mathcal{N}(0,\sigma^2)$, $|\phi|<1$, started from the stationary distribution $Y_0\sim\mathcal{N}\!\big(0,\sigma^2/(1-\phi^2)\big)$.

\emph{Why stationary.} By back-substitution, $Y_t=\sum_{j\ge 0}\phi^j\varepsilon_{t-j}$ (geometric series converges because $|\phi|<1$). Hence
\[
\mathbb{E}[Y_t]=0,\qquad \gamma(h)=\frac{\sigma^2\,\phi^{|h|}}{1-\phi^2},
\]
both $t$-free. The ACF $\rho(h)=\phi^{|h|}$ decays geometrically — the visual signature of an AR(1) on a correlogram.

\emph{R simulation:}

`mean(y)                ## ~ 0`
`var(y)                 ## ~ 1/(1-0.49) = 1.96`
`acf(y, lag.max=20)     ## sample ACF should match 0.7^h`

</details>

<details class="master-subpart" open>
<summary><span class="tag tag-2plus">≥2 ex</span> (Q4) <em>Suppose that the time series is \textbf{not} stationary. Can you use an ARMA model for it? Start your reply with: YES, because\ldots, NO because\ldots, or YES but only if\ldots</em>  [Sep 2025 Q2c]</summary>

\textbf{NO} — at least not directly. An ARMA$(p,q)$ model
\[
Y_t=\phi_1 Y_{t-1}+\dots+\phi_p Y_{t-p}+\varepsilon_t+\theta_1\varepsilon_{t-1}+\dots+\theta_q\varepsilon_{t-q}
\]
has \emph{constant} coefficients $(\phi_i,\theta_j,\sigma^2)$. A causal/invertible ARMA is automatically weakly stationary, with constant mean (zero, or $\mu$ after centring), constant variance, and lag-only ACVF. So an ARMA fit cannot reproduce a time-varying mean, time-varying variance, trend, seasonality, or unit-root behaviour.

\textbf{YES, but only if} the non-stationarity is first \emph{removed} or \emph{absorbed}:

- \textbf{Unit roots / stochastic trend} — difference: fit ARMA$(p,q)$ to $(1-B)^d Y_t$, i.e.\ ARIMA$(p,d,q)$.
- \textbf{Deterministic seasonality of period $s$} — seasonal difference $(1-B^s)$, i.e.\ SARIMA.
- \textbf{Deterministic trend / mean shifts} — regress out the trend (polynomial in $t$, splines, dummies) and fit ARMA to the residual.
- \textbf{Heteroskedasticity} — variance-stabilise (log, Box–Cox) before fitting.

\emph{Alternative:} if the non-stationarity is structural and easier to model directly (local-level, local-linear-trend, seasonal), switch to a \textbf{DLM} / structural time series — these do \emph{not} require stationarity.

`auto.arima(y)                                ## let it select d,p,q`

</details>

\textbf{Linked exams:} \texttt{exam_sep_2025_q2}, \texttt{exam_jun_2024_q1}.""",
    "images": ["images/master/master_t2a_ai.png"]
}

master_exercises_ts["t2b"] = {
    "title": "Master — ACVF / correlogram — when defined?",
    "content": r"""\textbf{\textcolor{red}{MASTER --- ACVF / correlogram — when defined?}}

\emph{Canonical framework:} take a univariate time series $(Y_t)_{t\ge 1}$ with $\mathbb{E}[Y_t^2]<\infty$ — picture the \texttt{Nile} annual-flows series, which has a famous level shift around 1898 (so it is not stationary). The questions below tease apart two different things that students routinely confuse: (a) \emph{when is the ACVF mathematically defined?}, and (b) \emph{when does the sample correlogram actually estimate it?} The answer to (a) needs only square-integrability; the answer to (b) requires stationarity (so a single lag-$h$ function exists) plus ergodicity (so time-averages converge).

---

<details class="master-subpart" open>
<summary><span class="tag tag-2plus">≥3 ex</span> (Q1) <em>The autocovariance function (acf) can be defined only if the time series is stationary. Answer as either: A. Yes, because: ... or B. No, because: ...</em>  [May 2025 Q1a, May 2022 Q2, May 2021 Q2]</summary>

\textbf{B. NO.} The autocovariance function
\[
\gamma(s,t)=\operatorname{Cov}(Y_s,Y_t)=\mathbb{E}\big[(Y_s-\mu(s))(Y_t-\mu(t))\big]
\]
is well-defined for \emph{any} stochastic process satisfying $\mathbb{E}[Y_t^2]<\infty$ for all $t$. Then $\operatorname{Var}(Y_s),\operatorname{Var}(Y_t)<\infty$ and Cauchy–Schwarz gives $|\gamma(s,t)|\le\sqrt{\operatorname{Var}(Y_s)\operatorname{Var}(Y_t)}<\infty$. Stationarity is \emph{not} required for the ACVF to \textbf{exist}.

\emph{What stationarity actually buys you} is a \textbf{simplification}, not the existence:

- Without stationarity, $\gamma(s,t)$ is a function of \emph{two} arguments.
- Under weak stationarity, $\gamma(s,t)=\gamma(s+k,t+k)$ for any shift $k$, hence depends only on the lag $h=t-s$: write $\tilde\gamma(h)=\gamma(t,t+h)$ — a function of \emph{one} argument.

\emph{Counterexample for the "Yes" camp.} The random walk $Y_t=\sum_{i\le t}\varepsilon_i$ with $\varepsilon_i\overset{\mathrm{iid}}{\sim}\mathcal{N}(0,\sigma^2)$ is \emph{not} stationary, yet
\[
\gamma(s,t)=\operatorname{Cov}\Big(\sum_{i\le s}\varepsilon_i,\sum_{j\le t}\varepsilon_j\Big)=\sigma^2\min(s,t)
\]
is perfectly well-defined — it just depends on both $s$ and $t$.

\emph{R analogue:}

`cov(y[1:499], y[2:500])         ## sample lag-1 covariance — finite, defined`

</details>

<details class="master-subpart" open>
<summary><span class="tag tag-2plus">≥1 ex</span> (Q2) <em>The correlogram can be used to estimate the acf only if the time series is stationary. Answer as either: A. Yes, because: ... or B. No, because: ...</em>  [May 2025 Q1b]</summary>

\textbf{A. YES} — essentially. The correlogram is the sample ACVF
\[
\hat\gamma(h)=\frac{1}{T}\sum_{t=1}^{T-|h|}(Y_t-\bar Y)(Y_{t+|h|}-\bar Y),
\]
(and the sample ACF $\hat\rho(h)=\hat\gamma(h)/\hat\gamma(0)$). It \emph{pools across $t$} to produce a single estimate of "the lag-$h$ autocovariance". That pooling is meaningful only if the underlying ACVF actually depends on the lag alone — i.e.\ under \textbf{weak stationarity}. Otherwise different summands $(Y_t-\bar Y)(Y_{t+|h|}-\bar Y)$ are estimating \emph{different} population quantities $\gamma(t,t+h)$, and their average has no clean interpretation as "$\hat\gamma(h)$".

A second condition is needed for \textbf{consistency}: \textbf{ergodicity}, so that the time-average $T^{-1}\sum_t\to\mathbb{E}$ as $T\to\infty$. (Stationarity alone implies unbiasedness — apart from the small-sample bias from dividing by $T$ rather than $T-|h|$ — but not consistency.)

\emph{Summary.}

- \emph{Definition of ACVF:} needs $\mathbb{E}[Y_t^2]<\infty$ only.
- \emph{Existence of a single lag-only function:} needs weak stationarity.
- \emph{Consistency of the correlogram:} needs weak stationarity + ergodicity.

\emph{Practical implication.} If you plot \texttt{acf(y)} on a non-stationary series (trended Nile, random walk), you'll see an artificially slow decay — that's the diagnostic. Difference / detrend first.

`acf(Nile)                       ## slow decay — flags non-stationarity`
`acf(diff(Nile))                 ## post-differencing, behaves like stationary`

</details>

\textbf{Linked exams:} \texttt{exam_may_2025_q1}, \texttt{exam_may_2022_q2}, \texttt{exam_may_2021_q2}.""",
    "images": ["images/master/master_t2b_ai.png"]
}

master_exercises_ts["t2c"] = {
    "title": "Master — Sample-mean estimator under stationarity + ergodicity",
    "content": r"""\textbf{\textcolor{red}{MASTER --- Sample-mean estimator under stationarity + ergodicity}}

\emph{Canonical framework:} a univariate time series $(Y_t)_{t\ge 1}$ with finite expectations $\mathbb{E}[Y_t]$, observed over $t=1,\dots,n$ — picture the \texttt{LakeHuron} annual-levels (one path, $n=98$). The natural non-parametric estimator of "the mean" is the \textbf{sample mean} $\bar Y_n=n^{-1}\sum_t Y_t$. The questions below combine the May 2024 "how do you non-parametrically estimate the mean?" with the Jun 2022 "is $\bar Y_n$ appropriate?" — both reduce to the same answer: stationarity makes "the mean" a single number, and ergodicity makes $\bar Y_n$ consistent for it.

---

<details class="master-subpart" open>
<summary><span class="tag tag-2plus">≥2 ex</span> (Q1) <em>Consider a univariate stationary time series $(Y_t)_{t\ge 1}$. Given data $(y_1,y_2,\dots,y_T)$ how can you estimate, non-parametrically, its mean function? \emph{[combined with]} we can estimate $\mathbb{E}[Y_n]$ by the sample mean $\bar Y_n=\tfrac{1}{n}\sum_{t=1}^n Y_t$. Is it an appropriate estimator?</em>  [May 2024 Q1, Jun 2022 Q1]</summary>

\textbf{Verdict.} \emph{YES, under conditions} — namely \textbf{stationarity} + \textbf{ergodicity}. The non-parametric estimator of the mean function is the \textbf{sample mean}:
\[
\boxed{\;\hat\mu_n=\bar Y_n=\frac{1}{n}\sum_{t=1}^n Y_t.\;}
\]

\textbf{Step 1 — Stationarity collapses $\mu(t)$ to a scalar.} Under weak stationarity, $\mu(t)\equiv\mu$ for every $t$, so "the mean function" is a single number $\mu$. Without this, "$\bar Y_n$" estimates a \emph{moving target} $\bar\mu_n=n^{-1}\sum_t\mathbb{E}[Y_t]$ — generically different from $\mathbb{E}[Y_n]$, the current mean. So stationarity is what makes pooling across $t$ \emph{meaningful}.

\textbf{Step 2 — Unbiasedness (needs only stationarity).}
\[
\mathbb{E}[\bar Y_n]=\frac{1}{n}\sum_{t=1}^n\mathbb{E}[Y_t]=\frac{1}{n}\cdot n\mu=\mu.\quad\checkmark
\]

\textbf{Step 3 — Consistency (needs ergodicity).} By the \textbf{ergodic theorem}, for a stationary \emph{and ergodic} process,
\[
\bar Y_n\;\xrightarrow{\text{a.s.}}\;\mu.
\]
\emph{Ergodicity intuition:} long enough time-averages explore the whole probability space (no "trapped" subsets of paths with their own mean). For weakly-stationary processes a sufficient mixing condition is $\sum_h|\gamma(h)|<\infty$.

\textbf{Step 4 — Asymptotic variance.} Direct computation:
\[
\operatorname{Var}(\bar Y_n)=\frac{1}{n^2}\sum_{s,t}\gamma(s-t)=\frac{1}{n}\sum_{|h|<n}\!\Big(1-\tfrac{|h|}{n}\Big)\gamma(h)\;\xrightarrow[n\to\infty]{}\;\frac{\sigma_\infty^2}{n},
\]
where $\sigma_\infty^2=\sum_{h\in\mathbb{Z}}\gamma(h)$ is the \textbf{long-run variance}. Under a CLT (mixing / ergodicity), $\sqrt n(\bar Y_n-\mu)\xrightarrow{d}\mathcal{N}(0,\sigma_\infty^2)$.

\textbf{Step 5 — SE in practice.} The naive $\sqrt{\hat{\operatorname{Var}}(Y_t)/n}$ \emph{underestimates} SE because it ignores positive autocorrelation. Use a \textbf{HAC} (Newey–West, Andrews) estimator:
\[
\hat\sigma_\infty^2=\hat\gamma(0)+2\sum_{h=1}^{L_n}\!\Big(1-\tfrac{h}{L_n+1}\Big)\hat\gamma(h),
\]
with bandwidth $L_n\propto n^{1/3}$.

\textbf{Without stationarity.}

- $\bar Y_n$ targets the \emph{average} $\bar\mu_n=n^{-1}\sum_t\mathbb{E}[Y_t]$, not $\mathbb{E}[Y_n]$.
- For a random walk, $\bar Y_n$ does not even converge (variance grows linearly).
- Remedy: detrend / difference / structural model first.

`mean(y)                                       ## sample mean — point estimate`

</details>

\textbf{Linked exams:} \texttt{exam_may_2024_q1}, \texttt{exam_jun_2022_q1}.""",
    "images": ["images/master/master_t2c_ai.png"]
}

master_exercises_ts["t3a"] = {
    "title": "Master — Markov property, DAG & conditional independence",
    "content": r"""\textbf{\textcolor{red}{MASTER --- Markov property, DAG & conditional independence}}

\emph{Canonical framework:} a discrete-time process $(Y_t)_{t\ge 0}$ with values in a finite state space $\mathcal{Y}=\{1,\dots,K\}$ — picture either a \emph{simulated random walk} $Y_t=Y_{t-1}+Z_t$ on $\mathbb{Z}$, or a finite-state categorical chain on $\{1,2,3\}$. The defining feature of a \textbf{Markov chain} (keydef \textbf{9a}) is the \textbf{Markov property}
\[
\mathbb{P}(Y_t=y_t\mid Y_{0:t-1}=y_{0:t-1})=\mathbb{P}(Y_t=y_t\mid Y_{t-1}=y_{t-1}),
\]
"the future depends on the past only through the present". Its DAG is the \emph{path graph} $Y_0\to Y_1\to\cdots\to Y_t\to\cdots$, and \textbf{homogeneity} adds $\mathbb{P}(Y_{t+1}=j\mid Y_t=i)\equiv p_{ij}$ (no $t$-dependence). The questions below cover (i) when a categorical series qualifies, (ii) the random-walk construction, (iii) deriving $Y_t\perp Y_{1:t-2}\mid Y_{t-1}$ from the DAG, and (iv) compatibility with homogeneity.

---

<details class="master-subpart" open>
<summary><span class="tag tag-2plus">≥1 ex</span> (Q1) <em>Consider a categorical time series $\{Y_t\}$ starting at $Y_0\sim\pi$, with $Y_t\in\{1,2,\dots,K\}$. Is it a Markov chain?</em>  [Jun 2025 Q1a]</summary>

\textbf{Not in general.} The two pieces of information given — a finite state space $\{1,\dots,K\}$ and an initial distribution $Y_0\sim\pi$ — are \emph{necessary} ingredients for a Markov chain but are far from \emph{sufficient}. They specify only:

- the state space $\mathcal{Y}=\{1,\dots,K\}$,
- the initial law $\pi$ on $\mathcal{Y}$.

What's missing — and what distinguishes a Markov chain from an arbitrary categorical process — is the \textbf{Markov property}:
\[
p(y_t\mid y_{0:t-1})=p(y_t\mid y_{t-1})\quad\text{for every }t\ge 1\text{ and every history }y_{0:t-1}.
\]

\emph{Counterexample.} A 2nd-order chain $\mathbb{P}(Y_t=j\mid Y_{t-1}=i,Y_{t-2}=k)$ that actually depends on $k$ has finite state space and an initial law $\pi$ but is \emph{not} Markov of order 1. (You can however always re-cast it as a 1st-order chain by augmenting the state to $(Y_{t-1},Y_{t-2})$.)

\emph{Bottom line:} say \emph{yes} only if additionally a transition matrix $\mathbf{P}=[p_{ij}]$ (or sequence $\mathbf{P}_t$, in the inhomogeneous case) is provided and the Markov property is assumed to hold.

</details>

<details class="master-subpart" open>
<summary><span class="tag tag-2plus">≥1 ex</span> (Q2) <em>Using the Directed Acyclic Graph (DAG) representation of the dependence structure of a Markov chain, show that $Y_t\perp(Y_1,\dots,Y_{t-2})\mid Y_{t-1}$. That is, $Y_t$ is conditionally independent from $Y_{t-2}$ given $Y_{t-1}$.</em>  [Jun 2025 Q1b]</summary>

\textbf{The DAG of a Markov chain.} The Markov property is encoded in the \textbf{path DAG}
\[
Y_0\;\longrightarrow\;Y_1\;\longrightarrow\;Y_2\;\longrightarrow\;\cdots\;\longrightarrow\;Y_{t-2}\;\longrightarrow\;Y_{t-1}\;\longrightarrow\;Y_t\;\longrightarrow\;\cdots
\]
Each $Y_s$ has a single parent $Y_{s-1}$ — the graphical translation of $p(y_s\mid y_{0:s-1})=p(y_s\mid y_{s-1})$.

\textbf{Proof via $d$-separation.} Take any node $Y_k$ with $1\le k\le t-2$ and any directed path from $Y_k$ to $Y_t$ in the DAG. Because the DAG is a single chain, every such path has the form
\[
Y_k\to Y_{k+1}\to\cdots\to Y_{t-1}\to Y_t.
\]
The node $Y_{t-1}$ appears on every path as a \textbf{serial / chain node} (one incoming edge $Y_{t-2}\to Y_{t-1}$, one outgoing edge $Y_{t-1}\to Y_t$). Conditioning on a chain node \emph{blocks} the path. Hence every path from $Y_{1:t-2}$ to $Y_t$ is blocked by $\{Y_{t-1}\}$, so by $d$-separation
\[
Y_t\;\perp\;(Y_1,\dots,Y_{t-2})\;\mid\;Y_{t-1}.
\]

\textbf{Algebraic check (equivalent).} By the Markov property and the chain rule,
\[
\mathbb{P}(Y_t=y_t\mid Y_{1:t-1}=y_{1:t-1})=\mathbb{P}(Y_t=y_t\mid Y_{t-1}=y_{t-1}),
\]
so conditional on $Y_{t-1}$ the law of $Y_t$ does not depend on $Y_{1:t-2}$ — that \emph{is} the conditional-independence statement.

\emph{Special case asked for.} $Y_t\perp Y_{t-2}\mid Y_{t-1}$ follows by taking the marginal over $Y_1,\dots,Y_{t-3}$.

</details>

<details class="master-subpart" open>
<summary><span class="tag tag-2plus">≥1 ex</span> (Q3) <em>Consider a stochastic process $(Y_t)_{t\ge 1}$ starting at $Y_0=0$, where $Y_t=\sum_{i=1}^t Z_i$ and $Z_i$ is i.i.d.\ with $\mathbb{P}(Z_i=-1)=p$ and $\mathbb{P}(Z_i=1)=1-p$. Is $(Y_t)_{t\ge 1}$ a Markov process?</em>  [May 2025 Q2]</summary>

\textbf{YES.} This is the canonical \textbf{random walk} on $\mathbb{Z}$.

\textbf{Proof.} Note $Y_t=Y_{t-1}+Z_t$, with $Z_t$ independent of $(Z_1,\dots,Z_{t-1})$ (iid). Since $Y_{0:t-1}=(Y_0,\dots,Y_{t-1})$ is a function of $(Z_1,\dots,Z_{t-1})$, $Z_t$ is also independent of $Y_{0:t-1}$. Therefore, for any history $y_{0:t}$,
\[
\mathbb{P}(Y_t=y_t\mid Y_{0:t-1}=y_{0:t-1})=\mathbb{P}(Z_t=y_t-y_{t-1}\mid Y_{0:t-1}=y_{0:t-1})=\mathbb{P}(Z_t=y_t-y_{t-1}),
\]
which depends only on $y_{t-1}$ — that's the Markov property:
\[
\mathbb{P}(Y_t=y_t\mid Y_{0:t-1})=\mathbb{P}(Y_t=y_t\mid Y_{t-1}).
\]

\textbf{Homogeneous transition.} The 1-step transitions are
\[
p_{y,\,y-1}=p,\qquad p_{y,\,y+1}=1-p,\qquad\text{all other entries }=0,
\]
independent of $t$. So it's a \emph{homogeneous} Markov chain on $\mathbb{Z}$.

\textbf{Side remark — not stationary.} $\mathbb{E}[Z_i]=1-2p$ and $\operatorname{Var}(Z_i)=4p(1-p)$, so
\[
\mathbb{E}[Y_t]=(1-2p)\,t,\qquad \operatorname{Var}(Y_t)=4p(1-p)\,t,
\]
both grow with $t$ (unless $p=1/2$ for the mean; the variance grows regardless). Markov $\ne$ stationary.

\emph{R simulation:}

</details>

<details class="master-subpart" open>
<summary><span class="tag tag-2plus">≥1 ex</span> (Q4) <em>Let $(X_n)_{n\ge 0}$ be a stochastic process, with $X_n\in\{1,2,3\}$. Suppose that $\mathbb{P}(X_4=1\mid X_3=3)=0.4$ and $\mathbb{P}(X_2=1\mid X_1=3)=0.5$. Could the process $(X_t)_{t\ge 0}$ be a homogeneous Markov chain?</em>  [May 2021 Q1]</summary>

\textbf{B. NO.} By definition, in a \textbf{homogeneous} Markov chain the 1-step transition probability does not depend on $n$:
\[
\mathbb{P}(X_{n+1}=j\mid X_n=i)=p_{ij}\quad\text{for every }n.
\]
Applying this to both stated conditional probabilities (both are 1-step transitions $i=3\to j=1$):
\[
p_{3,1}=\mathbb{P}(X_4=1\mid X_3=3)=0.4,\qquad p_{3,1}=\mathbb{P}(X_2=1\mid X_1=3)=0.5.
\]
This would force $0.4=p_{3,1}=0.5$, a contradiction. Hence $(X_t)$ \emph{cannot} be a homogeneous Markov chain.

\emph{Compatible alternatives.}

- \textbf{Inhomogeneous Markov chain} — different transition matrices $\mathbf{P}_n$ at different $n$; both numbers are then transitions of two different matrices. Compatible.
- \textbf{Non-Markov process} — full history matters; the two conditioning sets actually carry different information (they are derived from different marginals of a longer history). Compatible.

\emph{Pedagogical point.} Homogeneity is a strong, testable assumption — if any two 1-step transitions $i\to j$ disagree, homogeneity is dead. (Distinct from the Markov property itself, which is about the conditioning set.)

</details>

\textbf{Linked exams:} \texttt{exam_jun_2025_q1}, \texttt{exam_may_2025_q2}, \texttt{exam_may_2021_q1}.""",
    "images": ["images/master/master_t3a_ai.png"]
}

master_exercises_ts["t3b"] = {
    "title": "Master — Transition-matrix arithmetic & ergodic convergence Thm 2.1",
    "content": r"""\textbf{\textcolor{red}{MASTER --- Transition-matrix arithmetic & ergodic convergence Thm 2.1}}

\emph{Canonical framework:} a homogeneous Markov chain $(Y_t)_{t\ge 0}$ with finite state space $\mathcal{Y}=\{1,2,3\}$ and transition matrix $\mathbf{P}=[p_{ij}]$ (rows sum to 1; rows index "from", columns "to"). The questions below cover the four standard operational tasks that come up exam after exam: (i) \textbf{complete a partially-given $\mathbf{P}$} using row-sum-1; (ii) \textbf{compute $n$-step probabilities} as entries of $\mathbf{P}^n$ (the special case of uniform-row $\mathbf{P}$); (iii) \textbf{check ergodicity and convergence to the stationary distribution} via Theorem 2.1 (keydef \textbf{11d}); and (iv) \textbf{write joint path probabilities} as products of transitions using the Markov property (here applied to an HMM latent chain).

---

<details class="master-subpart" open>
<summary><span class="tag tag-2plus">≥1 ex</span> (Q1) <em>Consider a homogeneous Markov chain $(Y_t)_{t\ge 0}$ with state space $\mathcal Y=\{1,2,3\}$, initial value $Y_0=1$, and transition matrix with three missing entries (row 1 last entry, row 2 first entry, row 3 second entry). Complete the transition matrix.</em>  [Jun 2024 Q3a]</summary>

\textbf{Use the stochastic-matrix constraint:} each row of a transition matrix sums to 1 (since $\sum_j p_{ij}=\mathbb{P}(\text{land somewhere in }\mathcal{Y}\mid Y_{t-1}=i)=1$). Filling row by row:

- \emph{Row 1:} $.6+0+\square=1\;\Rightarrow\;\square=.4$.
- \emph{Row 2:} $\square+.6+.3=1\;\Rightarrow\;\square=.1$.
- \emph{Row 3:} $.3+\square+.6=1\;\Rightarrow\;\square=.1$.

Hence
\[
\mathbf{P}=\begin{pmatrix}.6 & 0  & .4\\ .1 & .6 & .3\\ .3 & .1 & .6\end{pmatrix}.
\]

\emph{Sanity check:} all rows sum to $1.0$, all entries in $[0,1]$. $\checkmark$

</details>

<details class="master-subpart" open>
<summary><span class="tag tag-2plus">≥2 ex</span> (Q2) <em>This Markov chain is aperiodic. As $n\to\infty$, does $\mathbb{P}(Y_n=j\mid Y_0=1)$ converge for each $j\in\{1,2,3\}$?</em>  [Jun 2024 Q3b, May 2022 Q3]</summary>

\textbf{C. YES, under the conditions of Theorem 2.1} (keydef \textbf{11d}, ergodic convergence). For a Markov chain on a finite state space, $\mathbb{P}(Y_n=j\mid Y_0=i)\to\pi_j$ (independent of the starting state $i$) provided $\mathbf{P}$ is \textbf{irreducible} and \textbf{aperiodic}.

\textbf{Irreducibility.} Check that every state communicates with every other in a finite number of steps with positive probability. From the $\mathbf{P}$ above:
\[
1\to 3\to 2,\qquad 2\to 1,\qquad 3\to 1,\qquad 1\to 3,\qquad 2\to 3,\qquad 3\to 2.
\]
All six ordered pairs are reachable; $\mathbf{P}$ is \emph{irreducible}. $\checkmark$

\textbf{Aperiodicity.} \emph{Given} in the question (and easy to verify: $p_{11}=.6>0$ so the period of state 1 — gcd of return times — is 1; irreducibility carries this period to all states). $\checkmark$

\textbf{Theorem 2.1.} Finite + irreducible + aperiodic $\Rightarrow$ there exists a \emph{unique} stationary distribution $\pi$ satisfying $\pi\mathbf{P}=\pi$, $\sum_j\pi_j=1$, and
\[
\boxed{\;\mathbb{P}(Y_n=j\mid Y_0=i)\xrightarrow[n\to\infty]{}\pi_j\quad\text{for every }i,j.\;}
\]
The limit is the same whether we start from $i=1$ or any other state — that is the strength of ergodicity.

\textbf{Solving $\pi\mathbf{P}=\pi$.} The linear system reads
\[
\pi_1=.6\pi_1+.1\pi_2+.3\pi_3,\quad \pi_2=.6\pi_2+.1\pi_3,\quad \pi_3=.4\pi_1+.3\pi_2+.6\pi_3,\quad \sum_j\pi_j=1.
\]
From the second equation, $.4\pi_2=.1\pi_3\;\Rightarrow\;\pi_3=4\pi_2$. Substituting into the others and normalising gives
\[
\pi\approx(0.394,\;0.121,\;0.485).
\]

\emph{R:}

</details>

<details class="master-subpart" open>
<summary><span class="tag tag-2plus">≥1 ex</span> (Q3) <em>And how about $\mathbb{P}(Y_n=j)$?</em>  [Jun 2024 Q3c]</summary>

\textbf{YES, also converges — to the same $\pi_j$.} Write the unconditional probability by conditioning on $Y_0$ with arbitrary initial law $\nu=(\nu_1,\nu_2,\nu_3)$:
\[
\mathbb{P}(Y_n=j)=\sum_{i=1}^3\nu_i\,\mathbb{P}(Y_n=j\mid Y_0=i).
\]
By Theorem 2.1, every conditional probability $\mathbb{P}(Y_n=j\mid Y_0=i)\to\pi_j$ as $n\to\infty$. Hence, as a finite weighted sum of converging sequences with weights summing to 1,
\[
\mathbb{P}(Y_n=j)\xrightarrow[n\to\infty]{}\sum_{i=1}^3\nu_i\pi_j=\pi_j\!\underbrace{\sum_i\nu_i}_{=1}=\pi_j.
\]

\emph{Key takeaway:} the limit does \emph{not} depend on the initial distribution $\nu$. In particular, the question's special case $Y_0=1$ ($\nu=(1,0,0)$) is just one instance of the same phenomenon — and the answer matches part (b).

</details>

<details class="master-subpart" open>
<summary><span class="tag tag-2plus">≥1 ex</span> (Q4) <em>Let $(Y_t)_{t\ge 0}$ be a Markov chain with state space $\mathcal Y=\{1,2,3\}$ and suppose that each row of the transition matrix is uniform. Write the transition matrix and compute $\mathbb{P}(Y_2=2\mid Y_0=1)$.</em>  [May 2023 Q2]</summary>

\textbf{Transition matrix.} "Each row is uniform" on $\mathcal{Y}=\{1,2,3\}$ means each row is $(1/3,1/3,1/3)$. So
\[
\mathbf{P}=\frac{1}{3}\,\mathbf{1}\mathbf{1}^{\top}=\begin{pmatrix}1/3 & 1/3 & 1/3\\ 1/3 & 1/3 & 1/3\\ 1/3 & 1/3 & 1/3\end{pmatrix}.
\]

\textbf{Compute $\mathbf{P}^2$.} Note $\mathbf{P}=\tfrac{1}{3}\mathbf{1}\mathbf{1}^\top$ where $\mathbf{1}^\top\mathbf{1}=3$, so
\[
\mathbf{P}^2=\tfrac{1}{9}\mathbf{1}\underbrace{\mathbf{1}^\top\mathbf{1}}_{=3}\mathbf{1}^\top=\tfrac{1}{3}\mathbf{1}\mathbf{1}^\top=\mathbf{P}.
\]
More generally, $\mathbf{P}^k=\mathbf{P}$ for every $k\ge 1$ — this $\mathbf{P}$ is \textbf{idempotent}. (Intuition: from any state you go to the uniform distribution in one step; another step keeps you there.)

\textbf{Answer.}
\[
\mathbb{P}(Y_2=2\mid Y_0=1)=(\mathbf{P}^2)_{1,2}=(\mathbf{P})_{1,2}=\frac{1}{3}.
\]

\emph{Bonus — stationary distribution.} The unique left-eigenvector with eigenvalue 1 is $\pi=(1/3,1/3,1/3)$ — and the chain is in the stationary distribution from $t=1$ onwards, regardless of the initial value.

</details>

<details class="master-subpart" open>
<summary><span class="tag tag-2plus">≥1 ex</span> (Q5) <em>Suppose that a time series $(Y_t)_{t\ge 1}$ is described as a Hidden Markov Model (HMM) with latent state process $(S_t)_{t\ge 0}$ taking values in $\{1,2,3\}$, starting at $S_0=1$. Write the expression of the joint probability $\mathbb{P}(S_1=1,S_2=1,S_3=2\mid S_0=1)$.</em>  [Jun 2022 Q5]</summary>

\textbf{Only the latent chain enters.} The observed process $(Y_t)$ does not appear in the event, so the emission distribution is irrelevant — this is just a question about transition probabilities of the latent Markov chain $(S_t)$, with transition matrix $\mathbf{P}=[p_{ij}]_{i,j=1}^3$, $p_{ij}=\mathbb{P}(S_t=j\mid S_{t-1}=i)$.

\textbf{Factorise using the Markov property.} Apply the chain rule and the Markov property:
\[
\mathbb{P}(S_{1:3}\mid S_0=1)=\mathbb{P}(S_1\mid S_0)\,\mathbb{P}(S_2\mid S_1)\,\mathbb{P}(S_3\mid S_2).
\]
Plug in $S_0=1$, $S_1=1$, $S_2=1$, $S_3=2$:
\[
\mathbb{P}(S_1=1,S_2=1,S_3=2\mid S_0=1)=p_{1,1}\cdot p_{1,1}\cdot p_{1,2}=p_{1,1}^2\,p_{1,2}.
\]

\boxed{\;\mathbb{P}(S_1=1,S_2=1,S_3=2\mid S_0=1)=p_{1,1}^2\,p_{1,2}.\;}

\emph{Why no emission terms.} The HMM joint law factorises as
\[
\mathbb{P}(S_{0:T},Y_{1:T})=\mathbb{P}(S_0)\prod_{t=1}^T p_{S_{t-1},S_t}\,e_{S_t,Y_t},
\]
and marginalising the $Y_t$'s away gives back the pure latent-chain product (the $e$'s sum to 1 over $Y_t$).

</details>

\textbf{Linked exams:} \texttt{exam_jun_2024_q3}, \texttt{exam_may_2023_q2}, \texttt{exam_may_2022_q3}, \texttt{exam_jun_2022_q5}.""",
    "images": ["images/master/master_t3b_ai.png"]
}
