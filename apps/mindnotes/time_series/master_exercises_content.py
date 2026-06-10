"""Master Exam Ready snippets — Time Series Analysis."""
master_exercises_ts = {}

master_exercises_ts["t1a"] = {
    "title": 'Master — Stochastic process / time series',
    "content": r"""\textbf{\textcolor{red}{MASTER --- Stochastic process / time series}}

---

<details class="master-subpart" open>
<summary><span class="tag tag-2plus">≥2 ex</span> (a) <em>What is a stochastic process?</em>  [Sep 2025 Q1a]</summary>

A \textbf{stochastic process} is a family of random variables (or vectors) $(Y_t)_{t\in T}$ indexed by a set $T$ --- the \emph{index set}, typically $T=\mathbb{N}$ or $\mathbb{Z}$ for discrete time --- with each $Y_t$ taking values in a state space $\mathcal{Y}$.

The process is fully specified by its \textbf{finite-dimensional distributions (f.d.d.s)}
\[ \bigl\{\,f_{Y_{t_1},\dots,Y_{t_k}}(y_1,\dots,y_k)\;:\;k\ge 1,\;(t_1,\dots,t_k)\in T^k\,\bigr\}. \]

Under Kolmogorov's consistency conditions, the extension theorem guarantees a unique probability measure on the path space $\mathcal{Y}^T$ realising those f.d.d.s.

\emph{Gaussian shortcut:} for Gaussian processes, $\mu(t)=\mathbb{E}[Y_t]$ and $\gamma(s,t)=\mathrm{Cov}(Y_s,Y_t)$ fully determine all f.d.d.s.

</details>

<details class="master-subpart" open>
<summary><span class="tag tag-2plus">≥2 ex</span> (b) <em>Define a time series as a stochastic process.</em>  [Sep 2025 Q1b]</summary>

A \textbf{time series} is a stochastic process $(Y_t)_{t\in T}$ with $T\subseteq\mathbb{Z}$ (discrete-time indexing). The observed data $(y_1,\dots,y_T)$ are a \textbf{single finite realisation} of \emph{one} path --- treat any observed series this way; we never see other paths from the same process.

\emph{Implication.} Inference from one path is possible \emph{only} under structural assumptions linking different time points:

- \textbf{Stationarity} (weak / strict): moments do not depend on $t$, so time-averaging substitutes for ensemble-averaging.
- \textbf{Parametric dynamics} (ARMA, DLM, HMM): a low-dimensional model ties $Y_t$ to past $Y_{s<t}$ or to a latent state.
- \textbf{Ergodicity}: time-averages converge to ensemble-averages.

Without such assumptions, a time series is \textbf{not} a sample of size $T$ from a population --- it is a sample of size 1 from a $T$-dimensional joint distribution.

</details>

<details class="master-subpart" open>
<summary><span class="tag tag-2plus">≥2 ex</span> (c) <em>What is a time series? An answer could be: "A time series is a sequence of observations taken over time." Are you happy with this definition?</em>  [May 2023 Q1]</summary>

\textbf{Answer: NO} --- the proposed definition is incomplete.

A time series is the \textbf{realization} of a stochastic process $(Y_t)_{t\in T}$, not merely a sequence of numbers.

\emph{What the naive definition misses:}

- No \textbf{probability model}. Without a joint distribution there is no notion of mean, variance, autocovariance, or stationarity.
- No basis for \textbf{inference}. CIs, tests, forecasts all require the random-variable view.
- No distinction between \textbf{the process} (the random object $(Y_t)_{t\in T}$) and \textbf{the path} (the observed sequence $(y_1,\dots,y_T)$).

\emph{Correct one-liner:} a time series is the observed realisation of a stochastic process $(Y_t)_{t\in T}$ indexed by time, equipped with the joint distribution that lets us reason about uncertainty.

</details>

\textbf{Linked exams:} \texttt{exam_sep_2025_q1}, \texttt{exam_may_2023_q1}.""",
    "images": []
}

master_exercises_ts["t2a"] = {
    "title": 'Master — Weak stationarity — definition & examples',
    "content": r"""\textbf{\textcolor{red}{MASTER --- Weak stationarity — definition & examples}}

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
    "images": ['images/master/master_t2a_ai.png']
}

master_exercises_ts["t2b"] = {
    "title": 'Master — ACVF / correlogram — when defined?',
    "content": r"""\textbf{\textcolor{red}{MASTER --- ACVF / correlogram — when defined?}}

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
    "images": ['images/master/master_t2b_ai.png']
}

master_exercises_ts["t2c"] = {
    "title": 'Master — Sample-mean estimator under stationarity + ergodicity',
    "content": r"""\textbf{\textcolor{red}{MASTER --- Sample-mean estimator under stationarity + ergodicity}}

---

<details class="master-subpart" open>
<summary><span class="tag tag-2plus">≥2 ex</span> (a) <em>Given data $(y_1,\dots,y_T)$, how do you estimate the mean function non-parametrically? Is $\bar Y_n$ an appropriate estimator?</em>  [May 2024 Q1, Jun 2022 Q1]</summary>

\textbf{One-line verdict.} YES --- under \textbf{stationarity + ergodicity}. \textbf{Consistency} is the operational property we care about; unbiasedness is a free byproduct, not the goal.

\[ \boxed{\;\hat\mu_n \;=\; \bar Y_n \;=\; \frac{1}{n}\sum_{t=1}^n Y_t.\;} \]

\medskip

\textbf{Step 1 --- Why stationarity: to even define a target.}

In general $\mu(t) = \mathbb{E}[Y_t]$ is a \emph{function} of $t$. With ONE observed path you have ONE observation per time $t$, so non-parametric estimation of the full function is impossible: the only "estimator" of $\mu(t)$ at time $t$ is $Y_t$ itself, with variance $\sigma^2$ that never shrinks. Useless.

Three ways to make estimation feasible:

- \textbf{Stationarity} $\mu(t)\equiv\mu$ --- collapses to a single scalar to estimate.
- Parametric model $\mu(t)=\alpha+\beta t$ (ruled out by "non-parametrically").
- Smoothness + local averaging (a different non-parametric route, treated separately).

So \textbf{stationarity} is the assumption that makes the problem well-posed: it collapses the function to one number $\mu$, and now pooling $n$ observations to estimate that number is meaningful.

\medskip

\textbf{Step 2 --- Consistency: the operational property.}

In empirical work we don't care much about unbiasedness on its own --- it tells you the estimator is centered correctly, but says nothing about precision. We care about \textbf{consistency}:
\[ \boxed{\;\bar Y_n \;\xrightarrow{P/\text{a.s.}}\;\mu\quad\text{as }n\to\infty.\;} \]
i.e.\ given enough data we get arbitrarily close to the truth.

For the sample mean of an IID sample, \textbf{the LLN gives consistency automatically} --- independence makes $\operatorname{Var}(\bar Y_n) = \sigma^2/n \to 0$ for free.

In time series the observations are correlated, so the IID-LLN does not apply. The relevant LLN is the \textbf{ergodic theorem} --- the LLN for stationary stochastic processes. It says
\[ \boxed{\;\text{time-average} \;=\; \text{ensemble-average}\;} \]
provided the process is \textbf{ergodic}. A practical sufficient condition: $\sum_h|\gamma(h)|<\infty$ (short memory --- autocovariances die fast enough).

\emph{Ergodicity intuition.} One long trajectory eventually visits every "region" of the probability space. No "trapped" subset where the chain stays forever. Then averaging over time of one path = averaging across many paths at one time.

So in this setting:
\[ \boxed{\;\text{stationarity + ergodicity}\;\Longrightarrow\;\text{ergodic theorem}\;\Longrightarrow\;\bar Y_n\to\mu.\;} \]

\medskip

\textbf{Aside on unbiasedness.} Free from stationarity:
\[ \mathbb{E}[\bar Y_n] = \frac{1}{n}\sum_t \mathbb{E}[Y_t] = \mu. \]
But unbiasedness alone is not enough --- e.g.\ $\hat\mu_n = Y_1$ is unbiased but its variance is constant $\sigma^2$, so it never converges. Consistency, not unbiasedness, is what we need.

\medskip

\textbf{Step 3 --- Variance: how fast does it converge?}

\[ \operatorname{Var}(\bar Y_n) = \frac{1}{n}\sum_{|h|<n}\!\Bigl(1-\tfrac{|h|}{n}\Bigr)\gamma(h) \;\xrightarrow[n\to\infty]{}\; \frac{\sigma_\infty^2}{n}, \qquad \sigma_\infty^2 = \sum_{h\in\mathbb{Z}}\gamma(h). \]

\emph{Intuition.} In IID land variance is $\sigma^2/n$ --- each new observation gives a fresh, independent bit of information. With positive autocorrelation, each new $Y_t$ partly repeats what we already knew: less independent info per data point, slower convergence. So the long-run variance $\sigma_\infty^2 = \sum_h \gamma(h)$ replaces $\sigma^2$, summing autocovariances at every lag.

CLT (mixing/ergodicity): $\sqrt n\,(\bar Y_n - \mu) \xrightarrow{d} \mathcal{N}(0, \sigma_\infty^2)$.

\medskip

\textbf{Step 4 --- Standard errors in practice.}

The naïve $\sqrt{\widehat{\operatorname{Var}}(Y_t)/n}$ uses only the lag-0 variance and \textbf{ignores autocovariance at lags $h\ne 0$}. Under positive autocorrelation it underestimates SE --- CIs too narrow, t-tests reject too often.

Fix: \textbf{HAC} (Newey-West) estimator sums sample autocovariances with a kernel weighting:
\[ \bar Y_n \;\pm\; z_{\alpha/2}\,\sqrt{\hat\sigma_\infty^2 / n}. \]

\medskip

\textbf{What goes wrong without stationarity:}

\begin{itemize}
\item No single $\mu$ to target --- $\bar Y_n$ would estimate the time-average $\bar\mu_n = n^{-1}\sum_t \mu(t)$, a meaningless mixture that depends on the sample window.
\item Worst case: a random walk has $\operatorname{Var}(\bar Y_n)$ growing in $n$ --- \textbf{$\bar Y_n$ is unbiased but inconsistent}, exactly the empirical-work cautionary tale.
\item Remedy: detrend / difference / model the structure first, then average the stationary residual.
\end{itemize}

\medskip

\textbf{Summary chain.} \emph{Stationarity makes the target ($\mu$) well-defined $\to$ ergodic theorem (the LLN for stationary processes) delivers consistency $\bar Y_n\to\mu$ $\to$ summing autocovariances gives the right precision ($\sigma_\infty^2/n$) $\to$ HAC implements that precision empirically.}

</details>

\textbf{Linked exams:} \texttt{exam_may_2024_q1}, \texttt{exam_jun_2022_q1}.""",
    "images": []
}

master_exercises_ts["t3a"] = {
    "title": 'Master — Markov property, DAG & conditional independence',
    "content": r"""\textbf{\textcolor{red}{MASTER --- Markov property, DAG & conditional independence}}

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
    "images": ['images/master/master_t3a_ai.png']
}

master_exercises_ts["t3b"] = {
    "title": 'Master — Transition-matrix arithmetic & ergodic convergence Thm 2.1',
    "content": r"""\textbf{\textcolor{red}{MASTER --- Transition-matrix arithmetic & ergodic convergence Thm 2.1}}

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
    "images": ['images/master/master_t3b_ai.png']
}

master_exercises_ts["t4a"] = {
    "title": 'Master — Panel transition-count likelihood & MLE',
    "content": r"""\textbf{\textcolor{red}{MASTER --- Panel transition-count likelihood & MLE}}

---

<details class="master-subpart" open>
<summary><span class="tag tag-2plus">≥2 ex</span> (Q1) <em>Write the expression of the likelihood of the $[p_{i,j}]_{i,j=1,2,3}$</em>  [May 2024 Q2a, Sep 2025 Q6]</summary>

\textbf{Setup.} Initial values $y_{k,0}$ are \emph{fixed} (no $\pi$ to estimate). By the Markov property, the conditional probability of one individual's path is
\[ \mathbb{P}(Y_{k,1:T}=y_{k,1:T}\mid Y_{k,0}=y_{k,0})=\prod_{t=1}^{T} p_{\,y_{k,t-1},\,y_{k,t}}. \]

By i.i.d.\ across $k$ and pooling exponents over $(k,t)$,
\[ \boxed{\;L(\mathbf P)=\prod_{k=1}^{n}\prod_{t=1}^{T} p_{\,y_{k,t-1},\,y_{k,t}}=\prod_{i=1}^{K}\prod_{j=1}^{K} p_{ij}^{\,n_{ij}},\quad n_{ij}=\sum_{k,t}\mathbf{1}\{y_{k,t-1}=i,\,y_{k,t}=j\},\;} \]
subject to the \emph{row-sum} simplex constraints $\sum_j p_{ij}=1$ for every $i$.

\emph{Key facts.}
- The pooled count matrix $\{n_{ij}\}$ is a \textbf{sufficient statistic} (Fisher--Neyman): the likelihood depends on the data only through $\{n_{ij}\}$.
- The likelihood \textbf{factorises by row}: rows of $\mathbf P$ are estimated independently, each a multinomial problem conditional on the visit count $n_{i,+}=\sum_j n_{ij}$.

</details>

<details class="master-subpart" open>
<summary><span class="tag tag-2plus">≥2 ex</span> (Q2) <em>Explain how you get their maximum likelihood estimates (MLE).</em>  [May 2024 Q2b, Sep 2025 Q6]</summary>

\textbf{Strategy.} Take logs, maximise row by row under $\sum_j p_{ij}=1$ via Lagrange multipliers.

\textbf{Step 1 — log-likelihood:}
\[ \ell(\mathbf P)=\sum_{i,j} n_{ij}\,\log p_{ij}. \]

\textbf{Step 2 — Lagrangian for row $i$}:
\[ \mathcal{L}_i=\sum_j n_{ij}\log p_{ij}-\lambda_i\Bigl(\sum_j p_{ij}-1\Bigr). \]
First-order conditions: $\partial\mathcal{L}_i/\partial p_{ij}=n_{ij}/p_{ij}-\lambda_i=0\;\Rightarrow\;p_{ij}=n_{ij}/\lambda_i$.

\textbf{Step 3 — solve the constraint:} summing over $j$, $1=\sum_j n_{ij}/\lambda_i\;\Rightarrow\;\lambda_i=n_{i,+}$.

\textbf{Step 4 — Anderson--Goodman closed form:}
\[ \boxed{\;\widehat p_{ij}=\frac{n_{ij}}{n_{i,+}}\;}\qquad\text{(conditional sample proportion).} \]

\emph{Interpretation.} Each row of $\widehat{\mathbf P}$ is the empirical conditional distribution of the next state, given the current state. Existence/uniqueness require $n_{i,+}>0$ (state $i$ visited at least once).

\emph{R (pool transition counts, then row-normalise):}

</details>

<details class="master-subpart" open>
<summary><span class="tag tag-2plus">≥2 ex</span> (Q3) <em>What is the estimated probability that an individual who is uncertain in May will become in favor in June? Provide the MLE, together with the asymptotic confidence interval of level 90\%.</em>  [May 2024 Q2c, Sep 2025 Q6]</summary>

\textbf{Identify the transition.} "Uncertain ($3$) in May $\to$ in favour ($1$) in June" is the entry $p_{3,1}$.

\textbf{Point estimate} (using the May 2024 panel, row $i=3$: $n_{3,1}=30$, $n_{3,+}=100$):
\[ \widehat p_{3,1}=\frac{n_{3,1}}{n_{3,+}}=\frac{30}{100}=\mathbf{0.30}. \]

\textbf{Asymptotic CI} (Anderson--Goodman). Conditional on the visit count $n_{3,+}$,
\[ (n_{3,1},n_{3,2},n_{3,3})\mid n_{3,+}\sim\mathrm{Multinom}(n_{3,+};\,p_{3,:}), \]
and a multinomial-CLT applied to the proportion $\widehat p_{3,1}=n_{3,1}/n_{3,+}$ gives
\[ \sqrt{n_{3,+}}\bigl(\widehat p_{3,1}-p_{3,1}\bigr)\xrightarrow{d}\mathcal{N}\bigl(0,\;p_{3,1}(1-p_{3,1})\bigr). \]
Plug-in standard error (justified by consistency + Slutsky):
\[ \widehat{\mathrm{SE}}=\sqrt{\widehat p_{3,1}(1-\widehat p_{3,1})/n_{3,+}}=\sqrt{0.3\cdot 0.7/100}\approx 0.0458. \]
\textbf{90\% Wald CI} uses $z_{0.95}=1.65$:
\[ 0.30\pm 1.65\cdot 0.0458=0.30\pm 0.0756\;\Rightarrow\;\boxed{[0.224,\,0.376]}. \]

\emph{General template.} For any cell $(i,j)$ with $n_{i,+}>0$,
\[ \widehat p_{ij}\pm z_{1-\alpha/2}\sqrt{\widehat p_{ij}(1-\widehat p_{ij})/n_{i,+}}. \]

\emph{R:}

</details>

\textbf{Linked exams:} \texttt{exam_sep_2025_q6}, \texttt{exam_may_2024_q2}.""",
    "images": ['images/master/master_t4a_ai.png']
}

master_exercises_ts["t4b"] = {
    "title": 'Master — Wald CI for p_ij + forecasting future percentages',
    "content": r"""\textbf{\textcolor{red}{MASTER --- Wald CI for $p_{ij}$ + forecasting future percentages}}

---

<details class="master-subpart" open>
<summary><span class="tag tag-2plus">≥2 ex</span> (Q1) <em>Estimate $p_{1,1}$, also providing the asymptotic confidence interval of level 90\%.</em>  [Jun 2025 Q2c, May 2022 Q5b]</summary>

\textbf{General recipe} for a 1-$\alpha$ asymptotic CI on any cell $p_{ij}$:

\[ \widehat p_{ij}=\frac{n_{ij}}{n_{i,+}},\qquad \widehat{\mathrm{SE}}=\sqrt{\widehat p_{ij}(1-\widehat p_{ij})/n_{i,+}},\qquad \widehat p_{ij}\pm z_{1-\alpha/2}\,\widehat{\mathrm{SE}}. \]

\textbf{Jun 2025 Q2c — $p_{1,1}$, 90\% CI.} Row 1: $n_{1,1}=70$, $n_{1,+}=150$.
\[ \widehat p_{1,1}=70/150\approx 0.467,\qquad \widehat{\mathrm{SE}}=\sqrt{0.467\cdot 0.533/150}\approx 0.0407. \]
With $z_{0.95}=1.65$:
\[ 0.467\pm 1.65\cdot 0.0407=0.467\pm 0.067\;\Rightarrow\;\boxed{[0.400,\,0.534]}. \]

\textbf{May 2022 Q5b — $p_{3,1}$, 95\% CI ("undecided in May $\to$ candidate A in June").} Row 3: $n_{3,1}=30$, $n_{3,+}=170$.
\[ \widehat p_{3,1}=30/170\approx 0.1765,\qquad \widehat{\mathrm{SE}}=\sqrt{0.1765\cdot 0.8235/170}\approx 0.0292. \]
With $z_{0.975}=1.96$:
\[ 0.1765\pm 1.96\cdot 0.0292\;\Rightarrow\;\boxed{[0.119,\,0.234]}. \]

\emph{MLE properties used:} (i) \textbf{consistency} of $\widehat p_{ij}$ (so plug-in in the SE is asymptotically valid by Slutsky); (ii) \textbf{asymptotic normality} of the MLE (Anderson--Goodman).

\emph{R (Jun 2025 Q2c):}

</details>

<details class="master-subpart" open>
<summary><span class="tag tag-2plus">≥2 ex</span> (Q2) <em>Explain the steps for obtaining the asymptotic distribution used above.</em>  [Jun 2025 Q2d, May 2022 Q5b ("What properties of the MLE are you using?")]</summary>

\textbf{Three steps.}

\textbf{Step 1 --- Conditional multinomial.} Condition on the visit counts $n_{i,+}$. The next-state outcomes from the $n_{i,+}$ visits to state $i$ are i.i.d.\ $\mathrm{Cat}(p_{i,:})$, so
\[ (n_{i,1},\dots,n_{i,K})\mid n_{i,+}\sim\mathrm{Multinom}(n_{i,+};\,p_{i,:}). \]

\textbf{Step 2 --- Multinomial CLT.} For the proportion $\widehat p_{ij}=n_{ij}/n_{i,+}$, the standard multinomial CLT (Anderson--Goodman) gives
\[ \sqrt{n_{i,+}}\,\bigl(\widehat p_{ij}-p_{ij}\bigr)\xrightarrow{d}\mathcal{N}\bigl(0,\;p_{ij}(1-p_{ij})\bigr). \]
\emph{Why:} marginal of a multinomial in coordinate $j$ is $\mathrm{Bin}(n_{i,+},p_{ij})$; rescale by $1/\sqrt{n_{i,+}}$ and apply the i.i.d.\ CLT.

\textbf{Step 3 --- Slutsky for the plug-in SE.} By the LLN $\widehat p_{ij}\xrightarrow{p}p_{ij}$ (\emph{consistency} of the MLE), and the continuous map $p\mapsto p(1-p)$ is continuous, so $\widehat p_{ij}(1-\widehat p_{ij})\xrightarrow{p}p_{ij}(1-p_{ij})$. Slutsky's theorem then gives
\[ \frac{\widehat p_{ij}-p_{ij}}{\sqrt{\widehat p_{ij}(1-\widehat p_{ij})/n_{i,+}}}\xrightarrow{d}\mathcal{N}(0,1), \]
which justifies the Wald CI.

\textbf{MLE properties used:} (i) \emph{consistency}; (ii) \emph{asymptotic normality}.

</details>

<details class="master-subpart" open>
<summary><span class="tag tag-2plus">1 ex</span> (Q3) <em>Denote by $q$ the probability that the percentage of people going to vote in July is higher than 50\%. Provide a point estimate of $q$.</em>  [Jun 2025 Q2e]</summary>

\textbf{Three-step plug-in forecasting.}

\textbf{Step 1 --- estimate the July "YES" probability for a single individual.} Propagate the empirical state distribution at $t=5$ (June), $\widehat\pi^{(5)}\propto (150,250,100)$, one step ahead through $\widehat{\mathbf P}$:
\[ \widehat p_{\text{YES}}=\sum_{i=1}^{3}\widehat\pi_i^{(5)}\,\widehat p_{i,1}. \]
Plug-in MLEs: $\widehat p_{1,1}=70/150\approx 0.467$, $\widehat p_{2,1}=75/250=0.300$, $\widehat p_{3,1}=30/100=0.300$. Hence
\[ \widehat p_{\text{YES}}=\frac{150\cdot 0.467+250\cdot 0.300+100\cdot 0.300}{500}=\frac{70+75+30}{500}=\frac{175}{500}=0.35. \]

\textbf{Step 2 --- panel CLT for the July percentage.} With $n=1000$ i.i.d.\ Bernoulli($\widehat p_{\text{YES}}$) outcomes,
\[ \bar Y_{\text{July}}\overset{\text{approx}}{\sim}\mathcal{N}\!\left(0.35,\;\frac{0.35\cdot 0.65}{1000}\right),\qquad \mathrm{SE}=\sqrt{0.35\cdot 0.65/1000}\approx 0.0151. \]

\textbf{Step 3 --- tail probability.}
\[ \widehat q=\mathbb{P}(\bar Y_{\text{July}}>0.5)=1-\Phi\!\left(\frac{0.5-0.35}{0.0151}\right)=1-\Phi(9.93)\approx \mathbf{0}. \]

\emph{Sanity:} the July YES rate is forecast at 35\%, way below 50\%, with tiny SE; the chance of crossing the 50\% threshold is negligible.

\emph{R:}

</details>

<details class="master-subpart" open>
<summary><span class="tag tag-2plus">1 ex</span> (Q4) <em>Consider the probability referring to individual 1 $\Prob(Y_{1,1}=3,Y_{1,2}=3,Y_{1,3}=3,Y_{1,4}=1\mid Y_{1,0}=1)$. Is it known, or is it to be estimated?</em>  [May 2022 Q5a]</summary>

\textbf{Structure of the path.} Starting from $Y_{1,0}=1$, the path is $1\to 3\to 3\to 3\to 1$: \emph{one} initial $1\to 3$, \emph{two} consecutive $3\to 3$, \emph{one} final $3\to 1$. By the Markov property and homogeneity,
\[ \mathbb{P}(Y_{1,1}=3,Y_{1,2}=3,Y_{1,3}=3,Y_{1,4}=1\mid Y_{1,0}=1)=p_{1,3}\cdot p_{3,3}\cdot p_{3,3}\cdot p_{3,1}=\boxed{\;p_{1,3}\,p_{3,3}^{\,2}\,p_{3,1}.\;} \]

\textbf{Known vs estimated.} The \emph{symbolic expression} (functional form in the $p_{ij}$'s) is \textbf{known} a priori from the model. The \emph{numerical value}, however, depends on the unknown $\mathbf P$ and must therefore be \textbf{estimated} --- e.g.\ by plug-in MLE,
\[ \widehat{\mathbb{P}}=\widehat p_{1,3}\,\widehat p_{3,3}^{\,2}\,\widehat p_{3,1}=\tfrac{30}{130}\cdot\bigl(\tfrac{100}{170}\bigr)^{\!2}\cdot\tfrac{30}{170}\approx 0.0143. \]

\emph{Why "estimated" is the right answer.} The probabilities $p_{ij}$ are population parameters; we only have the sample $\{n_{ij}\}$ from which we form $\widehat p_{ij}$ and propagate uncertainty via the delta method if needed.

</details>

\textbf{Linked exams:} \texttt{exam_jun_2025_q2}, \texttt{exam_may_2022_q5}.""",
    "images": ['images/master/master_t4b_ai.png']
}

master_exercises_ts["t5a"] = {
    "title": 'Master — HMM definition, parameters & forward-algorithm likelihood',
    "content": r"""\textbf{\textcolor{red}{MASTER --- HMM definition, parameters \& forward-algorithm likelihood}}

---

<details class="master-subpart" open>
<summary><span class="tag tag-2plus">≥4 ex</span> (Q1) <em>Define the model, including stating the assumptions made.</em>  [May 2025 Q3a, May 2023 Q3a, May 2022 Q6a, May 2021 Q4a]</summary>

\textbf{Three assumptions.}

\textbf{(i) Latent Markov chain.} $(S_t)_{t\ge 1}\in\{1,\dots,k\}$ is a homogeneous Markov chain with
\[ S_1\sim\pi=(\pi_1,\dots,\pi_k),\qquad p_{ij}=\mathbb{P}(S_{t+1}=j\mid S_t=i),\;\mathbf P=[p_{ij}]\in\mathbb{R}^{k\times k}. \]

\textbf{(ii) Emission.} Conditional on $S_t=i$, the observation $Y_t$ follows a state-dependent distribution $f_i$:
- \emph{Text / topics} (May 2025 Q3): $Y_t\mid S_t=i\sim\mathrm{Cat}(e_{i,1},\dots,e_{i,M})$ over a dictionary of $M$ words.
- \emph{Returns / volatility clusters} (May 2023 Q3, May 2022 Q6) with $k=3$: $Y_t\mid S_t=i\sim\mathcal{N}(0,\sigma_i^2)$.
- \emph{Weekly call counts} (May 2021 Q4) with $k=3$: $Y_t\mid S_t=i\sim\mathrm{Poisson}(\lambda_i)$.

\textbf{(iii) Conditional independence of observations.} Given the latent path, the $Y_t$'s are independent and each depends only on its \emph{contemporaneous} state:
\[ Y_t\perp(S_{-t},Y_{-t})\mid S_t. \]

\emph{Initial-state conventions across exams.}
- May 2025 Q3: $S_1\sim\pi$ (general).
- May 2023 Q3: $S_0=1$ (degenerate at 1; no $\pi$ to estimate).
- May 2022 Q6: $\pi=(1/3,1/3,1/3)$ (uniform; no $\pi$ to estimate).
- May 2021 Q4: $S_1\sim\pi$ (general).

\emph{Joint density (canonical factorisation).}
\[ p(y_{1:T},s_{1:T};\phi)=\pi_{s_1}\,f_{s_1}(y_1)\prod_{t=2}^{T} p_{s_{t-1},s_t}\,f_{s_t}(y_t). \]

</details>

<details class="master-subpart" open>
<summary><span class="tag tag-2plus">≥4 ex</span> (Q2) <em>What are the unknown parameters of the model?</em>  [May 2025 Q3b, May 2023 Q3b, May 2022 Q6b, May 2021 Q4b]</summary>

\textbf{General count.} $\phi=(\pi,\mathbf P,\boldsymbol\theta_{\text{emit}})$ subject to simplex constraints. The number of \emph{free} parameters is
\[ \underbrace{(k-1)}_{\pi}+\underbrace{k(k-1)}_{\mathbf P\;\text{rows}}+\#\,\text{emission parameters.} \]

\textbf{Per exam (all use $k=3$ where applicable):}

\textbf{May 2025 Q3 --- Categorical emission over $M$ words.}
\[ \#\text{free}=(k-1)+k(k-1)+k(M-1). \]

\textbf{May 2023 Q3 --- Gaussian emission $\mathcal{N}(0,\sigma_i^2)$, $k=3$, $S_0=1$ (no $\pi$).}
\[ \phi=(\mathbf P,\sigma_1^2,\sigma_2^2,\sigma_3^2)\;\Rightarrow\;6+3=\mathbf{9}\text{ free.} \]
Identifiability up to label-swapping --- enforce $\sigma_1^2<\sigma_2^2<\sigma_3^2$.

\textbf{May 2022 Q6 --- Gaussian emission $\mathcal{N}(0,\sigma_i^2)$, $k=3$, $\pi$ uniform.}
\[ \phi=(\mathbf P,\sigma_1^2,\sigma_2^2,\sigma_3^2)\;\Rightarrow\;6+3=\mathbf{9}\text{ free.} \]

\textbf{May 2021 Q4 --- Poisson emission $\mathrm{Poisson}(\lambda_i)$, $k=3$, $\pi$ free.}
\[ \phi=(\pi,\mathbf P,\lambda_1,\lambda_2,\lambda_3)\;\Rightarrow\;2+6+3=\mathbf{11}\text{ free.} \]

</details>

<details class="master-subpart" open>
<summary><span class="tag tag-2plus">≥2 ex</span> (Q3) <em>Write the expression of their likelihood, given data $y_{1:t}$.</em>  [May 2025 Q3c, May 2021 Q4c]</summary>

\textbf{Problem.} The likelihood is the marginal
\[ L(\phi;y_{1:T})=\sum_{s_{1:T}\in\{1,\dots,k\}^T} p(y_{1:T},s_{1:T};\phi), \]
a sum of $k^T$ terms --- exponential in $T$.

\textbf{Forward algorithm} ($O(k^2 T)$). Define the \emph{forward variable}
\[ \alpha_t(i)=\mathbb{P}(Y_{1:t}=y_{1:t},\,S_t=i;\phi). \]

\textbf{Recursion.}

\emph{Initialisation:}
\[ \alpha_1(i)=\pi_i\,f_i(y_1). \]

\emph{Induction step:} marginalise $S_{t-1}$ in the joint $p(Y_{1:t},S_t=j)=\sum_i p(Y_{1:t-1},S_{t-1}=i)\,p_{ij}\,f_j(y_t)$, giving
\[ \boxed{\;\alpha_t(j)=\Bigl(\sum_{i=1}^{k}\alpha_{t-1}(i)\,p_{ij}\Bigr)\,f_j(y_t).\;} \]

\emph{Termination:}
\[ L(\phi;y_{1:T})=\sum_{i=1}^{k}\alpha_T(i). \]

\textbf{Per-emission instantiations.}

\emph{May 2025 Q3 --- Categorical:}
\[ \alpha_1(i)=\pi_i\,e_{i,y_1};\qquad \alpha_t(j)=\Bigl(\sum_i\alpha_{t-1}(i)\,p_{ij}\Bigr)e_{j,y_t}. \]

\emph{May 2021 Q4 --- Poisson:}
\[ \alpha_1(i)=\pi_i\,\frac{e^{-\lambda_i}\lambda_i^{y_1}}{y_1!};\qquad \alpha_t(j)=\Bigl(\sum_i\alpha_{t-1}(i)\,p_{ij}\Bigr)\frac{e^{-\lambda_j}\lambda_j^{y_t}}{y_t!}. \]

\emph{Numerical hygiene.} The $\alpha_t$'s underflow rapidly. In practice, work in log-space with the log-sum-exp trick or rescale each $\alpha_t$ to sum to 1 and accumulate the log of the normaliser; the final log-likelihood is the sum of those log-normalisers.

\textbf{MLE.} No closed form. Use \textbf{EM / Baum--Welch}: E-step runs forward--backward to obtain $\gamma_t(i)=\mathbb{P}(S_t=i\mid y_{1:T};\phi)$ and $\xi_t(i,j)=\mathbb{P}(S_t=i,S_{t+1}=j\mid y_{1:T};\phi)$; M-step has closed-form updates for $\pi,\mathbf P$ and (for Poisson) $\widehat\lambda_i=\sum_t\gamma_t(i)y_t/\sum_t\gamma_t(i)$.

\emph{R:}

</details>

\textbf{Linked exams:} \texttt{exam_may_2025_q3}, \texttt{exam_may_2023_q3}, \texttt{exam_may_2022_q6}, \texttt{exam_may_2021_q4}.""",
    "images": ['images/master/master_t5a_ai.png']
}

master_exercises_ts["t5b"] = {
    "title": 'Master — Decoding (Viterbi / forward-backward) + path-probability',
    "content": r"""\textbf{\textcolor{red}{MASTER --- Decoding (Viterbi / forward--backward) + path-probability}}

---

<details class="master-subpart" open>
<summary><span class="tag tag-2plus">1 ex</span> (Q1) <em>The observed data are $(y_1,\dots,y_{13})=(2,1,1,1,2,3,3,2,1,1,2,3)$. Before taking the sample, what was the probability of observing such a result?</em>  [Jun 2024 Q4a]</summary>

\textbf{Setup.} The start-up status $(Y_t)_{t\ge 0}\in\{1,2,3\}$ is a homogeneous MC with $Y_0=1$. We want $\mathbb{P}(Y_{1:12}=y_{1:12}\mid Y_0=1)$ for the path
\[ 1\to 2\to 1\to 1\to 1\to 2\to 3\to 3\to 2\to 1\to 1\to 2\to 3. \]
(The prompt writes 13 observations; with $Y_0=1$ fixed this is $Y_1=2,Y_2=1,\dots,Y_{12}=3$ --- 12 transitions.)

\textbf{Count transitions along the path:}
\[ n_{11}=3,\quad n_{12}=3,\quad n_{21}=2,\quad n_{23}=2,\quad n_{32}=1,\quad n_{33}=1. \]
\emph{Sanity check:} $3+3+2+2+1+1=12$ transitions, as required.

\textbf{Apply the Markov property and homogeneity:}
\[ \mathbb{P}(Y_{1:12}\mid Y_0=1)=\prod_{t=1}^{12} p_{y_{t-1},y_t}=\prod_{i,j} p_{ij}^{n_{ij}}. \]

\[ \boxed{\;\mathbb{P}(Y_{1:12}\mid Y_0=1)=p_{11}^{3}\,p_{12}^{3}\,p_{21}^{2}\,p_{23}^{2}\,p_{32}\,p_{33}.\;} \]

\textbf{Known or estimated?} The \emph{form} is known a priori from the model; the \emph{numerical value} depends on the unknown transition matrix $\mathbf P$ and would need to be estimated (plug-in MLE $\widehat p_{ij}=n_{ij}/n_{i,+}$) if a number were required.

</details>

<details class="master-subpart" open>
<summary><span class="tag tag-2plus">1 ex</span> (Q2) <em>Let us now use a Hidden Markov Model (HMM) with two latent states. Write the model in a clean and precise way. What are the unknown parameters of the model?</em>  [Jun 2024 Q4b.i]</summary>

\textbf{Model statement ($k=2$ states, observations in $\{1,2,3\}$).}

\emph{Latent chain:} $(S_t)_{t\ge 1}\in\{1,2\}$ homogeneous Markov,
\[ S_1\sim\pi=(\pi_1,\pi_2),\qquad \mathbf P=\begin{pmatrix} p_{11}&p_{12}\\ p_{21}&p_{22}\end{pmatrix},\;p_{ij}=\mathbb{P}(S_{t+1}=j\mid S_t=i). \]

\emph{Emission:} $Y_t\mid S_t=i\sim\mathrm{Cat}(e_{i,1},e_{i,2},e_{i,3})$ over $\{1,2,3\}$ for $i=1,2$. Store as $\mathbf E\in\mathbb{R}^{2\times 3}$.

\emph{Conditional independence:} $Y_t\perp(S_{-t},Y_{-t})\mid S_t$.

\textbf{Parameters.} $\phi=(\pi,\mathbf P,\mathbf E)$ with free counts
\[ \underbrace{(2-1)}_{\pi}+\underbrace{2(2-1)}_{\mathbf P}+\underbrace{2(3-1)}_{\mathbf E}=1+2+4=\mathbf{7}\text{ free.} \]
Identifiable up to label-swapping of the two latent states.

</details>

<details class="master-subpart" open>
<summary><span class="tag tag-2plus">1 ex</span> (Q3) <em>Write the expression of the likelihood of the unknown parameters.</em>  [Jun 2024 Q4b.ii]</summary>

\textbf{Forward algorithm} ($O(k^2 T)$ with $k=2$).

\emph{Initialisation:} $\alpha_1(i)=\pi_i\,e_{i,y_1}$, $i=1,2$.

\emph{Recursion:}
\[ \alpha_t(j)=\Bigl(\sum_{i=1}^{2}\alpha_{t-1}(i)\,p_{ij}\Bigr)\,e_{j,y_t},\qquad t=2,\dots,T,\;j=1,2. \]

\emph{Termination:}
\[ \boxed{\;L(\phi;y_{1:T})=\sum_{i=1}^{2}\alpha_T(i).\;} \]

\emph{Why the recursion works.} Marginalise $S_{t-1}$ in the joint:
\[ \mathbb{P}(Y_{1:t},S_t=j)=\sum_i \mathbb{P}(Y_{1:t-1},S_{t-1}=i)\,\underbrace{\mathbb{P}(S_t=j\mid S_{t-1}=i)}_{p_{ij}}\,\underbrace{\mathbb{P}(Y_t=y_t\mid S_t=j)}_{e_{j,y_t}}. \]

\emph{Numerical hygiene.} Work in log-space or rescale $\alpha_t$ to sum to one and accumulate $\log\bigl(\sum_i\alpha_t(i)\bigr)$ to avoid underflow.

</details>

<details class="master-subpart" open>
<summary><span class="tag tag-2plus">1 ex</span> (Q4) <em>Then explain how you would solve decoding.</em>  [Jun 2024 Q4b.iii]</summary>

\textbf{Two flavours of decoding.}

\textbf{(A) Global decoding --- Viterbi (joint MAP path).}

\emph{Goal:} $\widehat s_{1:T}=\arg\max_{s_{1:T}}\mathbb{P}(S_{1:T}=s_{1:T}\mid y_{1:T};\phi)=\arg\max_{s_{1:T}}\mathbb{P}(S_{1:T},y_{1:T};\phi)$ (denominator $p(y_{1:T})$ is constant in $s_{1:T}$).

\emph{Recursion} --- replace "$\sum_i$" in the forward algorithm with "$\max_i$". Let
\[ \delta_t(j)=\max_{s_{1:t-1}}\mathbb{P}(S_{1:t-1}=s_{1:t-1},S_t=j,Y_{1:t}=y_{1:t};\phi). \]
\[ \delta_1(j)=\pi_j\,e_{j,y_1};\qquad \delta_t(j)=\Bigl[\max_{i}\delta_{t-1}(i)\,p_{ij}\Bigr]\,e_{j,y_t}. \]
Store \textbf{back-pointers} $\psi_t(j)=\arg\max_i \delta_{t-1}(i)\,p_{ij}$.

\emph{Termination + back-trace:}
\[ \widehat s_T=\arg\max_j\delta_T(j),\qquad \widehat s_t=\psi_{t+1}(\widehat s_{t+1}),\;t=T-1,\dots,1. \]

Complexity $O(k^2 T)$.

\textbf{(B) Local / pointwise decoding --- forward--backward (marginal MAP).}

\emph{Goal:} at each $t$ separately,
\[ \widehat s_t=\arg\max_{s\in\{1,\dots,k\}}\gamma_t(s),\qquad \gamma_t(s)=\mathbb{P}(S_t=s\mid y_{1:T};\phi). \]

\emph{Forward variable} $\alpha_t(i)=\mathbb{P}(Y_{1:t},S_t=i)$ (as above).

\emph{Backward variable}
\[ \beta_t(i)=\mathbb{P}(Y_{t+1:T}\mid S_t=i),\quad \beta_T(i)=1,\quad \beta_t(i)=\sum_j p_{ij}\,e_{j,y_{t+1}}\,\beta_{t+1}(j). \]

\emph{Posterior smoothing:}
\[ \boxed{\;\gamma_t(i)=\frac{\alpha_t(i)\,\beta_t(i)}{\sum_j\alpha_t(j)\,\beta_t(j)}.\;} \]

\textbf{Viterbi vs forward--backward --- when they differ.} Pointwise MAP maximises each $\gamma_t$ \emph{independently} and can return a path that has \textbf{zero joint probability} (e.g.\ if $\widehat s_t=1$, $\widehat s_{t+1}=3$ but $p_{13}=0$). Viterbi maximises the joint, so its path is always feasible. Use Viterbi when the path-as-a-whole matters (segmentation, speech); use forward--backward when you want the marginal posterior at a single time.

\emph{R:}

</details>

\textbf{Linked exams:} \texttt{exam_jun_2024_q4}.""",
    "images": ['images/master/master_t5b_ai.png']
}

master_exercises_ts["t6a"] = {
    "title": 'Master — SSM / DLM general definition (univariate & multivariate)',
    "content": r"""\textbf{\textcolor{red}{MASTER --- SSM / DLM general definition (univariate \& multivariate)}}

---

<details class="master-subpart" open>
<summary><span class="tag tag-2plus">≥2 ex</span> (Q1) <em>For an $m$-dimensional time series $(Y_t)_{t\ge 1}$, define (precisely) a dynamic linear model (DLM), denoting the $p$-dimensional state process by $(\theta_t)_{t\ge 0}$.</em>  [Sep 2025 Q4a, May 2024 Q5a]</summary>

\textbf{DLM definition} (DLMwR \S 2.3).

\emph{State equation} (latent, $p$-dimensional, linear-Gaussian Markov):
\[ \theta_t=G_t\,\theta_{t-1}+w_t,\qquad w_t\overset{\text{iid}}{\sim}\mathcal{N}_p(0,W_t). \]

\emph{Observation equation} (observed, $q$-dimensional, linear-Gaussian emission; here $q=m$):
\[ Y_t=F_t\,\theta_t+v_t,\qquad v_t\overset{\text{iid}}{\sim}\mathcal{N}_q(0,V_t). \]

\emph{Initial condition} and \emph{independence}:
\[ \theta_0\sim\mathcal{N}_p(m_0,C_0),\qquad \{w_t\}_{t\ge 1},\;\{v_s\}_{s\ge 1},\;\theta_0\text{ mutually independent.} \]

\textbf{Dimensions.} $Y_t\in\mathbb{R}^q$, $\theta_t\in\mathbb{R}^p$, $F_t\in\mathbb{R}^{q\times p}$, $G_t\in\mathbb{R}^{p\times p}$, $V_t\in\mathbb{R}^{q\times q}$ (sym. PSD), $W_t\in\mathbb{R}^{p\times p}$ (sym. PSD).

\textbf{Special cases.}
- \emph{Univariate.} $q=1$ is a strict special case; DLMs are not restricted to univariate.
- \emph{Time-invariant.} If $(F_t,G_t,V_t,W_t)=(F,G,V,W)$ for all $t$, the DLM is \emph{homogeneous}.
- \emph{Random walk + noise (local level).} $p=q=1$, $F=G=1$, $V,W$ scalars --- canonical baseline.

\emph{R:}

</details>

<details class="master-subpart" open>
<summary><span class="tag tag-2plus">1 ex</span> (Q2) <em>What is the conditional distribution of $Y_t$ given $(\theta_t,Y_{1:t-1})$?</em>  [Sep 2025 Q4b]</summary>

\textbf{Claim:}
\[ \boxed{\;Y_t\mid(\theta_t,Y_{1:t-1})\sim\mathcal{N}_q(F_t\theta_t,\,V_t).\;} \]

\textbf{Why.} The observation equation gives $Y_t=F_t\theta_t+v_t$ with $v_t\sim\mathcal{N}_q(0,V_t)$ independent of $(\theta_t,Y_{1:t-1})$ (independence chain: $v_t$ is independent of $\theta_0$, all $w_s$ and all $v_s$ with $s\ne t$, hence of any measurable function thereof).

\emph{Conditional independence of observations:} given the contemporaneous state $\theta_t$, past observations $Y_{1:t-1}$ are uninformative about $Y_t$ --- this is the \emph{observation Markov property} of the SSM:
\[ Y_t\perp Y_{1:t-1}\mid\theta_t. \]

Therefore conditioning further on $Y_{1:t-1}$ adds no information beyond $\theta_t$.

</details>

<details class="master-subpart" open>
<summary><span class="tag tag-2plus">1 ex</span> (Q3) <em>What is the conditional distribution of $\theta_t$ given $\theta_{0:t-1}$?</em>  [Sep 2025 Q4c]</summary>

\textbf{Claim:}
\[ \boxed{\;\theta_t\mid\theta_{0:t-1}\sim\mathcal{N}_p(G_t\theta_{t-1},\,W_t).\;} \]

\textbf{Why.} The state equation gives $\theta_t=G_t\theta_{t-1}+w_t$ with $w_t\sim\mathcal{N}_p(0,W_t)$ independent of $(\theta_0,w_1,\dots,w_{t-1})$ and therefore of $\theta_{0:t-1}=\bigl(\theta_0,\theta_0+w_1,\dots\bigr)$ (a deterministic function thereof).

\emph{State Markov property:} given $\theta_{t-1}$, earlier states $\theta_{0:t-2}$ are uninformative:
\[ \theta_t\perp\theta_{0:t-2}\mid\theta_{t-1}. \]
Conditioning further on the full past adds no information beyond $\theta_{t-1}$.

\emph{DAG view.} The DLM DAG is $\theta_0\to\theta_1\to\cdots\to\theta_t\to\cdots$ with $\theta_s\to Y_s$ at each $s$. Every path from $\theta_{0:t-2}$ to $\theta_t$ passes through $\theta_{t-1}$ as a serial (chain) node; conditioning on $\theta_{t-1}$ $d$-separates them.

</details>

<details class="master-subpart" open>
<summary><span class="tag tag-2plus">1 ex</span> (Q4) <em>Given $\theta_{t-1}\mid y_{1:t-1}\sim\mathcal{N}(m_{t-1},C_{t-1})$, explain how you obtain the one-step-ahead state predictive distribution of $\theta_t$ given $y_{1:t-1}$.</em>  [May 2024 Q5b]</summary>

\textbf{Kalman "predict" step (state).} Apply $\theta_t=G_t\theta_{t-1}+w_t$ to the filtering distribution.

\textbf{Step 1 --- Gaussianity.} $\theta_{t-1}\mid y_{1:t-1}\sim\mathcal{N}_p(m_{t-1},C_{t-1})$ and $w_t\sim\mathcal{N}_p(0,W_t)$ is Gaussian \emph{independent} of $\theta_{t-1}$ and of $y_{1:t-1}$ (state-eq.\ noise is future-independent of past observations). A linear combination of independent Gaussians is Gaussian, so $\theta_t\mid y_{1:t-1}$ is Gaussian.

\textbf{Step 2 --- mean.} By linearity of conditional expectation,
\[ a_t:=\mathbb{E}[\theta_t\mid y_{1:t-1}]=G_t\,\mathbb{E}[\theta_{t-1}\mid y_{1:t-1}]+\mathbb{E}[w_t\mid y_{1:t-1}]=G_t\,m_{t-1}+0=G_t m_{t-1}. \]

\textbf{Step 3 --- covariance.} By independence of $w_t$ from $\theta_{t-1}\mid y_{1:t-1}$,
\[ R_t:=\operatorname{Var}(\theta_t\mid y_{1:t-1})=G_t\,C_{t-1}\,G_t^{\top}+W_t. \]

\textbf{Conclusion.}
\[ \boxed{\;\theta_t\mid y_{1:t-1}\sim\mathcal{N}_p(a_t,R_t),\quad a_t=G_t m_{t-1},\;R_t=G_t C_{t-1}G_t^{\top}+W_t.\;} \]

</details>

<details class="master-subpart" open>
<summary><span class="tag tag-2plus">1 ex</span> (Q5) <em>How can one obtain the one-step-ahead predictive distribution of $Y_t$ given data $y_{1:t-1}$? Show all steps precisely.</em>  [Sep 2025 Q4d]</summary>

\textbf{Kalman one-step-ahead predictive for $Y_t$} --- two propagations (DLMwR \S 2.7.2, Prop.\ 2.2).

\textbf{Step 1 --- state predict} (as in Q4):
\[ \theta_t\mid y_{1:t-1}\sim\mathcal{N}_p(a_t,R_t),\quad a_t=G_t m_{t-1},\;R_t=G_t C_{t-1}G_t^{\top}+W_t. \]

\textbf{Step 2 --- propagate through the obs.\ equation.} $Y_t=F_t\theta_t+v_t$ with $v_t\sim\mathcal{N}_q(0,V_t)$ independent of $\theta_t$ and of $y_{1:t-1}$. Linear combination of independent Gaussians is Gaussian:
\[ \mathbb{E}[Y_t\mid y_{1:t-1}]=F_t\,a_t,\qquad \operatorname{Var}(Y_t\mid y_{1:t-1})=F_t R_t F_t^{\top}+V_t. \]

\textbf{Conclusion.}
\[ \boxed{\;Y_t\mid y_{1:t-1}\sim\mathcal{N}_q(f_t,Q_t),\quad f_t=F_t a_t,\;Q_t=F_t R_t F_t^{\top}+V_t.\;} \]

\emph{Why this matters.} The $(f_t,Q_t)$ pair underpins (i) point forecasts (Bayes optimal under quadratic / absolute loss = $f_t$, since $Y_t\mid y_{1:t-1}$ is Gaussian / symmetric), (ii) credible / prediction intervals $f_t\pm z_{1-\alpha/2}\sqrt{(Q_t)_{jj}}$, (iii) the \textbf{prediction-error decomposition} likelihood $\ell(\phi)=-\tfrac12\sum_t[q\log(2\pi)+\log|Q_t|+e_t^{\top}Q_t^{-1}e_t]$ with $e_t=y_t-f_t$.

\emph{R sketch (one-step-ahead from a fitted DLM):}

</details>

\textbf{Linked exams:} \texttt{exam_sep_2025_q4}, \texttt{exam_may_2024_q5}.""",
    "images": ['images/master/master_t6a_ai.png']
}

master_exercises_ts["t6b"] = {
    "title": 'Master — SSM flexibility — non-stationarity & SV models (is it a DLM?)',
    "content": r"""\textbf{\textcolor{red}{MASTER --- SSM flexibility --- non-stationarity \& SV models (is it a DLM?)}}

---

<details class="master-subpart" open>
<summary><span class="tag tag-2plus">1 ex</span> (Q1) <em>Consider an $m$-dimensional time series $(Y_t)_{t\ge 1}$, for example describing the prices of $m$ stocks in a portfolio over time, and suppose that it is \textbf{not} stationary. Can you use a state--space model for it? Start your reply with: \textbf{YES, because\ldots, NO because\ldots, or YES but only if\ldots}</em>  [Sep 2025 Q3]</summary>

\textbf{YES, because} SSMs do \emph{not} require $(Y_t)$ to be stationary --- the latent state $(\theta_t)$ itself may be non-stationary (random walk, integrated, regime-switching, trend + seasonal) and the observations inherit that non-stationarity through $Y_t=F_t\theta_t+v_t$. The Kalman filter / smoother is derived from \textbf{Markovianity of $(\theta_t)$} + \textbf{conditional Gaussianity}, \emph{not} from stationarity of either process.

\emph{Canonical example for $I(1)$ asset prices.} The multivariate local-level / random-walk-plus-noise DLM
\[\theta_t=\theta_{t-1}+w_t,\quad w_t\sim\mathcal{N}_m(0,W);\qquad Y_t=\theta_t+v_t,\quad v_t\sim\mathcal{N}_m(0,V),\]
treats each "ideal price" $\theta_{j,t}$ as a random walk and the observed price as a noisy reading. $\theta_t$ is non-stationary ($\operatorname{Var}(\theta_t)=tW$ grows linearly), $(Y_t)$ inherits the same growth, yet the Kalman filter applies exactly.

\emph{Key contrast with ARMA.} ARMA needs $(Y_t)$ weakly stationary; SSMs handle non-stationarity \textbf{inside the model} via $G_t$ (e.g. $G=1$ for RW) instead of requiring a differencing pre-step.

\emph{R sketch.}

</details>

<details class="master-subpart" open>
<summary><span class="tag tag-2plus">1 ex</span> (Q2) <em>Is this model a state-space process?</em> (Stochastic-volatility model $Y_t=\exp(\theta_t/2)v_t$, $\theta_t=\alpha_1+\alpha_2\theta_{t-1}+w_t$)  [May 2022 Q4a]</summary>

\textbf{YES, it is a state-space process.} The two defining properties of an SSM hold:

- \textbf{Latent Markov chain.} $(\theta_t)$ is a Gaussian AR(1), so it is a homogeneous Markov chain: $\theta_t\mid\theta_{0:t-1}=\theta_t\mid\theta_{t-1}\sim\mathcal{N}(\alpha_1+\alpha_2\theta_{t-1},\sigma^2)$.

- \textbf{Conditional independence of observations.} Given $\theta_t$, $Y_t=\exp(\theta_t/2)v_t\sim\mathcal{N}(0,e^{\theta_t})$ with $v_t\sim\mathcal{N}(0,1)$ independent of everything else; hence $Y_t\perp(Y_{1:t-1},\theta_{0:t-1},\theta_{t+1:T})\mid\theta_t$.

That is precisely the SSM template: a hidden Markov state $\theta_t$ that drives the observation density $p(y_t\mid\theta_t)$.

\emph{Interpretation.} This is the canonical \textbf{stochastic-volatility (SV) model}: log-volatility $\theta_t$ follows an AR(1), the return $Y_t$ has time-varying variance $e^{\theta_t}$ but \emph{zero mean}. Volatility clustering arises because nearby $\theta_t,\theta_{t+1}$ are highly correlated.

</details>

<details class="master-subpart" open>
<summary><span class="tag tag-2plus">1 ex</span> (Q3) <em>Is it a DLM?</em> (Stochastic-volatility model above)  [May 2022 Q4b]</summary>

\textbf{NO, it is not a DLM.} A DLM requires the observation equation $Y_t=F_t\theta_t+v_t$ to be \textbf{linear} in $\theta_t$ with \textbf{Gaussian additive} noise of $\theta_t$-independent variance. Here:

- $Y_t=\exp(\theta_t/2)v_t$ is \textbf{multiplicative} (and non-linear) in $\theta_t$ --- the state enters through $\exp(\theta_t/2)$, not as a linear coefficient.
- The conditional variance $\operatorname{Var}(Y_t\mid\theta_t)=e^{\theta_t}$ \emph{depends on the state} --- in a DLM, $V_t$ must be deterministic.

Consequence: the Kalman filter does \emph{not} apply (the joint $(\theta_t,Y_t)\mid y_{1:t-1}$ is no longer Gaussian).

\emph{Standard work-around: log-square linearisation.} Squaring and taking logs,
\[\log Y_t^2=\theta_t+\log v_t^2,\qquad \log v_t^2\sim\log\chi^2_1\ (\text{mean }\approx-1.27,\,\text{var}\approx 4.93).\]
Now the observation equation is linear in $\theta_t$, but the noise $\log\chi^2_1$ is \textbf{non-Gaussian}. Practical fixes:

(i) \emph{Quasi-Maximum Likelihood:} pretend $\log v_t^2$ is Gaussian (matched first two moments), run a standard DLM Kalman filter --- biased but consistent.

(ii) \emph{Gaussian mixture approximation} (Kim, Shephard, Chib 1998): approximate $\log\chi^2_1$ by a 7-component mixture of normals; conditional on the mixture indicator, the model becomes a DLM, enabling efficient MCMC.

(iii) \emph{Particle filter:} keep the original non-linear, non-Gaussian model and use sequential Monte Carlo.

\emph{R (KSC mixture-approximation MCMC for SV):} 

</details>

\textbf{Linked exams:} \texttt{exam_sep_2025_q3}, \texttt{exam_may_2022_q4}.""",
    "images": ['images/master/master_t6b_ai.png']
}

master_exercises_ts["t7a"] = {
    "title": 'Master — Random-walk + noise model — definition & independence proofs',
    "content": r"""\textbf{\textcolor{red}{MASTER --- Random-walk + noise model --- definition \& independence proofs}}

---

<details class="master-subpart" open>
<summary><span class="tag tag-2plus">≥2 ex</span> (Q1) <em>Define precisely a random-walk-plus-noise model. / Complete the model with the assumptions needed.</em>  [May 2024 Q3a, May 2023 Q4a]</summary>

\textbf{Random walk plus noise / local-level model.}

\textbf{Observation equation:} $Y_t=\theta_t+v_t$, $\quad v_t\overset{\text{iid}}{\sim}\mathcal{N}(0,V)$,
\textbf{State equation:} $\theta_t=\theta_{t-1}+w_t$, $\quad w_t\overset{\text{iid}}{\sim}\mathcal{N}(0,W)$,
\textbf{Initial state:} $\theta_0\sim\mathcal{N}(m_0,C_0)$,

with $\{v_t\}_{t\ge 1}$, $\{w_t\}_{t\ge 1}$, $\theta_0$ \textbf{mutually independent}.

\emph{DLM identification.} In the general DLM template $(F_t,G_t,V_t,W_t)$, this corresponds to the scalar case
\[F=1,\quad G=1,\quad V_t\equiv V,\quad W_t\equiv W\quad(\text{time-invariant, univariate}).\]

\emph{Properties.}

- \textbf{Latent process is non-stationary:} $\theta_t=\theta_0+\sum_{u=1}^{t}w_u$, so $\operatorname{Var}(\theta_t)=C_0+tW$ grows linearly.
- \textbf{Observation process is non-stationary:} $\operatorname{Var}(Y_t)=C_0+tW+V$.
- \textbf{Signal-to-noise ratio} $W/V$ controls how quickly the level adapts. $W\to 0$ collapses to a constant-mean model; $W\to\infty$ effectively re-initialises every step.
- \textbf{Steady-state Kalman gain} converges to $K^*=\tfrac{1}{2}(\sqrt{W^2+4VW}-W)/V$ (Riccati fixed point).

\emph{R:} 

</details>

<details class="master-subpart" open>
<summary><span class="tag tag-2plus">1 ex</span> (Q2) <em>Prove that $v_t$ and $(\theta_1,\dots,\theta_t)$ are independent.</em>  [May 2024 Q3b]</summary>

\textbf{Goal.} Show $v_t\perp(\theta_1,\dots,\theta_t)$.

\textbf{Step 1 --- unfold the state recursion.} Iterating $\theta_s=\theta_{s-1}+w_s$ from $s=1$ down to the initial state,
\[\theta_s=\theta_0+\sum_{u=1}^{s}w_u,\qquad s=1,\dots,t.\]
Therefore the random vector $(\theta_1,\dots,\theta_t)$ is a \emph{deterministic measurable function} of $(\theta_0,w_1,\dots,w_t)$:
\[(\theta_1,\dots,\theta_t)=\Phi(\theta_0,w_1,\dots,w_t)\quad\text{for some Borel-measurable }\Phi.\]

\textbf{Step 2 --- invoke the mutual-independence assumption.} The model specification states that $\{v_s\}_{s\ge 1}$, $\{w_s\}_{s\ge 1}$, $\theta_0$ are mutually independent. In particular, $v_t$ is independent of the entire collection $(\theta_0,w_1,\dots,w_t)$.

\textbf{Step 3 --- independence is preserved under measurable functions.} If $X\perp Y$ then $X\perp f(Y)$ for any Borel $f$. Apply with $X=v_t$, $Y=(\theta_0,w_1,\dots,w_t)$, $f=\Phi$:
\[v_t\perp\Phi(\theta_0,w_1,\dots,w_t)=(\theta_1,\dots,\theta_t).\qquad\Box\]

\emph{Why this matters.} This is one of the foundational independence properties used in deriving the Kalman filter --- it justifies treating $v_t$ as "fresh noise" uncorrelated with the past latent path when conditioning on $y_{1:t-1}$.

</details>

<details class="master-subpart" open>
<summary><span class="tag tag-2plus">1 ex</span> (Q3) <em>Prove that $w_t$ and $(\theta_1,\dots,\theta_{t-1})$ are independent.</em>  [May 2023 Q4b]</summary>

\textbf{Goal.} Show $w_t\perp(\theta_1,\dots,\theta_{t-1})$.

\textbf{Step 1 --- unfold the state recursion up to time $t-1$:}
\[\theta_s=\theta_0+\sum_{u=1}^{s}w_u,\quad s=1,\dots,t-1\;\Longrightarrow\;(\theta_1,\dots,\theta_{t-1})=\Phi(\theta_0,w_1,\dots,w_{t-1}).\]

\textbf{Step 2 --- mutual independence.} By the model assumption, $\{w_s\}_{s\ge 1}$ are i.i.d.\ and independent of $\theta_0$. In particular, $w_t$ is independent of the vector $(\theta_0,w_1,\dots,w_{t-1})$ (the time index $t$ is strictly later than $1,\dots,t-1$).

\textbf{Step 3 --- preservation under measurable functions.} As before, $w_t\perp(\theta_0,w_1,\dots,w_{t-1})$ implies
\[w_t\perp\Phi(\theta_0,w_1,\dots,w_{t-1})=(\theta_1,\dots,\theta_{t-1}).\qquad\Box\]

\emph{Why this matters.} This independence is exactly what makes the predict step $\theta_t\mid y_{1:t-1}\sim\mathcal{N}(m_{t-1},C_{t-1}+W)$ work: the state-innovation $w_t$ adds a clean $W$-variance independent of the inductive hypothesis $\theta_{t-1}\mid y_{1:t-1}\sim\mathcal{N}(m_{t-1},C_{t-1})$.

</details>

\textbf{Linked exams:} \texttt{exam_may_2024_q3}, \texttt{exam_may_2023_q4}.""",
    "images": ['images/master/master_t7a_ai.png']
}

master_exercises_ts["t7b"] = {
    "title": 'Master — Local linear trend / structural BSM',
    "content": r"""\textbf{\textcolor{red}{MASTER --- Local linear trend / structural BSM}}

---

<details class="master-subpart" open>
<summary><span class="tag tag-2plus">1 ex</span> (Q1) <em>The following is a popular model for the trend of a univariate time series $(Y_t)_{t\ge 1}$: $Y_t=\mu_t+\varepsilon_t$, $\mu_t=\mu_{t-1}+\beta_{t-1}+w_{1,t}$, $\beta_t=\beta_{t-1}+w_{2,t}$ with Gaussian iid noises. Write the model in the form of a DLM.</em>  [Sep 2025 Q5]</summary>

\textbf{Stack level + slope into a 2-dim.\ latent state.} Define
\[\theta_t=\begin{pmatrix}\mu_t\\\beta_t\end{pmatrix}\in\mathbb{R}^2.\]

\textbf{State equation.} Read the two updates as a matrix equation:
\[\theta_t=\underbrace{\begin{pmatrix}1&1\\0&1\end{pmatrix}}_{G}\theta_{t-1}+w_t,\qquad w_t=\begin{pmatrix}w_{1,t}\\w_{2,t}\end{pmatrix}\sim\mathcal{N}_2\!\left(0,\;\underbrace{\begin{pmatrix}\sigma_{w_1}^2&0\\0&\sigma_{w_2}^2\end{pmatrix}}_{W}\right).\]
Row 1: $\mu_t=\mu_{t-1}+\beta_{t-1}+w_{1,t}$ (level += previous slope + noise). Row 2: $\beta_t=\beta_{t-1}+w_{2,t}$ (slope is a RW). $W$ is diagonal because $(w_{1,t}),(w_{2,t})$ are independent.

\textbf{Observation equation.} Only the level $\mu_t$ is observed (noisily):
\[Y_t=\underbrace{(1,\;0)}_{F}\theta_t+\varepsilon_t,\qquad \varepsilon_t\sim\mathcal{N}(0,\underbrace{\sigma^2}_{V}).\]

\textbf{Initial state.} $\theta_0=(\mu_0,\beta_0)^{\top}\sim\mathcal{N}_2(m_0,C_0)$, independent of $\{w_t\},\{\varepsilon_t\}$.

\textbf{Summary box.}
\[\boxed{\;F=(1,0),\;V=\sigma^2,\;G=\begin{pmatrix}1&1\\0&1\end{pmatrix},\;W=\mathrm{diag}(\sigma_{w_1}^2,\sigma_{w_2}^2).\;}\]

\emph{Intuition.} If $\sigma_{w_2}^2=0$, the slope is constant and $\mu_t$ is a deterministic linear trend + RW. Letting $\sigma_{w_2}^2>0$ allows the slope itself to drift, producing curved (locally linear) trajectories.

\emph{R:} 

</details>

<details class="master-subpart" open>
<summary><span class="tag tag-2plus">1 ex</span> (Q2) <em>The plot below shows monthly measurements of the Co2 level... we clearly see a trend and a seasonal component. ... a common approach, for example in ARMA modeling, is to remove them, and model the detrended and deseasonalized data. Could we model the data without any preliminary transformation, by using a dynamic linear model (DLM)? A. Yes, because: B. No, because:</em>  [May 2021 Q3]</summary>

\textbf{YES.} Use a \textbf{structural / basic-structural DLM} (Harvey's BSM, DLMwR \S 3.2.2--3.2.3): local linear trend $+$ stochastic seasonal block. Letting $s=12$ (monthly seasonality),
\[Y_t=\mu_t+\gamma_t+v_t,\quad v_t\sim\mathcal{N}(0,V),\]
\[\mu_t=\mu_{t-1}+\beta_{t-1}+w_{1,t},\qquad \beta_t=\beta_{t-1}+w_{2,t}\quad\text{(trend block)},\]
\[\gamma_t=-\sum_{j=1}^{s-1}\gamma_{t-j}+w_{3,t}\quad\text{(seasonal block: ensures \(\sum_{j=0}^{s-1}\gamma_{t-j}=w_{3,t}\), zero on average)}.\]
Stack the $s+1$ components into a latent state $\theta_t=(\mu_t,\beta_t,\gamma_t,\gamma_{t-1},\dots,\gamma_{t-s+2})^{\top}\in\mathbb{R}^{s+1}$; the dynamics are linear-Gaussian, so the standard Kalman filter / smoother applies.

\textbf{Why this works without pre-transformation.}

- \emph{Non-stationarity lives inside the state, not the data.} The trend block has $G$-block with unit eigenvalues (level + slope), reproducing $I(2)$ behaviour; the seasonal block has roots on the unit circle. SSMs do not require $(Y_t)$ to be stationary.
- \emph{Diffuse / large-variance initialisation} on the non-stationary state components (large $C_0$ on $\mu_0,\beta_0,\gamma_0,\dots$) tells the filter "I have no prior information about the initial trend or seasonal phase"; after a short burn-in the filter converges.
- \emph{Contrast with ARMA workflow.} ARMA requires weak stationarity, so one must first apply $(1-B)^d(1-B^s)$ to remove trend and seasonality, fit ARMA to the residual, and then "glue back". With BSM the model \emph{is} the trend + seasonal + noise decomposition.

\emph{Bonus.} The smoothed components $\hat\mu_t,\hat\gamma_t$ are directly interpretable signal extractions --- the seasonal-adjusted series is $Y_t-\hat\gamma_t$.

\emph{R:} 

</details>

\textbf{Linked exams:} \texttt{exam_sep_2025_q5}, \texttt{exam_may_2021_q3}.""",
    "images": ['images/master/master_t7b_ai.png']
}

master_exercises_ts["t7c"] = {
    "title": 'Master — Time-varying-coefficient regression DLM',
    "content": r"""\textbf{\textcolor{red}{MASTER --- Time-varying-coefficient regression DLM}}

---

<details class="master-subpart" open>
<summary><span class="tag tag-2plus">≥2 ex</span> (Q1) <em>Explain how you can use DLMs to extend the linear model by allowing time varying regression parameters, that may better capture non-linear behaviour. Assume $\sigma^2$ is known. / Which state-space model could you suggest? Write your proposed model and motivate briefly.</em>  [May 2025 Q4a, Jun 2022 Q6]</summary>

\textbf{Time-varying-coefficient DLM.} Replace the static $(\alpha,\beta)$ by a 2-dim.\ latent state $\theta_t=(\alpha_t,\beta_t)^{\top}$:

\textbf{Observation equation} (with the time-$t$ regressor in $F_t$):
\[Y_t=F_t\theta_t+v_t,\qquad F_t=(1,\;x_t),\qquad v_t\overset{\text{iid}}{\sim}\mathcal{N}(0,\sigma^2).\]
This unpacks as $Y_t=\alpha_t+\beta_t x_t+v_t$ --- same shape as linear regression but with $(\alpha_t,\beta_t)$ now indexed by $t$.

\textbf{State equation} (coefficients evolve as a 2-dim.\ random walk):
\[\theta_t=\theta_{t-1}+w_t,\qquad G=I_2,\qquad w_t\overset{\text{iid}}{\sim}\mathcal{N}_2(0,W),\]
with initial state $\theta_0\sim\mathcal{N}_2(m_0,C_0)$.

\textbf{Why this captures non-linearity.}

- Static regression assumes \emph{one} slope $\beta$ for the entire dose range. If the true dose-response is, e.g.\ saturating, $\beta$ is small at high doses and large at low doses --- a single $\beta$ averages the two and mis-fits both.
- TVC lets $\beta_t$ drift, so the model traces out the local slope $\partial Y_t/\partial x_t\approx\beta_t$ along the dose trajectory. This is a \emph{nonparametric} flexibility: no need to specify a parametric form (logistic, quadratic, \dots).
- $W$ controls smoothness: $W\to 0$ recovers static linear regression; large $W$ allows abrupt coefficient changes.

\textbf{Limits.}

- \emph{Static recovery:} $W=0\Rightarrow\theta_t\equiv\theta_0$ and the model collapses to ordinary Bayesian linear regression with prior $\mathcal{N}_2(m_0,C_0)$.
- \emph{Identifiability:} $\alpha_t$ and $\beta_t$ drift in opposite directions can mimic each other --- centring $x_t$ or constraining $W$ (e.g.\ block diagonal with only $\beta_t$ drifting) helps.

`matplot(smo$s, type="l")   ## alpha_t and beta_t paths`

</details>

<details class="master-subpart" open>
<summary><span class="tag tag-2plus">1 ex</span> (Q2) <em>For inference, you would use the smoothing distribution $(\alpha_t,\beta_t)$. Explain: what is the smoothing distribution?</em>  [May 2025 Q4b]</summary>

\textbf{Definition.} The \textbf{smoothing distribution} is the conditional law of the latent state given \emph{all} observations $y_{1:n}$ (i.e.\ past, present \emph{and} future relative to time $t$). Two flavours:

- \textbf{Joint smoothing:} $\pi(\theta_{0:n}\mid y_{1:n})$ --- the full latent path given all data. For a DLM, jointly Gaussian and characterised by $(\mu^{(s)},\Sigma^{(s)})$ in dimension $2(n+1)$.

- \textbf{Marginal smoothing at time $t$:} $\pi(\theta_t\mid y_{1:n})=\mathcal{N}_2(s_t,S_t)$ --- the law of the state at a single time, given all data. Obtained from the RTS backward recursion (Rauch--Tung--Striebel; DLMwR Prop.\ 2.4).

\textbf{Contrast with filtering.} The \textbf{filtering distribution} $\pi(\theta_t\mid y_{1:t})$ conditions only on \emph{past + present}; smoothing additionally pools future data $y_{t+1:n}$, so it is \textbf{strictly more informative}:
\[\operatorname{Var}(\theta_t\mid y_{1:n})\preceq\operatorname{Var}(\theta_t\mid y_{1:t}),\qquad\text{i.e.\ }S_t\preceq C_t.\]

\textbf{Why use it for the TVC model.} In a clinical trial we want the best retrospective estimate of how $(\alpha_t,\beta_t)$ \emph{evolved} during the trial --- after-the-fact analysis benefits from using future doses to refine the slope at earlier times.

\emph{R:} 

</details>

<details class="master-subpart" open>
<summary><span class="tag tag-2plus">1 ex</span> (Q3) <em>How can you proceed if $\sigma^2$ is unknown?</em>  [May 2025 Q4c]</summary>

Two standard routes.

\textbf{(i) Maximum-likelihood plug-in (empirical Bayes).} Treat $\phi=(\sigma^2,W)$ as unknown parameters. Run the Kalman filter at each candidate $\phi$ to obtain the prediction-error decomposition
\[\ell(\phi)=-\tfrac{1}{2}\sum_{t=1}^{n}\bigl[\log Q_t(\phi)+e_t(\phi)^2/Q_t(\phi)\bigr]+\text{const},\quad e_t=y_t-f_t.\]
Maximise numerically (BFGS / Nelder--Mead, working on log-variance reparameterisations to enforce positivity). Plug $\widehat\phi=(\widehat\sigma^2,\widehat W)$ into the smoother to obtain $\pi(\theta_t\mid y_{1:n},\widehat\phi)$. \emph{Caveat:} ignores parameter uncertainty in $\widehat\phi$, so smoothing intervals are too narrow.

\textbf{(ii) Fully Bayesian via Gibbs + FFBS.} Put conjugate priors $\sigma^2\sim\mathrm{IG}(a_v,b_v)$ and $W\sim\mathrm{IW}(\nu,S)$. Iterate

(a) Sample $\theta_{0:n}\mid y_{1:n},\sigma^2,W$ by \textbf{Forward-Filter Backward-Sample} (FFBS): run KF forward to get $(m_t,C_t)$, then draw $\theta_n\sim\mathcal{N}(m_n,C_n)$ and backward $\theta_t\sim\pi(\theta_t\mid\theta_{t+1},y_{1:t})$.

(b) Sample $\sigma^2\mid y_{1:n},\theta_{0:n}$ from $\mathrm{IG}(a_v+n/2,\;b_v+\tfrac12\sum_t(y_t-F_t\theta_t)^2)$.

(c) Sample $W\mid\theta_{0:n}$ from $\mathrm{IW}(\nu+n,\;S+\sum_t(\theta_t-\theta_{t-1})(\theta_t-\theta_{t-1})^{\top})$.

Posterior credible intervals on $\theta_t$ are obtained from the marginalised samples and \emph{correctly} inflate for parameter uncertainty.

\emph{R:} 

</details>

<details class="master-subpart" open>
<summary><span class="tag tag-2plus">1 ex</span> (Q4) <em>Suppose that the same experiment is repeated in another hospital (similar, but not identical, to the first one). Can you propose a DLM that allows for borrowing strength, by sharing information between the hospitals in the learning process?</em>  [May 2025 Q4d]</summary>

\textbf{Yes --- a hierarchical (multi-level) DLM.} For hospitals $h=1,2$:

\textbf{Hospital-specific DLMs.}
\[Y_t^{(h)}=F_t^{(h)}\theta_t^{(h)}+v_t^{(h)},\qquad \theta_t^{(h)}=\theta_{t-1}^{(h)}+w_t^{(h)},\qquad h=1,2,\]
with $v_t^{(h)}\sim\mathcal{N}(0,\sigma^2)$, $w_t^{(h)}\sim\mathcal{N}_2(0,W)$.

\textbf{Population layer (the "glue").} Tie the two hospitals through a \emph{shared population-level prior} on the initial states (or, equivalently, on hospital-specific coefficient means):
\[\theta_0^{(h)}\mid\mu\sim\mathcal{N}_2(\mu,T),\quad h=1,2;\qquad \mu\sim\mathcal{N}_2(0,\Sigma_0)\quad\text{(hyperprior on population mean)}.\]

\textbf{How borrowing works.} The posterior of each $\theta_0^{(h)}$ is shrunk toward the shared $\mu$; the amount of shrinkage is automatically learned from data ($T$ small $\Rightarrow$ strong borrowing; $T$ large $\Rightarrow$ near-independent fits). Hospitals with few observations gain precision from the other's data, while hospitals with abundant data dominate the estimate of $\mu$.

\textbf{Common design choices.}

- Share only the level (intercept $\alpha$), not the slope --- if researchers believe the dose-response \emph{shape} is universal but baseline toxicity varies.
- Share $W$ across hospitals (same drift speed).
- Share $\sigma^2$ across hospitals (same measurement-error scale).
- Three-level extension: hospitals within country, country-level prior, etc.

\textbf{Inference.} Gibbs sampler: FFBS on each $\theta_{0:n}^{(h)}$ given $(\mu,T,\sigma^2,W)$; conjugate updates for the population layer $\mu\mid\{\theta_0^{(h)}\}$ and the variances. Implemented in \texttt{rjags}, \texttt{stan}, or \texttt{dlm} with hand-coded Gibbs.

\emph{R sketch.} `## stan code: theta0[h] ~ normal(mu, T_chol); mu ~ normal(0, Sigma0_chol); then per-hospital state RW`

</details>

\textbf{Linked exams:} \texttt{exam_may_2025_q4}, \texttt{exam_jun_2022_q6}.""",
    "images": ['images/master/master_t7c_ai.png']
}

master_exercises_ts["t7d"] = {
    "title": 'Master — Multivariate DLM & dependence between latent series',
    "content": r"""\textbf{\textcolor{red}{MASTER --- Multivariate DLM \& dependence between latent series}}

---

<details class="master-subpart" open>
<summary><span class="tag tag-2plus">1 ex</span> (Q1) <em>Write the general expression of a DLM for the time series $\{Y_t\}_{t\ge 1}$.</em> ($m$-dimensional financial-asset prices)  [Jun 2025 Q4a]</summary>

\textbf{General multivariate DLM.} Let $Y_t\in\mathbb{R}^m$ be the observation and $\theta_t\in\mathbb{R}^p$ the latent state.

\textbf{State equation:}
\[\theta_t=G_t\theta_{t-1}+w_t,\qquad w_t\overset{\text{iid}}{\sim}\mathcal{N}_p(0,W_t),\]
with $G_t\in\mathbb{R}^{p\times p}$ the (possibly time-varying) state-evolution matrix.

\textbf{Observation equation:}
\[Y_t=F_t\theta_t+v_t,\qquad v_t\overset{\text{iid}}{\sim}\mathcal{N}_m(0,V_t),\]
with $F_t\in\mathbb{R}^{m\times p}$.

\textbf{Initial state:} $\theta_0\sim\mathcal{N}_p(m_0,C_0)$.

\textbf{Independence:} the three families $\{w_t\}_{t\ge 1}$, $\{v_t\}_{t\ge 1}$, $\theta_0$ are mutually independent.

\emph{The DLM is fully specified by} $\phi=\{(F_t,G_t,V_t,W_t)_{t\ge 1},m_0,C_0\}$. Time-invariance ($F_t\equiv F$, etc.) is the most common special case.

</details>

<details class="master-subpart" open>
<summary><span class="tag tag-2plus">1 ex</span> (Q2) <em>Now let $m=2$. Suppose one models the $\{Y_t\}_{t\ge 1}$, $2\times 1$, as independent random walks plus noise (the random walk would be the assumption in perfect markets). What would be the system and covariance matrices of the DLM above?</em>  [Jun 2025 Q4b]</summary>

\textbf{Setup.} For each asset $j=1,2$, latent ideal price follows a random walk and the observed price is noisy:
\[\theta_{j,t}=\theta_{j,t-1}+w_{j,t},\qquad Y_{j,t}=\theta_{j,t}+v_{j,t},\]
with $w_{j,t},v_{j,t}$ Gaussian iid and \emph{all four families mutually independent}.

\textbf{Stack into 2-dim.\ vectors.} $\theta_t=(\theta_{1,t},\theta_{2,t})^{\top}$, $Y_t=(Y_{1,t},Y_{2,t})^{\top}$, $w_t=(w_{1,t},w_{2,t})^{\top}$, $v_t=(v_{1,t},v_{2,t})^{\top}$. The state equation $\theta_t=I_2\theta_{t-1}+w_t$ gives $G$; the observation $Y_t=I_2\theta_t+v_t$ gives $F$. Independence between assets forces the noise covariances to be \emph{diagonal}.

\textbf{Matrices.}
\[\boxed{\;F=I_2,\quad G=I_2,\quad V=\begin{pmatrix}\sigma_{v_1}^2&0\\0&\sigma_{v_2}^2\end{pmatrix},\quad W=\begin{pmatrix}\sigma_{w_1}^2&0\\0&\sigma_{w_2}^2\end{pmatrix}.\;}\]

\textbf{Initial state.} $\theta_0=(\theta_{1,0},\theta_{2,0})^{\top}\sim\mathcal{N}_2(m_0,C_0)$ with $C_0$ typically diagonal too (or diffuse).

\emph{Filter implication.} With diagonal $V,W,G,F$, the Kalman filter \emph{decouples} into two independent univariate local-level filters --- one per asset. There is no information transfer between assets, which is what "independent random walks" means.

\emph{R:} 

</details>

<details class="master-subpart" open>
<summary><span class="tag tag-2plus">1 ex</span> (Q3) <em>How could you instead introduce a dependence between the two latent random walks (the ideal prices in perfect markets) in the model at point (b)?</em>  [Jun 2025 Q4c]</summary>

Three orthogonal channels --- pick any one (or combine).

\textbf{(1) Correlated state-noise: make $W$ non-diagonal.}
\[W=\begin{pmatrix}\sigma_{w_1}^2 & \rho_w\sigma_{w_1}\sigma_{w_2}\\ \rho_w\sigma_{w_1}\sigma_{w_2} & \sigma_{w_2}^2\end{pmatrix},\quad |\rho_w|<1.\]
\emph{Interpretation:} the innovations to the two ideal prices are \textbf{contemporaneously correlated} (move together within a period) but each latent price is still a marginal RW. Captures common macro shocks that simultaneously push both assets.

\textbf{(2) Common latent factor (low-rank $W$, factor structure).}
\[\theta_t=Af_t,\qquad A\in\mathbb{R}^{2\times 1},\qquad f_t=f_{t-1}+\eta_t,\;\eta_t\sim\mathcal{N}(0,\sigma_f^2).\]
A \textbf{single} latent random walk $f_t$ drives \emph{both} observed prices via loadings $A=(a_1,a_2)^{\top}$. This is a cointegration / common-stochastic-trend model: the two prices share one source of long-run risk; their difference $a_2\theta_{1,t}-a_1\theta_{2,t}=0$ is stationary. Reduces the latent dimension from 2 to 1.

\textbf{(3) Cross terms in $G$: VAR-style spillovers.}
\[G=\begin{pmatrix}1&\delta\\ 0&1\end{pmatrix}\quad\text{or}\quad G=\begin{pmatrix}g_{11}&g_{12}\\g_{21}&g_{22}\end{pmatrix}\quad\text{(full VAR(1) state).}\]
Past innovations to asset 2's level feed into asset 1's level (and vice versa). Captures \textbf{lead-lag} relationships between assets: news about asset 2 today moves asset 1's ideal price tomorrow.

\textbf{Pros / cons summary.}

- Option (1) is the simplest and is enough if dependence is purely \emph{contemporaneous}.
- Option (2) is parsimonious and gives an interpretable common-trend structure --- good for portfolio risk.
- Option (3) is the most flexible but adds many parameters and may not be identifiable for short samples.

\emph{R (option 1):} 

</details>

\textbf{Linked exams:} \texttt{exam_jun_2025_q4}.""",
    "images": ['images/master/master_t7d_ai.png']
}

master_exercises_ts["t7e"] = {
    "title": 'Master — AR(p) as DLM (companion form)',
    "content": r"""\textbf{\textcolor{red}{MASTER --- AR(p) as DLM (companion form)}}

---

<details class="master-subpart" open>
<summary><span class="tag tag-2plus">1 ex</span> (Q1) <em>Consider an autoregressive model of order 2, AR(2): $Y_t=\alpha_1 Y_{t-1}+\alpha_2 Y_{t-2}+\varepsilon_t$, $\varepsilon_t\overset{\text{iid}}{\sim}\mathcal{N}(0,\sigma^2)$. An AR(2) process may be regarded as a special case of a DLM. Would the following be a valid DLM representation? $Y_t=(Y_{t-1},Y_{t-2})^{\top}(\alpha_1,\alpha_2)^{\top}+\varepsilon_t$, $\alpha_{1,t}=\alpha_1$, $\alpha_{2,t}=\alpha_2$.</em>  [Jun 2024 Q2]</summary>

\textbf{Answer: NO.}

The proposed object is \textbf{not} a valid DLM. Two fatal defects:

- \textbf{The "state" has no dynamics.} The proposal sets $\alpha_{1,t}=\alpha_1$, $\alpha_{2,t}=\alpha_2$ as constants. A DLM state must satisfy $\theta_t=G_t\theta_{t-1}+w_t$ with $w_t\sim\mathcal{N}_p(0,W_t)$. A constant "state" is not a stochastic state process.
- \textbf{$F_t$ depends on past observations.} Setting $F_t=(Y_{t-1},Y_{t-2})$ makes the design matrix \emph{data-dependent}. In a proper DLM $F_t$ is a deterministic (or exogenous) sequence. The defining conditional independence
\[ Y_t\perp Y_{1:t-1}\mid\theta_t \]
is then violated, because $Y_t$ depends on $Y_{1:t-1}$ \emph{through} $F_t$ even after conditioning on the state.

\textbf{Correct companion-form DLM (DLMwR \S 3.2.5).} Stack the last two observations into the latent state:
\[ \theta_t=\begin{pmatrix}Y_t\\Y_{t-1}\end{pmatrix},\qquad G=\begin{pmatrix}\alpha_1 & \alpha_2\\1 & 0\end{pmatrix},\qquad F=(1,\;0). \]
State equation: $\theta_t=G\theta_{t-1}+w_t$ with $w_t=(\varepsilon_t,0)^{\top}$, so
\[ W=\begin{pmatrix}\sigma^2 & 0\\0 & 0\end{pmatrix},\qquad V=0. \]
\emph{Check:} the first row of $G\theta_{t-1}+w_t$ gives $\alpha_1 Y_{t-1}+\alpha_2 Y_{t-2}+\varepsilon_t=Y_t$. \checkmark The second row gives $Y_{t-1}$ (a tautology), so the state correctly tracks the lag. Observation $Y_t=F\theta_t=(1,0)\theta_t=Y_t$ with zero observation noise.

\textbf{General AR($p$) companion form.} $\theta_t=(Y_t,\dots,Y_{t-p+1})^{\top}$,
\[ G=\begin{pmatrix}\alpha_1 & \alpha_2 & \cdots & \alpha_p\\1 & 0 & \cdots & 0\\\vdots & \ddots & \ddots & \vdots\\0 & \cdots & 1 & 0\end{pmatrix},\quad F=(1,0,\dots,0),\quad W=\sigma^2 e_1 e_1^{\top},\quad V=0. \]

\emph{Why this matters.} The companion form is the canonical bridge between the ARMA world (Box--Jenkins) and the state-space world (Kalman, particle filter). Once an AR($p$) is in DLM form, the Kalman filter delivers the exact likelihood (prediction-error decomposition) and exact one-step-ahead predictive --- no need for the conditional-likelihood approximation usually used in ARMA estimation.

\emph{R (compare AR(2) directly vs.\ companion-form DLM):}

`## DLM companion form`

`           V=0, W=diag(c(1,0)), m0=c(0,0), C0=diag(2)*1e7)`

</details>

\textbf{Linked exams:} \texttt{exam_jun_2024_q2}.""",
    "images": ['images/master/master_t7e_ai.png']
}

master_exercises_ts["t8a"] = {
    "title": 'Master — Filtering distribution: definition, not just a point estimate',
    "content": r"""\textbf{\textcolor{red}{MASTER --- Filtering distribution: definition, not just a point estimate}}

---

<details class="master-subpart" open>
<summary><span class="tag tag-2plus">1 ex</span> (Q1) <em>In state space models, with $(\theta_t)_{t\ge 0}$ denoting the latent state process, the problem of filtering consists in computing the estimate $\mathbb{E}(\theta_i\mid y_{1:t})$ and update it as new observations become available. Is that correct?</em>  [May 2024 Q4]</summary>

\textbf{Answer: NO} --- two defects in the proposed statement.

\textbf{Defect 1: filtering is a distribution, not a point estimate.}

The filtering target is
\[ \pi(\theta_t\mid y_{1:t}), \]
the conditional \emph{distribution} of the current state given the data so far. The conditional mean $\mathbb{E}[\theta_t\mid y_{1:t}]$ is the optimal point estimate \emph{under quadratic loss}, but it is one summary among many. The full distribution is needed for:

- \textbf{Credible intervals} --- requires the conditional variance $\operatorname{Var}(\theta_t\mid y_{1:t})$ at minimum, and the full shape in non-Gaussian cases.
- \textbf{Predictive distributions} --- $p(y_{t+1}\mid y_{1:t})=\int p(y_{t+1}\mid\theta_{t+1})p(\theta_{t+1}\mid y_{1:t})d\theta_{t+1}$ integrates over the whole filtering distribution.
- \textbf{Decisions under non-quadratic loss} --- absolute loss gives the median, asymmetric loss gives quantiles; all require the full distribution.

\textbf{Defect 2: indexing.}

The statement uses $\theta_i$ for arbitrary $i$. Filtering specifically concerns $\theta_t$ (the \emph{current} state, same $t$ as the latest observation):

- $i=t$: \emph{filtering} (definition).
- $i>t$: \emph{state prediction} ($\pi(\theta_{t+k}\mid y_{1:t})$).
- $i<t$: \emph{smoothing} ($\pi(\theta_i\mid y_{1:t})$).

These are three distinct problems. Lumping them together is a definitional error.

\textbf{Correct statement.} \emph{Filtering recursively computes the conditional distribution} $\pi(\theta_t\mid y_{1:t})$ \emph{of the current state given all data up to time $t$, and updates it as new observations $y_{t+1}$ arrive (via the predict + update steps of the Kalman filter in a DLM, or particle filter in general SSMs).}

</details>

<details class="master-subpart" open>
<summary><span class="tag tag-2plus">≥2 ex</span> (Q2) <em>The Kalman filter provides, recursively, the mean and covariance matrix of the filtering distribution. These two quantities are enough for characterizing the filtering distribution: is that correct?</em>  [Jun 2022 Q2; May 2023 Q6b]</summary>

\textbf{Answer: YES --- but only in a DLM (Gaussian linear SSM).}

\textbf{Why YES in a DLM.} Prove $\theta_t\mid y_{1:t}\sim\mathcal{N}_p(m_t,C_t)$ \emph{by induction on $t$}:

- \emph{Base.} $\theta_0\sim\mathcal{N}_p(m_0,C_0)$ Gaussian by assumption.
- \emph{Inductive step.} Assume $\theta_{t-1}\mid y_{1:t-1}\sim\mathcal{N}_p(m_{t-1},C_{t-1})$.
  - \emph{Predict.} $\theta_t=G_t\theta_{t-1}+w_t$ is an affine transformation of independent Gaussians, hence $\theta_t\mid y_{1:t-1}\sim\mathcal{N}_p(a_t,R_t)$, $a_t=G_t m_{t-1}$, $R_t=G_t C_{t-1}G_t^{\top}+W_t$.
  - \emph{Update.} $(\theta_t,Y_t)\mid y_{1:t-1}$ is jointly Gaussian (linear obs $Y_t=F_t\theta_t+v_t$). Gaussian conditioning on $Y_t=y_t$ preserves Gaussianity, giving $\theta_t\mid y_{1:t}\sim\mathcal{N}_p(m_t,C_t)$ with $m_t=a_t+K_t(y_t-f_t)$, $C_t=R_t-K_t Q_t K_t^{\top}$, $K_t=R_t F_t^{\top}Q_t^{-1}$.

Since the distribution is Gaussian, the pair $(m_t,C_t)$ \emph{fully} characterises it --- all higher moments are functions of these two.

\textbf{Credible intervals follow directly:}

- \emph{Marginal} $(1-\alpha)$ CI for component $j$: $(m_t)_j\pm z_{1-\alpha/2}\sqrt{(C_t)_{jj}}$.
- \emph{Joint} ellipsoid: $\{\theta:(\theta-m_t)^{\top}C_t^{-1}(\theta-m_t)\le \chi^2_{p,1-\alpha}\}$.

\textbf{Why NO in general SSMs.} For non-linear / non-Gaussian state-space models:

- The filtering distribution can be \textbf{multimodal} (e.g.\ tracking under data-association ambiguity), \textbf{skewed} (heavy-tailed observation noise), or \textbf{bounded} (constrained states). Mean+covariance \emph{cannot} encode any of these shapes.
- The KF formulas no longer apply --- one uses \textbf{particle filters} (sequential Monte Carlo) to maintain a weighted-sample approximation of the full filtering distribution, or extended/unscented KF as Gaussian approximations.

\textbf{Caveats even in a DLM.}

- With unknown parameters $\phi$ estimated as $\widehat\phi$, the plug-in $\mathcal{N}(m_t(\widehat\phi),C_t(\widehat\phi))$ \emph{ignores} parameter uncertainty and credible intervals are too narrow. The Bayesian fix is $p(\theta_t\mid y_{1:t})=\int p(\theta_t\mid y_{1:t},\phi)p(\phi\mid y_{1:t})d\phi$ (a mixture of Gaussians).

`## 95% marginal CI on component j`
`m_t[j] + c(-1,1)*qnorm(0.975)*sqrt(C_t[j,j])`

</details>

<details class="master-subpart" open>
<summary><span class="tag tag-2plus">1 ex</span> (Q3) <em>Write the general expression of a DLM for a multivariate time series.</em>  [May 2023 Q6a]</summary>

\textbf{General multivariate DLM} (DLMwR \S 2.3).

\emph{Observation equation:}
\[ Y_t=F_t\theta_t+v_t,\qquad v_t\overset{\text{iid}}{\sim}\mathcal{N}_q(0,V_t). \]

\emph{State (system) equation:}
\[ \theta_t=G_t\theta_{t-1}+w_t,\qquad w_t\overset{\text{iid}}{\sim}\mathcal{N}_p(0,W_t). \]

\emph{Initial condition:} $\theta_0\sim\mathcal{N}_p(m_0,C_0)$.

\emph{Independence:} $\{v_t\}$, $\{w_t\}$, $\theta_0$ mutually independent.

\textbf{Dimensions:}
\[ Y_t\in\mathbb{R}^q,\quad \theta_t\in\mathbb{R}^p,\quad F_t\in\mathbb{R}^{q\times p},\quad G_t\in\mathbb{R}^{p\times p},\quad V_t\in\mathbb{R}^{q\times q},\quad W_t\in\mathbb{R}^{p\times p}. \]

\emph{Key implied conditional-independence structure} (DAG: $\theta_0\to\theta_1\to\cdots\to\theta_T$ with $\theta_s\to Y_s$ at each $s$):

- $Y_t\perp Y_{1:t-1}\mid\theta_t$ (observations conditionally independent given state).
- $\theta_t\perp\theta_{0:t-2}\mid\theta_{t-1}$ (Markov state).

This DAG is what powers \emph{both} the forward Kalman filter \emph{and} the backward RTS smoother.

</details>

\textbf{Linked exams:} \texttt{exam_may_2024_q4}, \texttt{exam_jun_2022_q2}, \texttt{exam_may_2023_q6}.""",
    "images": ['images/master/master_t8a_ai.png']
}

master_exercises_ts["t8b"] = {
    "title": 'Master — KF predict + update derivation with Bayes step',
    "content": r"""\textbf{\textcolor{red}{MASTER --- KF predict + update derivation with Bayes step}}

---

<details class="master-subpart" open>
<summary><span class="tag tag-2plus">≥3 ex</span> (Q1) <em>Define a state-space model / DLM (general or random-walk-plus-noise).</em>  [Jun 2025 Q3a; May 2025 Q5a; May 2021 Q5a]</summary>

Three flavours of model definition appear across the linked exams.

\textbf{(i) General SSM (Jun 2025 Q3a).} A state-space model for $(Y_t)_{t\ge 1}$ consists of a latent Markov state $(\theta_t)_{t\ge 0}$ and observations $(Y_t)_{t\ge 1}$:
\[ \theta_0\sim\pi(\theta_0),\quad \theta_t\mid\theta_{t-1}\sim f(\theta_t\mid\theta_{t-1}),\quad Y_t\mid\theta_t\sim f(y_t\mid\theta_t), \]
with conditional independence: $\theta_t\perp\theta_{0:t-2}\mid\theta_{t-1}$ (Markov state) and $Y_t\perp(Y_{1:t-1},\theta_{0:t-1})\mid\theta_t$ (conditional independence of observations given state).

\textbf{(ii) General DLM (May 2021 Q5a).} The Gaussian-linear special case:
\[ \theta_t=G_t\theta_{t-1}+w_t,\;w_t\overset{\text{iid}}{\sim}\mathcal{N}_p(0,W_t);\qquad Y_t=F_t\theta_t+v_t,\;v_t\overset{\text{iid}}{\sim}\mathcal{N}_q(0,V_t); \]
\[ \theta_0\sim\mathcal{N}_p(m_0,C_0);\qquad \{w_t\},\{v_s\},\theta_0\text{ mutually independent}. \]

\textbf{(iii) Random walk plus noise (May 2025 Q5a; local-level DLM).} Univariate scalar case with $F=G=1$:
\[ Y_t=\theta_t+v_t,\;v_t\overset{\text{iid}}{\sim}\mathcal{N}(0,V);\qquad \theta_t=\theta_{t-1}+w_t,\;w_t\overset{\text{iid}}{\sim}\mathcal{N}(0,W); \]
\[ \theta_0\sim\mathcal{N}(m_0,C_0);\qquad \{v_t\},\{w_t\},\theta_0\text{ mutually independent}. \]

\emph{Hierarchy:} (i) $\supset$ (ii) $\supset$ (iii). The Kalman filter derivation that follows applies to (ii); for (iii) just specialise $F=G=1$.

</details>

<details class="master-subpart" open>
<summary><span class="tag tag-2plus">1 ex</span> (Q2) <em>In state-space models, filtering consists in recursively providing a point estimate of the state $\theta_t$ given $y_{1:t}$. Is this correct?</em>  [Jun 2025 Q3b]</summary>

\textbf{Answer: NO.}

Filtering targets the conditional \emph{distribution}
\[ \pi(\theta_t\mid y_{1:t}), \]
not a point estimate. The conditional mean $\mathbb{E}[\theta_t\mid y_{1:t}]$ is the optimal point estimate \emph{under quadratic loss} (Bayes' theorem), and the conditional median is optimal under absolute loss --- both are summaries of the same underlying distribution.

\emph{Why this matters operationally:}

- Credible intervals require at least the variance (in DLM) or the full distribution (in general SSMs, which can be multi-modal).
- The one-step-ahead predictive $p(y_{t+1}\mid y_{1:t})=\int p(y_{t+1}\mid\theta_{t+1})p(\theta_{t+1}\mid y_{1:t})d\theta_{t+1}$ integrates over the whole filtering distribution.

\textbf{Correct statement.} Filtering recursively delivers $\pi(\theta_t\mid y_{1:t})$. In a DLM this is Gaussian and is summarised by $(m_t,C_t)$; in general SSMs it is approximated by particle filters.

</details>

<details class="master-subpart" open>
<summary><span class="tag tag-2plus">≥3 ex</span> (Q3) <em>Derive the Kalman filter recursion: given $\theta_{t-1}\mid y_{1:t-1}\sim\mathcal{N}(m_{t-1},C_{t-1})$, obtain $\theta_t\mid y_{1:t}\sim\mathcal{N}(m_t,C_t)$. State clearly where Bayes' rule enters.</em>  [Jun 2025 Q3c; May 2025 Q5b; May 2021 Q5b]</summary>

\textbf{Three steps.} Inductive hypothesis: $\theta_{t-1}\mid y_{1:t-1}\sim\mathcal{N}_p(m_{t-1},C_{t-1})$.

\textbf{Step 1 --- Predict the state} (affine + Gaussian closure on state equation).

State equation: $\theta_t=G_t\theta_{t-1}+w_t$ with $w_t\sim\mathcal{N}_p(0,W_t)$ independent of $\theta_{t-1}$ and of $y_{1:t-1}$.

\emph{Mean:} $\mathbb{E}[\theta_t\mid y_{1:t-1}]=G_t\mathbb{E}[\theta_{t-1}\mid y_{1:t-1}]+\mathbb{E}[w_t\mid y_{1:t-1}]=G_t m_{t-1}+0\equiv a_t$.

\emph{Variance:} $\operatorname{Var}(\theta_t\mid y_{1:t-1})=G_t\operatorname{Var}(\theta_{t-1}\mid y_{1:t-1})G_t^{\top}+\operatorname{Var}(w_t\mid y_{1:t-1})=G_t C_{t-1}G_t^{\top}+W_t\equiv R_t$ (cross term vanishes by independence).

\emph{Gaussianity:} $\theta_t$ is an affine combination of two independent Gaussians, so
\[ \boxed{\;\theta_t\mid y_{1:t-1}\sim\mathcal{N}_p(a_t,R_t),\quad a_t=G_t m_{t-1},\;R_t=G_t C_{t-1}G_t^{\top}+W_t.\;} \]

\textbf{Step 2 --- Predict the observation} (affine + Gaussian closure on obs equation).

Observation equation: $Y_t=F_t\theta_t+v_t$ with $v_t\sim\mathcal{N}_q(0,V_t)$ independent of $\theta_t$ and of $y_{1:t-1}$.

\emph{Joint Gaussianity of $(\theta_t,Y_t)\mid y_{1:t-1}$:}
\[ \begin{pmatrix}\theta_t\\Y_t\end{pmatrix}\Big|\,y_{1:t-1}\sim\mathcal{N}_{p+q}\!\left(\begin{pmatrix}a_t\\f_t\end{pmatrix},\;\begin{pmatrix}R_t & R_t F_t^{\top}\\F_t R_t & Q_t\end{pmatrix}\right), \]
with
\[ f_t=F_t a_t,\qquad Q_t=F_t R_t F_t^{\top}+V_t,\qquad \operatorname{Cov}(\theta_t,Y_t\mid y_{1:t-1})=R_t F_t^{\top}. \]
Marginal: $Y_t\mid y_{1:t-1}\sim\mathcal{N}_q(f_t,Q_t)$.

\textbf{Step 3 --- Update via Bayes' rule} (\emph{this is where the new data $y_t$ enters}).

\emph{Bayes' rule:}
\[ p(\theta_t\mid y_{1:t})=p(\theta_t\mid y_{1:t-1},y_t)\propto p(y_t\mid\theta_t,y_{1:t-1})\,p(\theta_t\mid y_{1:t-1})=p(y_t\mid\theta_t)\,p(\theta_t\mid y_{1:t-1}), \]
using the conditional-independence assumption $Y_t\perp Y_{1:t-1}\mid\theta_t$.

\emph{Gaussian $\times$ Gaussian = Gaussian.} Equivalently, condition the joint Gaussian from Step 2 on $Y_t=y_t$ using the standard formula for Normal conditioning $(X_1\mid X_2=x_2)\sim\mathcal{N}(\mu_1+\Sigma_{12}\Sigma_{22}^{-1}(x_2-\mu_2),\Sigma_{11}-\Sigma_{12}\Sigma_{22}^{-1}\Sigma_{21})$. With $X_1=\theta_t$, $X_2=Y_t$:
\[ \boxed{\;\theta_t\mid y_{1:t}\sim\mathcal{N}_p(m_t,C_t),\quad m_t=a_t+K_t(y_t-f_t),\;C_t=R_t-K_t Q_t K_t^{\top},\;K_t=R_t F_t^{\top}Q_t^{-1}.\;} \]

\textbf{Reading the update.}

- $K_t=R_t F_t^{\top}Q_t^{-1}$ = \textbf{Kalman gain}: regression coefficient of $\theta_t$ on $Y_t$ conditional on $y_{1:t-1}$.
- $y_t-f_t$ = \textbf{innovation}: surprise carried by the new observation.
- $m_t=a_t+K_t(y_t-f_t)$: prior mean corrected by gain $\times$ innovation.
- $C_t=R_t-K_t Q_t K_t^{\top}\preceq R_t$: observing $y_t$ \emph{reduces} posterior variance (information never hurts).

\textbf{Scalar local-level specialization (May 2025 Q5).} With $F=G=1$, $V_t=V$, $W_t=W$:
\[ a_t=m_{t-1},\;R_t=C_{t-1}+W,\;f_t=a_t,\;Q_t=R_t+V,\;K_t=\frac{R_t}{Q_t}\in(0,1), \]
\[ m_t=a_t+K_t(y_t-f_t)=(1-K_t)m_{t-1}+K_t y_t,\quad C_t=(1-K_t)R_t=\frac{V(C_{t-1}+W)}{C_{t-1}+W+V}. \]

\emph{R:}

</details>

<details class="master-subpart" open>
<summary><span class="tag tag-2plus">1 ex</span> (Q4) <em>Prove the first step (state prediction): $\theta_t\mid y_{1:t-1}\sim\mathcal{N}_p(a_t,R_t)$.</em>  [May 2021 Q5c]</summary>

\textbf{Proof of Step 1.}

\emph{Setup.} Inductive hypothesis: $\theta_{t-1}\mid y_{1:t-1}\sim\mathcal{N}_p(m_{t-1},C_{t-1})$. State equation: $\theta_t=G_t\theta_{t-1}+w_t$, with $w_t\sim\mathcal{N}_p(0,W_t)$ and $w_t\perp\sigma(\theta_{0:t-1},Y_{1:t-1})$ (model assumption).

\textbf{Conditional mean.} By linearity of conditional expectation,
\[ \mathbb{E}[\theta_t\mid y_{1:t-1}]=G_t\,\mathbb{E}[\theta_{t-1}\mid y_{1:t-1}]+\mathbb{E}[w_t\mid y_{1:t-1}]=G_t m_{t-1}+0=a_t, \]
using $\mathbb{E}[w_t\mid y_{1:t-1}]=\mathbb{E}[w_t]=0$ by independence of $w_t$ from $y_{1:t-1}$.

\textbf{Conditional variance.} Independence kills cross terms:
\begin{align*}
\operatorname{Var}(\theta_t\mid y_{1:t-1}) &= \operatorname{Var}(G_t\theta_{t-1}+w_t\mid y_{1:t-1})\\
&= G_t\operatorname{Var}(\theta_{t-1}\mid y_{1:t-1})G_t^{\top}+\operatorname{Var}(w_t\mid y_{1:t-1})+\underbrace{2G_t\operatorname{Cov}(\theta_{t-1},w_t\mid y_{1:t-1})}_{=0}\\
&= G_t C_{t-1}G_t^{\top}+W_t=R_t.
\end{align*}

\textbf{Gaussianity.} The pair $(\theta_{t-1},w_t)\mid y_{1:t-1}$ is jointly Gaussian (independent Gaussian components conditional on $y_{1:t-1}$). $\theta_t=G_t\theta_{t-1}+w_t$ is an affine transformation, and affine transformations of Gaussians are Gaussian. Therefore
\[ \theta_t\mid y_{1:t-1}\sim\mathcal{N}_p(a_t,R_t). \qquad\Box \]

\textbf{Initial step of induction ($t=1$).} $\theta_1\mid y_{1:0}=\theta_1\sim\mathcal{N}_p(G_1 m_0,G_1 C_0 G_1^{\top}+W_1)=\mathcal{N}_p(a_1,R_1)$, directly from $\theta_0\sim\mathcal{N}_p(m_0,C_0)$ and the state equation.

</details>

<details class="master-subpart" open>
<summary><span class="tag tag-2plus">1 ex</span> (Q5) <em>Does the variance $C_t$ converge to zero as $t\to\infty$?</em>  [May 2025 Q5c]</summary>

\textbf{Answer: NO (provided $W>0$).}

\textbf{Setup.} In the scalar local-level DLM,
\[ C_t=\frac{V(C_{t-1}+W)}{C_{t-1}+W+V}\equiv\Phi(C_{t-1}). \]
This is a Riccati-type recursion. $\Phi$ is increasing on $[0,\infty)$ with $\Phi(0)=VW/(W+V)>0$ and $\Phi(\infty)=V$; it is a contraction in a neighbourhood of its fixed point.

\textbf{Fixed point.} At equilibrium $C^*=\Phi(C^*)$:
\[ C^*(C^*+W+V)=V(C^*+W)\;\Longrightarrow\; C^{*2}+WC^*-VW=0\;\Longrightarrow\; C^*=\tfrac12\Big(-W+\sqrt{W^2+4VW}\Big)>0. \]

So $C_t\to C^*>0$, \emph{not} to zero.

\textbf{Intuition.} Fresh state noise $w_t\sim\mathcal{N}(0,W)$ is injected \emph{every} step. Even after infinite past data, the current state $\theta_t=\theta_{t-1}+w_t$ has irreducible uncertainty from the most recent innovation $w_t$. The filter cannot reduce $C_t$ below the noise it inherits.

\textbf{Degenerate case $W=0$.} Then $\theta_t=\theta_{t-1}=\theta_0$ is static; the recursion becomes
\[ C_t=\frac{V C_{t-1}}{C_{t-1}+V}=\frac{V C_0}{V+t C_0}\sim\frac{V}{t}\to 0, \]
at rate $1/t$ --- the standard "sample mean shrinks variance" result.

\textbf{Take-away.} The asymptotic variance $C^*$ is the \emph{steady-state} filter precision and a key quantity for designing alarm levels, control limits, and exploration policies in adaptive filtering.

\emph{R (iterate the Riccati recursion to the limit):}

`C                          ## ~ 0.5*(-W + sqrt(W^2+4*V*W))`
`0.5*(-W + sqrt(W^2+4*V*W)) ## closed-form C*`

</details>

\textbf{Linked exams:} \texttt{exam_jun_2025_q3}, \texttt{exam_may_2025_q5}, \texttt{exam_may_2021_q5}.""",
    "images": ['images/master/master_t8b_ai.png']
}

master_exercises_ts["t9a"] = {
    "title": 'Master — Filtering vs smoothing — definitions & DAG-based proofs',
    "content": r"""\textbf{\textcolor{red}{MASTER --- Filtering vs smoothing — definitions \& DAG-based proofs}}

---

<details class="master-subpart" open>
<summary><span class="tag tag-2plus">≥2 ex</span> (Q1) <em>What is the difference between filtering and smoothing? Give precise definitions (not expressions).</em>  [Sep 2025 Q7b; May 2022 Q7]</summary>

\textbf{Filtering distribution.}
\[ \pi(\theta_t\mid y_{1:t}) \]
The conditional distribution of the \emph{current} state $\theta_t$ given the data \emph{up to now}. \emph{Online}: computed recursively as each new observation arrives (Kalman filter forward pass in a DLM).

\textbf{Joint smoothing distribution.}
\[ \pi(\theta_{0:T}\mid y_{1:T}) \]
The conditional distribution of the \emph{entire} latent trajectory given \emph{all} the data. Captures \emph{cross-time} dependence between past states.

\textbf{Marginal smoothing distribution.}
\[ \pi(\theta_t\mid y_{1:T}),\qquad t<T \]
The conditional distribution of a \emph{past} state given \emph{all} the data, \emph{including observations after $t$}. \emph{Offline}: requires the full data $y_{1:T}$ to be available.

\textbf{Why smoothing is more informative.} Smoothing conditions on a \emph{superset} of the data ($y_{1:T}\supseteq y_{1:t}$), so by the conditional-variance reduction property:
\[ \operatorname{Var}(\theta_t\mid y_{1:T})\preceq\operatorname{Var}(\theta_t\mid y_{1:t}). \]
Future observations $y_{t+1},\dots,y_T$ carry information about $\theta_t$ through the chain $\theta_t\to\theta_{t+1}\to\cdots\to\theta_T$ and $\theta_s\to Y_s$ for $s>t$. The smoother propagates this future info back to time $t$.

\textbf{Algorithms in a DLM.}

- \emph{Filtering} = forward Kalman filter (predict + update).
- \emph{Smoothing} = forward KF + backward Rauch--Tung--Striebel sweep.

\textbf{Use cases.}

- \emph{Filtering}: real-time tracking, online control, sequential decision-making.
- \emph{Smoothing}: retrospective analysis, scientific reconstruction of latent processes, EM-algorithm E-step for parameter estimation.

</details>

<details class="master-subpart" open>
<summary><span class="tag tag-2plus">1 ex</span> (Q2) <em>The problem of smoothing in a state-space model with observation process $(Y_t)_{t\ge 1}$ and state process $(\theta_t)_{t\ge 0}$ consists in providing the point estimates $\mathbb{E}(\theta_t\mid y_{1:T})$ for $t\le T$. Is that correct?</em>  [Jun 2022 Q3]</summary>

\textbf{Answer: NO} --- same conceptual mistake as the analogous filtering question.

Smoothing targets a \emph{distribution}, not a point estimate. Specifically:

- \emph{Marginal smoothing:} $\pi(\theta_t\mid y_{1:T})$ for each $t\le T$.
- \emph{Joint smoothing:} $\pi(\theta_{0:T}\mid y_{1:T})$ (richer; encodes cross-time correlations).

The conditional mean $\mathbb{E}[\theta_t\mid y_{1:T}]$ is just one summary. To quantify uncertainty (credible intervals, scientific reconstructions, decision-making under loss) we need the full distribution. In a DLM the marginal smoothing distribution is Gaussian, $\mathcal{N}(s_t,S_t)$, and the RTS smoother returns both $s_t$ and $S_t$ --- not just $s_t$.

\textbf{Correct statement.} Smoothing recursively computes the marginal (or joint) smoothing \emph{distribution} $\pi(\theta_t\mid y_{1:T})$ for each $t\le T$. In a DLM this is $\mathcal{N}(s_t,S_t)$ from the RTS backward sweep.

</details>

<details class="master-subpart" open>
<summary><span class="tag tag-2plus">1 ex</span> (Q3) <em>Using the DAG representation of the conditional-independence structure of a DLM, prove $\theta_t\perp\!\!\!\perp(Y_{t+1},\dots,Y_T)\mid\theta_{t+1}$.</em>  [May 2023 Q5]</summary>

\textbf{Proof via $d$-separation.}

\textbf{The DAG.} A DLM is fully described by:
\[ \theta_0\to\theta_1\to\theta_2\to\cdots\to\theta_T\quad\text{(state Markov chain)} \]
with at each node $s$ an arrow $\theta_s\to Y_s$ (observation given state). There are no other edges (this \emph{is} the defining conditional-independence structure of a DLM).

\textbf{Identifying the paths.} Consider any directed path from $\theta_t$ to $Y_s$ for $s\ge t+1$. The only way to reach $Y_s$ is via the chain
\[ \theta_t\to\theta_{t+1}\to\theta_{t+2}\to\cdots\to\theta_s\to Y_s. \]
\emph{Every} such path passes through $\theta_{t+1}$ at a \textbf{serial} (chain) node $\theta_t\to\theta_{t+1}\to\theta_{t+2}$ at the second position.

\textbf{Blocking by conditioning.} At a serial node, conditioning on the middle node \emph{blocks} the path. Conditioning on $\theta_{t+1}$ therefore blocks \emph{every} directed path from $\theta_t$ to any $Y_s$ with $s\ge t+1$.

\textbf{Conclusion.} By the $d$-separation criterion (Geiger--Verma--Pearl),
\[ \theta_t\perp\!\!\!\perp(Y_{t+1},Y_{t+2},\dots,Y_T)\mid\theta_{t+1}. \qquad\Box \]

\textbf{Why this matters (RTS smoother).} This is exactly the \emph{backward Markov property} used in the Rauch--Tung--Striebel smoother:
\[ p(\theta_t\mid\theta_{t+1},y_{1:T})=p(\theta_t\mid\theta_{t+1},y_{1:t}), \]
which lets us pre-compute the backward kernel $p(\theta_t\mid\theta_{t+1},y_{1:t})$ during the forward filtering pass (since it depends only on filter quantities) and then sweep backward marginalising against $p(\theta_{t+1}\mid y_{1:T})$.

\emph{Equivalently:} once you know $\theta_{t+1}$, future observations $y_{t+1:T}$ add no information about $\theta_t$ --- because all the information they carry about $\theta_t$ flows \emph{through} $\theta_{t+1}$.

</details>

<details class="master-subpart" open>
<summary><span class="tag tag-2plus">1 ex</span> (Q4) <em>What are the recursion steps of the Kalman (RTS) smoother for obtaining the marginal smoothing distribution? Provide hints of the proof.</em>  [Sep 2025 Q7c]</summary>

\textbf{RTS (Rauch--Tung--Striebel) smoother} (DLMwR Prop.\ 2.4, p.\ 61).

\textbf{Setup.} Run the Kalman filter forward over $t=1,\dots,T$, storing for each $t$:
\[ m_t,\;C_t\;(\text{filter means/covs}),\qquad a_{t+1}=G_{t+1}m_t,\;R_{t+1}=G_{t+1}C_t G_{t+1}^{\top}+W_{t+1}\;(\text{predictives}). \]

\textbf{Backward recursion.} Initialise at $t=T$: $s_T=m_T$, $S_T=C_T$. For $t=T-1,T-2,\dots,0$, compute the \textbf{smoother gain}
\[ J_t=C_t G_{t+1}^{\top}R_{t+1}^{-1} \]
and update:
\[ \boxed{\;s_t=m_t+J_t(s_{t+1}-a_{t+1}),\quad S_t=C_t-J_t(R_{t+1}-S_{t+1})J_t^{\top}.\;} \]
Then $\theta_t\mid y_{1:T}\sim\mathcal{N}_p(s_t,S_t)$ is the marginal smoothing distribution.

\textbf{Proof hint.}

\emph{Step 1 --- backward Markov.} By the DAG argument of Q3,
\[ p(\theta_t\mid\theta_{t+1},y_{1:T})=p(\theta_t\mid\theta_{t+1},y_{1:t}). \]

\emph{Step 2 --- Gaussian conditioning.} The joint $(\theta_t,\theta_{t+1})\mid y_{1:t}$ is Gaussian with means $(m_t,a_{t+1})$, covariances $(C_t,R_{t+1})$, and cross-covariance $C_t G_{t+1}^{\top}$ (from $\theta_{t+1}=G_{t+1}\theta_t+w_{t+1}$). Standard Gaussian conditioning gives
\[ \theta_t\mid\theta_{t+1},y_{1:t}\sim\mathcal{N}_p\big(m_t+J_t(\theta_{t+1}-a_{t+1}),\;C_t-J_t R_{t+1}J_t^{\top}\big),\qquad J_t=C_t G_{t+1}^{\top}R_{t+1}^{-1}. \]

\emph{Step 3 --- marginalise against the smoothed $\theta_{t+1}$.} Combine with $\theta_{t+1}\mid y_{1:T}\sim\mathcal{N}(s_{t+1},S_{t+1})$:
\[ \theta_t\mid y_{1:T}=\mathbb{E}[\theta_t\mid\theta_{t+1},y_{1:T}]+\text{noise}=m_t+J_t(s_{t+1}-a_{t+1})+\text{noise}. \]
Tower property + variance-decomposition gives the formulae above.

\emph{R:}

</details>

<details class="master-subpart" open>
<summary><span class="tag tag-2plus">1 ex</span> (Q5) <em>Based on data $y_1,\dots,y_t$, write the expression of the likelihood of $\phi$.</em>  [Sep 2025 Q7a]</summary>

\textbf{Prediction-error decomposition} (DLMwR \S 4.1, eq.\ 4.1, p.\ 144).

\textbf{Key chain-rule identity.} For any joint density,
\[ p(y_{1:T}\mid\phi)=\prod_{t=1}^T p(y_t\mid y_{1:t-1},\phi). \]
The Kalman filter (run at $\phi$) provides each factor in closed form:
\[ Y_t\mid y_{1:t-1},\phi\sim\mathcal{N}_q\big(f_t(\phi),\,Q_t(\phi)\big). \]

\textbf{Likelihood:}
\[ \boxed{\;L(\phi\mid y_{1:T})=\prod_{t=1}^T\mathcal{N}_q\big(y_t;\,f_t(\phi),Q_t(\phi)\big).\;} \]

\textbf{Log-likelihood:}
\[ \ell(\phi)=-\tfrac12\sum_{t=1}^T\big[q\log(2\pi)+\log|Q_t(\phi)|+e_t(\phi)^{\top}Q_t(\phi)^{-1}e_t(\phi)\big], \]
where $e_t=y_t-f_t$ are the innovations. Numerical maximisation by BFGS or EM; each evaluation = one Kalman-filter pass.

\emph{R:}

</details>

<details class="master-subpart" open>
<summary><span class="tag tag-2plus">1 ex</span> (Q6) <em>Define a general DLM $((Y_t,\theta_t))_{t\ge 1}$ starting at $\theta_0\sim\mathcal{N}_p(m_0,C_0)$, where $Y_t$ is $m$-dimensional and $\theta_t$ is $p$-dimensional.</em>  [May 2022 Q7]</summary>

\textbf{General DLM.}
\[ Y_t=F_t\theta_t+v_t,\qquad v_t\overset{\text{iid}}{\sim}\mathcal{N}_m(0,V_t), \]
\[ \theta_t=G_t\theta_{t-1}+w_t,\qquad w_t\overset{\text{iid}}{\sim}\mathcal{N}_p(0,W_t), \]
\[ \theta_0\sim\mathcal{N}_p(m_0,C_0),\qquad \{v_t\},\{w_t\},\theta_0\text{ mutually independent.} \]

Dimensions: $Y_t\in\mathbb{R}^m$, $\theta_t\in\mathbb{R}^p$, $F_t\in\mathbb{R}^{m\times p}$, $G_t\in\mathbb{R}^{p\times p}$, $V_t\in\mathbb{R}^{m\times m}$, $W_t\in\mathbb{R}^{p\times p}$.

The DAG and the Markov / observation-conditional-independence properties used in Q1--Q4 follow from this definition.

</details>

\textbf{Linked exams:} \texttt{exam_sep_2025_q7}, \texttt{exam_jun_2022_q3}, \texttt{exam_may_2023_q5}, \texttt{exam_may_2022_q7}.""",
    "images": ['images/master/master_t9a_ai.png']
}

master_exercises_ts["t10a"] = {
    "title": 'Master — Predictive distribution N(f_t,Q_t) — derivation',
    "content": r"""\textbf{\textcolor{red}{MASTER --- Predictive distribution $\mathcal{N}(f_t,Q_t)$ — derivation}}

---

<details class="master-subpart" open>
<summary><span class="tag tag-2plus">1 ex</span> (Q1) <em>Dynamic linear models (DLMs) can be only defined for univariate time series. Is that correct?</em>  [Jun 2024 Q6a]</summary>

\textbf{Answer: NO.}

A DLM is defined in the \emph{general multivariate} setting:
\[ Y_t=F_t\theta_t+v_t,\quad v_t\sim\mathcal{N}_q(0,V_t),\qquad \theta_t=G_t\theta_{t-1}+w_t,\quad w_t\sim\mathcal{N}_p(0,W_t), \]
with $\theta_0\sim\mathcal{N}_p(m_0,C_0)$ and the usual mutual-independence assumption. The dimensions are:

- $Y_t\in\mathbb{R}^q$ --- observation can be \emph{any} dimension $q\ge 1$;
- $\theta_t\in\mathbb{R}^p$ --- state of \emph{any} dimension $p\ge 1$;
- $F_t\in\mathbb{R}^{q\times p}$ --- observation matrix;
- $G_t\in\mathbb{R}^{p\times p}$ --- state-transition matrix;
- $V_t\in\mathbb{R}^{q\times q}$, $W_t\in\mathbb{R}^{p\times p}$ --- noise covariance matrices.

The univariate case ($q=1$) is just a special case. The Kalman filter/smoother derivation only uses Gaussianity + linearity + the conditional-independence DAG, none of which require $q=1$.

\emph{Examples of genuinely multivariate DLMs:}

- \emph{Portfolio of $m$ asset prices}, each as random-walk-plus-noise (DLMwR \S 9): $q=m$, $\theta_t=$ latent fair-value vector, $F=I_m$, $G=I_m$.
- \emph{Seemingly unrelated time series equations} (SUTSE).
- \emph{VARMA-as-DLM}, with companion-form state of dimension $p>q$.

</details>

<details class="master-subpart" open>
<summary><span class="tag tag-2plus">1 ex</span> (Q2) <em>Now consider a DLM with state process $(\theta_t)_{t\ge 0}$. Given data $y_{1:t-1}$, we know that $\theta_t\mid y_{1:t-1}\sim\mathcal{N}(a_t,R_t)$; explain how one obtains the one-step-ahead predictive distribution of $y_t$ given $y_{1:t-1}$.</em>  [Jun 2024 Q6b]</summary>

\textbf{Goal.} Obtain $p(y_t\mid y_{1:t-1})$ from the state predictive $\theta_t\mid y_{1:t-1}\sim\mathcal{N}_p(a_t,R_t)$ and the observation equation $Y_t=F_t\theta_t+v_t$.

\textbf{Step 1: identify the ingredients.}

- $\theta_t\mid y_{1:t-1}\sim\mathcal{N}_p(a_t,R_t)$ (given).
- $v_t\sim\mathcal{N}_q(0,V_t)$, independent of $\theta_t$ and of $y_{1:t-1}$ (DLM assumption).
- Observation equation: $Y_t=F_t\theta_t+v_t$ --- \emph{affine} in $\theta_t$.

\textbf{Step 2: Gaussianity by affine + Gaussian closure.}

$Y_t\mid y_{1:t-1}$ is a linear combination of two independent Gaussians ($\theta_t\mid y_{1:t-1}$ and $v_t$), hence Gaussian.

\textbf{Step 3: compute mean.}
\begin{align*}
f_t &= \mathbb{E}[Y_t\mid y_{1:t-1}]\\
&= F_t\,\mathbb{E}[\theta_t\mid y_{1:t-1}]+\mathbb{E}[v_t\mid y_{1:t-1}]\\
&= F_t a_t+0=F_t a_t,
\end{align*}
using independence of $v_t$ and $y_{1:t-1}$ ($\mathbb{E}[v_t\mid y_{1:t-1}]=\mathbb{E}[v_t]=0$).

\textbf{Step 4: compute variance.}
\begin{align*}
Q_t &= \operatorname{Var}(Y_t\mid y_{1:t-1})\\
&= \operatorname{Var}(F_t\theta_t+v_t\mid y_{1:t-1})\\
&= F_t\operatorname{Var}(\theta_t\mid y_{1:t-1})F_t^{\top}+\operatorname{Var}(v_t\mid y_{1:t-1})+\underbrace{2F_t\operatorname{Cov}(\theta_t,v_t\mid y_{1:t-1})}_{=0}\\
&= F_t R_t F_t^{\top}+V_t,
\end{align*}
the cross term vanishing by independence of $v_t$ and $\theta_t$.

\textbf{Step 5: assemble.}
\[ \boxed{\;Y_t\mid y_{1:t-1}\sim\mathcal{N}_q(f_t,Q_t),\quad f_t=F_t a_t,\quad Q_t=F_t R_t F_t^{\top}+V_t.\;} \]

\textbf{Reading the result.}

- $f_t$ = optimal point forecast under quadratic loss (Bayes estimator = conditional mean of a symmetric distribution = also conditional median).
- $Q_t$ = forecast variance, decomposed into:
  - $F_t R_t F_t^{\top}$ = \emph{state uncertainty} propagated through the observation matrix;
  - $V_t$ = \emph{irreducible observation noise}.
- $(1-\alpha)$ predictive credible interval (scalar case): $f_t\pm z_{1-\alpha/2}\sqrt{Q_t}$.

\textbf{Use cases.}

- \emph{Forecasting:} report $f_t$ with interval $f_t\pm z_{1-\alpha/2}\sqrt{Q_t}$.
- \emph{Likelihood (prediction-error decomposition):} $L(\phi)=\prod_t\mathcal{N}_q(y_t;f_t,Q_t)$ --- used for MLE of unknown DLM parameters.
- \emph{Model checking:} standardised innovations $\tilde e_t=Q_t^{-1/2}(y_t-f_t)$ should be i.i.d.\ $\mathcal{N}(0,I_q)$ under correct specification (QQ-plot, Ljung--Box test).

`              kf$mod$FF %*% dlmSvd2var(kf$U.R[[i]], kf$D.R[i,]) %*% t(kf$mod$FF)`
`              + kf$mod$V)`
`## 95% predictive CI`

</details>

\textbf{Linked exams:} \texttt{exam_jun_2024_q6}.""",
    "images": ['images/master/master_t10a_ai.png']
}

master_exercises_ts["t10b"] = {
    "title": 'Master — Forecast function, k-step intervals, SES & loss functions',
    "content": r"""\textbf{\textcolor{red}{MASTER --- Forecast function, k-step intervals, SES & loss functions}}

---

<details class="master-subpart" open>
<summary><span class="tag tag-2plus">≥2 ex</span> (Q1) <em>Consider a random walk plus noise model (write it precisely, but without unnecessary details).</em>  [Jun 2024 Q5, Jun 2022 Q4]</summary>

\textbf{Random walk plus noise (local-level DLM).}
\[
Y_t=\theta_t+v_t,\qquad v_t\overset{\text{iid}}{\sim}\mathcal{N}(0,V),
\]
\[
\theta_t=\theta_{t-1}+w_t,\qquad w_t\overset{\text{iid}}{\sim}\mathcal{N}(0,W),
\]
\[
\theta_0\sim\mathcal{N}(m_0,C_0),\qquad \{v_t\},\{w_t\},\theta_0\text{ mutually independent}.
\]

This is the DLM with $F_t=G_t=1$, $V_t=V$, $W_t=W$ --- the simplest non-stationary structural model.

</details>

<details class="master-subpart" open>
<summary><span class="tag tag-2plus">1 ex</span> (Q2) <em>What is the signal-to-noise ratio?</em>  [Jun 2024 Q5]</summary>

\textbf{Definition.} The \emph{signal-to-noise ratio} (SNR) of the local-level model is
\[
\boxed{\;\kappa=\dfrac{W}{V}.\;}
\]

\emph{Interpretation.}
- $\kappa\to 0$ (low signal, high noise) $\Rightarrow$ $\theta_t$ barely moves; forecasts are nearly the running mean and SES uses a tiny smoothing constant.
- $\kappa\to\infty$ (high signal) $\Rightarrow$ the latent level wanders a lot; forecasts track recent observations.

\emph{Link to SES discount $\alpha$.} At steady state the Kalman gain solves $K^2+\kappa K-\kappa=0$, $K=\alpha\in(0,1)$, with smaller $\kappa$ giving smaller $\alpha$ (more smoothing).

</details>

<details class="master-subpart" open>
<summary><span class="tag tag-2plus">1 ex</span> (Q3) <em>The one-step-ahead predictive distribution of $Y_t$, given the observations $y_{1:t-1}$, is $\mathcal{N}(f_t,Q_t)$. Explain how you obtain the expression of $f_t$ and $Q_t$.</em>  [Jun 2022 Q4]</summary>

\emph{Two-step Kalman predict.}

\textbf{Step 1 (state prediction).} From the state equation $\theta_t=\theta_{t-1}+w_t$ with $w_t\perp y_{1:t-1}$ and inductive $\theta_{t-1}\mid y_{1:t-1}\sim\mathcal{N}(m_{t-1},C_{t-1})$,
\[
\theta_t\mid y_{1:t-1}\sim\mathcal{N}(a_t,R_t),\qquad a_t=m_{t-1},\;R_t=C_{t-1}+W.
\]

\textbf{Step 2 (obs prediction).} From $Y_t=\theta_t+v_t$ with $v_t\perp(\theta_t,y_{1:t-1})$ and Gaussian-Gaussian closure,
\[
\boxed{\;f_t=\mathbb{E}[Y_t\mid y_{1:t-1}]=a_t=m_{t-1},\qquad Q_t=\operatorname{Var}(Y_t\mid y_{1:t-1})=R_t+V=C_{t-1}+W+V.\;}
\]

So $Y_t\mid y_{1:t-1}\sim\mathcal{N}(m_{t-1},\,C_{t-1}+W+V)$.

\emph{R sanity check:}

</details>

<details class="master-subpart" open>
<summary><span class="tag tag-2plus">1 ex</span> (Q4) <em>Given data $y_{1:t}$, what is the forecast function? Write it, and then provide the expression of the $(1-\alpha)$ credible interval for $Y_{t+2}\mid y_1,\dots,y_t$.</em>  [Jun 2024 Q5]</summary>

\emph{Iterate the state forward $k$ steps.} Conditional on $y_{1:t}$, $\theta_t\sim\mathcal{N}(m_t,C_t)$. Then
\[
\theta_{t+k}=\theta_t+\sum_{j=1}^{k}w_{t+j},\qquad Y_{t+k}=\theta_{t+k}+v_{t+k},
\]
with $w_{t+j},v_{t+k}$ independent of $y_{1:t}$ and of each other. Combining variances:
\[
\boxed{\;Y_{t+k}\mid y_{1:t}\sim\mathcal{N}\!\bigl(m_t,\;C_t+kW+V\bigr).\;}
\]

\emph{Forecast function} (point forecast):
\[
\hat y_{t+k\mid t}=m_t\quad\text{for every }k\ge 1\quad\text{(flat / horizontal — local-level character).}
\]

\emph{$(1-\alpha)$ credible interval for $Y_{t+2}$:}
\[
\boxed{\;m_t\;\pm\;z_{1-\alpha/2}\sqrt{C_t+2W+V}.\;}
\]

Note how the variance grows linearly in $k$: $W$ accumulates over the $k$ random-walk steps, while $V$ enters once (observation noise at $t+k$). Intervals fan out at rate $\sqrt{kW}$.

`fc$f[2]; sqrt(fc$Q[[2]])              ## point + sd for Y_{t+2}`
`fc$f[2] + c(-1,1)*qnorm(1-alpha/2)*sqrt(fc$Q[[2]])`

</details>

<details class="master-subpart" open>
<summary><span class="tag tag-2plus">1 ex</span> (Q5) <em>What is the point forecast of $Y_t$ given $y_{1:t-1}$ with respect to: (i) Quadratic loss? (ii) Absolute loss $L(y_t,\hat y_t)=|y_t-\hat y_t|$?</em>  [Jun 2022 Q4]</summary>

\textbf{Bayes-estimator principle.} Under a loss $L$, the optimal point forecast minimises $\mathbb{E}[L(Y_t,\hat y_t)\mid y_{1:t-1}]$ over $\hat y_t$.

\textbf{(i) Quadratic loss} $L=(Y_t-\hat y_t)^2$. The minimiser is the conditional mean:
\[
\hat y_t^{\,\mathrm{quad}}=\mathbb{E}[Y_t\mid y_{1:t-1}]=f_t=m_{t-1}.
\]

\textbf{(ii) Absolute loss} $L=|Y_t-\hat y_t|$. The minimiser is the conditional median:
\[
\hat y_t^{\,\mathrm{abs}}=\mathrm{median}(Y_t\mid y_{1:t-1}).
\]

\emph{Gaussian symmetry.} Here $Y_t\mid y_{1:t-1}\sim\mathcal{N}(f_t,Q_t)$ is symmetric, so mean $=$ median:
\[
\boxed{\;\hat y_t^{\,\mathrm{quad}}=\hat y_t^{\,\mathrm{abs}}=f_t=m_{t-1}.\;}
\]

\emph{Caveat.} The two estimators would differ in non-Gaussian / asymmetric predictives (e.g.\ a Poisson HMM forecast, or a heavy-tailed innovation). Quadratic loss is sensitive to outliers; absolute loss is more robust.

</details>

<details class="master-subpart" open>
<summary><span class="tag tag-2plus">1 ex</span> (Q6) <em>Simple exponential smoothing is a simple and clever algorithm that recursively provides one-step-ahead point forecasts $\hat y_{t+1\mid t}$. Does it provide any quantification of the uncertainty about the point forecasts?</em>  [May 2022 Q1]</summary>

\textbf{NO}, as a stand-alone recursion. SES is defined purely as
\[
\hat y_{t+1\mid t}=\alpha y_t+(1-\alpha)\hat y_{t\mid t-1},\qquad \alpha\in(0,1),
\]
which delivers a \emph{point} forecast --- there is no variance, no interval, no distribution attached.

\textbf{However --- SES = steady-state KF of the local-level DLM.} As $t\to\infty$ the KF gain in the random-walk-plus-noise model converges to a constant $\alpha$, and the filter mean update becomes exactly the SES recursion with discount $\alpha=K_\infty$. Embedding SES in that DLM \emph{does} produce intervals:
\[
Y_{t+1}\mid y_{1:t}\sim\mathcal{N}\!\bigl(\hat y_{t+1\mid t},\;Q_{t+1}\bigr),\qquad Q_{t+1}=C_t+W+V.
\]

\emph{Bottom line.} The DLM \emph{has} the intervals; the SES algorithm by itself does not. To get prediction intervals from SES you must specify the underlying DLM (i.e.\ choose $V,W$) and report $Q_{t+k}$.

\emph{R:}

</details>

\textbf{Linked exams:} \texttt{exam_jun_2024_q5}, \texttt{exam_jun_2022_q4}, \texttt{exam_may_2022_q1}.""",
    "images": ['images/master/master_t10b_ai.png']
}

master_exercises_ts["t11a"] = {
    "title": 'Master — Innovations: zero-mean, orthogonality, standardisation',
    "content": r"""\textbf{\textcolor{red}{MASTER --- Innovations: zero-mean, orthogonality, standardisation}}

---

<details class="master-subpart" open>
<summary><span class="tag tag-2plus">≥2 ex</span> (Q1) <em>Consider the forecast errors $e_t=Y_t-f_t$, $f_t=\mathbb{E}(Y_t\mid Y_{t-1})$. Show that $\mathbb{E}(e_t)=0$.</em>  [Jun 2025 Q5, May 2021 Q6]</summary>

\emph{Read $f_t=\mathbb{E}(Y_t\mid Y_{1:t-1})$} (the standard one-step-ahead predictive mean from the KF; the prompt's $Y_{t-1}$ is shorthand for the past). The result has \textbf{two clean proofs}.

\textbf{Proof 1 --- tower property.} By the tower / iterated expectation rule:
\[
\mathbb{E}[e_t]=\mathbb{E}[Y_t]-\mathbb{E}\bigl[\mathbb{E}(Y_t\mid Y_{1:t-1})\bigr]=\mathbb{E}[Y_t]-\mathbb{E}[Y_t]=0.
\]

\textbf{Proof 2 --- via conditional expectation.} For every $t$,
\[
\mathbb{E}[e_t\mid\mathcal F_{t-1}]=\mathbb{E}[Y_t\mid\mathcal F_{t-1}]-f_t=f_t-f_t=0,
\]
so $\mathbb{E}[e_t]=\mathbb{E}[\mathbb{E}[e_t\mid\mathcal F_{t-1}]]=0$.

\emph{Why this matters.} Zero-mean forecast errors are the foundation of likelihood-based DLM estimation via the \emph{prediction-error decomposition}: with $e_t\sim\mathcal{N}(0,Q_t)$ independently,
\[
\ell(\phi)=-\tfrac12\sum_{t=1}^{n}\bigl[\log|Q_t(\phi)|+e_t(\phi)^{\top}Q_t(\phi)^{-1}e_t(\phi)\bigr]+\text{const}.
\]

\emph{R diagnostic check:}

`mean(e)                          ## should be ~ 0`

</details>

<details class="master-subpart" open>
<summary><span class="tag tag-2plus">1 ex</span> (Q2) <em>Show that for any $t>s$, $\operatorname{Cov}(e_t,e_s)=0$.</em>  [May 2021 Q6]</summary>

\emph{Orthogonality of innovations} (proven via tower + pull-out).

For $t>s$, $e_s=Y_s-f_s$ is $\mathcal F_{s}$-measurable, hence \emph{a fortiori} $\mathcal F_{t-1}$-measurable. Pulling $e_s$ out of the inner expectation:
\[
\mathbb{E}[e_t e_s]=\mathbb{E}\bigl[\mathbb{E}[e_t e_s\mid\mathcal F_{t-1}]\bigr]=\mathbb{E}\bigl[e_s\cdot\mathbb{E}[e_t\mid\mathcal F_{t-1}]\bigr]=\mathbb{E}[e_s\cdot 0]=0.
\]
Combined with $\mathbb{E}[e_t]=\mathbb{E}[e_s]=0$:
\[
\boxed{\;\operatorname{Cov}(e_t,e_s)=\mathbb{E}[e_t e_s]-\mathbb{E}[e_t]\mathbb{E}[e_s]=0.\;}
\]

\emph{Interpretation.} $(e_t)$ is a \textbf{martingale-difference sequence}: orthogonal but, in general, not independent (independence holds under Gaussianity). Together with $\mathbb{E}[e_t]=0$ this is exactly what gets exploited in:
- Likelihood-based estimation (prediction-error decomposition gives independent Gaussian factors).
- Diagnostic testing: the \textbf{Ljung--Box} statistic on raw $e_t$ should be non-significant; non-zero autocorrelation flags model mis-specification.

\emph{R diagnostic:}
`Box.test(e, lag=20, type="Ljung-Box")   ## H0: innovations uncorrelated up to lag 20`

</details>

<details class="master-subpart" open>
<summary><span class="tag tag-2plus">1 ex</span> (Q3) <em>Consider a DLM with no unknown parameters. Let $f_t=\mathbb{E}[Y_t\mid y_{1:t-1}]$ be the point forecasts of $Y_t$ given $y_{1:t-1}$, and let $e_t=Y_t-f_t$ be the forecast error (or innovation). Suppose $e_t\overset{\text{iid}}{\sim}\mathcal{N}(0,1)$. Is that correct?</em>  [May 2024 Q6]</summary>

\textbf{NO.} The claim conflates \emph{three} distinct facts; in particular the unit variance is wrong.

\emph{Correct properties of the raw innovations $e_t$:}
- \textbf{Zero mean.} $\mathbb{E}[e_t]=0$ (Q1).
- \textbf{Uncorrelated} across $t$: $\operatorname{Cov}(e_t,e_s)=0$ for $t\ne s$ (Q2).
- \textbf{Conditionally Gaussian} given the past, with variance \emph{depending on $t$}: $e_t\mid y_{1:t-1}\sim\mathcal{N}(0,Q_t)$, where $Q_t=F_tR_tF_t^{\top}+V_t$ from the Kalman filter.

So $e_t\sim\mathcal{N}(0,Q_t)$ in general, \emph{not} $\mathcal{N}(0,1)$. The unit-variance form only holds after standardisation.

\textbf{Standardised innovations} are what one actually uses for diagnostics:
\[
\boxed{\;\tilde e_t=Q_t^{-1/2}e_t\overset{\text{iid}}{\sim}\mathcal{N}(0,I)\quad\text{under correct specification.}\;}
\]

\emph{Model checking on $\tilde e_t$:}
- \textbf{QQ-plot} of $\tilde e_t$ vs standard normal (tail / skew anomalies).
- \textbf{Ljung--Box} on $\tilde e_t$ (autocorrelation $\Rightarrow$ structure left in residuals).
- \textbf{Ljung--Box on $\tilde e_t^2$} (ARCH effects / volatility clustering).
- \textbf{CUSUM} of $\tilde e_t$ (parameter drift).

`qqnorm(et); qqline(et)`
`Box.test(et,   lag=20, type="Ljung-Box") ## mean structure`
`Box.test(et^2, lag=20, type="Ljung-Box") ## variance structure`

</details>

\textbf{Linked exams:} \texttt{exam_jun_2025_q5}, \texttt{exam_may_2024_q6}, \texttt{exam_may_2021_q6}.""",
    "images": ['images/master/master_t11a_ai.png']
}

master_exercises_ts["t12a"] = {
    "title": 'Master — Likelihood of phi via prediction-error decomposition (MLE)',
    "content": r"""\textbf{\textcolor{red}{MASTER --- Likelihood of phi via prediction-error decomposition (MLE)}}

---

<details class="master-subpart" open>
<summary><span class="tag tag-2plus">≥2 ex</span> (Q1) <em>Consider a DLM with unknown parameters $\phi$. What is the expression of the joint density $p(y_1,\dots,y_n)$? Does it depend on $\phi$? / Given data $(y_1,\dots,y_n)$, write the expression of the likelihood of $\phi$.</em>  [Jun 2024 Q7, May 2024 Q7]</summary>

\textbf{Prediction-error decomposition} (DLMwR \S 4.1).

\emph{Setup.} For each $t$, the Kalman filter run at parameter $\phi$ produces the one-step-ahead Gaussian predictive
\[
Y_t\mid y_{1:t-1},\phi\sim\mathcal{N}_q\!\bigl(f_t(\phi),Q_t(\phi)\bigr),
\]
with $f_t(\phi)=F_t a_t(\phi)$, $Q_t(\phi)=F_t R_t(\phi) F_t^{\top}+V_t(\phi)$.

\emph{Factorise the joint by the chain rule.} $p(y_{1:n}\mid\phi)=\prod_{t=1}^{n}p(y_t\mid y_{1:t-1},\phi)$, hence
\[
\boxed{\;L(\phi)\;=\;p(y_{1:n}\mid\phi)\;=\;\prod_{t=1}^{n}\mathcal{N}_q\!\bigl(y_t;\,f_t(\phi),Q_t(\phi)\bigr).\;}
\]

\textbf{Yes, it depends on $\phi$} --- this is exactly the \emph{likelihood} used for MLE.

\emph{Log-likelihood (the actual computation):}
\[
\ell(\phi)=-\tfrac12\sum_{t=1}^{n}\Bigl[q\log(2\pi)+\log\det Q_t(\phi)+e_t(\phi)^{\top}Q_t(\phi)^{-1}e_t(\phi)\Bigr],\quad e_t(\phi)=y_t-f_t(\phi).
\]

</details>

<details class="master-subpart" open>
<summary><span class="tag tag-2plus">1 ex</span> (Q2) <em>Explain how you would compute the maximum likelihood estimator $\hat\phi$.</em>  [May 2024 Q7]</summary>

\emph{Numerical optimisation.} The log-likelihood $\ell(\phi)$ is \textbf{not} available in closed form (each evaluation needs a Kalman-filter pass), but it is a smooth function of $\phi$.

\textbf{Algorithm.}
- Start from $\phi^{(0)}$ (e.g.\ method-of-moments initialiser, or use $V,W=$ sample variance of differenced data).
- Use a quasi-Newton method (\textbf{BFGS}) or derivative-free (\textbf{Nelder--Mead}). Each iteration:
  - Evaluate $\ell(\phi^{(k)})$ via one KF pass: produce $\{(f_t,Q_t,e_t)\}_{t=1}^n$ then accumulate $\ell$.
  - Gradient: analytic (chain rule through the KF recursions) or by finite differences.
- Stop when $\|\nabla\ell\|<\epsilon$ or $|\ell^{(k+1)}-\ell^{(k)}|<\epsilon$.
- Standard errors: $\widehat\phi$ has asymptotic covariance $-[\nabla^2\ell(\hat\phi)]^{-1}$ (observed information).

\textbf{Practical issues.}
- Variances must be positive: optimise over $\log V,\log W$ (i.e.\ reparametrise).
- Multimodality is common in large state spaces $\Rightarrow$ try multiple starts.
- Identifiability: only the ratio $W/V$ (signal-to-noise) is identified in some structural models.

`}`

</details>

<details class="master-subpart" open>
<summary><span class="tag tag-2plus">1 ex</span> (Q3) <em>Now suppose that, at time $t-1$, you have data $y_{1:t-1}$ and have already computed the MLE $\hat\phi$. How would you proceed to compute the predictive distribution of $Y_t$ given $y_{1:t-1}$?</em>  [May 2024 Q7]</summary>

\textbf{Plug-in approach.} Treat $\hat\phi$ as the true parameter, and read off the Kalman one-step-ahead predictive:
\[
\boxed{\;Y_t\mid y_{1:t-1}\;\approx\;\mathcal{N}_q\!\bigl(f_t(\hat\phi),\,Q_t(\hat\phi)\bigr).\;}
\]

\emph{Recipe.}
- Run the KF on $y_{1:t-1}$ with $\phi=\hat\phi$ to obtain $(m_{t-1},C_{t-1})$.
- Predict step: $a_t=Gm_{t-1}$, $R_t=GC_{t-1}G^{\top}+W(\hat\phi)$, $f_t=Fa_t$, $Q_t=FR_tF^{\top}+V(\hat\phi)$.

\textbf{Caveat (under-coverage).} This ignores parameter uncertainty in $\hat\phi$. Credible / prediction intervals are systematically \emph{too narrow}, especially when $n$ is small or $\hat\phi$ is poorly identified. A delta-method correction inflates $Q_t$ by $(\partial f_t/\partial\phi)^{\top}\,\widehat{\operatorname{Var}}(\hat\phi)\,(\partial f_t/\partial\phi)$, but the cleanest fix is to go fully Bayesian (Q5).

`fc$f; sqrt(fc$Q[[1]])`

</details>

<details class="master-subpart" open>
<summary><span class="tag tag-2plus">1 ex</span> (Q4) <em>In a Bayesian approach, the unknown parameters are treated differently. What is now the expression of $p(y_1,\dots,y_n)$?</em>  [Jun 2024 Q7]</summary>

\textbf{Bayesian marginal (prior predictive / evidence).} The parameter $\phi$ is treated as random with prior $\pi(\phi)$. The data density \emph{integrates out} $\phi$:
\[
\boxed{\;p(y_{1:n})\;=\;\int p(y_{1:n}\mid\phi)\,\pi(\phi)\,d\phi
\;=\;\int\Bigl[\prod_{t=1}^{n}\mathcal{N}_q(y_t;f_t(\phi),Q_t(\phi))\Bigr]\pi(\phi)\,d\phi.\;}
\]

\textbf{Key contrast with (Q1).} This object \emph{does not} depend on $\phi$ --- it has been integrated out --- whereas the frequentist likelihood $L(\phi)=p(y_{1:n}\mid\phi)$ does.

\textbf{Use.} $p(y_{1:n})$ is the \emph{marginal likelihood / model evidence}, used for
- Bayes factors and model comparison: $\mathrm{BF}_{12}=p_1(y_{1:n})/p_2(y_{1:n})$.
- Sequential Bayesian model averaging.

\textbf{Computation.} Typically intractable in closed form. Estimated via:
- Harmonic mean / importance sampling (unstable, avoid).
- \emph{Bridge sampling} or \emph{annealed importance sampling}.
- Nested sampling, or thermodynamic integration over MCMC output.

\emph{R sketch:}
`# bridgesampling on a Stan / JAGS fit of the DLM with prior pi(phi):`

`bs$logml         ## log p(y_{1:n})`

</details>

<details class="master-subpart" open>
<summary><span class="tag tag-2plus">1 ex</span> (Q5) <em>How would you instead proceed, in the Bayesian approach, for inference on $\phi$ and for computing the predictive distribution $p(Y_t\mid y_{1:t-1})$?</em>  [May 2024 Q7]</summary>

\textbf{Bayesian inference.} Choose a prior $\pi(\phi)$ (typically inverse-Gamma on variances, multivariate normal on transition coefficients). Posterior:
\[
p(\phi\mid y_{1:t-1})\propto L(\phi)\,\pi(\phi),
\]
where $L(\phi)=\prod_s\mathcal{N}_q(y_s;f_s(\phi),Q_s(\phi))$ (Q1).

\emph{Sampling the posterior.} The closed form is unavailable; use MCMC:
- \textbf{Gibbs} when $W,V$ have inverse-Gamma priors: alternate between sampling $(W,V)\mid\theta_{0:T},y_{1:T}$ (conditionally inverse-Gamma) and $\theta_{0:T}\mid W,V,y_{1:T}$ via \textbf{FFBS} (forward filter / backward sampler).
- \textbf{Metropolis-Hastings} for non-conjugate $\phi$: propose $\phi^\star\sim q$, accept with prob $\min\{1,L(\phi^\star)\pi(\phi^\star)/[L(\phi)\pi(\phi)]\cdot q(\phi\mid\phi^\star)/q(\phi^\star\mid\phi)\}$.

\textbf{Predictive distribution.} Marginalise over the posterior:
\[
\boxed{\;p(y_t\mid y_{1:t-1})=\int p(y_t\mid y_{1:t-1},\phi)\,p(\phi\mid y_{1:t-1})\,d\phi
\;\approx\;\frac{1}{S}\sum_{s=1}^{S}\mathcal{N}_q\!\bigl(y_t;\,f_t(\phi^{(s)}),Q_t(\phi^{(s)})\bigr),\;}
\]
a Monte-Carlo \emph{mixture of Gaussians} (one KF run per posterior draw $\phi^{(s)}$).

\emph{Honest uncertainty.} The Bayesian predictive is \textbf{wider} than the plug-in $\mathcal{N}(f_t(\hat\phi),Q_t(\hat\phi))$ (Q3): it properly inflates intervals for $\phi$-uncertainty, recovering correct coverage even in small samples.

\emph{R (Gibbs + FFBS for $V,W$ via dlm):}

`  rnorm(1, fc$f, sqrt(unlist(fc$Q)))`
`})`
`quantile(pred, c(0.025, 0.975))              ## Bayesian 95% PI`

</details>

\textbf{Linked exams:} \texttt{exam_jun_2024_q7}, \texttt{exam_may_2024_q7}.""",
    "images": ['images/master/master_t12a_ai.png']
}

master_exercises_ts["t13a"] = {
    "title": 'Master — Conjugate Normal-Normal posterior (static theta / Case A)',
    "content": r"""\textbf{\textcolor{red}{MASTER --- Conjugate Normal-Normal posterior (static theta / Case A)}}

---

<details class="master-subpart" open>
<summary><span class="tag tag-2plus">1 ex</span> (Q1) <em>Consider $Y_t=\theta+\varepsilon_t$, $t\ge 1$, where $\varepsilon_t\overset{\text{iid}}{\sim}\mathcal{N}(0,\sigma^2=1)$ and $\theta$ is unknown. In a Bayesian approach, what is the joint density of $(Y_1,\dots,Y_t,\theta)$?</em>  [Sep 2025 Q8]</summary>

\textbf{Set up the Bayesian model.} Place a conjugate prior $\theta\sim\mathcal{N}(m_0,C_0)$ on the unknown parameter (a flat prior is the limit $C_0\to\infty$).

\textbf{Factorise the joint via $p(y,\theta)=p(\theta)\,p(y\mid\theta)$.} Conditional on $\theta$, the $Y_s$ are i.i.d.\ $\mathcal{N}(\theta,1)$, so
\[
\boxed{\;p(y_{1:t},\theta)\;=\;\mathcal{N}(\theta;m_0,C_0)\,\prod_{s=1}^{t}\mathcal{N}(y_s;\theta,1).\;}
\]

\emph{Written out:}
\[
p(y_{1:t},\theta)\propto\exp\!\left\{-\frac{1}{2C_0}(\theta-m_0)^2-\frac{1}{2}\sum_{s=1}^{t}(y_s-\theta)^2\right\}.
\]

\emph{Three building blocks.}
- $p(\theta)=\mathcal{N}(\theta;m_0,C_0)$: the prior.
- $p(y_s\mid\theta)=\mathcal{N}(y_s;\theta,1)$: each observation conditional on $\theta$.
- Conditional independence of $Y_s$ given $\theta$ $\Rightarrow$ product in the likelihood.

This is the joint that gets fed to Bayes' rule in (Q2): dividing by the marginal $p(y_{1:t})$ gives the posterior $p(\theta\mid y_{1:t})$.

</details>

<details class="master-subpart" open>
<summary><span class="tag tag-2plus">1 ex</span> (Q2) <em>Given data $y_{1:n}\equiv(Y_1=y_1,\dots,Y_n=y_n)$, with $n=20$ and sample mean $\bar y_n=4$, use Bayes' rule and compute the posterior density of $\theta$, that is $p(\theta\mid y_{1:n})$. Describe all the steps of the computations.</em>  [Sep 2025 Q8]</summary>

\textbf{Step 1 --- Bayes' rule.}
\[
p(\theta\mid y_{1:n})\propto p(\theta)\prod_{s=1}^{n}p(y_s\mid\theta)
\propto\exp\!\left\{-\frac{1}{2C_0}(\theta-m_0)^2-\frac{1}{2\sigma^2}\sum_{s=1}^{n}(y_s-\theta)^2\right\}.
\]

\textbf{Step 2 --- expand and collect terms in $\theta$.}
\[
-\frac{1}{2}\Bigl[\underbrace{\Bigl(\tfrac{1}{C_0}+\tfrac{n}{\sigma^2}\Bigr)}_{\text{precision}}\theta^2-2\underbrace{\Bigl(\tfrac{m_0}{C_0}+\tfrac{n\bar y_n}{\sigma^2}\Bigr)}_{\text{precision}\,\times\,\text{mean}}\theta\Bigr]+\text{const}.
\]
(Used $\sum_s y_s=n\bar y_n$ and $\sum_s y_s^2$ absorbed into the constant.)

\textbf{Step 3 --- complete the square in $\theta$.} Match against a Normal $\exp\{-(\theta-m_n)^2/(2C_n)\}$:
\[
\boxed{\;\theta\mid y_{1:n}\sim\mathcal{N}(m_n,C_n),\qquad
\dfrac{1}{C_n}=\dfrac{1}{C_0}+\dfrac{n}{\sigma^2},\qquad
m_n=C_n\Bigl(\dfrac{m_0}{C_0}+\dfrac{n\bar y_n}{\sigma^2}\Bigr).\;}
\]

\emph{Interpretation.}
- \textbf{Precisions add}: posterior precision $1/C_n=$ prior precision $1/C_0$ $+$ data precision $n/\sigma^2$.
- $m_n$ is the \textbf{precision-weighted average} of $m_0$ and $\bar y_n$. As $n\to\infty$, $m_n\to\bar y_n$ and $C_n\to 0$ (concentration on truth).

\textbf{Step 4 --- numerical computation.} With $\sigma^2=1$, $n=20$, $\bar y_n=4$, flat prior $C_0\to\infty$ ($1/C_0\to 0$, $m_0/C_0\to 0$):
\[
\frac{1}{C_n}=0+\frac{20}{1}=20\;\Rightarrow\;C_n=\frac{1}{20}=0.05,
\]
\[
m_n=0.05\cdot(0+20\cdot 4)=0.05\cdot 80=4.
\]
Hence $\theta\mid y_{1:20}\sim\mathcal{N}(4,\,0.05)$.

\emph{95\% credible interval:} $4\pm 1.96\sqrt{0.05}\approx 4\pm 0.438\approx[3.56,\,4.44]$.

`mn + c(-1,1)*qnorm(0.975)*sqrt(Cn)   ## [3.56, 4.44]`

</details>

<details class="master-subpart" open>
<summary><span class="tag tag-2plus">1 ex</span> (Q3) <em>Consider a random walk plus noise model, where the variance $\sigma_w^2$ of the evolution error $w_t$ is zero. Given the initial distribution $\mathcal{N}(m_0,C_0)$, and observations $y_{1:n}\equiv(y_1,\dots,y_n)$, use Bayes' rule to obtain the conditional distribution of $\theta_n\mid y_{1:n}$.</em>  [Jun 2022 Q7]</summary>

\textbf{Step 1 --- reduce to the static-$\theta$ model.} With $W=\sigma_w^2=0$ the state equation becomes
\[
\theta_t=\theta_{t-1}+0=\theta_{t-1}\quad\Rightarrow\quad \theta_t\equiv\theta_0=:\theta\;\text{ for all }t.
\]
The random walk \emph{collapses} to a constant. The model reduces to:
\[
\theta\sim\mathcal{N}(m_0,C_0)\quad\text{(prior)},\qquad Y_i\mid\theta\overset{\text{iid}}{\sim}\mathcal{N}(\theta,V),\;i=1,\dots,n.
\]

\textbf{Step 2 --- apply Bayes' rule.} $\pi(\theta\mid y_{1:n})\propto p(y_{1:n}\mid\theta)\pi(\theta)$.

\emph{Likelihood (in $\theta$):} write $\sum_i(y_i-\theta)^2=\sum_i(y_i-\bar y_n)^2+n(\theta-\bar y_n)^2$, so
\[
p(y_{1:n}\mid\theta)\propto\exp\!\left\{-\frac{n}{2V}(\theta-\bar y_n)^2\right\},\qquad\bar y_n=n^{-1}\sum_i y_i.
\]

\emph{Prior:} $\pi(\theta)\propto\exp\!\left\{-\frac{1}{2C_0}(\theta-m_0)^2\right\}$.

\textbf{Step 3 --- multiply and complete the square in $\theta$.} The exponent reads
\[
-\frac12\Bigl[\tfrac{1}{C_0}+\tfrac{n}{V}\Bigr]\theta^2+\Bigl[\tfrac{m_0}{C_0}+\tfrac{n\bar y_n}{V}\Bigr]\theta+\text{const}.
\]
Matching to $\mathcal{N}(m_n,C_n)$:
\[
\boxed{\;\theta_n\equiv\theta\mid y_{1:n}\sim\mathcal{N}(m_n,C_n),\qquad
\dfrac{1}{C_n}=\dfrac{1}{C_0}+\dfrac{n}{V},\qquad
m_n=C_n\!\left(\dfrac{m_0}{C_0}+\dfrac{n\bar y_n}{V}\right).\;}
\]

\emph{Equivalent closed form (helpful):}
\[
m_n=\frac{V\,m_0+n\,C_0\,\bar y_n}{V+n\,C_0},\qquad C_n=\frac{C_0\,V}{V+n\,C_0}.
\]

\emph{Interpretation.}
- Posterior precision $1/C_n$ $=$ prior precision $+$ $n\times$ per-observation data precision.
- Posterior mean $m_n$ $=$ precision-weighted average of prior mean $m_0$ and sample mean $\bar y_n$.
- The KF recursion in this degenerate case gives the same formula --- a useful internal consistency check.

\emph{Numerical mini-example} ($m_0=0$, $C_0=10$, $V=1$, $n=10$, $\bar y_n=2$):
\[
1/C_n=0.1+10=10.1\;\Rightarrow\;C_n\approx 0.0990,\quad m_n=0.0990\cdot(0+20)=1.980.
\]

`c(mn=mn, Cn=Cn)         ## 1.980  0.0990`

</details>

\textbf{Linked exams:} \texttt{exam_sep_2025_q8}, \texttt{exam_jun_2022_q7}.""",
    "images": ['images/master/master_t13a_ai.png']
}

master_exercises_ts["t13b"] = {
    "title": 'Master — Bayesian predictive integrating out phi (+ MCMC)',
    "content": r"""\textbf{\textcolor{red}{MASTER --- Bayesian predictive integrating out phi (+ MCMC)}}

---

<details class="master-subpart" open>
<summary><span class="tag tag-2plus">1 ex</span> (Q1) <em>Consider a DLM with unknown parameters $\phi$. How do you treat uncertainty on the unknown parameters in the Bayesian approach? Define a state-space process for an $m$-dimensional time series $(Y_t)_{t\ge 1}$.</em>  [May 2025 Q6]</summary>

\textbf{Bayesian treatment of $\phi$.} Promote $\phi$ from a fixed unknown to a \emph{random variable} with prior $\pi(\phi)$ encoding pre-data beliefs. Posterior inference targets
\[
p(\phi\mid y_{1:t})\propto L(\phi)\,\pi(\phi),\qquad L(\phi)=\prod_{s=1}^{t}\mathcal{N}_q(y_s;f_s(\phi),Q_s(\phi)).
\]
All downstream objects are obtained by \emph{marginalising} $\phi$.

\textbf{State-space process for $Y_t\in\mathbb{R}^m$, conditional on $\phi$:}
\[
\theta_0\sim\mathcal{N}_p(m_0,C_0),
\]
\[
\theta_t=G_t\theta_{t-1}+w_t,\qquad w_t\sim\mathcal{N}_p\!\bigl(0,W_t(\phi)\bigr),
\]
\[
Y_t=F_t\theta_t+v_t,\qquad v_t\sim\mathcal{N}_m\!\bigl(0,V_t(\phi)\bigr),
\]
with $\{w_t\},\{v_s\},\theta_0$ mutually independent given $\phi$. Here $\phi$ collects the unknown entries of $G,F,W,V$ (often $\log$-variances and AR coefficients).

\textbf{Joint posterior.} Inference is on
\[
p(\theta_{0:t},\phi\mid y_{1:t})\propto p(y_{1:t}\mid\theta_{0:t},\phi)\,p(\theta_{0:t}\mid\phi)\,\pi(\phi),
\]
typically explored by MCMC over $(\theta_{0:t},\phi)$.

\emph{Typical priors.}
- $V,W$: independent inverse-Gamma (conjugate).
- AR coefficients: truncated Normal on stationarity region.

</details>

<details class="master-subpart" open>
<summary><span class="tag tag-2plus">1 ex</span> (Q2) <em>Write the expression of the filtering distribution of $\theta_t$ given $y_{1:t}$. Comment briefly.</em>  [May 2025 Q6]</summary>

\textbf{Filtering distribution under parameter uncertainty.} Marginalise $\phi$ from the joint posterior:
\[
\boxed{\;p(\theta_t\mid y_{1:t})=\int p(\theta_t\mid y_{1:t},\phi)\,p(\phi\mid y_{1:t})\,d\phi.\;}
\]

\emph{Each ingredient.}
- For each fixed $\phi$, the inner factor $p(\theta_t\mid y_{1:t},\phi)=\mathcal{N}_p(m_t(\phi),C_t(\phi))$ comes from a Kalman-filter pass at $\phi$.
- The weighting kernel $p(\phi\mid y_{1:t})\propto L(\phi)\pi(\phi)$ uses the prediction-error likelihood (DLMwR \S 4.1).

\emph{Comments.}
- The marginal is a \textbf{mixture of Gaussians}, generally \emph{non-Gaussian} (could be skewed or multimodal).
- Approximated by Monte Carlo: draw $\phi^{(s)}\sim p(\phi\mid y_{1:t})$ for $s=1,\dots,S$, run the KF at each draw to get $(m_t^{(s)},C_t^{(s)})$, then mix.
- \textbf{Intervals are wider} than the plug-in $\mathcal{N}_p(m_t(\hat\phi),C_t(\hat\phi))$ at the MLE; the Bayesian version performs \emph{honest uncertainty quantification} (correct coverage even with little data).

`apply(theta_t, 1, mean)                          ## posterior mean`
`apply(theta_t, 1, quantile, c(0.025, 0.975))     ## 95% CrI per component`

</details>

<details class="master-subpart" open>
<summary><span class="tag tag-2plus">≥2 ex</span> (Q3) <em>Consider a DLM for a univariate time series $\{Y_t\}_{t\ge 1}$, with unknown parameters. How would you compute the predictive density $p(y_{t+1}\mid y_{1:t})$ in a Bayesian approach? Write its expression and comment briefly. / In the Bayesian approach how would you compute the one-step-ahead predictive distribution $p(y_t\mid y_{1:t-1})$?</em>  [Jun 2025 Q6, May 2022 Q8]</summary>

\textbf{Bayesian one-step-ahead predictive.} The predictive density marginalises out \emph{both} the state \emph{and} the parameters. Starting from the joint $p(y_{t+1},\phi\mid y_{1:t})=p(y_{t+1}\mid y_{1:t},\phi)\,p(\phi\mid y_{1:t})$ and integrating $\phi$:
\[
\boxed{\;p(y_{t+1}\mid y_{1:t})=\int p(y_{t+1}\mid y_{1:t},\phi)\,p(\phi\mid y_{1:t})\,d\phi.\;}
\]

\emph{Each ingredient.}
- For each fixed $\phi$: $p(y_{t+1}\mid y_{1:t},\phi)=\mathcal{N}(f_{t+1}(\phi),Q_{t+1}(\phi))$ from the Kalman filter (the state $\theta_t$ has already been marginalised inside the KF).
- $p(\phi\mid y_{1:t})\propto L(\phi)\pi(\phi)$, typically intractable in closed form, explored by MCMC (Metropolis-Hastings, or Gibbs over $(\phi,\theta_{0:t})$ using FFBS).

\emph{Form of the integral.} A \textbf{mixture of Gaussians}, generally non-Gaussian (heavier tails, possible skew).

\textbf{Monte-Carlo approximation.} Draw $\phi^{(s)}\sim p(\phi\mid y_{1:t})$ for $s=1,\dots,S$ (from MCMC), run the KF at each draw, then
\[
p(y_{t+1}\mid y_{1:t})\approx\frac{1}{S}\sum_{s=1}^{S}\mathcal{N}\!\bigl(y_{t+1};f_{t+1}(\phi^{(s)}),Q_{t+1}(\phi^{(s)})\bigr).
\]

\emph{Honest uncertainty.} The Bayesian predictive is \textbf{wider} than the plug-in $\mathcal{N}(f_{t+1}(\hat\phi),Q_{t+1}(\hat\phi))$, which ignores parameter uncertainty.

\emph{R (Gibbs + KF):}

`  rnorm(1, fc$f, sqrt(unlist(fc$Q)))`
`})`
`quantile(pred, c(0.025, 0.975))   ## Bayesian 95% prediction interval`

</details>

<details class="master-subpart" open>
<summary><span class="tag tag-2plus">1 ex</span> (Q4) <em>In the Bayesian approach, how would you proceed for inference on the unknown parameters $\phi$?</em>  [May 2022 Q8]</summary>

\textbf{Recipe.}
- Choose a prior $\pi(\phi)$ that encodes pre-data beliefs (e.g.\ inverse-Gamma on variances $V,W$ for conjugacy; truncated Normal on AR coefficients for stationarity).
- Posterior:
\[
p(\phi\mid y_{1:t-1})\propto L(\phi)\,\pi(\phi),\qquad L(\phi)=\prod_{s=1}^{t-1}\mathcal{N}_q(y_s;f_s(\phi),Q_s(\phi)).
\]
- Closed form is generically unavailable $\Rightarrow$ sample via MCMC.

\textbf{Standard MCMC schemes for DLMs.}

\emph{(1) Gibbs with FFBS} (when $V,W$ have conjugate inverse-Gamma priors). Alternate:
- Sample $\theta_{0:T}\mid V,W,y_{1:T}$ by the \textbf{forward-filter / backward-sampler (FFBS)} algorithm (joint draw from the Gaussian smoothing distribution).
- Sample $V\mid\theta_{0:T},y_{1:T}\sim\mathrm{IG}\!\left(a_V+T/2,\;b_V+\tfrac12\sum_t(y_t-F\theta_t)^2\right)$.
- Sample $W\mid\theta_{0:T}\sim\mathrm{IG}\!\left(a_W+T/2,\;b_W+\tfrac12\sum_t(\theta_t-G\theta_{t-1})^2\right)$.

\emph{(2) Metropolis-Hastings} for non-conjugate $\phi$. Propose $\phi^\star\sim q(\cdot\mid\phi)$; accept with probability
\[
\alpha=\min\!\left\{1,\;\frac{L(\phi^\star)\pi(\phi^\star)q(\phi\mid\phi^\star)}{L(\phi)\pi(\phi)q(\phi^\star\mid\phi)}\right\}.
\]
Each $L(\phi^\star)$ evaluation = one KF pass.

\emph{Diagnostics.} Run multiple chains, check Gelman-Rubin $\hat R<1.01$, effective sample size, traceplots, posterior predictive checks.

\emph{R (Gibbs with FFBS in dlm):}

`hist(V_post); quantile(V_post, c(0.025, 0.5, 0.975))   ## marginal posterior of V`

</details>

<details class="master-subpart" open>
<summary><span class="tag tag-2plus">1 ex</span> (Q5) <em>Markov Chain Monte Carlo (MCMC) is typically used to approximate the conditional distribution $p(\theta_{0:T},\phi\mid y_{1:T})$. How is MCMC related to Markov chains?</em>  [May 2022 Q8]</summary>

\textbf{The Markov-chain inside MCMC.} An MCMC sampler is itself a \emph{Markov chain} $\{X^{(s)}\}_{s\ge 0}$ on the parameter / latent space (in our case $X=(\theta_{0:T},\phi)$). It is constructed so that its \textbf{stationary distribution coincides with the target posterior} $\pi=p(\theta_{0:T},\phi\mid y_{1:T})$.

\textbf{Two ingredients ensuring this works.}

\emph{(a) Stationarity of the target.} The transition kernel $K(x,y)$ is engineered to satisfy \textbf{detailed balance}:
\[
\pi(x)\,K(x,y)=\pi(y)\,K(y,x),
\]
which implies $\pi$ is invariant: $\int\pi(x)K(x,y)\,dx=\pi(y)$.
- Metropolis-Hastings: detailed balance is enforced by construction via the acceptance ratio.
- Gibbs: detailed balance holds in each component update.

\emph{(b) Ergodicity.} \textbf{Irreducibility} (any state reachable from any other in finite time) and \textbf{aperiodicity} (no cyclic structure) give, by the ergodic theorem (Theorem 2.1 applied to the sampler's own chain),
\[
\frac{1}{S}\sum_{s=1}^{S}h(X^{(s)})\;\xrightarrow{a.s.}\;\mathbb{E}_\pi[h(X)]=\int h(x)\,\pi(x)\,dx\quad\text{as }S\to\infty,
\]
\emph{regardless of the starting point} $X^{(0)}$.

\textbf{Bottom line.} MCMC \emph{is} a Markov chain --- the same theory as in Chapter 2 of the course applies: detailed balance pins down the limit distribution; ergodicity gives the LLN that justifies estimating posterior expectations as sample averages.

\emph{Caveats.}
- Burn-in / mixing time: early draws are still far from $\pi$.
- Autocorrelation reduces effective sample size $\Rightarrow$ thin or run longer.
- Convergence diagnostics: $\hat R$, ESS, traceplots, multiple chains.

`coda::gelman.diag(coda::as.mcmc.list(list(chain1, chain2)))   ## Rhat`
`coda::effectiveSize(chain1)                                   ## ESS per parameter`

</details>

\textbf{Linked exams:} \texttt{exam_jun_2025_q6}, \texttt{exam_may_2025_q6}, \texttt{exam_may_2022_q8}.""",
    "images": ['images/master/master_t13b_ai.png']
}
