"""
Theory column entries for sub-topics in topics T1, T2, T3.
"""
theory_content_ts = {}

# =============================================================================
# t1a — What is a stochastic process / time series
# =============================================================================
theory_content_ts["t1a"] = {
    "title": "Theory — What is a stochastic process / time series",
    "content": r"""\textbf{\textcolor{red}{THEORY --- What is a stochastic process / time series [Topic: T1 — Stochastic processes \& time series — foundations]}}

\textbf{1. Stochastic process — definition.}

A \emph{stochastic process} is a family
\[
(Y_t)_{t\in T}
\]
of random variables (or random vectors) defined on a common probability space $(\Omega,\mathcal F,\mathbb{P})$ and indexed by a set $T$, the \emph{index set}. Each $Y_t:\Omega\to\mathcal Y$ takes values in a \emph{state space} $\mathcal Y$ (e.g.\ $\mathbb{R}$, $\mathbb{R}^m$, or a finite alphabet $\{1,\dots,K\}$). Typical choices for the index set are $T=\mathbb{N}$, $T=\mathbb{Z}$ (discrete time), or $T=[0,\infty)$ (continuous time); $T$ may also represent space.

\boxed{\;(Y_t)_{t\in T}\text{ — random object whose value at each time }t\text{ is itself random.}\;}

\textbf{2. Single realisation vs.\ family of paths.}

For each fixed $\omega\in\Omega$, the map $t\mapsto Y_t(\omega)$ is a \emph{sample path} (or \emph{trajectory}, \emph{realisation}). The whole process is the collection of all such paths together with their joint probability law. The probabilistic information of the process is encoded by all joint distributions of finite-dimensional slices.

\textbf{3. Finite-dimensional distributions (f.d.d.s).}

The process $(Y_t)_{t\in T}$ is fully specified by its family of finite-dimensional distributions:
\[
\Bigl\{\,F_{t_1,\dots,t_k}(y_1,\dots,y_k)=\mathbb{P}(Y_{t_1}\le y_1,\dots,Y_{t_k}\le y_k)\;:\;k\ge 1,\;(t_1,\dots,t_k)\in T^k\,\Bigr\},
\]
or equivalently, when densities exist, by $f_{Y_{t_1},\dots,Y_{t_k}}(y_1,\dots,y_k)$.

\textbf{Kolmogorov's extension / consistency theorem.} If the f.d.d.s satisfy
\begin{itemize}
\item \emph{Permutation symmetry}: $F_{t_{\pi(1)},\dots,t_{\pi(k)}}(y_{\pi(1)},\dots,y_{\pi(k)})=F_{t_1,\dots,t_k}(y_1,\dots,y_k)$ for every permutation $\pi$;
\item \emph{Marginal consistency}: $\lim_{y_{k+1}\to\infty}F_{t_1,\dots,t_k,t_{k+1}}(y_1,\dots,y_k,y_{k+1})=F_{t_1,\dots,t_k}(y_1,\dots,y_k)$,
\end{itemize}
then there exists a unique probability measure on $(\mathcal Y^T,\mathcal B(\mathcal Y^T))$ producing these f.d.d.s. This is the legal basis for "specify all the joint distributions" as a way to define a process.

\emph{Gaussian shortcut.} If the process is Gaussian, every f.d.d.\ is $\mathcal{N}_k(\mu_k,\Sigma_k)$ where $\mu_k=(\mathbb{E}[Y_{t_i}])_i$ and $\Sigma_k=(\operatorname{Cov}(Y_{t_i},Y_{t_j}))_{ij}$. Hence the \emph{mean function} $\mu(t)=\mathbb{E}[Y_t]$ and the \emph{covariance function} $\gamma(s,t)=\operatorname{Cov}(Y_s,Y_t)$ are enough to describe the whole law.

\textbf{4. Time series — definition.}

A \emph{time series} is a stochastic process $(Y_t)_{t\in T}$ with $T\subseteq\mathbb{Z}$ (discrete time, possibly with a temporal interpretation: hours, days, months, quarters, years). The \textbf{observed data}
\[
(y_1,y_2,\dots,y_T)
\]
are a \emph{single finite realisation} of \emph{one} sample path of the process. They are \textbf{not} a random sample of size $T$ from a population — they are a sample of size $1$ from a $T$-dimensional joint distribution.

\boxed{\;\text{Time series }=\text{ stochastic process }(Y_t)_{t\in T},\ T\subseteq\mathbb{Z}.\quad\text{Data: one path }y_{1:T}.\;}

\textbf{5. Why the probabilistic framing matters (and a naive definition is incomplete).}

A common loose definition is "a time series is a sequence of observations taken over time". This is incomplete because it omits the probabilistic model: without $(Y_t)_{t\in T}$ as a process, the words \emph{mean}, \emph{variance}, \emph{autocovariance}, \emph{stationarity}, \emph{forecast}, \emph{credible interval} have no meaning. Statistical inference (estimation, testing, forecasting with uncertainty) requires the underlying probability law.

\textbf{6. The single-path problem and structural assumptions.}

With only one path of length $T$, naive averaging across realisations is impossible. Inference becomes feasible only by tying different time points together via structural assumptions:
\begin{itemize}
\item \textbf{Stationarity} (strict or weak; see t2a): the joint law (or first two moments) is shift-invariant, so averaging over $t$ approximates averaging over realisations.
\item \textbf{Parametric dynamics}: ARMA, DLM, HMM impose a low-dimensional model that links $Y_t$ to past $Y_{s<t}$ or to a latent state $\theta_t$, drastically reducing the effective number of unknowns.
\item \textbf{Ergodicity}: time-averages converge to ensemble-averages, $\bar Y_n\xrightarrow{\mathrm{a.s.}}\mathbb{E}[Y_1]$. This is the precondition that turns one long path into a useful sample.
\end{itemize}

\textbf{7. Micro-example.}

(a) \emph{i.i.d.\ Gaussian noise.} $Y_t\overset{\text{iid}}{\sim}\mathcal{N}(0,1)$, $T=\mathbb{N}$. f.d.d.: $(Y_{t_1},\dots,Y_{t_k})\sim\mathcal{N}_k(0,I_k)$ for every $(t_1,\dots,t_k)$. The simplest non-trivial process; baseline ("white noise") for diagnostic comparison.

(b) \emph{Random walk.} $Y_0=0$, $Y_t=Y_{t-1}+Z_t$, $Z_t\overset{\text{iid}}{\sim}\mathcal{N}(0,1)$. Then $Y_t=\sum_{s\le t}Z_s\sim\mathcal{N}(0,t)$; cross-covariance $\operatorname{Cov}(Y_s,Y_t)=\min(s,t)$ depends on $(s,t)$ separately — \emph{not} weakly stationary, but still a perfectly well-defined process.

\textbf{8. Notation conventions used downstream.}

$\mu(t)=\mathbb{E}[Y_t]$ — mean function; $\gamma(s,t)=\operatorname{Cov}(Y_s,Y_t)$ — autocovariance function; under weak stationarity these reduce to $\mu(t)\equiv\mu$ and $\gamma(s,t)=\tilde\gamma(|s-t|)$ — a single-argument lag function (see t2a, t2b).

\textbf{9. R — viewing a single realisation.}

The plot displays \emph{one} sample path; the unobserved (counterfactual) alternative paths are part of the same process and constitute the conceptual population we infer over.

\textbf{Exam pointers.}
\begin{itemize}
\item[$\triangleright$] Used in \texttt{exam\_sep\_2025\_q1}: defines a stochastic process and then a time series as a stochastic process; explicit reference to f.d.d.s and Kolmogorov.
\item[$\triangleright$] Used in \texttt{exam\_may\_2023\_q1}: critiques the naive "sequence of observations over time" definition; correct answer = "NO, incomplete — it misses the probabilistic process structure that gives meaning to mean, variance, ACVF, inference".
\end{itemize}
""",
}

# =============================================================================
# t2a — Weak stationarity — definition & examples
# =============================================================================
theory_content_ts["t2a"] = {
    "title": "Theory — Weak stationarity — definition & examples",
    "content": r"""\textbf{\textcolor{red}{THEORY --- Weak stationarity — definition \& examples [Topic: T2 — Stationarity, ACVF \& sample mean]}}

\textbf{1. Mean function and autocovariance function.}

Let $(Y_t)_{t\in T}$ be a univariate time series with $\mathbb{E}[Y_t^2]<\infty$ for all $t$. Define
\[
\mu(t)=\mathbb{E}[Y_t]\quad\text{(mean function)},\qquad
\gamma(s,t)=\operatorname{Cov}(Y_s,Y_t)=\mathbb{E}[(Y_s-\mu(s))(Y_t-\mu(t))]\quad\text{(autocovariance function, ACVF)}.
\]
The ACVF $\gamma(s,t)$ is a function of \emph{two} arguments and is well-defined for \emph{any} second-order process, with no stationarity required.

\textbf{2. Strict stationarity (for context).}

$(Y_t)$ is \emph{strictly stationary} if its joint law is shift-invariant: for every $k,h$ and $(t_1,\dots,t_k)$,
\[
(Y_{t_1},\dots,Y_{t_k})\stackrel{d}{=}(Y_{t_1+h},\dots,Y_{t_k+h}).
\]
Strict stationarity is hard to verify; in practice we work with the weaker, moment-based notion.

\textbf{3. Weak (covariance) stationarity — boxed definition.}

\boxed{\;(Y_t)\text{ is \emph{weakly stationary} iff }\mathbb{E}[Y_t]\text{ and }\mathbb{E}[Y_tY_{t+h}]\text{ are finite and independent of }t,\text{ for every }h\in\mathbb{Z}.\;}

Equivalently:
\begin{itemize}
\item \emph{Constant mean}: $\mu(t)=\mu$ for all $t$;
\item \emph{Constant variance}: $\sigma^2(t)=\operatorname{Var}(Y_t)=\sigma^2$ for all $t$;
\item \emph{Lag-only ACVF}: $\gamma(t,t+h)=\tilde\gamma(h)$, a function of the lag $h$ alone.
\end{itemize}

The single-argument $\tilde\gamma(h)$ inherits the usual properties: $\tilde\gamma(0)=\sigma^2\ge 0$, $\tilde\gamma(-h)=\tilde\gamma(h)$, $|\tilde\gamma(h)|\le\tilde\gamma(0)$, and $\tilde\gamma$ is positive semi-definite ($\sum_{i,j}c_i c_j\tilde\gamma(t_i-t_j)\ge 0$).

\emph{Relation between strict and weak.} Strict + finite second moments $\Rightarrow$ weak. Weak $\not\Rightarrow$ strict in general; for Gaussian processes the two coincide.

\textbf{4. Canonical examples.}

\textbf{(i) White noise.} $Y_t\overset{\text{iid}}{\sim}\mathcal{N}(0,\sigma^2)$. Then $\mu=0$, $\sigma^2(t)=\sigma^2$, and $\tilde\gamma(h)=\sigma^2\,\mathbf{1}\{h=0\}$ — manifestly $t$-free. Weakly (and strictly) stationary.

\textbf{(ii) Causal AR(1) — full proof.} Take $Y_t=\phi Y_{t-1}+\varepsilon_t$ with $\varepsilon_t\overset{\text{iid}}{\sim}\mathcal{N}(0,\sigma^2)$, $|\phi|<1$, and start from the stationary distribution $Y_0\sim\mathcal{N}\bigl(0,\sigma^2/(1-\phi^2)\bigr)$, with $Y_0\perp(\varepsilon_t)_{t\ge 1}$.

Iterating the recursion gives the \emph{causal MA representation}
\[
Y_t=\sum_{j=0}^{\infty}\phi^j\varepsilon_{t-j},
\]
which converges in $L^2$ because $\sum_j\phi^{2j}=1/(1-\phi^2)<\infty$ (geometric series, $|\phi|<1$).

Then
\[
\mathbb{E}[Y_t]=\sum_j\phi^j\mathbb{E}[\varepsilon_{t-j}]=0,\qquad
\tilde\gamma(h)=\operatorname{Cov}(Y_t,Y_{t+h})=\sigma^2\sum_{j\ge 0}\phi^j\phi^{j+h}=\frac{\sigma^2\phi^{|h|}}{1-\phi^2}.
\]
Both $\mu$ and $\tilde\gamma$ depend only on the lag, not on $t$ — so $(Y_t)$ is weakly stationary. (If $|\phi|\ge 1$ the MA series diverges and stationarity fails; for $|\phi|=1$ this is a random walk, which is non-stationary.)

\textbf{(iii) Non-example — random walk.} $Y_t=\sum_{s\le t}\varepsilon_s$ has $\operatorname{Var}(Y_t)=t\sigma^2$, which grows with $t$. The variance is not constant, so $Y_t$ is \emph{not} weakly stationary.

\textbf{(iv) Non-example — deterministic trend.} $Y_t=\alpha+\beta t+\varepsilon_t$ has $\mathbb{E}[Y_t]=\alpha+\beta t$, depending on $t$. Not stationary unless $\beta=0$.

\textbf{5. Can ARMA describe non-stationary data?}

ARMA$(p,q)$ has \emph{constant} coefficients $(\phi_1,\dots,\phi_p,\theta_1,\dots,\theta_q,\sigma^2)$. As a consequence, its mean, variance and ACVF are constant in $t$, so ARMA cannot describe a series with a non-constant mean, a non-constant variance, or a non-lag-only autocovariance.

\boxed{\;\text{NO — ARMA cannot be used directly on a non-stationary series.}\;}

\boxed{\;\text{YES, but only if the non-stationarity is first removed:}\;}
\begin{itemize}
\item \emph{Unit roots / stochastic trend}: difference $d$ times, $(1-B)^d Y_t$, then fit ARMA — i.e.\ ARIMA$(p,d,q)$.
\item \emph{Seasonality of period $s$}: seasonal differencing $(1-B^s)Y_t$, leading to SARIMA.
\item \emph{Deterministic trend / seasonality}: subtract a parametric trend $\widehat\mu_t$ and/or seasonal indicators $\widehat\gamma_t$, then fit ARMA to the residuals.
\end{itemize}

\textbf{6. R — checking weak stationarity numerically.}

\textbf{7. Practical signs of non-stationarity (for the descriptive answer).}

\begin{itemize}
\item Visible trend (mean drifts) — fails constant-mean.
\item Visible seasonality (mean cycles) — fails constant-mean.
\item Changing variance / volatility clusters — fails constant-variance (or motivates a conditional-variance model like GARCH).
\item Sample ACF that does not decay (or decays slowly and linearly) — indicates a unit root.
\end{itemize}

\textbf{Exam pointers.}
\begin{itemize}
\item[$\triangleright$] Used in \texttt{exam\_sep\_2025\_q2}: (a) define $\mu$ and $\gamma$; (b) define weak stationarity; (c) "Can ARMA fit a non-stationary series?" — NO directly, YES if non-stationarity is removed first (differencing for ARIMA, $(1-B^s)$ for SARIMA, or de-trend + de-season residuals).
\item[$\triangleright$] Used in \texttt{exam\_jun\_2024\_q1}: (a) define weak stationarity; (b) give an example — white noise or causal AR(1) with $|\phi|<1$ started in its stationary distribution.
\end{itemize}
""",
}

# =============================================================================
# t2b — ACVF / correlogram — when defined?
# =============================================================================
theory_content_ts["t2b"] = {
    "title": "Theory — ACVF / correlogram — when defined?",
    "content": r"""\textbf{\textcolor{red}{THEORY --- ACVF / correlogram — when defined? [Topic: T2 — Stationarity, ACVF \& sample mean]}}

\textbf{1. Autocovariance function (ACVF) — population definition.}

For a univariate time series $(Y_t)_{t\in T}$ with $\mathbb{E}[Y_t^2]<\infty$ for all $t$, the \emph{autocovariance function} is
\[
\gamma(s,t)=\operatorname{Cov}(Y_s,Y_t)=\mathbb{E}\bigl[(Y_s-\mu(s))(Y_t-\mu(t))\bigr],\qquad s,t\in T.
\]

\boxed{\;\gamma(s,t)\text{ is defined whenever the second moments are finite — \textbf{no} stationarity required.}\;}

It is a function of \emph{two} arguments. The autocorrelation function (ACF) is the normalised version
$\rho(s,t)=\gamma(s,t)/\sqrt{\gamma(s,s)\gamma(t,t)}$.

\textbf{2. What stationarity changes (\emph{not} whether $\gamma$ exists, but how it depends on $(s,t)$).}

Under weak stationarity (see t2a), the ACVF reduces to a function of the lag only:
\[
\gamma(s,t)=\tilde\gamma(t-s)=\tilde\gamma(h),\qquad h=t-s.
\]
i.e.\ a 1-argument function. Stationarity is therefore the assumption that makes "the autocovariance at lag $h$" — a single number — well-posed.

\emph{Properties of $\tilde\gamma$ under weak stationarity}: $\tilde\gamma(0)=\sigma^2\ge 0$; $\tilde\gamma(-h)=\tilde\gamma(h)$; $|\tilde\gamma(h)|\le\tilde\gamma(0)$ (Cauchy–Schwarz); positive semi-definite ($\sum_{i,j}c_ic_j\tilde\gamma(t_i-t_j)\ge 0$).

\textbf{3. So: "the ACVF can be defined only if the series is stationary" — TRUE or FALSE?}

\boxed{\;\textbf{FALSE / NO.}\;}\quad The two-argument $\gamma(s,t)$ exists for any second-order process. Stationarity is needed only to \emph{reduce} the ACVF to a single-argument lag function $\tilde\gamma(h)$ — \emph{not} to define it.

\textbf{4. Sample ACVF (correlogram) — estimator.}

Given data $y_1,\dots,y_T$ from a univariate series with sample mean $\bar Y_T=T^{-1}\sum_t Y_t$, the standard \emph{sample autocovariance} at lag $h\ge 0$ is
\[
\widehat\gamma(h)=\frac{1}{T}\sum_{t=1}^{T-|h|}(Y_t-\bar Y_T)(Y_{t+|h|}-\bar Y_T),\qquad |h|<T,
\]
and the sample autocorrelation is $\widehat\rho(h)=\widehat\gamma(h)/\widehat\gamma(0)$. The \emph{correlogram} is the plot $h\mapsto\widehat\rho(h)$ (with 95\% confidence bands $\pm 1.96/\sqrt T$ under the white-noise null).

\textbf{5. When does the correlogram make sense as an estimator?}

The sample ACVF \emph{pools across $t$}: it averages many products $(Y_t-\bar Y)(Y_{t+h}-\bar Y)$ to estimate a \emph{single} quantity. This pooling is meaningful only if there is a single quantity to estimate — i.e.\ only if $\gamma(t,t+h)$ does \emph{not} depend on $t$. That is exactly weak stationarity.

Furthermore, for $\widehat\gamma(h)\xrightarrow{\mathrm{a.s.}}\tilde\gamma(h)$ (consistency), we also need \emph{ergodicity}: time-averages converge to ensemble-averages,
\[
\frac{1}{T-|h|}\sum_{t=1}^{T-|h|}(Y_t-\mu)(Y_{t+|h|}-\mu)\xrightarrow{\mathrm{a.s.}}\tilde\gamma(h).
\]
For stationary Gaussian processes, ergodicity follows from $\tilde\gamma(h)\to 0$ as $h\to\infty$ — a mild mixing condition.

\boxed{\;\text{Correlogram interpretable as an estimate of "the ACVF" }\Longleftrightarrow\;\text{weak stationarity + ergodicity.}\;}

If $(Y_t)$ is \emph{not} stationary (trend, seasonality, drift), then $\gamma(t,t+h)$ depends on $t$. The sample ACVF averages over inhomogeneous quantities and has no clean population target — the correlogram becomes a misleading diagnostic ("spurious autocorrelation"). Classic symptom: correlogram of an integrated series decays linearly to zero rather than geometrically, reflecting the unit root rather than any meaningful $\tilde\gamma(h)$.

\textbf{6. Worked micro-example.}

For a causal AR(1) $Y_t=\phi Y_{t-1}+\varepsilon_t$ with $|\phi|=0.7$, $\sigma^2=1$, the population ACVF is $\tilde\gamma(h)=0.7^{|h|}/(1-0.49)\approx 1.96\cdot 0.7^{|h|}$. The correlogram from $T=500$ should overlay these values within $\pm 1.96/\sqrt{500}\approx 0.088$ bands at long lags. For a random walk $Y_t=\sum_{s\le t}\varepsilon_s$, the sample ACF stays close to 1 for many lags before slowly tailing off — diagnostic of non-stationarity, not of "the ACVF" of the series.

\textbf{7. R — computing and plotting the correlogram.}

\textbf{8. Summary — answers to the two recurring exam questions.}

\emph{(a) "The ACVF can be defined only if the series is stationary." Correct?}\quad
\boxed{\;\textbf{NO.}\;}\;\;The ACVF $\gamma(s,t)$ is defined for any second-order process. Stationarity merely reduces it from a 2-argument function to a 1-argument lag function $\tilde\gamma(h)$.

\emph{(b) "The correlogram can be used to estimate the ACVF only if the series is stationary." Correct?}\quad
\boxed{\;\textbf{YES (essentially).}\;}\;\;The correlogram pools across $t$ to estimate one number per lag; this targets a well-defined quantity only when $\gamma(t,t+h)$ depends on the lag alone, i.e.\ under weak stationarity. Add ergodicity for consistency.

\textbf{Exam pointers.}
\begin{itemize}
\item[$\triangleright$] Used in \texttt{exam\_may\_2025\_q1}: (a) "ACVF defined only if stationary?" — NO, defined whenever $\mathbb{E}[Y_t^2]<\infty$. (b) "Correlogram can estimate ACVF only if stationary?" — YES, because pooling across $t$ is meaningful only when $\gamma$ depends on the lag alone; plus ergodicity for consistency.
\item[$\triangleright$] Used in \texttt{exam\_may\_2022\_q2}: same as (a) — NO, the ACVF $\gamma(s,t)=\operatorname{Cov}(Y_s,Y_t)$ is defined whenever second moments are finite.
\item[$\triangleright$] Used in \texttt{exam\_may\_2021\_q2}: same as (a) — NO.
\end{itemize}
""",
}

# =============================================================================
# t2c — Sample-mean estimator under stationarity + ergodicity
# =============================================================================
theory_content_ts["t2c"] = {
    "title": "Theory — Sample-mean estimator under stationarity + ergodicity",
    "content": r"""\textbf{\textcolor{red}{THEORY --- Sample-mean estimator under stationarity + ergodicity [Topic: T2 — Stationarity, ACVF \& sample mean]}}

\textbf{1. Setup.}

Let $(Y_t)_{t\ge 1}$ be a univariate time series with finite first moment $\mathbb{E}[Y_t]<\infty$. Given data $y_{1:n}$, the natural non-parametric estimator of the mean is the \emph{sample mean}
\[
\bar Y_n=\frac{1}{n}\sum_{t=1}^n Y_t.
\]
\textbf{Question.} Is $\bar Y_n$ a sensible estimator of $\mathbb{E}[Y_n]$ (or of "the mean" of the series)? Under what conditions?

\textbf{2. The non-stationary obstacle.}

Without stationarity, $\mu(t)=\mathbb{E}[Y_t]$ may depend on $t$. By linearity,
\[
\mathbb{E}[\bar Y_n]=\frac{1}{n}\sum_{t=1}^n\mathbb{E}[Y_t]=\frac{1}{n}\sum_{t=1}^n\mu(t)=:\bar\mu_n.
\]
This time-average mixture $\bar\mu_n$ is generically \emph{different} from $\mu(n)=\mathbb{E}[Y_n]$, so $\bar Y_n$ is a biased estimator of the latter. Example: with a trend $\mu(t)=\alpha+\beta t$, $\bar\mu_n=\alpha+\beta(n+1)/2\ne\alpha+\beta n=\mu(n)$.

\boxed{\;\text{Without stationarity, }\bar Y_n\text{ does \emph{not} target }\mathbb{E}[Y_n].\;}

\textbf{3. Stationarity makes $\bar Y_n$ unbiased.}

\textbf{Proposition.} If $(Y_t)$ is weakly stationary with $\mathbb{E}[Y_t]\equiv\mu$, then
\[
\mathbb{E}[\bar Y_n]=\mu\quad\text{for every }n\ge 1.
\]

\emph{Proof.} $\mathbb{E}[\bar Y_n]=n^{-1}\sum_t\mu=\mu$. $\square$

\textbf{4. Variance under stationarity — long-run variance.}

Under weak stationarity with ACVF $\tilde\gamma(h)$,
\[
\operatorname{Var}(\bar Y_n)=\frac{1}{n^2}\sum_{s,t=1}^n\operatorname{Cov}(Y_s,Y_t)=\frac{1}{n^2}\sum_{s,t=1}^n\tilde\gamma(t-s)=\frac{1}{n}\sum_{h=-(n-1)}^{n-1}\Bigl(1-\frac{|h|}{n}\Bigr)\tilde\gamma(h).
\]
If $\sum_h|\tilde\gamma(h)|<\infty$ (short-memory),
\[
n\operatorname{Var}(\bar Y_n)\;\xrightarrow[n\to\infty]{}\;\sigma_{\mathrm{LR}}^2:=\sum_{h\in\mathbb{Z}}\tilde\gamma(h)=2\pi f(0),
\]
the \emph{long-run variance} (spectral density at frequency $0$). Note this can be much larger or smaller than $\tilde\gamma(0)=\operatorname{Var}(Y_t)$ depending on the sign and magnitude of the autocorrelations.

\boxed{\;\operatorname{Var}(\bar Y_n)\approx\sigma_{\mathrm{LR}}^2/n,\qquad \sigma_{\mathrm{LR}}^2=\sum_h\tilde\gamma(h).\;}

\textbf{5. Consistency — need ergodicity.}

Unbiasedness alone does not give $\bar Y_n\xrightarrow{}\mu$. We need \emph{ergodicity}: time-averages converge to ensemble-averages.

\textbf{Ergodic theorem (Birkhoff–Khinchin, version for stationary sequences).} If $(Y_t)$ is strictly stationary and ergodic with $\mathbb{E}|Y_1|<\infty$, then
\[
\bar Y_n\;\xrightarrow[n\to\infty]{\mathrm{a.s.}}\;\mathbb{E}[Y_1]=\mu.
\]

\emph{Mean-square (weak) version, sufficient in practice.} If $(Y_t)$ is weakly stationary and $\tilde\gamma(h)\to 0$ as $h\to\infty$ (asymptotic decorrelation), then $\operatorname{Var}(\bar Y_n)\to 0$ and $\bar Y_n\xrightarrow{L^2}\mu$, hence $\bar Y_n\xrightarrow{\mathbb P}\mu$. This is the working consistency result.

\emph{Proof sketch (mean-square).} $\operatorname{Var}(\bar Y_n)=n^{-1}\sum_h(1-|h|/n)\tilde\gamma(h)$. By Cesàro, since $\tilde\gamma(h)\to 0$, this sum divided by $n$ tends to $0$. $\square$

\boxed{\;\text{Consistency: }\bar Y_n\to\mu\;\;\text{requires stationarity}\;+\;\text{ergodicity.}\;}

\textbf{6. Asymptotic normality — CLT for stationary sequences.}

Under stationarity + suitable mixing (e.g.\ $\alpha$-mixing with summable rates, or summable ACVF),
\[
\sqrt n\,(\bar Y_n-\mu)\xrightarrow{d}\mathcal{N}(0,\sigma_{\mathrm{LR}}^2),
\]
so an asymptotic CI is
\[
\bar Y_n\pm z_{1-\alpha/2}\,\widehat\sigma_{\mathrm{LR}}/\sqrt n,
\]
where $\widehat\sigma_{\mathrm{LR}}^2$ is a HAC (Newey–West) estimator of the long-run variance.

\emph{Naive SE warning.} The iid SE $\widehat\sigma/\sqrt n$ (using $\widehat\sigma^2=\widehat\gamma(0)$) ignores autocorrelation and is wrong: it understates uncertainty for positively-autocorrelated data (the common case) and overstates it for negatively-autocorrelated data.

\textbf{7. Worked micro-example.}

For a causal AR(1) with $\phi=0.7$, $\sigma^2=1$: $\tilde\gamma(0)=1/(1-0.49)\approx 1.96$, $\sigma_{\mathrm{LR}}^2=\sigma^2/(1-\phi)^2=1/0.09\approx 11.11$. With $n=500$, $\operatorname{Var}(\bar Y_n)\approx 11.11/500\approx 0.022$, so $\mathrm{SE}\approx 0.149$. The iid SE would be $\sqrt{1.96/500}\approx 0.063$ — too small by a factor of $\sim 2.4$.

\textbf{8. Bottom line — verdict.}

\boxed{\;\bar Y_n\text{ is an appropriate non-parametric estimator of }\mu=\mathbb{E}[Y_t]\;\;\text{UNDER stationarity + ergodicity.}\;}

Without stationarity, the target itself is unclear ($\mu(t)$ depends on $t$); without ergodicity, even an unbiased estimator may fail to converge.

\textbf{9. R — sample mean + HAC standard error.}

\textbf{Exam pointers.}
\begin{itemize}
\item[$\triangleright$] Used in \texttt{exam\_may\_2024\_q1}: "Estimate the mean function non-parametrically." Answer: under stationarity $\mu(t)\equiv\mu$, use $\widehat\mu_T=\bar Y_T$; unbiased; consistent under stationarity + ergodicity; SE based on long-run variance (HAC).
\item[$\triangleright$] Used in \texttt{exam\_jun\_2022\_q1}: "Is $\bar Y_n$ an appropriate estimator of $\mathbb{E}[Y_n]$?" Answer: YES, under conditions = stationarity + ergodicity. Without stationarity, $\bar Y_n$ targets the time-average mixture $\bar\mu_n=n^{-1}\sum_t\mu(t)\ne\mu(n)$ in general.
\end{itemize}
""",
}

# =============================================================================
# t3a — Markov property, DAG & conditional independence
# =============================================================================
theory_content_ts["t3a"] = {
    "title": "Theory — Markov property, DAG & conditional independence",
    "content": r"""\textbf{\textcolor{red}{THEORY --- Markov property, DAG \& conditional independence [Topic: T3 — Markov chains]}}

\textbf{1. Discrete-time Markov chain — definition.}

Let $(Y_t)_{t\ge 0}$ be a stochastic process with values in a (countable) state space $\mathcal Y$. The process is a \emph{Markov chain (MC)} if
\[
\boxed{\;\mathbb{P}(Y_t=y_t\mid Y_{0}=y_0,\dots,Y_{t-1}=y_{t-1})=\mathbb{P}(Y_t=y_t\mid Y_{t-1}=y_{t-1})\;}\qquad\text{for every }t\ge 1,\ y_{0:t}\in\mathcal Y^{t+1}.
\]

This is the \emph{(first-order) Markov property}: the conditional distribution of $Y_t$ given the entire past depends only on the immediately preceding state $Y_{t-1}$.

\textbf{Initial law and transition kernel.} A MC is specified by
\begin{itemize}
\item the initial distribution $\nu_i=\mathbb{P}(Y_0=i)$, $i\in\mathcal Y$;
\item the transition kernel $p_t(i,j)=\mathbb{P}(Y_t=j\mid Y_{t-1}=i)$.
\end{itemize}

\textbf{Homogeneity.} The MC is \emph{(time-)homogeneous} if $p_t(i,j)=p(i,j)=p_{ij}$ does not depend on $t$. Then the transition matrix $\mathbf P=[p_{ij}]$ is a $|\mathcal Y|\times|\mathcal Y|$ stochastic matrix ($p_{ij}\ge 0$, $\sum_j p_{ij}=1$).

\boxed{\;\text{Categorical/finite-state process }\ne\text{ Markov chain in general.}\;}\quad The state space and initial law alone do not imply the Markov property; one must verify the conditional independence above.

\textbf{2. Equivalent formulation — conditional independence.}

The Markov property is equivalent to: for every $t\ge 1$,
\[
Y_t\;\perp\!\!\!\perp\;(Y_0,\dots,Y_{t-2})\;\;\bigm|\;\;Y_{t-1}.
\]
That is, conditionally on $Y_{t-1}$, the present is independent of the distant past.

\textbf{3. DAG representation.}

The conditional-independence structure of a MC is encoded by the \emph{Directed Acyclic Graph}
\[
Y_0\;\to\;Y_1\;\to\;Y_2\;\to\;\cdots\;\to\;Y_{t-2}\;\to\;Y_{t-1}\;\to\;Y_t\;\to\;\cdots
\]
— a single directed chain, one arrow per time step. Every node has one parent (the previous state) and one child (the next state); the joint density factorises as
\[
p(y_{0:T})=p(y_0)\prod_{t=1}^T p(y_t\mid y_{t-1}).
\]

\textbf{4. $d$-separation — the graphical conditional-independence criterion.}

In a DAG, a path between two sets of nodes $A$ and $B$ is \emph{blocked} by a conditioning set $C$ if some node on the path is either
\begin{itemize}
\item a \emph{chain} ($a\to v\to b$) or \emph{fork} ($a\leftarrow v\to b$) with $v\in C$; or
\item a \emph{collider} ($a\to v\leftarrow b$) with neither $v$ nor any descendant of $v$ in $C$.
\end{itemize}
$A$ and $B$ are \emph{$d$-separated} by $C$ if every path from $A$ to $B$ is blocked. $d$-separation implies conditional independence in any distribution faithful to the DAG.

\textbf{5. Theorem (graphical Markov property).}

\boxed{\;Y_t\perp\!\!\!\perp(Y_1,\dots,Y_{t-2})\mid Y_{t-1}.\;}

\emph{Proof via the DAG.} Every directed path from any $Y_{1:t-2}$ to $Y_t$ in the MC DAG must travel along the chain $Y_{1}\to\cdots\to Y_{t-2}\to Y_{t-1}\to Y_t$ — hence pass through the \emph{serial/chain node} $Y_{t-1}$. Conditioning on $Y_{t-1}$ blocks every such path. By $d$-separation, $Y_t\perp\!\!\!\perp Y_{1:t-2}\mid Y_{t-1}$. $\square$

\emph{Algebraic verification.} From the joint factorisation and the definition of conditional probability,
\[
\mathbb{P}(Y_t=y_t\mid Y_{0:t-1}=y_{0:t-1})=\frac{p(y_{0:t})}{p(y_{0:t-1})}=\frac{p(y_0)\prod_{s=1}^t p(y_s\mid y_{s-1})}{p(y_0)\prod_{s=1}^{t-1}p(y_s\mid y_{s-1})}=p(y_t\mid y_{t-1}),
\]
which depends only on $y_{t-1}$.

\textbf{6. Worked micro-example — random walk is Markov.}

\emph{(Example 1.)} Let $Y_0=0$, $Y_t=\sum_{i=1}^t Z_i$, with $Z_i$ i.i.d., $\mathbb{P}(Z_i=-1)=p$, $\mathbb{P}(Z_i=+1)=1-p$. Then $Y_t=Y_{t-1}+Z_t$.

Because $Z_t$ is independent of $(Z_1,\dots,Z_{t-1})$ and therefore of $Y_{0:t-1}=(Y_0,\dots,Y_{t-1})$, for any path $y_{0:t}$
\[
\mathbb{P}(Y_t=y_t\mid Y_{0:t-1}=y_{0:t-1})=\mathbb{P}(Z_t=y_t-y_{t-1}\mid Y_{0:t-1}=y_{0:t-1})=\mathbb{P}(Z_t=y_t-y_{t-1}),
\]
which depends only on $y_{t-1}$. Hence $(Y_t)$ is Markov. \emph{Not} stationary: $\mathbb{E}[Y_t]=(1-2p)t$, $\operatorname{Var}(Y_t)=4p(1-p)\,t$ both depend on $t$.

\textbf{7. Verifying homogeneity from sample restrictions.}

If the data force two different one-step transition probabilities for the same $(i,j)$ at different times, the chain cannot be homogeneous.

\emph{Mini-illustration.} Suppose $\mathbb{P}(X_4=1\mid X_3=3)=0.4$ and $\mathbb{P}(X_2=1\mid X_1=3)=0.5$. Under homogeneity both equal $p_{3,1}$, hence $0.4=0.5$: contradiction. The process can still be \emph{inhomogeneous} Markov, or non-Markov altogether — but not a \emph{homogeneous} MC.

\textbf{8. Sufficient condition: deterministic update with independent innovations.}

A clean sufficient condition for the Markov property: if $Y_t=g(Y_{t-1},Z_t)$ for some measurable $g$ and an independent innovation sequence $(Z_t)$, with $Z_t\perp Y_{0:t-1}$, then $(Y_t)$ is Markov. This is the prototypical "transition" recipe: random walks, AR(1), nonlinear autoregressions, dynamical systems with i.i.d.\ noise.

\textbf{9. R — simulate and empirically check the Markov property.}

\textbf{10. Bottom line.}

\boxed{\;\text{MC: }\mathbb{P}(Y_t\mid Y_{0:t-1})=\mathbb{P}(Y_t\mid Y_{t-1}).\;\text{ Equivalently }Y_t\perp Y_{0:t-2}\mid Y_{t-1}.\;}

The DAG is the chain $Y_0\to Y_1\to\cdots$; the Markov property follows from $d$-separation by the serial node $Y_{t-1}$. Categorical + given initial law does \emph{not} imply Markov; the conditional-independence statement above must hold.

\textbf{Exam pointers.}
\begin{itemize}
\item[$\triangleright$] Used in \texttt{exam\_jun\_2025\_q1}: (a) "Is a categorical time series a MC?" — NO in general; one must verify $\mathbb{P}(Y_t\mid Y_{0:t-1})=\mathbb{P}(Y_t\mid Y_{t-1})$. (b) DAG proof of $Y_t\perp Y_{1:t-2}\mid Y_{t-1}$ by $d$-separation through the serial node $Y_{t-1}$.
\item[$\triangleright$] Used in \texttt{exam\_may\_2025\_q2}: random walk $Y_t=\sum_{i\le t}Z_i$ — YES, Markov, by writing $Y_t=Y_{t-1}+Z_t$ with $Z_t\perp Y_{0:t-1}$. Note: not stationary (mean and variance grow in $t$).
\item[$\triangleright$] Used in \texttt{exam\_may\_2021\_q1}: forced equality $p_{3,1}=0.4=0.5$ contradiction — cannot be a \emph{homogeneous} MC; could be inhomogeneous or non-Markov.
\end{itemize}
""",
}

# =============================================================================
# t3b — Transition-matrix arithmetic & ergodic convergence (Thm 2.1)
# =============================================================================
theory_content_ts["t3b"] = {
    "title": "Theory — Transition-matrix arithmetic & ergodic convergence (Thm 2.1)",
    "content": r"""\textbf{\textcolor{red}{THEORY --- Transition-matrix arithmetic \& ergodic convergence (Thm 2.1) [Topic: T3 — Markov chains]}}

\textbf{1. Transition matrix — recap.}

Let $(Y_t)_{t\ge 0}$ be a homogeneous Markov chain on a finite state space $\mathcal Y=\{1,2,\dots,K\}$. Encode:
\begin{itemize}
\item \emph{Initial distribution} as a row vector $\nu=(\nu_1,\dots,\nu_K)$ with $\nu_i=\mathbb{P}(Y_0=i)$ and $\sum_i\nu_i=1$.
\item \emph{Transition matrix} $\mathbf P=[p_{ij}]_{i,j=1}^K$ with $p_{ij}=\mathbb{P}(Y_t=j\mid Y_{t-1}=i)\ge 0$ and $\sum_j p_{ij}=1$ for every $i$ (rows sum to $1$ — \emph{stochastic matrix}).
\end{itemize}

\textbf{2. Chapman–Kolmogorov: $n$-step probabilities.}

For any $n\ge 1$ and any $i,j\in\mathcal Y$,
\[
\boxed{\;\mathbb{P}(Y_n=j\mid Y_0=i)=(\mathbf P^n)_{ij}.\;}
\]

\emph{Proof by induction.} Base case $n=1$ is the definition. Suppose $\mathbb{P}(Y_{n-1}=k\mid Y_0=i)=(\mathbf P^{n-1})_{ik}$. By the Markov property and the law of total probability,
\[
\mathbb{P}(Y_n=j\mid Y_0=i)=\sum_k\mathbb{P}(Y_n=j\mid Y_{n-1}=k)\,\mathbb{P}(Y_{n-1}=k\mid Y_0=i)=\sum_k p_{kj}(\mathbf P^{n-1})_{ik}=(\mathbf P^{n-1}\mathbf P)_{ij}=(\mathbf P^n)_{ij}. \square
\]

\textbf{Marginal at time $n$.} The marginal distribution is $\nu_n=\nu_0\mathbf P^n$:
\[
\mathbb{P}(Y_n=j)=\sum_i\nu_i(\mathbf P^n)_{ij}.
\]

\textbf{3. Joint-path probability — multi-step example.}

By repeated Markov factorisation,
\[
\mathbb{P}(Y_1=y_1,Y_2=y_2,\dots,Y_T=y_T\mid Y_0=y_0)=\prod_{t=1}^T p_{y_{t-1},y_t}.
\]

\emph{Micro-example.} $\mathbb{P}(S_1=1,S_2=1,S_3=2\mid S_0=1)=p_{11}\cdot p_{11}\cdot p_{12}=p_{11}^2\,p_{12}$.

\textbf{4. Uniform-row chain — trivial computation.}

If every row of $\mathbf P$ is the uniform vector $(1/K,\dots,1/K)$, then $\mathbf P=\tfrac{1}{K}\mathbf 1\mathbf 1^\top$ and $\mathbf P^n=\mathbf P$ for every $n\ge 1$ (since $\mathbf P^2=\tfrac{1}{K^2}\mathbf 1\mathbf 1^\top\mathbf 1\mathbf 1^\top=\tfrac{K}{K^2}\mathbf 1\mathbf 1^\top=\mathbf P$). So
\[
\mathbb{P}(Y_n=j\mid Y_0=i)=1/K\quad\text{for every }n\ge 1.
\]
\emph{Micro-example.} With $K=3$ and all rows uniform, $\mathbb{P}(Y_2=2\mid Y_0=1)=(\mathbf P^2)_{12}=1/3$.

\textbf{5. Filling in a stochastic matrix.}

Rows must sum to $1$. So given a partially specified matrix, the missing entries are determined (in $\ge 0$) by the row sums. Example with $K=3$:
\[
\mathbf P=\begin{pmatrix}.6 & 0 & \square\\ \square & .6 & .3\\ .3 & \square & .6\end{pmatrix}\;\Longrightarrow\;
\mathbf P=\begin{pmatrix}.6 & 0 & .4\\ .1 & .6 & .3\\ .3 & .1 & .6\end{pmatrix}.
\]

\textbf{6. Classification of states.}

\begin{itemize}
\item \emph{Reachability}: $i\to j$ if $(\mathbf P^n)_{ij}>0$ for some $n\ge 0$. Write $i\leftrightarrow j$ if both hold.
\item \emph{Irreducibility}: $\mathbf P$ is irreducible if $i\leftrightarrow j$ for every pair $(i,j)$ — a single communicating class.
\item \emph{Period} of state $i$: $d(i)=\gcd\{n\ge 1:(\mathbf P^n)_{ii}>0\}$. \emph{Aperiodic} if $d(i)=1$. (Period is a class property; sufficient condition for aperiodicity: $p_{ii}>0$ for some $i$.)
\end{itemize}

\textbf{Stationary distribution.} A probability row vector $\pi=(\pi_1,\dots,\pi_K)$ with $\pi\mathbf P=\pi$ and $\sum_j\pi_j=1$. Solves the left-eigenvector problem at eigenvalue $1$.

\textbf{7. Theorem 2.1 — ergodic convergence on finite state spaces.}

\boxed{\;\textbf{Thm 2.1.}\;\text{If }\mathbf P\text{ is irreducible and aperiodic on a finite }\mathcal Y,\text{ then there exists a unique stationary distribution }\pi\text{ with }\pi\mathbf P=\pi,\;}
\[
\boxed{\;\text{and }\mathbb{P}(Y_n=j\mid Y_0=i)\xrightarrow[n\to\infty]{}\pi_j\quad\text{for every }i,j.\;}
\]

Equivalently, $(\mathbf P^n)_{ij}\to\pi_j$ as $n\to\infty$, exponentially fast (geometric rate determined by the second-largest eigenvalue of $\mathbf P$).

\emph{Proof sketch (Perron–Frobenius).} Irreducibility + aperiodicity on finite $\mathcal Y\Rightarrow$ some power $\mathbf P^{n_0}$ has strictly positive entries (primitive matrix). Perron–Frobenius applied to the primitive non-negative matrix $\mathbf P^{n_0}$ gives a unique dominant eigenvalue $1$ with positive left/right eigenvectors; all other eigenvalues have modulus $<1$. Spectral decomposition yields $\mathbf P^n\to\mathbf 1\pi$ in matrix sense, i.e.\ every row converges to $\pi$. $\square$

\textbf{8. Convergence of the unconditional marginal.}

By the law of total probability and Thm 2.1,
\[
\mathbb{P}(Y_n=j)=\sum_i\nu_i\,\mathbb{P}(Y_n=j\mid Y_0=i)\;\xrightarrow[n\to\infty]{}\;\sum_i\nu_i\pi_j=\pi_j,
\]
which holds for \emph{any} initial law $\nu$ (the limit forgets $\nu$). So both the conditional and the unconditional marginal converge to the same $\pi$.

\textbf{9. Solving $\pi\mathbf P=\pi$ — by hand.}

The system $\pi\mathbf P=\pi$ + $\sum\pi_j=1$ is $K$ linear equations in $K$ unknowns (one is redundant). Two routes:

\emph{(a) Linear solve.} Replace one of the equations of $\pi(\mathbf P-I)=0$ with $\sum_j\pi_j=1$ and invert.

\emph{(b) Left eigenvector.} $\pi$ is the (row) left eigenvector of $\mathbf P$ at eigenvalue $1$, normalised to sum to $1$.

\emph{Worked example.} $\mathbf P=\begin{pmatrix}.6&0&.4\\.1&.6&.3\\.3&.1&.6\end{pmatrix}$. Solving $\pi\mathbf P=\pi$ with $\pi_1+\pi_2+\pi_3=1$ gives $\pi\approx(0.394,\,0.121,\,0.485)$. Irreducible (one can reach any state from any other) and aperiodic ($p_{11}>0$), so by Thm 2.1, $\mathbb{P}(Y_n=j\mid Y_0=i)\to\pi_j$ for every $i,j$, and also $\mathbb{P}(Y_n=j)\to\pi_j$ for every initial $\nu$.

\textbf{10. When Thm 2.1 fails.}

\begin{itemize}
\item \emph{Reducible chains}: there are absorbing classes; the limit depends on the initial state.
\item \emph{Periodic chains}: e.g.\ $\mathbf P=\begin{pmatrix}0&1\\1&0\end{pmatrix}$, period $2$; $\mathbf P^n$ oscillates between $\mathbf P$ and $I$ and does not converge. Cesàro averages still converge to $\pi$, but the marginal itself does not.
\item \emph{Infinite state spaces}: an additional \emph{positive recurrence} condition is needed; null-recurrent chains have $\pi_j\to 0$ but no proper stationary law.
\end{itemize}

\textbf{11. R — compute powers and stationary distribution.}

\textbf{12. Closed-form joint probabilities — wrap-up.}

For \emph{any} homogeneous MC, conditional joint path probabilities are products of one-step transitions; multi-step marginal-from-initial probabilities are matrix powers; under irreducibility + aperiodicity, the long-run marginal forgets the initial state and equals the unique stationary distribution $\pi$.

\textbf{Exam pointers.}
\begin{itemize}
\item[$\triangleright$] Used in \texttt{exam\_jun\_2024\_q3}: complete the rows so each sums to $1$; verify irreducible + aperiodic ($p_{11}>0$); apply Thm 2.1 to conclude $\mathbb{P}(Y_n=j\mid Y_0=1)\to\pi_j$ for every $j$, with $\pi\approx(0.394,0.121,0.485)$; then $\mathbb{P}(Y_n=j)\to\pi_j$ for every initial law.
\item[$\triangleright$] Used in \texttt{exam\_may\_2023\_q2}: uniform-rows transition $\mathbf P=\tfrac13\mathbf 1\mathbf 1^\top$, so $\mathbf P^n=\mathbf P$ and $\mathbb{P}(Y_2=2\mid Y_0=1)=1/3$.
\item[$\triangleright$] Used in \texttt{exam\_may\_2022\_q3}: same as Jun 2024 Q3 — irreducible + aperiodic on finite $\mathcal Y\Rightarrow$ Thm 2.1, convergence to $\pi\approx(0.394,0.121,0.485)$.
\item[$\triangleright$] Used in \texttt{exam\_jun\_2022\_q5}: joint conditional probability factorises via Markov property: $\mathbb{P}(S_1=1,S_2=1,S_3=2\mid S_0=1)=p_{11}\cdot p_{11}\cdot p_{12}=p_{11}^2\,p_{12}$.
\end{itemize}
""",
}
