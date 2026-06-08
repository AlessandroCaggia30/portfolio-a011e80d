"""
Theory column entries for sub-topics in topics T10, T11, T12, T13.
"""
theory_content_ts = {}


# =============================================================================
# t10a — Predictive distribution N(f_t, Q_t) — derivation
# =============================================================================
theory_content_ts["t10a"] = {
    "title": "Theory — Predictive distribution N(f_t,Q_t) — derivation",
    "content": r"""\textbf{\textcolor{red}{THEORY --- Predictive distribution $\mathcal{N}(f_t,Q_t)$ --- derivation [Topic: T10 --- One-step-ahead prediction \& forecast function]}}

\textbf{1. Setup --- the DLM.}

A \emph{dynamic linear model} (DLM) for a $q$-variate time series $(Y_t)_{t\ge 1}$ has the linear-Gaussian state-space form
\[
\boxed{\;\theta_t=G_t\theta_{t-1}+w_t,\;w_t\overset{\text{iid}}{\sim}\mathcal{N}_p(0,W_t);\quad Y_t=F_t\theta_t+v_t,\;v_t\overset{\text{iid}}{\sim}\mathcal{N}_q(0,V_t);\quad\theta_0\sim\mathcal{N}_p(m_0,C_0),\;}
\]
with $\{w_t\},\{v_t\},\theta_0$ \emph{mutually independent}. $F_t$ is $q\times p$, $G_t$ is $p\times p$.

\boxed{\;\text{DLMs are }\textbf{not}\text{ limited to univariate }Y_t\text{: }Y_t\in\mathbb{R}^q\text{ for any }q\ge 1.\;}

\textbf{2. Question — the one-step-ahead predictive of $Y_t$.}

Given data $y_{1:t-1}$ and the previously computed \emph{state predictive}
\[
\theta_t\mid y_{1:t-1}\sim\mathcal{N}_p(a_t,R_t),\qquad a_t=G_t m_{t-1},\;R_t=G_t C_{t-1}G_t'+W_t,
\]
what is the distribution of $Y_t\mid y_{1:t-1}$?

\textbf{3. Theorem (one-step observation predictive).}

\boxed{\;Y_t\mid y_{1:t-1}\sim\mathcal{N}_q(f_t,Q_t),\quad f_t=F_t a_t,\quad Q_t=F_t R_t F_t'+V_t.\;}

\textbf{4. Derivation.}

Conditional on $y_{1:t-1}$, the building blocks are:
\begin{itemize}
\item $\theta_t\mid y_{1:t-1}\sim\mathcal{N}_p(a_t,R_t)$ (given, from the KF predict step);
\item $v_t\sim\mathcal{N}_q(0,V_t)$, $v_t\perp\theta_t\mid y_{1:t-1}$ (by the DLM construction: $v_t$ is independent of $\theta_{0:t}$ and $y_{1:t-1}$);
\item $Y_t=F_t\theta_t+v_t$ (the observation equation).
\end{itemize}

A linear combination of \emph{independent Gaussians} is Gaussian, so $Y_t\mid y_{1:t-1}$ is Gaussian. Its first two moments follow by linearity:

\emph{Mean.}
\[
f_t=\mathbb{E}[Y_t\mid y_{1:t-1}]=F_t\,\mathbb{E}[\theta_t\mid y_{1:t-1}]+\underbrace{\mathbb{E}[v_t\mid y_{1:t-1}]}_{=0}=F_t a_t.
\]

\emph{Variance.}
\[
Q_t=\operatorname{Var}(Y_t\mid y_{1:t-1})=F_t\operatorname{Var}(\theta_t\mid y_{1:t-1})F_t'+\operatorname{Var}(v_t\mid y_{1:t-1})+2F_t\underbrace{\operatorname{Cov}(\theta_t,v_t\mid y_{1:t-1})}_{=0}=F_t R_t F_t'+V_t.
\]

The cross-covariance term vanishes because $v_t\perp\theta_t\mid y_{1:t-1}$. Combining,
\[
\boxed{\;Y_t\mid y_{1:t-1}\sim\mathcal{N}_q\bigl(F_t a_t,\;F_t R_t F_t'+V_t\bigr)=\mathcal{N}_q(f_t,Q_t).\;}\qquad\square
\]

\textbf{5. Univariate special case (random walk plus noise).}

Take $q=p=1$, $F_t=G_t=1$, $V_t=V$, $W_t=W$. Then $a_t=m_{t-1}$, $R_t=C_{t-1}+W$, and
\[
f_t=m_{t-1},\qquad Q_t=R_t+V=C_{t-1}+W+V.
\]
So $Y_t\mid y_{1:t-1}\sim\mathcal{N}(m_{t-1},C_{t-1}+W+V)$: the one-step point forecast equals the previous filtered mean; the predictive variance is the sum of \emph{posterior uncertainty} $C_{t-1}$, fresh \emph{state noise} $W$, and \emph{observation noise} $V$.

\textbf{6. Geometric reading.}

$F_t a_t$ projects the state forecast into the observation space (data scale). $F_t R_t F_t'$ propagates the state uncertainty through the same linear map; $V_t$ adds the measurement-error component. The two variance pieces are additive precisely because they originate from \emph{independent} sources ($\theta_t$ and $v_t$). This decomposition is what later powers the \emph{Kalman gain} $K_t=R_t F_t' Q_t^{-1}$: gain large when $V_t$ dominates (data informative) or small when state uncertainty $R_t$ is small.

\textbf{7. Why "is the DLM only univariate?" --- answer NO.}

A common multiple-choice trap on exams: \emph{"DLMs are only defined for univariate time series."} The answer is \textbf{NO}: $Y_t\in\mathbb{R}^q$ for any $q$, and $F_t$ is the matrix that links a $p$-dimensional latent state to a $q$-dimensional observation. The derivation above uses $F_t$ as a matrix throughout — no scalar simplification was needed. Examples: multivariate financial DLM (asset prices), multivariate local-level, seasonal multivariate dynamic regression.

\textbf{8. Worked micro-example --- bivariate $F$, scalar $\theta$.}

Let $p=1$, $q=2$, $F_t=\binom{1}{1}$, $G_t=1$, $V_t=I_2$, $W_t=1$. Then with $\theta_t\mid y_{1:t-1}\sim\mathcal{N}(2,3)$ (so $a_t=2,R_t=3$):
\[
f_t=F_t a_t=\binom{2}{2},\qquad Q_t=F_t R_t F_t'+V_t=\binom{1}{1}\!\cdot 3\cdot(1,1)+I_2=\begin{pmatrix}3&3\\3&3\end{pmatrix}+\begin{pmatrix}1&0\\0&1\end{pmatrix}=\begin{pmatrix}4&3\\3&4\end{pmatrix}.
\]
The two components of $Y_t$ are predicted equal in mean (because $F$ is constant), positively correlated (shared state), each with marginal variance $4$.

\textbf{9. R --- one-step-ahead predictive via \texttt{dlm}.}

```R
library(dlm)
# Random walk plus noise (local level)
mod <- dlmModPoly(order = 1, dV = 1.0, dW = 0.5, m0 = 0, C0 = 1e7)
y   <- as.numeric(Nile)
kf  <- dlmFilter(y, mod)
# Filtered means/variances of theta_t
m_t <- dropFirst(kf$m)
C_t <- sapply(dropFirst(kf$U.C), function(u) u^2) * dropFirst(kf$D.C)^2
# One-step-ahead predictive of Y_{t+1} given y_{1:t}
f_next <- tail(kf$f, 1)            ## f_t = F * a_t
Q_next <- tail(kf$U.R, 1)[[1]]^2 * tail(kf$D.R, 1)^2 + 1.0  ## R_t + V
c(f_next, sqrt(Q_next))            ## point forecast and SE
```

\textbf{10. Pitfalls.}

\begin{itemize}
\item Confusing $a_t,R_t$ (state predictive) with $f_t,Q_t$ (observation predictive). They live in different spaces ($p$ vs $q$) and serve different roles.
\item Forgetting $V_t$ in $Q_t$ — only true if the observation is noiseless ($V_t=0$).
\item In the univariate scalar derivation, writing $Q_t=R_t$ — missing the $+V$ measurement noise.
\end{itemize}

\textbf{Exam pointers.}
\begin{itemize}
\item[$\triangleright$] \texttt{exam\_jun\_2024\_q6}: (a) "DLMs only univariate?" $\Rightarrow$ \textbf{NO}; (b) derive $Y_t\mid y_{1:t-1}\sim\mathcal{N}_q(F_t a_t,F_t R_t F_t'+V_t)=\mathcal{N}_q(f_t,Q_t)$ exactly as in step 4 above.
\end{itemize}
""",
}


# =============================================================================
# t10b — Forecast function, k-step intervals, SES & loss functions
# =============================================================================
theory_content_ts["t10b"] = {
    "title": "Theory — Forecast function, k-step intervals, SES & loss functions",
    "content": r"""\textbf{\textcolor{red}{THEORY --- Forecast function, $k$-step intervals, SES \& loss functions [Topic: T10 --- One-step-ahead prediction \& forecast function]}}

\textbf{1. Definitions.}

\boxed{\;\text{Forecast function: }\hat y_{t+k\mid t}=\mathbb{E}[Y_{t+k}\mid y_{1:t}]\text{ for }k=1,2,\dots\;}

\boxed{\;k\text{-step state forecast: }a_t(k)=\mathbb{E}[\theta_{t+k}\mid y_{1:t}];\quad R_t(k)=\operatorname{Var}(\theta_{t+k}\mid y_{1:t}).\;}

The $k$-step \emph{observation forecast} is $f_t(k)=\mathbb{E}[Y_{t+k}\mid y_{1:t}]=F_{t+k}a_t(k)$; predictive variance $Q_t(k)=F_{t+k}R_t(k)F_{t+k}'+V_{t+k}$.

\textbf{2. $k$-step recursions in a DLM.}

Iterating the state equation from time $t$ forward, $\theta_{t+k}=G_{t+k}\theta_{t+k-1}+w_{t+k}$, take conditional expectation given $y_{1:t}$:
\[
a_t(k)=G_{t+k}a_t(k-1),\qquad a_t(0)=m_t.
\]
With time-homogeneous $G_t\equiv G$: $a_t(k)=G^k m_t$.
Conditional variance:
\[
R_t(k)=G_{t+k}R_t(k-1)G_{t+k}'+W_{t+k},\qquad R_t(0)=C_t.
\]
For homogeneous $(G,W)$: $R_t(k)=G^k C_t (G')^k+\sum_{j=1}^k G^{k-j}W(G')^{k-j}$.

\textbf{3. Random walk plus noise --- closed form.}

$F=G=1$, $V_t=V$, $W_t=W$. Then $a_t(k)=m_t$, $R_t(k)=C_t+kW$, and
\[
\boxed{\;Y_{t+k}\mid y_{1:t}\sim\mathcal{N}(m_t,\,C_t+kW+V).\;}
\]

\emph{Two key consequences.}

(i) \emph{Flat forecast function}: $\hat y_{t+k\mid t}=m_t$ for every $k\ge 1$. The local-level DLM has no memory of trend or seasonality, so the best forecast is the last filtered level.

(ii) \emph{Fan-out variance}: predictive standard deviation grows like $\sqrt{C_t+kW+V}$ — linearly fanning out in $k$ via the cumulative state noise.

\textbf{4. $(1-\alpha)$ credible / prediction intervals.}

By Gaussianity of the predictive, a $(1-\alpha)$ central interval for $Y_{t+k}\mid y_{1:t}$ is
\[
\boxed{\;\hat y_{t+k\mid t}\;\pm\;z_{1-\alpha/2}\sqrt{Q_t(k)},\;}
\]
where $z_{1-\alpha/2}$ is the standard normal quantile. For RW + noise:
\[
m_t\pm z_{1-\alpha/2}\sqrt{C_t+kW+V}.
\]

\emph{Example $k=2$.} $m_t\pm z_{1-\alpha/2}\sqrt{C_t+2W+V}$. For $\alpha=0.05$, $z=1.96$; for $\alpha=0.10$, $z=1.65$.

\textbf{5. Loss functions and Bayes point forecasts.}

A \emph{point forecast} $\hat y_{t+k}$ is chosen to minimise expected loss $\mathbb{E}[L(Y_{t+k},\hat y_{t+k})\mid y_{1:t}]$. Three canonical choices:

\boxed{\;\text{Quadratic loss }L(y,\hat y)=(y-\hat y)^2\;\Rightarrow\;\hat y^{\text{Bayes}}=\mathbb{E}[Y_{t+k}\mid y_{1:t}]=f_t(k).\;}

\emph{Proof.} $\mathbb{E}[(Y-\hat y)^2\mid y_{1:t}]$ in $\hat y$: differentiating, $-2\mathbb{E}[Y-\hat y\mid y_{1:t}]=0\Rightarrow\hat y=\mathbb{E}[Y\mid y_{1:t}]$. $\square$

\boxed{\;\text{Absolute loss }L(y,\hat y)=|y-\hat y|\;\Rightarrow\;\hat y^{\text{Bayes}}=\operatorname{median}(Y_{t+k}\mid y_{1:t}).\;}

\emph{Proof.} Standard quantile-loss minimisation: $\partial_{\hat y}\int|y-\hat y|p(y)dy=\mathbb{P}(Y\le\hat y)-\mathbb{P}(Y>\hat y)=0\Leftrightarrow\hat y=$ median. $\square$

\boxed{\;\text{0-1 loss }L(y,\hat y)=\mathbb{1}\{y\neq\hat y\}\;\Rightarrow\;\hat y^{\text{Bayes}}=\operatorname{mode}(p(y_{t+k}\mid y_{1:t})).\;}

\emph{Gaussian collapse.} If $Y_{t+k}\mid y_{1:t}\sim\mathcal{N}(\mu,\sigma^2)$ is symmetric and unimodal, then $\operatorname{mean}=\operatorname{median}=\operatorname{mode}=\mu$: \emph{all three loss functions give the same point forecast} $\mu=f_t(k)$. The three procedures only diverge under asymmetric / multimodal predictives (e.g.\ non-Gaussian SSMs).

\textbf{6. Simple Exponential Smoothing (SES) --- the algorithm.}

SES recursively updates a level estimate as data arrive:
\[
\boxed{\;\hat y_{t+1\mid t}=\alpha y_t+(1-\alpha)\hat y_{t\mid t-1},\quad \alpha\in(0,1),\;}
\]
equivalently $\hat y_{t+1\mid t}=\sum_{j=0}^{t-1}\alpha(1-\alpha)^j y_{t-j}+(1-\alpha)^t\hat y_{1\mid 0}$ — an exponentially weighted moving average. Recent data carry weight $\alpha$, older data are damped geometrically.

\textbf{7. SES as the steady-state KF of the local-level DLM.}

The RW+noise filter $m_t=m_{t-1}+K_t(y_t-m_{t-1})$ with $K_t=R_t/(R_t+V)=(C_{t-1}+W)/(C_{t-1}+W+V)$. As $t\to\infty$, $C_t\to C^\star$ (Riccati fixed point), so $K_t\to K^\star=(C^\star+W)/(C^\star+W+V)\in(0,1)$. The recursion in the limit becomes
\[
m_t=K^\star y_t+(1-K^\star)m_{t-1},
\]
which is \textbf{exactly} SES with $\alpha=K^\star$. So SES is the asymptotic / steady-state Kalman filter of the local-level DLM.

\boxed{\;\text{SES}\equiv\text{steady-state KF of RW+noise; }\alpha=K^\star=\frac{C^\star+W}{C^\star+W+V}.\;}

\textbf{8. SES uncertainty quantification?}

\textbf{NO} as a standalone algorithm: SES is a recursion that returns \emph{only} a point forecast — no variance, no interval. \emph{However}, embedding SES in its underlying RW+noise DLM gives full predictive distributions $Y_{t+1}\mid y_{1:t}\sim\mathcal{N}(\hat y_{t+1\mid t},Q_{t+1})$ with $Q_{t+1}=C_t+W+V$. The DLM \emph{has} the intervals; the bare SES algorithm does not.

This is why exam answers say: SES alone gives no uncertainty quantification, but the DLM formulation provides $(1-\alpha)$ predictive intervals via $\hat y_{t+1\mid t}\pm z_{1-\alpha/2}\sqrt{Q_{t+1}}$.

\textbf{9. Worked micro-example --- $k=2$ forecast and 95\% CI.}

RW+noise with $V=1$, $W=0.5$, and after filtering $m_t=10$, $C_t=0.8$. Then
\[
\hat y_{t+2\mid t}=m_t=10;\qquad Q_t(2)=C_t+2W+V=0.8+1+1=2.8;\qquad\text{SE}=\sqrt{2.8}\approx 1.673.
\]
95\% predictive interval: $10\pm 1.96\cdot 1.673=10\pm 3.28=[6.72,\,13.28]$.

\textbf{10. R --- $k$-step forecasts and intervals.}

```R
library(dlm)
y   <- as.numeric(Nile)
mod <- dlmModPoly(order = 1, dV = 15100, dW = 1469)
kf  <- dlmFilter(y, mod)
fc  <- dlmForecast(kf, nAhead = 10)        ## k-step predictive
mu  <- as.numeric(fc$f)                    ## point forecasts f_t(k)
Qk  <- as.numeric(fc$Q)                    ## predictive variances Q_t(k)
lo  <- mu - 1.96 * sqrt(Qk)
hi  <- mu + 1.96 * sqrt(Qk)
cbind(k = 1:10, mu, sd = sqrt(Qk), lo, hi)
```

For SES specifically (no DLM wrapper):
```R
ses <- HoltWinters(y, beta = FALSE, gamma = FALSE)   ## SES
predict(ses, n.ahead = 5, prediction.interval = TRUE, level = 0.95)
## prediction.interval=TRUE uses the underlying state-space DLM, not SES alone
```

\textbf{11. Pitfalls.}

\begin{itemize}
\item In RW+noise, the forecast function is \emph{flat} — students sometimes try to extrapolate a trend that the model does not contain.
\item Confusing $R_t(k)$ (state) with $Q_t(k)$ (observation); only $Q_t(k)=R_t(k)+V$ gives the data-scale interval.
\item Claiming SES provides intervals "by itself" — it does not. The DLM behind it does.
\item Under quadratic vs.\ absolute loss with a \emph{Gaussian} predictive, both give the same point forecast (mean $=$ median); only non-Gaussian / skewed predictives separate them.
\end{itemize}

\textbf{Exam pointers.}
\begin{itemize}
\item[$\triangleright$] \texttt{exam\_jun\_2024\_q5}: RW+noise; (a) signal-to-noise ratio $\kappa=W/V$; (b) $Y_{t+k}\mid y_{1:t}\sim\mathcal{N}(m_t,C_t+kW+V)$, forecast function flat at $m_t$; $k=2$ CI: $m_t\pm z_{1-\alpha/2}\sqrt{C_t+2W+V}$.
\item[$\triangleright$] \texttt{exam\_jun\_2022\_q4}: RW+noise; $f_t=a_t=m_{t-1}$, $Q_t=R_t+V=C_{t-1}+W+V$; quadratic-loss point forecast = mean = $f_t$; absolute-loss point forecast = median = $f_t$ (Gaussian symmetry).
\item[$\triangleright$] \texttt{exam\_may\_2022\_q1}: SES standalone provides \textbf{no} uncertainty quantification; only the underlying local-level DLM does, giving $\mathcal{N}(\hat y_{t+1\mid t},Q_{t+1})$ with $Q_{t+1}=C_t+W+V$.
\end{itemize}
""",
}


# =============================================================================
# t11a — Innovations: zero-mean, orthogonality, standardisation
# =============================================================================
theory_content_ts["t11a"] = {
    "title": "Theory — Innovations: zero-mean, orthogonality, standardisation",
    "content": r"""\textbf{\textcolor{red}{THEORY --- Innovations: zero-mean, orthogonality, standardisation [Topic: T11 --- Forecast errors / innovations \& model checking]}}

\textbf{1. Definition --- forecast error / innovation.}

\boxed{\;e_t=Y_t-f_t,\quad f_t=\mathbb{E}[Y_t\mid y_{1:t-1}],\;t\ge 1.\;}

In a DLM with known parameters, $f_t=F_t a_t$ is the one-step-ahead point forecast and $e_t$ is the \emph{innovation}: the part of $Y_t$ that could not have been anticipated from the past $y_{1:t-1}$. The companion variance is
\[
Q_t=\operatorname{Var}(Y_t\mid y_{1:t-1})=F_t R_t F_t'+V_t,
\]
giving $e_t\mid y_{1:t-1}\sim\mathcal{N}_q(0,Q_t)$.

Let $\mathcal F_{t-1}=\sigma(y_{1:t-1})$ denote the past sigma-algebra.

\textbf{2. Property A --- zero mean.}

\boxed{\;\mathbb{E}[e_t]=0\text{ for every }t.\;}

\emph{Proof.} Using the tower property of conditional expectation,
\[
\mathbb{E}[e_t]=\mathbb{E}\bigl[\mathbb{E}[Y_t-f_t\mid\mathcal F_{t-1}]\bigr]=\mathbb{E}[f_t-f_t]=\mathbb{E}[0]=0.
\]
Equivalently, $f_t$ is the conditional mean of $Y_t$, so by construction $\mathbb{E}[Y_t\mid\mathcal F_{t-1}]-f_t=0$ a.s.; taking unconditional expectation preserves zero. $\square$

\textbf{3. Property B --- orthogonality across time.}

\boxed{\;\operatorname{Cov}(e_t,e_s)=0\text{ for every }t\ne s.\;}

\emph{Proof.} Take $t>s$. Then $e_s$ is $\mathcal F_{t-1}$-measurable (since $s\le t-1$ and $e_s$ is a function of $Y_{1:s},f_s$, both $\mathcal F_{s}\subseteq\mathcal F_{t-1}$-measurable). Apply tower + pull-out (conditional expectation of an $\mathcal F_{t-1}$-measurable function commutes with multiplication):
\[
\mathbb{E}[e_t e_s]=\mathbb{E}\bigl[\mathbb{E}[e_t e_s\mid\mathcal F_{t-1}]\bigr]=\mathbb{E}\bigl[e_s\cdot\underbrace{\mathbb{E}[e_t\mid\mathcal F_{t-1}]}_{=0}\bigr]=0.
\]
Since $\mathbb{E}[e_t]=\mathbb{E}[e_s]=0$, $\operatorname{Cov}(e_t,e_s)=\mathbb{E}[e_t e_s]=0$. $\square$

\emph{Martingale-difference interpretation.} $(e_t,\mathcal F_t)$ is a \emph{martingale-difference sequence} (MDS): $\mathbb{E}[e_t\mid\mathcal F_{t-1}]=0$. MDS automatically have uncorrelated terms.

\textbf{4. Property C --- conditional Gaussianity (DLM).}

In a (Gaussian) DLM with no unknown parameters,
\[
\boxed{\;e_t\mid\mathcal F_{t-1}\sim\mathcal{N}_q(0,Q_t),\;}
\]
because $Y_t\mid\mathcal F_{t-1}$ is Gaussian (proved in t10a) and $f_t$ is $\mathcal F_{t-1}$-measurable; subtracting the conditional mean keeps Gaussianity. \textbf{Caveat.} $Q_t$ depends on $t$ (it is computed from the KF recursion). So while each $e_t$ marginally has $\operatorname{Var}(e_t)=\mathbb{E}[Q_t]$ (need not be constant), the conditional variance $Q_t$ is the right quantity for inference.

\textbf{5. Property D --- standardised innovations are i.i.d.\ $\mathcal{N}(0,1)$.}

\boxed{\;\tilde e_t=Q_t^{-1/2}e_t\overset{\text{iid}}{\sim}\mathcal{N}_q(0,I_q)\quad\text{under correct DLM specification.}\;}

\emph{Sketch.} (i) $\tilde e_t\mid\mathcal F_{t-1}\sim\mathcal{N}_q(0,I_q)$ by the conditional Gaussian property above. (ii) Since this conditional law does \emph{not} depend on $\mathcal F_{t-1}$, $\tilde e_t$ is independent of $\mathcal F_{t-1}$; in particular independent of $\tilde e_{1:t-1}$ (since those are $\mathcal F_{t-1}$-measurable). (iii) Marginally each $\tilde e_t\sim\mathcal{N}_q(0,I_q)$. Combining, $\{\tilde e_t\}$ is an i.i.d.\ $\mathcal{N}_q(0,I_q)$ sequence. $\square$

\textbf{6. Why the unstandardised $e_t$ are \emph{not} $\mathcal{N}(0,1)$.}

A common exam trap: claiming $e_t\overset{\text{iid}}{\sim}\mathcal{N}(0,1)$. This is \textbf{wrong} on two counts:
\begin{itemize}
\item \emph{Variance not unit:} $\operatorname{Var}(e_t)\ne 1$ in general — it equals $Q_t$, which depends on $t$ and on the DLM dynamics. E.g.\ in RW+noise, $Q_t=C_{t-1}+W+V$.
\item \emph{Not identically distributed:} $Q_t$ varies with $t$ in transient regimes (only reaches a steady-state $Q^\star=C^\star+W+V$ in the limit).
\end{itemize}
The correct statement is on the \emph{standardised} innovations $\tilde e_t=Q_t^{-1/2}e_t$.

\textbf{7. Practical impact --- prediction-error decomposition of the likelihood.}

Because the innovations are independent (under correct specification),
\[
p(y_{1:n}\mid\phi)=\prod_{t=1}^n p(y_t\mid y_{1:t-1},\phi)=\prod_{t=1}^n\mathcal{N}_q\bigl(y_t;f_t(\phi),Q_t(\phi)\bigr),
\]
so the log-likelihood is
\[
\ell(\phi)=-\tfrac12\sum_{t=1}^n\bigl[q\log 2\pi+\log|Q_t(\phi)|+e_t(\phi)'Q_t(\phi)^{-1}e_t(\phi)\bigr].
\]
This \emph{prediction-error decomposition} (DLMwR \S 4.1) is the workhorse for MLE in DLMs.

\textbf{8. Model-checking diagnostics built on $\tilde e_t$.}

Under correct specification, the standardised innovations behave like i.i.d.\ $\mathcal{N}(0,1)$ noise. Diagnostics:
\begin{itemize}
\item \emph{Normality.} QQ-plot of $\tilde e_t$ against $\mathcal{N}(0,1)$; Shapiro--Wilk; Jarque--Bera.
\item \emph{Lack of autocorrelation.} ACF / PACF of $\tilde e_t$; Ljung--Box portmanteau test on the first $H$ lags:
\[
Q^{\text{LB}}=n(n+2)\sum_{h=1}^H\frac{\hat\rho^2_h}{n-h}\;\xrightarrow{d}\;\chi^2_H\quad\text{under }H_0:\text{no autocorrelation.}
\]
\item \emph{Homoscedasticity.} Plot $\tilde e_t$ against time and against $f_t$.
\item \emph{No conditional dependence in second moments.} ACF of $\tilde e_t^2$; ARCH/McLeod--Li tests on $\tilde e_t^2$.
\end{itemize}
If any diagnostic flags structure, the DLM is misspecified (missing trend, seasonality, ARCH effects, etc.).

\textbf{9. Worked micro-example --- RW+noise.}

$V=1$, $W=0.5$, $m_0=0$, $C_0=10$. After 1 step: $f_1=m_0=0$, $Q_1=C_0+W+V=11.5$. If observed $y_1=2$: $e_1=2-0=2$, $\tilde e_1=2/\sqrt{11.5}\approx 0.59$. The unstandardised error has SD $\sqrt{11.5}\approx 3.39$ (\emph{not} 1); the standardised one is $\sim\mathcal{N}(0,1)$.

\textbf{10. R --- diagnostic suite.}

```R
library(dlm)
y   <- as.numeric(Nile)
mod <- dlmModPoly(order = 1, dV = 15100, dW = 1469)
kf  <- dlmFilter(y, mod)

# Raw and standardised innovations
e_t       <- residuals(kf, type = "raw",        sd = FALSE)   ## y_t - f_t
e_t_std   <- residuals(kf, type = "standardized", sd = FALSE) ## (y_t - f_t)/sqrt(Q_t)
mean(e_t_std);  var(e_t_std)             ## should be ~0 and ~1

# Diagnostics
qqnorm(e_t_std); qqline(e_t_std)         ## normality
acf(e_t_std)                             ## should be inside CI bands
Box.test(e_t_std, lag = 10, type = "Ljung-Box")
acf(e_t_std^2)                           ## check for residual heteroscedasticity
```

\textbf{Exam pointers.}
\begin{itemize}
\item[$\triangleright$] \texttt{exam\_jun\_2025\_q5}: prove $\mathbb{E}[e_t]=0$. Use tower property: $\mathbb{E}[e_t]=\mathbb{E}[Y_t-\mathbb{E}[Y_t\mid\mathcal F_{t-1}]]=\mathbb{E}[Y_t]-\mathbb{E}[Y_t]=0$. Mention that this is the foundation of the prediction-error likelihood.
\item[$\triangleright$] \texttt{exam\_may\_2024\_q6}: "$e_t\overset{\text{iid}}{\sim}\mathcal{N}(0,1)$?" $\Rightarrow$ \textbf{NO}. Innovations are zero-mean, uncorrelated, conditionally Gaussian with variance $Q_t$ depending on $t$; only the \emph{standardised} innovations $\tilde e_t=Q_t^{-1/2}e_t$ are i.i.d.\ $\mathcal{N}(0,1)$.
\item[$\triangleright$] \texttt{exam\_may\_2021\_q6}: prove $\mathbb{E}[e_t]=0$ and $\operatorname{Cov}(e_t,e_s)=0$ for $t>s$. Use tower property for the mean; use $e_s\in\mathcal F_{t-1}$ + pull-out + zero conditional mean for the orthogonality. State the MDS interpretation and the diagnostic uses (QQ, Ljung--Box on $\tilde e_t$).
\end{itemize}
""",
}


# =============================================================================
# t12a — Likelihood of phi via prediction-error decomposition (MLE)
# =============================================================================
theory_content_ts["t12a"] = {
    "title": "Theory — Likelihood of phi via prediction-error decomposition (MLE)",
    "content": r"""\textbf{\textcolor{red}{THEORY --- Likelihood of $\phi$ via prediction-error decomposition (MLE) [Topic: T12 --- Parameter estimation in DLM --- MLE \& prediction-error decomp.]}}

\textbf{1. Setup --- DLM parameterised by $\phi$.}

\boxed{\;\theta_t=G_t(\phi)\theta_{t-1}+w_t,\;w_t\sim\mathcal{N}_p(0,W_t(\phi));\quad Y_t=F_t(\phi)\theta_t+v_t,\;v_t\sim\mathcal{N}_q(0,V_t(\phi));\;\theta_0\sim\mathcal{N}_p(m_0,C_0).\;}

The unknown $\phi$ may enter any of $G_t,W_t,F_t,V_t$ (e.g.\ in RW+noise $\phi=(V,W)$). The Gaussian and conditional-independence structure is preserved for every $\phi$.

\textbf{2. The naïve likelihood is intractable.}

The joint density of the observations factorises as
\[
p(y_{1:n}\mid\phi)=\int p(y_{1:n}\mid\theta_{0:n},\phi)\,p(\theta_{0:n}\mid\phi)\,d\theta_{0:n},
\]
an $(n+1)p$-dimensional integral. Direct evaluation is infeasible for moderate $n$.

\textbf{3. Prediction-error decomposition.}

Use the chain rule of probability (no integral required):
\[
p(y_{1:n}\mid\phi)=\prod_{t=1}^n p(y_t\mid y_{1:t-1},\phi).
\]
Each factor is the \emph{one-step-ahead predictive} of the DLM. From t10a, under Gaussian DLM,
\[
\boxed{\;Y_t\mid y_{1:t-1},\phi\sim\mathcal{N}_q\bigl(f_t(\phi),\,Q_t(\phi)\bigr),\;}
\]
with $f_t(\phi)=F_t a_t(\phi)$, $Q_t(\phi)=F_t R_t(\phi)F_t'+V_t$, both available from one pass of the Kalman filter at parameter value $\phi$.

\boxed{\;p(y_{1:n}\mid\phi)=\prod_{t=1}^n\mathcal{N}_q\bigl(y_t;\,f_t(\phi),Q_t(\phi)\bigr).\;}

(DLMwR eq.\ 4.1.) This is called the \emph{prediction-error decomposition} because the building blocks $e_t=y_t-f_t$ are the one-step forecast errors.

\textbf{4. Log-likelihood --- explicit form.}

\boxed{\;\ell(\phi)=-\tfrac{1}{2}\sum_{t=1}^n\Bigl[q\log(2\pi)+\log|Q_t(\phi)|+e_t(\phi)'Q_t(\phi)^{-1}e_t(\phi)\Bigr],\quad e_t(\phi)=y_t-f_t(\phi).\;}

The constant $q\log(2\pi)$ is usually dropped in optimisation.

\textbf{5. MLE algorithm.}

\boxed{\;\widehat\phi=\arg\max_{\phi\in\Phi}\ell(\phi).\;}

\emph{Numerical recipe:}
\begin{enumerate}
\item Choose a parameterisation that respects positivity / PSD constraints (e.g.\ $\log V$ for variances; Cholesky factors for covariance matrices).
\item For each candidate $\phi$: run KF $\to$ get $\{f_t(\phi),Q_t(\phi)\}_{t=1}^n$ $\to$ compute $\ell(\phi)$.
\item Optimise with a quasi-Newton method (BFGS, L-BFGS-B). Each function evaluation = one filter pass, $O(np^3)$.
\item Repeat from multiple random starts (the likelihood is generally non-convex; identifiability of $(V,W)$ near $V\!:\!W\!=\!0$ boundaries can create ridges).
\end{enumerate}

\emph{Standard errors.} Either (i) inverse observed information $\widehat\Sigma_{\widehat\phi}=\bigl(-\nabla^2\ell(\widehat\phi)\bigr)^{-1}$ obtained from the optimiser's Hessian, or (ii) parametric bootstrap.

\textbf{6. Step-by-step justification of the decomposition.}

Why is $p(y_{1:n}\mid\phi)=\prod_t p(y_t\mid y_{1:t-1},\phi)$ Gaussian factor-by-factor? Two ingredients:

(i) Chain rule (always holds, no model assumption): $p(y_{1:n})=p(y_1)p(y_2\mid y_1)\cdots p(y_n\mid y_{1:n-1})$.

(ii) The DLM gives \emph{closed-form Gaussian} predictives: for each $t$, the linear-Gaussian filter produces $\theta_t\mid y_{1:t-1}\sim\mathcal{N}_p(a_t,R_t)$ and hence $Y_t\mid y_{1:t-1}\sim\mathcal{N}_q(F_t a_t,F_tR_tF_t'+V_t)$ (t10a). Each factor is tractable.

Combining, the likelihood is a product of $n$ Gaussian densities, each parameterised by $(f_t(\phi),Q_t(\phi))$ obtained from one KF run at $\phi$. The $(n+1)p$-dimensional integral over $\theta_{0:n}$ has been replaced by an iterative recursion --- the KF is, fundamentally, a way to integrate out the latent state on the fly.

\textbf{7. Frequentist predictive (plug-in).}

After MLE,
\[
\boxed{\;Y_{t+1}\mid y_{1:t},\widehat\phi\sim\mathcal{N}_q\bigl(f_{t+1}(\widehat\phi),Q_{t+1}(\widehat\phi)\bigr).\;}
\]
This is the \emph{empirical Bayes} / plug-in predictive. \textbf{Caveat.} It \emph{ignores parameter uncertainty}: intervals are too narrow, especially for small $n$ or weakly identified parameters. The Bayesian approach (t13b) fixes this by integrating over $p(\phi\mid y_{1:t})$.

\textbf{8. Bayesian prior predictive (a.k.a.\ evidence).}

If instead $\phi$ is treated as random with prior $\pi(\phi)$, the joint \emph{prior predictive} of the data is
\[
\boxed{\;p(y_{1:n})=\int p(y_{1:n}\mid\phi)\,\pi(\phi)\,d\phi=\int\!\Bigl(\prod_t\mathcal{N}_q(y_t;f_t(\phi),Q_t(\phi))\Bigr)\pi(\phi)\,d\phi,\;}
\]
which \emph{does not depend on $\phi$} (it is integrated out). This is the \emph{evidence} (a.k.a.\ marginal likelihood). It is typically intractable; used for Bayes factors when model comparison is needed. Approximations: Laplace, bridge sampling, nested sampling.

\boxed{\;\text{Frequentist }p(y_{1:n}\mid\phi)\text{ depends on }\phi;\text{ Bayesian }p(y_{1:n})\text{ does not.}\;}

\textbf{9. Worked micro-example --- RW+noise, $n=2$.}

Suppose $\phi=(V,W)$, $m_0=0$, $C_0=10$. Data $(y_1,y_2)$.

Step $t=1$: $a_1=0,R_1=C_0+W=10+W$; $f_1=0,Q_1=R_1+V=10+W+V$; $e_1=y_1$. After update: $K_1=R_1/Q_1$, $m_1=K_1 y_1$, $C_1=R_1-K_1^2 Q_1=V R_1/Q_1$.

Step $t=2$: $a_2=m_1,R_2=C_1+W$; $f_2=m_1,Q_2=C_1+W+V$; $e_2=y_2-m_1$.

$\ell(V,W)=-\tfrac12\bigl[\log Q_1+y_1^2/Q_1+\log Q_2+(y_2-m_1)^2/Q_2\bigr]+$ const. Even this tiny example shows nonlinear dependence of $(m_1,Q_t)$ on $(V,W)$ via the recursions — closed-form MLE is rare; numerical optimisation is the rule.

\textbf{10. R --- MLE via \texttt{dlmMLE}.}

```R
library(dlm)
y <- as.numeric(Nile)

# Build the DLM as a function of phi = (log V, log W)
build <- function(phi) {
  dlmModPoly(order = 1, dV = exp(phi[1]), dW = exp(phi[2]),
             m0 = 0, C0 = 1e7)
}

# MLE by numerical optimisation (BFGS); each evaluation = one KF pass
fit <- dlmMLE(y, parm = c(log(100), log(10)), build = build,
              hessian = TRUE)
fit$convergence            ## 0 = converged
phi_hat <- fit$par
V_hat   <- exp(phi_hat[1]) ## observation variance
W_hat   <- exp(phi_hat[2]) ## state variance
c(V_hat, W_hat)

# Asymptotic SEs from observed information
Sigma_phi <- solve(fit$hessian)
sqrt(diag(Sigma_phi))      ## SEs on the log scale

# Plug-in predictive at phi_hat
mod_hat <- build(phi_hat)
kf_hat  <- dlmFilter(y, mod_hat)
fc      <- dlmForecast(kf_hat, nAhead = 5)
data.frame(f = as.numeric(fc$f),
           lo95 = as.numeric(fc$f) - 1.96 * sqrt(as.numeric(fc$Q)),
           hi95 = as.numeric(fc$f) + 1.96 * sqrt(as.numeric(fc$Q)))
```

\textbf{11. Pitfalls.}

\begin{itemize}
\item \emph{Forgetting to log-transform variances:} unconstrained optimisers may step into $V<0$. Always parameterise via $\log$ (or softplus) for variances.
\item \emph{Local maxima:} the RW+noise likelihood often has a near-zero $W$ local max. Try multiple starts.
\item \emph{Plug-in intervals are too narrow:} report this as a caveat. For honest UQ either profile-likelihood, parametric bootstrap, or full Bayes.
\item \emph{Confusing prior predictive vs.\ likelihood:} frequentist likelihood depends on $\phi$; Bayesian prior predictive integrates $\phi$ out.
\end{itemize}

\textbf{Exam pointers.}
\begin{itemize}
\item[$\triangleright$] \texttt{exam\_jun\_2024\_q7}: (a) Frequentist $p(y_{1:n}\mid\phi)=\prod_t\mathcal{N}_q(y_t;f_t(\phi),Q_t(\phi))$, \emph{depends on $\phi$}; (b) Bayesian $p(y_{1:n})=\int p(y_{1:n}\mid\phi)\pi(\phi)d\phi$, \emph{does not depend on $\phi$} (integrated out). Note this is the evidence used in Bayes factors.
\item[$\triangleright$] \texttt{exam\_may\_2024\_q7}: (a) Write $L(\phi)=\prod_t\mathcal{N}_q(y_t;f_t(\phi),Q_t(\phi))$, MLE by numerical optimisation (BFGS), each evaluation = one KF run; (b) plug-in $Y_t\mid y_{1:t-1},\widehat\phi\sim\mathcal{N}_q(f_t(\widehat\phi),Q_t(\widehat\phi))$, ignores parameter uncertainty (too narrow); (c) Bayesian: prior $\pi(\phi)$, posterior $p(\phi\mid y_{1:t-1})\propto L(\phi)\pi(\phi)$ via MCMC (Gibbs / FFBS), predictive $p(y_t\mid y_{1:t-1})=\int p(y_t\mid y_{1:t-1},\phi)p(\phi\mid y_{1:t-1})d\phi$ --- mixture of Gaussians, properly inflated for $\phi$-uncertainty.
\end{itemize}
""",
}


# =============================================================================
# t13a — Conjugate Normal-Normal posterior (static theta / Case A)
# =============================================================================
theory_content_ts["t13a"] = {
    "title": "Theory — Conjugate Normal-Normal posterior (static theta / Case A)",
    "content": r"""\textbf{\textcolor{red}{THEORY --- Conjugate Normal-Normal posterior (static $\theta$ / Case A) [Topic: T13 --- Bayesian inference, conjugate updates \& MCMC]}}

\textbf{1. Setup --- the static-parameter Gaussian model (Case A).}

\boxed{\;Y_t=\theta+\varepsilon_t,\quad \varepsilon_t\overset{\text{iid}}{\sim}\mathcal{N}(0,V),\;t=1,\dots,n;\quad \theta\sim\mathcal{N}(m_0,C_0)\text{ (prior).}\;}

$V$ is known; $\theta$ is unknown, \emph{static} (does not depend on $t$). This is the \emph{Case A} of the DLMwR Chapter 5 conjugate menu and also the static limit of the RW+noise DLM when the state-evolution variance $W\to 0$ (then $\theta_t=\theta_0\equiv\theta$).

\textbf{2. Question --- what is $p(\theta\mid y_{1:n})$?}

By Bayes' rule,
\[
p(\theta\mid y_{1:n})\propto p(\theta)\,p(y_{1:n}\mid\theta)=\mathcal{N}(\theta;m_0,C_0)\,\prod_{t=1}^n\mathcal{N}(y_t;\theta,V).
\]
Conjugacy (Normal--Normal) implies the posterior is Gaussian.

\textbf{3. Theorem (Normal--Normal conjugate posterior).}

\boxed{\;\theta\mid y_{1:n}\sim\mathcal{N}(m_n,C_n),\quad \frac{1}{C_n}=\frac{1}{C_0}+\frac{n}{V},\quad m_n=C_n\!\left(\frac{m_0}{C_0}+\frac{n\bar y_n}{V}\right),\;}

where $\bar y_n=\tfrac1n\sum_{t=1}^n y_t$ is the sample mean.

\emph{Equivalent closed form:}
\[
C_n=\frac{C_0 V}{V+nC_0},\qquad m_n=\frac{V\,m_0+nC_0\,\bar y_n}{V+nC_0}.
\]

\textbf{4. Derivation by completing the square.}

The log-posterior (up to a constant in $\theta$) is
\[
\log p(\theta\mid y_{1:n})=-\frac{(\theta-m_0)^2}{2C_0}-\sum_{t=1}^n\frac{(y_t-\theta)^2}{2V}+\text{const}.
\]
Expanding:
\[
-\frac{1}{2C_0}\theta^2+\frac{m_0}{C_0}\theta-\sum_t\Bigl[\frac{y_t^2}{2V}-\frac{y_t\theta}{V}+\frac{\theta^2}{2V}\Bigr]+\text{const}.
\]
Collecting powers of $\theta$:
\[
-\frac{1}{2}\Bigl(\underbrace{\frac{1}{C_0}+\frac{n}{V}}_{1/C_n}\Bigr)\theta^2+\Bigl(\underbrace{\frac{m_0}{C_0}+\frac{\sum_t y_t}{V}}_{m_n/C_n}\Bigr)\theta+\text{const}.
\]
This is $-\tfrac{1}{2C_n}(\theta-m_n)^2$ up to a constant, hence
$\theta\mid y_{1:n}\sim\mathcal{N}(m_n,C_n)$ with $1/C_n=1/C_0+n/V$ and $m_n=C_n(m_0/C_0+n\bar y_n/V)$. $\square$

\textbf{5. Interpretation in terms of precisions.}

\boxed{\;\text{Posterior precision }=\text{ prior precision }+\text{ data precision: }\;\tau_n=\tau_0+n\tau_V,\;\tau_0=1/C_0,\;\tau_V=1/V.\;}

\boxed{\;\text{Posterior mean }=\text{ precision-weighted average: }\;m_n=\frac{\tau_0\,m_0+n\tau_V\,\bar y_n}{\tau_0+n\tau_V}.\;}

\emph{Limiting cases.}
\begin{itemize}
\item Flat prior $C_0\to\infty$ (i.e.\ $\tau_0\to 0$): $C_n=V/n$, $m_n=\bar y_n$ — recovers the MLE / sampling distribution mean.
\item Dogmatic prior $C_0\to 0$: $m_n\to m_0$, $C_n\to 0$ — posterior equals prior, data ignored.
\item Large sample $n\to\infty$ with fixed prior: $m_n\to\bar y_n$, $C_n\to V/n\to 0$ — Bernstein--von Mises: posterior concentrates at the truth, dominated by likelihood.
\end{itemize}

\textbf{6. Sequential / recursive form.}

Posteriors update Bayesian-style as data arrive:
\[
\frac{1}{C_{t}}=\frac{1}{C_{t-1}}+\frac{1}{V},\qquad m_t=C_t\!\left(\frac{m_{t-1}}{C_{t-1}}+\frac{y_t}{V}\right),\quad t=1,2,\dots,n,
\]
starting from $(m_0,C_0)$. Equivalently, in Kalman-gain form, with $K_t=C_{t-1}/(C_{t-1}+V)$:
\[
m_t=m_{t-1}+K_t(y_t-m_{t-1}),\qquad C_t=(1-K_t)C_{t-1}=\frac{C_{t-1}V}{C_{t-1}+V}.
\]
This is the KF of the RW+noise DLM \emph{with $W=0$}, confirming Case A = static $\theta$ = RW+noise with zero evolution noise.

\textbf{7. Predictive distribution.}

For a new observation $Y_{n+1}=\theta+\varepsilon_{n+1}$,
\[
\boxed{\;Y_{n+1}\mid y_{1:n}\sim\mathcal{N}(m_n,\;C_n+V).\;}
\]
The variance has two pieces: posterior uncertainty about $\theta$ ($C_n$) plus measurement noise ($V$).

\textbf{8. Worked micro-examples.}

\emph{Example 1 (flat prior, $V=1$, $n=20$, $\bar y_n=4$).} With $C_0\to\infty$, $1/C_n=0+20/1=20$, $C_n=0.05$, $m_n=0.05\cdot 20\cdot 4=4$. So $\theta\mid y_{1:20}\sim\mathcal{N}(4,0.05)$; 95\% credible interval $4\pm 1.96\sqrt{0.05}\approx[3.56,4.44]$.

\emph{Example 2 (informative prior).} $V=1$, $m_0=0$, $C_0=4$, $n=20$, $\bar y_n=4$. Then $1/C_n=1/4+20=20.25$, $C_n\approx 0.0494$; $m_n=0.0494(0/4+20\cdot 4)=0.0494\cdot 80\approx 3.95$. Prior pulls posterior mean slightly toward $m_0=0$. As $n$ grows, the pull vanishes.

\emph{Example 3 (RW+noise with $W=0$).} Setting $W=0$ collapses the state recursion: $\theta_t=\theta_0=\theta$ for all $t$, so $\theta\mid y_{1:n}$ is the same Normal-Normal posterior above. Exam shortcut: "with $W=0$, the local-level filter reduces to the conjugate Normal--Normal update."

\textbf{9. R --- exact conjugate update and simulation check.}

```R
# Inputs
sigma2 <- 1            ## V (known)
m0     <- 0;  C0 <- Inf   ## flat prior
n      <- 20
ybar   <- 4

# Closed-form posterior
Cn <- 1 / (1/C0 + n/sigma2)            ## 0.05
mn <- Cn * (m0/C0 + n*ybar/sigma2)     ## 4
c(mn = mn, Cn = Cn, SD = sqrt(Cn))

# 95% credible interval
mn + c(-1, 1) * qnorm(0.975) * sqrt(Cn)  ## (3.56, 4.44)

# Simulation check: posterior samples
set.seed(1)
ys     <- rnorm(n, mean = 4, sd = sqrt(sigma2))   ## fake data
ybar_s <- mean(ys)
Cn_s   <- 1 / (0 + n/sigma2)
mn_s   <- Cn_s * (n*ybar_s/sigma2)
post   <- rnorm(20000, mn_s, sqrt(Cn_s))
hist(post, prob = TRUE, breaks = 60,
     main = "Posterior of theta | y_{1:n}")
curve(dnorm(x, mn_s, sqrt(Cn_s)), add = TRUE, lwd = 2)
```

For unknown $V$, use the Normal--Inverse-Gamma conjugate pair (DLMwR \S 5.2) — out of scope here.

\textbf{10. Why "Bayesian" matters --- contrast with MLE.}

The MLE of $\theta$ is $\widehat\theta=\bar y_n$, with sampling distribution $\widehat\theta\sim\mathcal{N}(\theta,V/n)$. The Bayes posterior is $\theta\mid y_{1:n}\sim\mathcal{N}(m_n,C_n)$. With a flat prior, $m_n=\bar y_n$ and $C_n=V/n$ — numerically identical. The \emph{interpretation} differs: the Bayesian gives a probability distribution over $\theta$ (a random parameter) given the data; the frequentist gives a confidence statement on the random interval given the fixed $\theta$. With informative priors, the Bayesian also \emph{shrinks} toward $m_0$.

\textbf{Exam pointers.}
\begin{itemize}
\item[$\triangleright$] \texttt{exam\_sep\_2025\_q8}: (a) Joint density $p(y_{1:t},\theta)=\mathcal{N}(\theta;m_0,C_0)\prod_s\mathcal{N}(y_s;\theta,1)$; (b) Conjugate Normal--Normal update with $\sigma^2=1$, $n=20$, $\bar y_n=4$, flat prior $C_0\to\infty$: $1/C_n=20$, $C_n=0.05$, $m_n=4$, so $\theta\mid y_{1:20}\sim\mathcal{N}(4,0.05)$; 95\% CI $\approx[3.56,4.44]$.
\item[$\triangleright$] \texttt{exam\_jun\_2022\_q7}: RW+noise with $W=0$ $\Rightarrow$ $\theta_t=\theta$ static $\Rightarrow$ apply Case A formula: $\theta_n\mid y_{1:n}\sim\mathcal{N}(m_n,C_n)$ with $1/C_n=1/C_0+n/V$, $m_n=C_n(m_0/C_0+n\bar y_n/V)$; closed form $m_n=(Vm_0+nC_0\bar y_n)/(V+nC_0)$, $C_n=C_0V/(V+nC_0)$.
\end{itemize}
""",
}


# =============================================================================
# t13b — Bayesian predictive distribution integrating out phi (+ MCMC)
# =============================================================================
theory_content_ts["t13b"] = {
    "title": "Theory — Bayesian predictive distribution integrating out phi (+ MCMC)",
    "content": r"""\textbf{\textcolor{red}{THEORY --- Bayesian predictive distribution integrating out $\phi$ (+ MCMC) [Topic: T13 --- Bayesian inference, conjugate updates \& MCMC]}}

\textbf{1. Setup --- DLM with unknown parameters $\phi$ and a prior.}

\boxed{\;\theta_t=G_t(\phi)\theta_{t-1}+w_t,\;w_t\sim\mathcal{N}_p(0,W_t(\phi));\quad Y_t=F_t(\phi)\theta_t+v_t,\;v_t\sim\mathcal{N}_q(0,V_t(\phi));\;\phi\sim\pi(\phi).\;}

We seek the \emph{one-step-ahead Bayesian predictive} $p(y_{t+1}\mid y_{1:t})$, which \emph{honestly accounts for parameter uncertainty} by averaging the conditional predictive over the posterior of $\phi$.

\textbf{2. Theorem --- Bayesian predictive (compound mixture).}

\boxed{\;p(y_{t+1}\mid y_{1:t})=\int p(y_{t+1}\mid y_{1:t},\phi)\,p(\phi\mid y_{1:t})\,d\phi.\;}

\emph{Each ingredient.}

(i) For each fixed $\phi$, the DLM one-step-ahead predictive is Gaussian (t10a):
\[
p(y_{t+1}\mid y_{1:t},\phi)=\mathcal{N}_q\bigl(y_{t+1};\,f_{t+1}(\phi),Q_{t+1}(\phi)\bigr),
\]
obtained by running the Kalman filter at parameter $\phi$.

(ii) The posterior of $\phi$ is
\[
p(\phi\mid y_{1:t})\propto L(\phi)\,\pi(\phi),\qquad L(\phi)=\prod_{s=1}^t\mathcal{N}_q\bigl(y_s;f_s(\phi),Q_s(\phi)\bigr)
\]
via the prediction-error likelihood (t12a).

\textbf{3. Derivation.}

Start from the joint of $(y_{t+1},\phi)$ given $y_{1:t}$ and marginalise:
\[
p(y_{t+1},\phi\mid y_{1:t})=p(y_{t+1}\mid y_{1:t},\phi)\,p(\phi\mid y_{1:t});\qquad
p(y_{t+1}\mid y_{1:t})=\int p(y_{t+1},\phi\mid y_{1:t})\,d\phi.
\]
Combining gives the boxed formula. The conditional independence $Y_{t+1}\perp y_{1:t}\mid\phi,\theta_{t+1}$ is built into the DLM, and the Kalman filter integrates $\theta_{t+1}$ out for each $\phi$. $\square$

\textbf{4. Why the result is a \emph{mixture of Gaussians}.}

Because $p(\phi\mid y_{1:t})$ is a continuous mixing distribution over $\phi$, and the conditional kernel is Gaussian, the marginal predictive is a continuous mixture (compound) of Gaussians:
\[
\boxed{\;p(y_{t+1}\mid y_{1:t})=\int\mathcal{N}_q(y_{t+1};f_{t+1}(\phi),Q_{t+1}(\phi))\,p(\phi\mid y_{1:t})\,d\phi.\;}
\]
\emph{Generally non-Gaussian}: mixtures of Gaussians can be skewed, heavy-tailed, or multimodal depending on the spread of $\phi\mid y_{1:t}$.

\textbf{5. Honest uncertainty.}

The plug-in predictive $\mathcal{N}_q(f_{t+1}(\widehat\phi),Q_{t+1}(\widehat\phi))$ uses a point estimate $\widehat\phi$ and ignores the spread of $\phi\mid y_{1:t}$. By the law of total variance,
\[
\operatorname{Var}(Y_{t+1}\mid y_{1:t})=\underbrace{\mathbb{E}_\phi[Q_{t+1}(\phi)\mid y_{1:t}]}_{\text{avg.\ predictive var}}+\underbrace{\operatorname{Var}_\phi(f_{t+1}(\phi)\mid y_{1:t})}_{\text{added by }\phi\text{-uncertainty}}.
\]
The second term is missing in the plug-in. Hence:

\boxed{\;\text{Bayesian predictive intervals are }\textbf{wider}\text{ than plug-in intervals --- this is the correct (honest) uncertainty.}\;}

\textbf{6. Computation --- the integral is intractable.}

In general $p(\phi\mid y_{1:t})$ is non-conjugate and high-dimensional; the predictive integral has no closed form. Two classes of approach:

\emph{(a) Monte-Carlo by direct posterior sampling (MCMC).} Draw $\phi^{(s)}\sim p(\phi\mid y_{1:t})$, $s=1,\dots,S$, and approximate
\[
p(y_{t+1}\mid y_{1:t})\;\approx\;\frac{1}{S}\sum_{s=1}^S\mathcal{N}_q\bigl(y_{t+1};\,f_{t+1}(\phi^{(s)}),Q_{t+1}(\phi^{(s)})\bigr).
\]
Each evaluation runs the KF at $\phi^{(s)}$. Predictive samples can be drawn by $y_{t+1}^{(s)}\sim\mathcal{N}_q(f_{t+1}(\phi^{(s)}),Q_{t+1}(\phi^{(s)}))$, one per MCMC draw.

\emph{(b) Importance sampling / Laplace.} Sample $\phi^{(s)}\sim g(\phi)$ from a tractable proposal and reweight by $w^{(s)}\propto L(\phi^{(s)})\pi(\phi^{(s)})/g(\phi^{(s)})$; weighted mixture.

\textbf{7. MCMC --- what is it and why does it work?}

\emph{Idea.} Construct a Markov chain $(\Phi^{(s)})_{s\ge 1}$ on the parameter / latent space whose \textbf{stationary distribution} is the target posterior $p(\phi,\theta_{0:t}\mid y_{1:t})$. Run the chain long enough that draws are approximately from $\pi$; estimate posterior expectations by ergodic averages
\[
\frac{1}{S}\sum_{s=1}^S h(\Phi^{(s)})\;\xrightarrow{\text{a.s.}}\;\mathbb{E}_\pi[h(\Phi)]\quad\text{(MCMC ergodic theorem; cf.\ keydef Thm 2.1 for ergodic Markov chains).}
\]

\emph{Detailed balance.} A transition kernel $K$ satisfies $\pi(x)K(x,y)=\pi(y)K(y,x)$ implies $\pi$ is invariant: $\int\pi(x)K(x,y)dx=\pi(y)$. Combined with irreducibility + aperiodicity, this gives convergence to $\pi$ and the ergodic LLN.

\emph{Two workhorse samplers for DLMs.}

\emph{(I) Gibbs sampler.} Cycle: $\phi\mid \theta_{0:t},y_{1:t}\to\theta_{0:t}\mid\phi,y_{1:t}\to\dots$ Each conditional is sampled directly. For DLMs with conjugate priors $V\sim\text{IG},W\sim\text{IW}$, the $\phi$-block has closed-form Inverse-Gamma / Inverse-Wishart full conditionals.

\emph{(II) Forward-Filtering Backward-Sampling (FFBS).} The $\theta_{0:t}$-block is sampled \emph{jointly} given $(\phi,y_{1:t})$ in two passes:
\begin{itemize}
\item \emph{Forward pass:} run the KF to get $\{(m_s,C_s)\}_{s=0}^t$.
\item \emph{Backward pass:} sample $\theta_t\sim\mathcal{N}(m_t,C_t)$; then recursively, for $s=t-1,t-2,\dots,0$:
\[
\theta_s\mid\theta_{s+1},y_{1:t}\sim\mathcal{N}\!\left(m_s+C_s G_{s+1}'R_{s+1}^{-1}(\theta_{s+1}-a_{s+1}),\;C_s-C_s G_{s+1}'R_{s+1}^{-1}G_{s+1}C_s\right).
\]
\end{itemize}
FFBS exploits the Markov structure of the smoothing distribution: $\theta_s\mid\theta_{s+1},y_{1:t}\sim\theta_s\mid\theta_{s+1},y_{1:s}$. It is the \emph{joint} smoothing sampler.

\emph{Predictive samples.} For each retained MCMC draw $(\phi^{(s)},\theta_{0:t}^{(s)})$: (i) propagate state forward $\theta_{t+1}^{(s)}=G\theta_t^{(s)}+w^{(s)},w^{(s)}\sim\mathcal{N}(0,W(\phi^{(s)}))$; (ii) generate $y_{t+1}^{(s)}=F\theta_{t+1}^{(s)}+v^{(s)},v^{(s)}\sim\mathcal{N}(0,V(\phi^{(s)}))$.

\textbf{8. Metropolis-Hastings for non-conjugate $\phi$.}

When no closed-form full conditional for $\phi$ is available, embed an MH step:
\begin{itemize}
\item Propose $\phi^\star\sim q(\phi^\star\mid\phi^{(s-1)})$.
\item Accept with probability $\alpha=\min\!\Bigl\{1,\dfrac{L(\phi^\star)\pi(\phi^\star)q(\phi^{(s-1)}\mid\phi^\star)}{L(\phi^{(s-1)})\pi(\phi^{(s-1)})q(\phi^\star\mid\phi^{(s-1)})}\Bigr\}$.
\end{itemize}
$L(\phi)$ here is the prediction-error likelihood — each acceptance test requires one KF pass. \emph{MH within Gibbs} = the standard recipe for DLMs with general $\phi$.

\textbf{9. MCMC and Markov chains --- the connection.}

The sampler chain $(\Phi^{(s)})$ \textbf{is} a Markov chain. Its construction (Gibbs, MH, FFBS) ensures $\pi$ is the unique stationary distribution; ergodicity (irreducibility + aperiodicity) gives convergence and the LLN. So MCMC = "design a Markov chain whose long-run behavior gives posterior samples", a direct application of the Markov-chain machinery of t3 (keydef Thm 2.1).

\textbf{10. Worked schematic --- one-step-ahead predictive via MCMC.}

\begin{enumerate}
\item Run MCMC (Gibbs + FFBS) on the joint posterior $p(\phi,\theta_{0:t}\mid y_{1:t})$ for $S$ iterations; discard burn-in; thin if needed. Retain draws $\{\phi^{(s)},\theta_t^{(s)}\}_{s=1}^S$.
\item For each $s$: propagate $\theta_{t+1}^{(s)}\sim\mathcal{N}(G\theta_t^{(s)},W(\phi^{(s)}))$; then $y_{t+1}^{(s)}\sim\mathcal{N}(F\theta_{t+1}^{(s)},V(\phi^{(s)}))$.
\item Predictive summaries: $\hat y_{t+1}=\tfrac1S\sum y_{t+1}^{(s)}$; predictive interval = empirical quantiles of $\{y_{t+1}^{(s)}\}$; predictive density = kernel-density estimate.
\end{enumerate}

\textbf{11. R --- Bayesian DLM via Gibbs + FFBS.}

```R
library(dlm)
y <- as.numeric(Nile)

# Model builder parameterised by (V, W)
build <- function(parm) {
  dlmModPoly(order = 1, dV = exp(parm[1]), dW = exp(parm[2]),
             m0 = 0, C0 = 1e7)
}

# Gibbs sampler with conjugate Inverse-Gamma priors on V, W
# (DLMwR provides dlmGibbsDIG specifically for the discount/IG case)
set.seed(1)
gibbs <- dlmGibbsDIG(y, mod = build(c(0, 0)),
                     a.y = 1, b.y = 1000,    ## IG prior on V
                     a.theta = 1, b.theta = 1000,
                     n.sample = 2000, thin = 1)
burn <- 500
V_samps <- gibbs$dV[-(1:burn)]
W_samps <- gibbs$dW[-(1:burn)]

# One-step-ahead Bayesian predictive samples
n  <- length(y)
ys <- numeric(length(V_samps))
for (s in seq_along(V_samps)) {
  mod_s <- dlmModPoly(order = 1, dV = V_samps[s], dW = W_samps[s],
                      m0 = 0, C0 = 1e7)
  kf_s  <- dlmFilter(y, mod_s)
  fc_s  <- dlmForecast(kf_s, nAhead = 1, sampleNew = 1)
  ys[s] <- as.numeric(fc_s$newObs[[1]])
}
# Predictive summaries (honest UQ)
mean(ys); sd(ys)
quantile(ys, c(0.025, 0.5, 0.975))   ## 95% predictive interval
hist(ys, prob = TRUE, breaks = 60,
     main = "Bayesian predictive p(y_{n+1} | y_{1:n})")
```

\textbf{12. Plug-in vs.\ Bayesian intervals --- empirical.}

For Nile data with $\widehat V,\widehat W$ from MLE, the plug-in 95\% interval may be $\sim\pm 200$; the full-Bayes interval $\pm 230$ — about 15\% wider, reflecting the variance contribution from $(V,W)$ uncertainty. This widening is the \emph{whole point} of integrating out $\phi$.

\textbf{Exam pointers.}
\begin{itemize}
\item[$\triangleright$] \texttt{exam\_jun\_2025\_q6}: write $p(y_{t+1}\mid y_{1:t})=\int p(y_{t+1}\mid y_{1:t},\phi)\,p(\phi\mid y_{1:t})d\phi$; each piece = KF Gaussian one-step ahead times posterior of $\phi$; mixture of Gaussians, non-Gaussian; MCMC (Gibbs + FFBS) gives draws of $\phi$; Bayesian intervals wider than plug-in.
\item[$\triangleright$] \texttt{exam\_may\_2025\_q6}: filtering distribution under unknown $\phi$: $p(\theta_t\mid y_{1:t})=\int p(\theta_t\mid y_{1:t},\phi)p(\phi\mid y_{1:t})d\phi$, mixture of Gaussians; approximated by MCMC; intervals wider than plug-in (honest UQ). Same machinery as the predictive — just integrate out $\phi$ from $p(\theta_t\mid y_{1:t},\phi)$.
\item[$\triangleright$] \texttt{exam\_may\_2022\_q8}: (a) Bayesian inference on $\phi$ via posterior $p(\phi\mid y_{1:t-1})\propto L(\phi)\pi(\phi)$ approximated by MCMC; predictive $p(y_t\mid y_{1:t-1})=\int p(y_t\mid y_{1:t-1},\phi)p(\phi\mid y_{1:t-1})d\phi\approx S^{-1}\sum_s\mathcal{N}_q(y_t;f_t(\phi^{(s)}),Q_t(\phi^{(s)}))$, mixture, properly inflated. (b) MCMC \emph{is} a Markov chain: construct ergodic kernel with stationary distribution = posterior; detailed balance $\pi(x)K(x,y)=\pi(y)K(y,x)\Rightarrow$ $\pi$-invariance; irreducibility + aperiodicity $\Rightarrow$ ergodic LLN $\tfrac1S\sum h(X^{(s)})\to\mathbb{E}_\pi[h]$.
\end{itemize}
""",
}
