"""
Theory column entries for sub-topics in topics T7, T8, T9.
"""
theory_content_ts = {}


# =============================================================================
# t7a — Random-walk + noise model — definition & independence proofs
# =============================================================================
theory_content_ts["t7a"] = {
    "title": "Theory — Random-walk + noise model — definition & independence proofs",
    "content": r"""\textbf{\textcolor{red}{THEORY --- Random-walk + noise model --- definition \& independence proofs [Topic: T7 --- DLM building blocks]}}

\textbf{1. The random-walk-plus-noise (local-level) model.}

The \emph{random-walk plus noise} model — also called the \emph{local-level} DLM (DLMwR \S 2.3.2) — is the simplest non-trivial Dynamic Linear Model. It posits a univariate observation $Y_t$ tracking a latent level $\theta_t$ that itself follows a random walk.

\boxed{\;
\begin{aligned}
Y_t&=\theta_t+v_t,\qquad v_t\overset{\text{iid}}{\sim}\mathcal{N}(0,V),\\
\theta_t&=\theta_{t-1}+w_t,\qquad w_t\overset{\text{iid}}{\sim}\mathcal{N}(0,W),\\
\theta_0&\sim\mathcal{N}(m_0,C_0),
\end{aligned}\quad\text{with }(v_t)_{t\ge 1},\,(w_t)_{t\ge 1},\,\theta_0\text{ mutually independent.}\;}

As a DLM in standard notation: $p=q=1$, $F=1$, $G=1$, with system covariances $V$ (observation noise) and $W$ (state noise).

\textbf{2. Why it is "the right" minimal non-stationary model.}

\begin{itemize}
\item The latent level $\theta_t$ is a random walk, so it is non-stationary: $\operatorname{Var}(\theta_t)=C_0+tW$ grows with $t$.
\item Observations $Y_t$ inherit non-stationarity from the latent level, so the model can describe series that drift (no constant mean) — directly, with no differencing.
\item The signal-to-noise ratio $\kappa=W/V$ controls how quickly the level adapts: $\kappa$ small $\Rightarrow$ smooth level (close to a constant); $\kappa$ large $\Rightarrow$ level tracks the data tightly.
\end{itemize}

\textbf{3. Marginal moments — quick computation.}

From the assumptions, taking expectations and variances (using independence to kill cross-terms):
\[
\mathbb{E}[\theta_t]=m_0,\qquad\operatorname{Var}(\theta_t)=C_0+tW,\qquad \mathbb{E}[Y_t]=m_0,\qquad\operatorname{Var}(Y_t)=C_0+tW+V.
\]
Lag-$h$ autocovariance: for $s\le t$,
\[
\operatorname{Cov}(\theta_s,\theta_t)=C_0+sW,\qquad
\operatorname{Cov}(Y_s,Y_t)=C_0+sW\quad(s<t).
\]
Neither $\operatorname{Var}(Y_t)$ nor $\operatorname{Cov}(Y_s,Y_t)$ depends on the lag alone, so $(Y_t)$ is \emph{not} weakly stationary.

\textbf{4. State as a function of innovations.}

By iterating the state recursion,
\[
\boxed{\;\theta_s=\theta_0+\sum_{u=1}^{s}w_u,\qquad s\ge 1.\;}
\]
Hence the vector $(\theta_1,\dots,\theta_t)$ is a (deterministic) measurable function of $(\theta_0,w_1,\dots,w_t)$:
\[
(\theta_1,\dots,\theta_t)=\Phi(\theta_0,w_1,\dots,w_t),\qquad \Phi:\mathbb{R}^{t+1}\to\mathbb{R}^t,
\]
with $\Phi$ the lower-triangular linear map $\theta_s=\theta_0+w_1+\cdots+w_s$.

\textbf{5. Independence statement 1 --- $v_t\perp(\theta_1,\dots,\theta_t)$.}

\boxed{\;v_t\;\perp\!\!\!\perp\;(\theta_1,\dots,\theta_t).\;}

\emph{Proof.} By assumption (Section 1), the family $\bigl\{(v_s)_{s\ge 1},\,(w_s)_{s\ge 1},\,\theta_0\bigr\}$ is mutually independent. In particular,
\[
v_t\perp(\theta_0,w_1,\dots,w_t).
\]
By Section 4, $(\theta_1,\dots,\theta_t)$ is a measurable function of $(\theta_0,w_1,\dots,w_t)$. Independence is preserved under measurable transformations: if $X\perp Z$ and $Y=g(Z)$, then $X\perp Y$. Therefore
\[
v_t\perp(\theta_1,\dots,\theta_t). \qquad\square
\]

\textbf{6. Independence statement 2 --- $w_t\perp(\theta_1,\dots,\theta_{t-1})$.}

\boxed{\;w_t\;\perp\!\!\!\perp\;(\theta_1,\dots,\theta_{t-1}).\;}

\emph{Proof.} For $s\le t-1$, $\theta_s=\theta_0+w_1+\cdots+w_s$ involves only $w_1,\dots,w_{s}\subseteq w_{1:t-1}$ and $\theta_0$. Hence $(\theta_1,\dots,\theta_{t-1})$ is a measurable function of $(\theta_0,w_1,\dots,w_{t-1})$. By the mutual-independence assumption,
\[
w_t\perp(\theta_0,w_1,\dots,w_{t-1}),
\]
so again by preservation under measurable transformations, $w_t\perp(\theta_1,\dots,\theta_{t-1})$. $\square$

\textbf{7. Why these independence facts matter for the Kalman filter.}

These two statements are the hidden engines of the KF derivation:
\begin{itemize}
\item $v_t\perp\theta_t$ (a corollary of statement 1) is needed in the \emph{observation-prediction step}: $Y_t=\theta_t+v_t$, so $\operatorname{Var}(Y_t\mid y_{1:t-1})=\operatorname{Var}(\theta_t\mid y_{1:t-1})+\operatorname{Var}(v_t)=R_t+V=Q_t$.
\item $w_t\perp(\theta_{t-1},y_{1:t-1})$ (a corollary of statement 2 plus $y_{1:t-1}$ being a function of $(\theta_0,w_{1:t-1},v_{1:t-1})$) is needed in the \emph{state-prediction step}: $\theta_t=\theta_{t-1}+w_t$, so $\mathbb{E}[\theta_t\mid y_{1:t-1}]=m_{t-1}=a_t$, $\operatorname{Var}(\theta_t\mid y_{1:t-1})=C_{t-1}+W=R_t$ with no cross term.
\end{itemize}

\textbf{8. Worked micro-example.}

Take $V=1$, $W=0.5$, $m_0=0$, $C_0=10$. Then $\operatorname{Var}(\theta_t)=10+0.5t$ and $\operatorname{Var}(Y_t)=11+0.5t$. At $t=5$: $\operatorname{Var}(\theta_5)=12.5$, $\operatorname{Var}(Y_5)=13.5$. The two independence statements are used implicitly when computing these moments — for instance $\operatorname{Var}(Y_5)=\operatorname{Var}(\theta_5)+\operatorname{Var}(v_5)$ only because $v_5\perp\theta_5$.

\textbf{9. R --- simulate and inspect.}

```R
set.seed(1)
T  <- 200; V <- 1; W <- 0.5; m0 <- 0; C0 <- 10
theta0 <- rnorm(1, m0, sqrt(C0))
w  <- rnorm(T, 0, sqrt(W));  v <- rnorm(T, 0, sqrt(V))
theta <- theta0 + cumsum(w)
y     <- theta + v
plot.ts(y, ylab="y_t", main="Random walk + noise (local level)")
lines(theta, col="red", lwd=2)   # latent level
legend("topleft", c("Y_t (obs)","theta_t (state)"), col=c("black","red"), lty=1)

# Build the same model with the dlm package
library(dlm)
mod <- dlmModPoly(order = 1, dV = V, dW = W, m0 = m0, C0 = C0)
mod$FF; mod$GG; mod$V; mod$W   # F=1, G=1, V=1, W=0.5
```

\textbf{10. Exam pointers.}
\begin{itemize}
\item[$\triangleright$] \texttt{exam\_may\_2024\_q3}: (a) write the missing parts of the RW+noise model (state equation, distributions of $v_t,w_t,\theta_0$, mutual independence). (b) Prove $v_t\perp(\theta_1,\dots,\theta_t)$: by mutual indep., $v_t\perp(\theta_0,w_{1:t})$; $(\theta_1,\dots,\theta_t)$ is a function of $(\theta_0,w_{1:t})$, so independence is preserved.
\item[$\triangleright$] \texttt{exam\_may\_2023\_q4}: same model + symmetric proof for $w_t\perp(\theta_1,\dots,\theta_{t-1})$: by mutual indep., $w_t\perp(\theta_0,w_{1:t-1})$; $(\theta_1,\dots,\theta_{t-1})$ is a function thereof.
\end{itemize}
""",
}


# =============================================================================
# t7b — Local linear trend / structural BSM (trend + seasonality)
# =============================================================================
theory_content_ts["t7b"] = {
    "title": "Theory — Local linear trend / structural BSM (trend + seasonality)",
    "content": r"""\textbf{\textcolor{red}{THEORY --- Local linear trend / structural BSM [Topic: T7 --- DLM building blocks]}}

\textbf{1. Why structural DLMs.}

Many real series carry a non-stationary trend and/or a seasonal cycle (CO$_2$, temperatures, retail sales, electricity demand). ARMA cannot model such a series directly — its first two moments are constant in $t$. Two approaches:
\begin{itemize}
\item \emph{ARMA route.} Remove the trend (differencing $\to$ ARIMA) and seasonality ($1-B^s$ $\to$ SARIMA), then fit ARMA to the residuals.
\item \emph{DLM/SSM route.} Build a \emph{structural} DLM where the latent state \emph{is} the non-stationary trend (plus seasonality plus possibly cycles), and let the KF/smoother handle it directly — no differencing, no de-seasoning.
\end{itemize}

\boxed{\;\text{DLMs do NOT require stationarity; the latent state carries the non-stationarity.}\;}

\textbf{2. The local-linear-trend (LLT) DLM.}

The LLT (DLMwR \S 3.2.2) adds a stochastic slope $\beta_t$ to the random-walk-plus-noise level:

\boxed{\;
\begin{aligned}
Y_t&=\mu_t+\varepsilon_t,\qquad \varepsilon_t\overset{\text{iid}}{\sim}\mathcal{N}(0,\sigma^2),\\
\mu_t&=\mu_{t-1}+\beta_{t-1}+w_{1,t},\qquad w_{1,t}\overset{\text{iid}}{\sim}\mathcal{N}(0,\sigma_{w_1}^2),\\
\beta_t&=\beta_{t-1}+w_{2,t},\qquad w_{2,t}\overset{\text{iid}}{\sim}\mathcal{N}(0,\sigma_{w_2}^2),
\end{aligned}\;}

with $(\varepsilon_t),(w_{1,t}),(w_{2,t})$ mutually independent and independent of $(\mu_0,\beta_0)$.

\emph{Reading.} The level $\mu_t$ moves by its current slope $\beta_{t-1}$ plus a level shock $w_{1,t}$; the slope itself is a random walk. Setting $\sigma_{w_2}^2=0$ gives a constant slope (smooth trend); setting $\sigma_{w_1}^2=\sigma_{w_2}^2=0$ recovers a deterministic line $\mu_t=\mu_0+\beta_0\,t$.

\textbf{3. LLT in DLM form.}

Stack $\theta_t=(\mu_t,\beta_t)'$ (2-dimensional state). Then

\boxed{\;
F=(1,\;0),\qquad
G=\begin{pmatrix}1&1\\0&1\end{pmatrix},\qquad
V=\sigma^2,\qquad
W=\begin{pmatrix}\sigma_{w_1}^2 & 0\\ 0 & \sigma_{w_2}^2\end{pmatrix},\qquad
\theta_0=\begin{pmatrix}\mu_0\\\beta_0\end{pmatrix}\sim\mathcal{N}_2(m_0,C_0).\;}

\emph{Verification.} The first row of $G\theta_{t-1}=(\mu_{t-1}+\beta_{t-1},\,\beta_{t-1})'$ is the level recursion; the second row is the slope recursion. The observation $Y_t=F\theta_t+\varepsilon_t=(1,0)\theta_t+\varepsilon_t=\mu_t+\varepsilon_t$ matches the data equation. \emph{R:} `dlmModPoly(order = 2, dV = sigma^2, dW = c(sw1^2, sw2^2))`.

\textbf{4. Seasonal component --- two equivalent parameterisations.}

For period $s$ (e.g.\ $s=12$ for monthly data), a stochastic seasonal $\gamma_t$ satisfying $\gamma_t+\gamma_{t-1}+\cdots+\gamma_{t-s+1}=w_{3,t}$ (so that the sum of any $s$ consecutive seasonal effects averages to zero, with mild noise) admits the recursion

\boxed{\;\gamma_t=-\sum_{j=1}^{s-1}\gamma_{t-j}+w_{3,t},\qquad w_{3,t}\overset{\text{iid}}{\sim}\mathcal{N}(0,\sigma_{w_3}^2).\;}

In DLM form, $\theta^{\mathrm{seas}}_t=(\gamma_t,\gamma_{t-1},\dots,\gamma_{t-s+2})'$ is $(s-1)$-dim. with
\[
G^{\mathrm{seas}}=\begin{pmatrix}-1 & -1 & \cdots & -1 & -1\\ 1 & 0 & \cdots & 0 & 0\\ 0 & 1 & \cdots & 0 & 0\\ \vdots & & \ddots & & \vdots\\ 0 & 0 & \cdots & 1 & 0\end{pmatrix},\qquad F^{\mathrm{seas}}=(1,0,\dots,0).
\]
Alternative: trigonometric seasonal (sums of sines/cosines at the harmonic frequencies); see DLMwR \S 3.2.3.

\textbf{5. The Basic Structural Model (BSM, Harvey).}

Combine trend + seasonality additively:

\boxed{\;Y_t=\mu_t+\gamma_t+v_t,\;}

with $\mu_t$ following LLT and $\gamma_t$ following the seasonal recursion. Stack states $\theta_t=(\mu_t,\beta_t,\gamma_t,\gamma_{t-1},\dots,\gamma_{t-s+2})'$ ($1+1+(s-1)=s+1$ dim.). The full DLM is built block-diagonally:
\[
G=\mathrm{diag}\bigl(G^{\mathrm{trend}},\,G^{\mathrm{seas}}\bigr),\quad
W=\mathrm{diag}\bigl(W^{\mathrm{trend}},\,W^{\mathrm{seas}}\bigr),\quad
F=(F^{\mathrm{trend}},F^{\mathrm{seas}})=(1,0,1,0,\dots,0).
\]
\emph{R:} `mod <- dlmModPoly(2, dV=sigma^2, dW=c(sw1^2,sw2^2)) + dlmModSeas(frequency=12, dW=c(sw3^2, rep(0,10)))`.

\textbf{6. Why structural DLMs work on non-stationary data.}

The state $\theta_t$ \emph{is} the non-stationary component (random-walk level, random-walk slope, seasonal). KF runs forward and computes the filtering distribution at each $t$; the predictive distribution and forecast function are derived from the state at time $t$ — no preliminary differencing or seasonal-mean subtraction is needed. Forecast function from LLT (no seasonal): $\hat y_{t+k\mid t}=m_t^{(\mu)}+k\,m_t^{(\beta)}$ (a line with the current best slope), naturally extrapolating the trend.

\textbf{Diffuse prior.} For non-stationary components ($\mu_0,\beta_0$ are scale-free a priori), the standard recipe is a diffuse initial: $C_0=\kappa I$ with $\kappa\to\infty$ (or use exact diffuse initialisation, DLMwR \S 2.7.4). The KF stabilises quickly.

\textbf{7. Worked micro-example.}

\emph{Setting.} Monthly CO$_2$, $T=480$. Fit BSM with LLT + seasonal of period 12. Estimate $(\sigma^2,\sigma_{w_1}^2,\sigma_{w_2}^2,\sigma_{w_3}^2)$ by MLE (prediction-error decomposition). Typical fitted picture: $m_t^{(\mu)}$ tracks the smooth upward trend; $m_t^{(\beta)}\approx\,$constant positive (steady growth); $m_t^{(\gamma)}$ a tight annual cycle. A purely deterministic alternative (set $\sigma_{w_2}^2=0$) would force a constant slope — fine on CO$_2$ but inadequate on a series whose growth rate visibly changes.

\textbf{8. R --- full BSM for CO$_2$.}

```R
library(dlm)
data(co2)        # monthly CO2, Mauna Loa
y <- co2

build <- function(p) {
  dlmModPoly(2, dV=exp(p[1]), dW=c(exp(p[2]), exp(p[3]))) +
  dlmModSeas(frequency=12, dW=c(exp(p[4]), rep(0,10)))
}
fit <- dlmMLE(y, parm=rep(-2,4), build=build)
mod <- build(fit$par)
sm  <- dlmSmooth(y, mod)

# Component decomposition
mu_smooth   <- sm$s[, 1]    # smoothed level mu_t
beta_smooth <- sm$s[, 2]    # smoothed slope beta_t
gam_smooth  <- sm$s[, 3]    # smoothed seasonal gamma_t

plot.ts(cbind(y, mu_smooth, gam_smooth),
        main="BSM decomposition: level + seasonal")

# k-step forecast
fc <- dlmForecast(dlmFilter(y, mod), nAhead=24)
fc$f                          # point forecasts (line + season pattern)
```

\textbf{9. Bottom line.}

\boxed{\;\text{Structural DLM = LLT trend + stochastic seasonal + noise.}\;}\quad Handles trend \emph{and} seasonality without differencing; latent state is the non-stationary component; KF/smoother provide filtering, smoothing, and forecasting in one pass.

\textbf{10. Exam pointers.}
\begin{itemize}
\item[$\triangleright$] \texttt{exam\_sep\_2025\_q5}: write the given LLT model as a DLM. Take $\theta_t=(\mu_t,\beta_t)'$; $F=(1,0)$, $G=\bigl(\begin{smallmatrix}1&1\\0&1\end{smallmatrix}\bigr)$, $V=\sigma^2$, $W=\mathrm{diag}(\sigma_{w_1}^2,\sigma_{w_2}^2)$. \emph{R:} `dlmModPoly(order=2, dV=sigma^2, dW=c(sw1^2,sw2^2))`.
\item[$\triangleright$] \texttt{exam\_may\_2021\_q3}: CO$_2$ series with trend + seasonality. "Can we model with a DLM without differencing/de-seasoning?" \emph{YES}: use a \emph{structural / BSM} DLM, $Y_t=\mu_t+\gamma_t+v_t$ with LLT for $\mu_t$ and a seasonal recursion $\gamma_t=-\sum_{j=1}^{s-1}\gamma_{t-j}+w_{3,t}$ ($s=12$); the latent state carries the non-stationarity; diffuse prior on initial state; KF runs as usual.
\end{itemize}
""",
}


# =============================================================================
# t7c — Time-varying-coefficient regression DLM
# =============================================================================
theory_content_ts["t7c"] = {
    "title": "Theory — Time-varying-coefficient regression DLM",
    "content": r"""\textbf{\textcolor{red}{THEORY --- Time-varying-coefficient regression DLM [Topic: T7 --- DLM building blocks]}}

\textbf{1. From static to dynamic linear regression.}

Static linear regression with a single regressor $x_t$ assumes
\[
Y_t=\alpha+\beta x_t+\varepsilon_t,\qquad \varepsilon_t\overset{\text{iid}}{\sim}\mathcal{N}(0,\sigma^2),
\]
i.e.\ the relationship $x\to y$ is fixed throughout the sample. In many applications (pharmacology with adaptive dose response, finance with regime-shifting risk premia, marketing mix with decaying elasticities) the relationship \emph{evolves smoothly with time}. The natural extension lets the intercept and slope drift as latent processes.

\textbf{2. Time-varying-coefficient (TVC) DLM.}

\boxed{\;
\begin{aligned}
Y_t&=\alpha_t+\beta_t x_t+v_t,\qquad v_t\overset{\text{iid}}{\sim}\mathcal{N}(0,V),\\
\begin{pmatrix}\alpha_t\\\beta_t\end{pmatrix}&=\begin{pmatrix}\alpha_{t-1}\\\beta_{t-1}\end{pmatrix}+w_t,\qquad w_t\overset{\text{iid}}{\sim}\mathcal{N}_2(0,W),\\
\theta_0&=(\alpha_0,\beta_0)'\sim\mathcal{N}_2(m_0,C_0),
\end{aligned}\;}

with $(v_t)$, $(w_t)$, $\theta_0$ mutually independent. In DLM notation, $\theta_t=(\alpha_t,\beta_t)'$ (2-dim. state), and crucially the \emph{observation matrix is time-varying}:
\[
F_t=(1,\;x_t),\qquad G_t=G=I_2,\qquad V_t=V,\qquad W_t=W.
\]

\emph{Reading.} The regressor $x_t$ is treated as known (deterministic input), placed into $F_t$. The state $\theta_t$ contains the unknown coefficients, which drift as a 2-dim. random walk. Static linear regression is the special case $W=0$ (no drift).

\textbf{3. Generalisation: multiple regressors.}

For $k$ regressors $x_t=(x_{t,1},\dots,x_{t,k})'$, take $\theta_t=(\alpha_t,\beta_{t,1},\dots,\beta_{t,k})'\in\mathbb{R}^{k+1}$, $F_t=(1,x_t')$ ($1\times(k+1)$), $G=I_{k+1}$, $W\in\mathbb{R}^{(k+1)\times(k+1)}$. The structure is identical; just stack regressors and coefficients.

\textbf{4. Role of $W$ --- shrinkage between static and fully adaptive.}

The covariance $W$ controls the \emph{adaptation speed} of $(\alpha_t,\beta_t)$:
\begin{itemize}
\item $W\to 0$: coefficients frozen at $(\alpha_0,\beta_0)$ — exactly static OLS-style regression. Filtered state $m_t$ converges to the OLS estimate.
\item $W$ moderate: smooth drift; current estimate gives more weight to recent observations.
\item $W$ large: rapid drift; estimate tracks each observation closely — high variance, low bias.
\end{itemize}
$W$ is typically estimated by MLE (prediction-error decomposition over the free entries of $W$) or assigned a hierarchical prior (Bayesian).

\textbf{5. Inference targets.}

After running the KF/smoother:
\begin{itemize}
\item \emph{Filtering} $(\alpha_t,\beta_t)\mid y_{1:t}\sim\mathcal{N}_2(m_t,C_t)$ — coefficients as of time $t$, real-time estimate (online).
\item \emph{Smoothing} $(\alpha_t,\beta_t)\mid y_{1:T}\sim\mathcal{N}_2(s_t,S_t)$ — coefficients at time $t$ given \emph{all} data (offline, retrospective). Uses past \emph{and} future data, hence $S_t\preceq C_t$.
\item \emph{One-step prediction} $Y_t\mid y_{1:t-1}\sim\mathcal{N}(f_t,Q_t)$, $f_t=F_t a_t=a_t^{(\alpha)}+x_t\,a_t^{(\beta)}$, $Q_t=F_t R_t F_t'+V$.
\end{itemize}

\textbf{6. Handling unknown $\sigma^2$ (and $W$).}

Two standard routes:

(i) \emph{MLE plug-in / empirical Bayes.} Maximise the prediction-error log-likelihood
\[
\ell(\sigma^2,W)=-\tfrac12\sum_t\bigl[\log Q_t+(y_t-f_t)^2/Q_t\bigr]+\text{const},
\]
returning $(\widehat\sigma^2,\widehat W)$; then plug into KF/smoother.

(ii) \emph{Fully Bayesian.} Conjugate priors $\sigma^2\sim\mathrm{IG}(a_v,b_v)$, $W\sim\mathrm{IW}(\nu,S_0)$. Gibbs sampler over $(\theta_{0:T},\sigma^2,W)$ using \emph{FFBS} (forward-filter, backward-sample, DLMwR \S 4.5) for the state path. Posterior summaries integrate out parameter uncertainty.

\textbf{7. Borrowing strength across groups --- hierarchical TVC DLM.}

When the same experiment is run in $H$ groups (hospitals, mice, regions), model each as its own TVC DLM and tie them via a shared population structure on the initial states or on the dynamics:
\[
\begin{aligned}
Y^{(h)}_t&=F^{(h)}_t\theta^{(h)}_t+v^{(h)}_t,\quad \theta^{(h)}_t=\theta^{(h)}_{t-1}+w^{(h)}_t,\\
\theta^{(h)}_0&\sim\mathcal{N}_2(\mu,T),\quad h=1,\dots,H;\quad \mu\sim\mathcal{N}_2(0,\Sigma_0).
\end{aligned}
\]
The hyperprior on $\mu$ shrinks each group's coefficient path toward the shared population mean — \emph{borrowing strength}. The degree of shrinkage is learned from data via the population variance $T$. Other shared elements (common $W$, common $V$, common dynamics) yield further pooling.

\textbf{8. Worked micro-example --- adaptive intercept / slope on a synthetic dose-response.}

Simulate $x_t$ linearly increasing, $\alpha_t=2+0.001t^2$, $\beta_t=0.3+0.4\sin(2\pi t/100)$, $Y_t=\alpha_t+\beta_t x_t+v_t$. Static OLS gives a single $(\widehat\alpha,\widehat\beta)$ ignoring the time variation; the TVC DLM filtered/smoothed paths $(m_t,s_t)$ should recover the curved $\alpha_t$ and oscillating $\beta_t$.

\textbf{9. R --- TVC regression DLM.}

```R
library(dlm)
set.seed(1); T <- 200
x  <- seq(0, 10, length=T)
al <- 2 + 0.001*(1:T)^2
be <- 0.3 + 0.4*sin(2*pi*(1:T)/100)
y  <- al + be*x + rnorm(T, 0, 0.3)

# Build the TVC regression DLM with intercept
build <- function(p) {
  dlmModReg(X = x, addInt = TRUE,
            dV = exp(p[1]),
            dW = c(exp(p[2]), exp(p[3])))
}
fit <- dlmMLE(y, parm = c(0, -3, -3), build = build)
mod <- build(fit$par)
smo <- dlmSmooth(y, mod)
alpha_hat <- smo$s[-1, 1]   # smoothed intercept
beta_hat  <- smo$s[-1, 2]   # smoothed slope

par(mfrow=c(2,1))
plot.ts(cbind(al, alpha_hat), main="alpha_t (true vs smoothed)")
plot.ts(cbind(be, beta_hat ), main="beta_t  (true vs smoothed)")

# Static OLS comparison
coef(lm(y ~ x))             # constant — ignores drift
```

\textbf{10. Bottom line.}

\boxed{\;Y_t=F_t\theta_t+v_t,\ \theta_t=\theta_{t-1}+w_t,\ F_t=(1,x_t'),\ G=I.\;}\quad Latent state \emph{is} the drifting coefficient vector. $W$ controls adaptation speed; KF gives filtering, smoother gives retrospective inference; hierarchical extension shares strength across groups.

\textbf{11. Exam pointers.}
\begin{itemize}
\item[$\triangleright$] \texttt{exam\_may\_2025\_q4}: (a) write the TVC DLM with $\theta_t=(\alpha_t,\beta_t)'$, $F_t=(1,x_t)$, $G=I_2$; static regression is the $W=0$ corner. (b) Smoothing distribution = $\pi(\theta_{0:n}\mid y_{1:n})$ (joint) or $\pi(\theta_t\mid y_{1:n})$ (marginal); pools past and future data. (c) If $\sigma^2$ unknown: MLE plug-in (prediction-error likelihood) or fully Bayesian (IG/IW conjugate + Gibbs/FFBS). (d) Two-hospital borrowing strength: hierarchical DLM with shared initial state $\theta_0^{(h)}\sim\mathcal{N}_2(\mu,T)$, hyperprior on $\mu$.
\item[$\triangleright$] \texttt{exam\_jun\_2022\_q6}: pharmaceutical dose-response with non-linear time dynamics. Propose $Y_t=\alpha_t+\beta_t x_t+v_t$ with $(\alpha_t,\beta_t)'=(\alpha_{t-1},\beta_{t-1})'+w_t$, $F_t=(1,x_t)$, $G=I_2$. Coefficients drift as RW — captures non-linear trend without imposing a parametric form. $W$ controls smoothness.
\end{itemize}
""",
}


# =============================================================================
# t7d — Multivariate DLM & dependence between latent series
# =============================================================================
theory_content_ts["t7d"] = {
    "title": "Theory — Multivariate DLM & dependence between latent series",
    "content": r"""\textbf{\textcolor{red}{THEORY --- Multivariate DLM \& dependence between latent series [Topic: T7 --- DLM building blocks]}}

\textbf{1. The general multivariate DLM.}

Let $Y_t=(Y_{1,t},\dots,Y_{m,t})'\in\mathbb{R}^m$ be a multivariate time series (e.g.\ prices of $m$ financial assets). A \emph{multivariate Dynamic Linear Model} (DLMwR \S 2.3, \S 3.3) is

\boxed{\;
\begin{aligned}
Y_t&=F_t\theta_t+v_t,\qquad v_t\overset{\text{iid}}{\sim}\mathcal{N}_m(0,V_t),\\
\theta_t&=G_t\theta_{t-1}+w_t,\qquad w_t\overset{\text{iid}}{\sim}\mathcal{N}_p(0,W_t),\\
\theta_0&\sim\mathcal{N}_p(m_0,C_0),
\end{aligned}\;}

with $\{v_t\},\{w_t\},\theta_0$ mutually independent, $F_t$ of size $m\times p$, $G_t$ of size $p\times p$, $V_t$ of size $m\times m$, $W_t$ of size $p\times p$. The state dimension $p$ is decoupled from the observation dimension $m$.

\textbf{2. Multivariate random-walk-plus-noise — independent assets baseline.}

Take $m=p=2$. \emph{Independent} latent random walks plus noise: for $j=1,2$,
\[
\theta_{j,t}=\theta_{j,t-1}+w_{j,t},\qquad Y_{j,t}=\theta_{j,t}+v_{j,t},
\]
with $(w_{1,t}),(w_{2,t}),(v_{1,t}),(v_{2,t})$ mutually independent. Stacking:

\boxed{\;F=I_2,\quad G=I_2,\quad V=\mathrm{diag}(\sigma_{v_1}^2,\sigma_{v_2}^2),\quad W=\mathrm{diag}(\sigma_{w_1}^2,\sigma_{w_2}^2).\;}

\emph{Reading.} Both diagonal $V$ and diagonal $W$ encode the independence assumption: shocks (both observation and state) are uncorrelated across assets. The model reduces to two separate univariate RW+noise models.

\textbf{Why this is the "perfect-markets" baseline.} Under perfect markets / efficient-markets hypothesis, asset prices follow independent random walks (no exploitable lead-lag), so $\theta_{1,t}$ and $\theta_{2,t}$ are independent latent processes. Real markets exhibit co-movement (factor structure, sector links) — that is what motivates introducing dependence.

\textbf{3. Introducing dependence --- three canonical routes.}

\textbf{Route (1) --- Correlated state noise (contemporaneous cross-asset shocks).}

Keep $F=G=I_2$, but make $W$ non-diagonal:
\[
W=\begin{pmatrix}\sigma_{w_1}^2 & \rho_w\sigma_{w_1}\sigma_{w_2}\\ \rho_w\sigma_{w_1}\sigma_{w_2} & \sigma_{w_2}^2\end{pmatrix},\qquad \rho_w\in(-1,1).
\]
The latent prices are still random walks individually, but their innovations $w_{1,t}, w_{2,t}$ are contemporaneously correlated: when one moves up, the other tends to move up too. Captures co-movement at the period frequency without changing the marginal dynamics.

\textbf{Route (2) --- Common latent factor (cointegration / factor structure).}

Introduce a 1-dim.\ common factor $f_t$ following a random walk, and let both prices load on it:
\[
f_t=f_{t-1}+w_{f,t},\qquad \theta_t=A f_t+\eta_t,\qquad A=\begin{pmatrix}a_1\\a_2\end{pmatrix}\in\mathbb{R}^2,
\]
optionally with idiosyncratic stochastic levels $\eta_{j,t}=\eta_{j,t-1}+w_{\eta_j,t}$. Then both prices share a common stochastic trend — exactly the structure of cointegration. Equivalent reformulation: keep $\theta_t$ 2-dim. but reduce $W$ to rank 1 ($W=\tau^2 A A'$).

\textbf{Route (3) --- Cross terms in $G$ (VAR-type spillovers).}

Let the latent state evolve as a 2-dim. VAR(1):
\[
G=\begin{pmatrix}1 & \delta\\ 0 & 1\end{pmatrix}\quad\text{or full}\quad G=\begin{pmatrix}g_{11} & g_{12}\\ g_{21} & g_{22}\end{pmatrix}.
\]
Past movements in asset 2 spill over into the level of asset 1 (and vice versa). Capable of capturing lead-lag relationships, not just contemporaneous correlation.

\textbf{Route (4) --- Correlated observation noise.}

Make $V$ non-diagonal. Captures contemporaneous measurement co-shocks; useful when both assets are observed through a common market-microstructure noise (bid-ask spread, common rounding).

\textbf{4. Generalisation --- $m$ assets, factor model.}

For $m$ large, the typical structure is a \emph{$k$-factor DLM} with $k\ll m$ common factors:
\[
\theta_t=A f_t+\eta_t,\qquad f_t=f_{t-1}+w_{f,t}\in\mathbb{R}^k,
\]
with $A\in\mathbb{R}^{m\times k}$ the factor loadings. This is the dynamic version of factor models, encompassing static factor analysis ($A f_t$ part), latent cointegration vectors, and dynamic principal components.

\textbf{5. Identifiability remark.}

Factor / cross-term DLMs can be observationally equivalent under rotations of $A$ and rescalings of $f_t$. Standard conventions: $A'A=I_k$, $f_t$ orthonormal factors, lower-triangular $A$. Without such constraints MLE / Gibbs can drift between modes.

\textbf{6. Inference --- same KF.}

The general multivariate KF (predict / observation predict / update) handles all four routes identically; only the matrices $F,G,V,W$ change. Time-varying matrices are accommodated via $F_t,G_t,V_t,W_t$.

\textbf{7. Worked micro-example.}

\emph{2 assets, route (1).} Set $\sigma_{w_1}=\sigma_{w_2}=0.1$, $\sigma_{v_1}=\sigma_{v_2}=0.05$, $\rho_w=0.7$. Simulate; fit a) the wrongly-specified diagonal $W$ model, b) the correctly-specified $\rho_w$-included model. Compare predictive log-likelihood: the $\rho_w$-model yields tighter joint predictive bands precisely because conditioning on asset 1's innovation halves the uncertainty about asset 2.

\textbf{8. R --- multivariate RW+noise with correlated state noise.}

```R
library(dlm)
m <- 2
sv <- c(0.05, 0.05)
sw <- c(0.10, 0.10); rho <- 0.7

V <- diag(sv^2)
W <- matrix(c(sw[1]^2,          rho*sw[1]*sw[2],
              rho*sw[1]*sw[2],  sw[2]^2),
            2, 2)

mod <- dlm(FF = diag(2), GG = diag(2),
           V  = V,        W  = W,
           m0 = c(0, 0),  C0 = diag(2) * 1e7)
mod$FF; mod$GG; mod$V; mod$W

# Simulate 500 time points
set.seed(1); T <- 500
sim <- dlmForecast(mod, nAhead = T, sampleNew = 1)
Y   <- sim$newObs[[1]]; head(Y)

# Fit a (mis-specified) diagonal-W model vs the correct one and compare loglik
kf_full <- dlmFilter(Y, mod)
loglik_full <- sum(dnorm(Y[,1], kf_full$f[,1], sqrt(sapply(kf_full$U.Y, function(u) u[1,1])), log = TRUE)) # rough sketch
```

\textbf{9. Bottom line.}

\boxed{\;\text{General DLM: }Y_t=F_t\theta_t+v_t,\ \theta_t=G_t\theta_{t-1}+w_t,\;v_t\sim\mathcal{N}_m(0,V_t),\;w_t\sim\mathcal{N}_p(0,W_t).\;}

Dependence between latent series is introduced via (1) non-diagonal $W$ (contemporaneous), (2) common factors / reduced-rank $W$ (shared stochastic trend), (3) cross terms in $G$ (lead-lag / VAR), or (4) non-diagonal $V$ (measurement co-shocks).

\textbf{10. Exam pointers.}
\begin{itemize}
\item[$\triangleright$] \texttt{exam\_jun\_2025\_q4}: (a) general DLM expression for $m$-dim. $Y_t$ with state $\theta_t\in\mathbb{R}^p$, system $(F_t,G_t,V_t,W_t)$ and mutual independence. (b) $m=2$ independent RW+noise: $F=G=I_2$, $V=\mathrm{diag}(\sigma_{v_1}^2,\sigma_{v_2}^2)$, $W=\mathrm{diag}(\sigma_{w_1}^2,\sigma_{w_2}^2)$. (c) Three options to introduce dependence: (i) $W$ off-diagonal $W_{12}=\rho_w\sigma_{w_1}\sigma_{w_2}$; (ii) common-factor model $\theta_t=A f_t$ with shared 1-dim.\ RW $f_t$; (iii) cross terms in $G$ (latent VAR(1)).
\end{itemize}
""",
}


# =============================================================================
# t7e — AR(p) as DLM (companion form)
# =============================================================================
theory_content_ts["t7e"] = {
    "title": "Theory — AR(p) as DLM (companion form)",
    "content": r"""\textbf{\textcolor{red}{THEORY --- AR(p) as DLM (companion form) [Topic: T7 --- DLM building blocks]}}

\textbf{1. AR$(p)$ recap.}

An autoregressive model of order $p$ is
\[
Y_t=\alpha_1 Y_{t-1}+\alpha_2 Y_{t-2}+\cdots+\alpha_p Y_{t-p}+\varepsilon_t,\qquad \varepsilon_t\overset{\text{iid}}{\sim}\mathcal{N}(0,\sigma^2).
\]
\emph{Causality / stationarity condition.} All roots of $1-\alpha_1 z-\cdots-\alpha_p z^p=0$ lie strictly outside the unit disc; equivalently, the eigenvalues of the companion matrix (below) have modulus $<1$.

\textbf{2. Why "AR is a DLM" needs a careful construction.}

A DLM has two distinct ingredients: a \emph{latent / unobserved} state $\theta_t$ with its own Markov dynamics $\theta_t=G\theta_{t-1}+w_t$, and an observation equation $Y_t=F\theta_t+v_t$ with $v_t\perp\theta_t$. The conditional independence $Y_t\perp Y_{1:t-1}\mid\theta_t$ must hold.

\boxed{\;\text{Non-example. Placing past observations into }F_t\text{ does NOT give a DLM.}\;}\quad Specifically, $Y_t=(Y_{t-1},Y_{t-2})\,(\alpha_1,\alpha_2)'+\varepsilon_t$ with "state" $(\alpha_1,\alpha_2)$ frozen and "$F_t$"$=(Y_{t-1},Y_{t-2})$ violates the DLM template in two ways:
\begin{itemize}
\item There is no state innovation ($w_t=0$); the "state" is constant. Trivially OK in itself, but…
\item $F_t=(Y_{t-1},Y_{t-2})$ depends on past \emph{observations} (not deterministic inputs). Then $Y_t\mid\theta_t$ would depend on $Y_{1:t-1}$, breaking $Y_t\perp Y_{1:t-1}\mid\theta_t$.
\end{itemize}
So the proposed "AR$(2)$ as DLM" with that observation matrix is \emph{wrong} as a DLM.

\textbf{3. Correct construction --- companion form (DLMwR \S 3.2.5).}

The right idea: put \emph{$Y_t$ and its needed lags} into the latent state, so that the AR dynamics become a vector-valued first-order Markov recursion. For AR$(p)$ take

\boxed{\;\theta_t=\begin{pmatrix}Y_t\\ Y_{t-1}\\ \vdots\\ Y_{t-p+1}\end{pmatrix}\in\mathbb{R}^p.\;}

Then $\theta_t=G\theta_{t-1}+w_t$ with the \emph{companion matrix}
\[
G=\begin{pmatrix}\alpha_1 & \alpha_2 & \cdots & \alpha_{p-1} & \alpha_p\\ 1 & 0 & \cdots & 0 & 0\\ 0 & 1 & \cdots & 0 & 0\\ \vdots & & \ddots & & \vdots\\ 0 & 0 & \cdots & 1 & 0\end{pmatrix},\quad w_t=\begin{pmatrix}\varepsilon_t\\ 0\\ \vdots\\ 0\end{pmatrix},
\]
and observation equation
\[
Y_t=F\theta_t,\qquad F=(1,\,0,\dots,0),\qquad V=0.
\]
State noise covariance:
\[
W=\begin{pmatrix}\sigma^2 & 0 & \cdots & 0\\ 0 & 0 & \cdots & 0\\ \vdots & & \ddots & \vdots\\ 0 & 0 & \cdots & 0\end{pmatrix}\quad(\text{rank-1, only top-left non-zero}).
\]

\textbf{Verification (AR$(2)$ specialisation).}

\boxed{\;G=\begin{pmatrix}\alpha_1 & \alpha_2\\ 1 & 0\end{pmatrix},\quad F=(1,0),\quad V=0,\quad W=\begin{pmatrix}\sigma^2 & 0\\ 0 & 0\end{pmatrix}.\;}

\emph{Row 1 of} $G\theta_{t-1}+w_t$: $\alpha_1 Y_{t-1}+\alpha_2 Y_{t-2}+\varepsilon_t=Y_t$. \emph{Row 2}: $Y_{t-1}$, the lagged first coordinate. \emph{Observation}: $F\theta_t=Y_t$ (deterministic since $V=0$). All consistency requirements (state Markov, conditional indep.\ $Y_t\perp Y_{1:t-1}\mid\theta_t$ — trivially, $\theta_t$ contains $Y_t$) hold.

\textbf{4. ARMA$(p,q)$ as DLM.}

Same idea generalises: take the state to be $(Y_t,Y_{t-1},\dots,Y_{t-r+1})$ with $r=\max(p,q+1)$, fold the moving-average part into $w_t$ via a suitable observation/state noise loading $R$, $w_t=R\eta_t$. See DLMwR \S 3.2.5 for the canonical Harvey form. The qualitative takeaway: any (causal) ARMA admits a DLM representation, opening it to KF estimation, missing-data handling, and Bayesian inference.

\textbf{5. Why bother --- benefits of the DLM representation.}

\begin{itemize}
\item \emph{Missing data}: KF handles gaps automatically (just skip the observation update).
\item \emph{Multivariate}: VARMA fits into the same framework.
\item \emph{Time-varying parameters}: replace constant $\alpha_j$ by drifting $\alpha_{j,t}$ trivially.
\item \emph{Inference unified}: same MLE / Bayesian / FFBS machinery as any other DLM.
\item \emph{Forecasting}: KF gives the predictive automatically with proper uncertainty.
\end{itemize}

\textbf{6. Worked micro-example.}

AR$(2)$ with $\alpha_1=1.2,\alpha_2=-0.35,\sigma=1$ (causal, roots outside unit circle). Companion $G=\bigl(\begin{smallmatrix}1.2 & -0.35\\ 1 & 0\end{smallmatrix}\bigr)$; eigenvalues $0.6\pm 0.06i\Rightarrow$ moduli $\approx 0.6<1$, OK. State $\theta_t=(Y_t,Y_{t-1})'$; one-step forecast $\widehat Y_{t+1\mid t}=\alpha_1 Y_t+\alpha_2 Y_{t-1}=$ first row of $G\theta_t$.

\textbf{7. R --- AR(2) as DLM.}

```R
library(dlm)
alpha <- c(1.2, -0.35); sigma2 <- 1

# Direct simulation as AR(2)
y_ar <- arima.sim(model = list(ar = alpha), n = 500, sd = sqrt(sigma2))

# Companion-form DLM (manual)
G <- rbind(alpha, c(1, 0))
F_ <- c(1, 0)
V  <- 0
W  <- diag(c(sigma2, 0))
mod <- dlm(FF = F_, GG = G, V = V, W = W,
           m0 = c(0, 0), C0 = diag(2) * 1e7)

# Equivalent built-in
mod_eq <- dlmModARMA(ar = alpha, sigma2 = sigma2)

# Filter on the simulated series
kf <- dlmFilter(y_ar, mod)
plot.ts(cbind(y_ar, kf$f), col = c("black","red"),
        main="AR(2) data with KF 1-step forecasts")

# Forecast 10 steps ahead
fc <- dlmForecast(kf, nAhead = 10)
fc$f      # forecast means
fc$Q      # forecast variances
```

\textbf{8. Bottom line.}

\boxed{\;\text{AR}(p)\text{ is a DLM with state }\theta_t=(Y_t,\dots,Y_{t-p+1})',\;G=\text{companion},\;F=(1,0,\dots,0),\;V=0,\;W=\sigma^2 e_1 e_1'.\;}

The wrong construction puts past observations into $F_t$ and freezes the "state" at the coefficients — this is not a DLM. Companion form is the canonical DLM-isation.

\textbf{9. Exam pointers.}
\begin{itemize}
\item[$\triangleright$] \texttt{exam\_jun\_2024\_q2}: "Is the proposed observation matrix $F_t=(Y_{t-1},Y_{t-2})$ with frozen state $(\alpha_1,\alpha_2)$ a valid DLM for AR$(2)$?" \emph{NO}: $\theta_t$ should be a latent unobserved process with its own dynamics, not constant coefficients; and $F_t$ should not depend on past observations, otherwise $Y_t\perp Y_{1:t-1}\mid\theta_t$ fails. \emph{Correct construction}: companion form $\theta_t=(Y_t,Y_{t-1})'$, $G=\bigl(\begin{smallmatrix}\alpha_1 & \alpha_2\\ 1 & 0\end{smallmatrix}\bigr)$, $F=(1,0)$, $V=0$, $W=\mathrm{diag}(\sigma^2,0)$.
\end{itemize}
""",
}


# =============================================================================
# t8a — Filtering distribution: definition, not just a point estimate
# =============================================================================
theory_content_ts["t8a"] = {
    "title": "Theory — Filtering distribution: definition, not just a point estimate",
    "content": r"""\textbf{\textcolor{red}{THEORY --- Filtering distribution: definition, not just a point estimate [Topic: T8 --- Kalman filter]}}

\textbf{1. The filtering problem.}

Consider a general state-space model with latent state $(\theta_t)_{t\ge 0}$ and observation process $(Y_t)_{t\ge 1}$:
\[
\theta_0\sim\pi,\qquad \theta_t\mid\theta_{t-1}\sim f(\theta_t\mid\theta_{t-1}),\qquad Y_t\mid\theta_t\sim f(y_t\mid\theta_t).
\]
The \emph{filtering problem} asks: given observations $y_{1:t}=(y_1,\dots,y_t)$ up to the current time $t$, what can we say about the present state $\theta_t$?

\boxed{\;\text{Filtering distribution: }\pi(\theta_t\mid y_{1:t}).\;}

\textbf{2. Why it is a distribution, not a point.}

\boxed{\;\textbf{Common error to flag:}\;\text{Filtering does NOT mean "computing }\mathbb{E}[\theta_t\mid y_{1:t}]\text{".}\;}

The full filtering distribution $\pi(\theta_t\mid y_{1:t})$ is the object of inference, for three reasons.

\emph{(i) Uncertainty quantification.} Practical decisions (forecast credible intervals, risk control, hypothesis testing) need not just a point but a measure of uncertainty: variance, credible region, predictive density. The conditional mean alone discards all of that.

\emph{(ii) Non-Gaussian shape.} In general SSMs (non-Gaussian observation, non-linear dynamics), the filtering distribution can be multi-modal, skewed, heavy-tailed. The mean is then a poor summary; the distribution carries the full information.

\emph{(iii) Sequential update.} The filtering recursion propagates the \emph{distribution} forward (predict + update via Bayes), not just its mean. Trying to propagate only the mean breaks the recursion in non-Gaussian / non-linear settings.

\textbf{3. Filtering vs other inference targets.}

The targets to distinguish (DLMwR \S 2.5):
\begin{itemize}
\item \emph{Filtering} $\pi(\theta_t\mid y_{1:t})$ — current state, data up to now (online; the index of state matches the latest data point).
\item \emph{Prediction (state)} $\pi(\theta_{t+k}\mid y_{1:t})$, $k\ge 1$ — future state.
\item \emph{Prediction (observation)} $\pi(Y_{t+k}\mid y_{1:t})$, $k\ge 1$ — future observation.
\item \emph{Smoothing (marginal)} $\pi(\theta_t\mid y_{1:T})$, $t<T$ — past state given \emph{all} data.
\item \emph{Smoothing (joint)} $\pi(\theta_{0:T}\mid y_{1:T})$ — full latent path.
\end{itemize}

So saying "filtering computes $\mathbb{E}(\theta_i\mid y_{1:t})$" is doubly wrong: it confuses distribution with mean, \emph{and} the index $i$ with the current time $t$ (the latter would be prediction or smoothing depending on $i\gtreqless t$).

\textbf{4. The DLM exception --- when the mean and covariance ARE enough.}

For a DLM (Gaussian linear SSM), the filtering distribution is Gaussian by induction on $t$. Hence $(m_t,C_t)$ — mean and covariance — \emph{fully} characterise it:
\[
\theta_t\mid y_{1:t}\sim\mathcal{N}_p(m_t,C_t)\quad\Longleftrightarrow\quad\pi(\theta_t\mid y_{1:t})\text{ is determined by }(m_t,C_t).
\]
The KF returns precisely $(m_t,C_t)$ recursively.

\boxed{\;\text{In a DLM: }(m_t,C_t)\text{ suffice to characterise }\pi(\theta_t\mid y_{1:t}).\;}

\emph{Proof of induction.} Base: $\theta_0\sim\mathcal{N}_p(m_0,C_0)$. Step: assume $\theta_{t-1}\mid y_{1:t-1}\sim\mathcal{N}_p(m_{t-1},C_{t-1})$. The predict step $\theta_t=G_t\theta_{t-1}+w_t$ with $w_t\perp\theta_{t-1}\mid y_{1:t-1}$, $w_t\sim\mathcal{N}_p(0,W_t)$, gives a linear combination of jointly Gaussian variables, so $\theta_t\mid y_{1:t-1}\sim\mathcal{N}_p(a_t,R_t)$ with $a_t=G_t m_{t-1}$, $R_t=G_t C_{t-1}G_t'+W_t$. Then $(Y_t,\theta_t)\mid y_{1:t-1}$ is jointly Gaussian, so the conditional $\theta_t\mid(y_{1:t-1},Y_t=y_t)=\theta_t\mid y_{1:t}$ is Gaussian (Gaussian conditional formulas). \hfill$\square$

\textbf{5. Credible intervals from $(m_t,C_t)$ --- DLM case.}

For a DLM, marginal $(1-\alpha)$ credible interval for component $j$ of $\theta_t$:
\[
\bigl(m_t\bigr)_j\;\pm\;z_{1-\alpha/2}\,\sqrt{(C_t)_{jj}}.
\]
Joint $(1-\alpha)$ credible ellipsoid: $\{\theta:(\theta-m_t)'C_t^{-1}(\theta-m_t)\le\chi^2_{p,1-\alpha}\}$.

\textbf{6. When $(m_t,C_t)$ are NOT enough.}

\emph{(i) Non-Gaussian / non-linear SSMs.} If the observation noise is heavy-tailed, the state transition is non-linear (e.g.\ stochastic volatility, count data with Poisson emission), or the prior is non-Gaussian, the filtering distribution is not Gaussian. Mean and covariance no longer determine credible intervals or the shape. Need:
\begin{itemize}
\item \emph{Extended KF (EKF)}: linearise around current estimate — approximate.
\item \emph{Unscented KF}: deterministic sigma-point matching — better than EKF.
\item \emph{Particle filter} (sequential Monte Carlo): represent $\pi(\theta_t\mid y_{1:t})$ by weighted samples — exact in the large-particle limit, handles any non-linearity / non-Gaussianity.
\end{itemize}

\emph{(ii) DLM with unknown parameters $\phi$.} The marginal filtering distribution
\[
p(\theta_t\mid y_{1:t})=\int p(\theta_t\mid y_{1:t},\phi)\,p(\phi\mid y_{1:t})\,d\phi
\]
is a \emph{mixture of Gaussians}, generally non-Gaussian. Plug-in $\widehat\phi$ ignores parameter uncertainty; full Bayesian inference (Gibbs / MCMC) is needed.

\textbf{7. Worked micro-example --- random-walk-plus-noise.}

Take $V=W=1$, $m_0=0$, $C_0=1$. After $t=1$ with $y_1=2$:
\[
a_1=0,\;R_1=2,\;f_1=0,\;Q_1=3,\;K_1=2/3,\;m_1=2/3\cdot 2\approx 1.33,\;C_1=2-(2/3)^2\cdot 3\approx 0.67.
\]
So $\theta_1\mid y_1=2\sim\mathcal{N}(1.33,0.67)$. A 95\% CI is $1.33\pm 1.96\sqrt{0.67}\approx[-0.27,2.93]$ — \emph{the distribution}, not just the point $1.33$. The CI is the part of the answer that requires the full conditional distribution.

\textbf{8. R --- inspect the full filtering distribution.}

```R
library(dlm)
mod <- dlmModPoly(order = 1, dV = 1, dW = 1, m0 = 0, C0 = 1)
y   <- c(2, 1.5, 1.8, 0.7, 1.2)
kf  <- dlmFilter(y, mod)
m   <- kf$m[-1]                            # filtering means m_t
C   <- sapply(dlmSvd2var(kf$U.C, kf$D.C), as.numeric)[-1]  # filtering variances C_t

# Filtering distribution at each t:
data.frame(t = 1:length(y), m_t = m, C_t = C,
           lo95 = m - 1.96*sqrt(C),
           hi95 = m + 1.96*sqrt(C))

# Plot filtering mean +/- 1.96 SD bands
plot.ts(y, ylab="y_t")
lines(m, col="red"); lines(m - 1.96*sqrt(C), col="red", lty=2); lines(m + 1.96*sqrt(C), col="red", lty=2)
```

\textbf{9. Bottom line --- correct verdict for the typical exam prompt.}

\boxed{\;\text{Filtering = computing }\pi(\theta_t\mid y_{1:t})\text{ (a \emph{distribution}), not just }\mathbb{E}(\theta_t\mid y_{1:t}).\;}

In a \emph{DLM} (linear, Gaussian), the filtering distribution is Gaussian and is fully characterised by $(m_t,C_t)$ — so the KF mean and variance \emph{do} suffice for credible intervals. In general state-space models they do not, and one must keep the full distribution (or its samples).

\textbf{10. Exam pointers.}
\begin{itemize}
\item[$\triangleright$] \texttt{exam\_may\_2024\_q4}: "Filtering = computing $\mathbb{E}(\theta_i\mid y_{1:t})$ — correct?" \emph{NO}: (1) filtering targets the conditional \emph{distribution} $\pi(\theta_t\mid y_{1:t})$, not a point estimate; (2) the index should be $t$ (current state), not arbitrary $i$ (that would be prediction $i>t$ or smoothing $i<t$).
\item[$\triangleright$] \texttt{exam\_jun\_2022\_q2}: "In a DLM, are $(m_t,C_t)$ enough to characterise the filtering distribution?" \emph{YES} in a DLM: by induction, $\theta_t\mid y_{1:t}$ is Gaussian (KF preserves Gaussianity), so the two moments determine the distribution. \emph{NO} in general SSMs (non-Gaussian / non-linear): the filtering distribution can be multimodal/skewed; one needs particle filters.
\item[$\triangleright$] \texttt{exam\_may\_2023\_q6}: same point — in a DLM the filtering distribution is Gaussian, so $(m_t,C_t)$ from the KF give marginal CIs $(m_t)_j\pm z_{1-\alpha/2}\sqrt{(C_t)_{jj}}$ and joint ellipsoids. Caveats: fails for non-Gaussian SSMs (need particle filters) and for plug-in at $\widehat\phi$ (parameter uncertainty understated).
\end{itemize}
""",
}


# =============================================================================
# t8b — KF predict + update derivation (with Bayes step)
# =============================================================================
theory_content_ts["t8b"] = {
    "title": "Theory — KF predict + update derivation (with Bayes step)",
    "content": r"""\textbf{\textcolor{red}{THEORY --- KF predict + update derivation (with Bayes step) [Topic: T8 --- Kalman filter]}}

\textbf{1. The KF goal.}

In a DLM
\[
\theta_t=G_t\theta_{t-1}+w_t,\;w_t\sim\mathcal{N}_p(0,W_t);\qquad Y_t=F_t\theta_t+v_t,\;v_t\sim\mathcal{N}_q(0,V_t);
\]
with $\theta_0\sim\mathcal{N}_p(m_0,C_0)$ and $\{w_t\},\{v_t\},\theta_0$ mutually independent, the \emph{Kalman filter} computes recursively
\[
\theta_{t-1}\mid y_{1:t-1}\sim\mathcal{N}_p(m_{t-1},C_{t-1})\;\longrightarrow\;\theta_t\mid y_{1:t}\sim\mathcal{N}_p(m_t,C_t).
\]
The recursion has three steps: (1) predict the state, (2) predict the observation, (3) update via Bayes.

\textbf{2. Step 1 --- predict the state.}

\boxed{\;\theta_t\mid y_{1:t-1}\sim\mathcal{N}_p(a_t,R_t),\quad a_t=G_t m_{t-1},\quad R_t=G_t C_{t-1}G_t'+W_t.\;}

\emph{Proof.} The state recursion gives $\theta_t=G_t\theta_{t-1}+w_t$. We use two facts about $w_t$:
\begin{itemize}
\item $w_t\perp\sigma(\theta_{t-1},y_{1:t-1})$. \emph{Reason:} $y_{1:t-1}$ is a function of $(\theta_{0:t-1},v_{1:t-1})$; by the mutual independence of $\{w_t\},\{v_t\},\theta_0$, $w_t$ is independent of all of $(\theta_{0:t-1},v_{1:t-1})$ — see independence proofs in t7a.
\item Conditionally on $y_{1:t-1}$, $\theta_{t-1}\sim\mathcal{N}_p(m_{t-1},C_{t-1})$ by induction hypothesis.
\end{itemize}
Hence the conditional moments:
\[
\mathbb{E}[\theta_t\mid y_{1:t-1}]=G_t\mathbb{E}[\theta_{t-1}\mid y_{1:t-1}]+\mathbb{E}[w_t]=G_t m_{t-1}=a_t,
\]
\[
\operatorname{Var}(\theta_t\mid y_{1:t-1})=G_t\operatorname{Var}(\theta_{t-1}\mid y_{1:t-1})G_t'+\operatorname{Var}(w_t)=G_t C_{t-1}G_t'+W_t=R_t,
\]
with the cross term $\operatorname{Cov}(\theta_{t-1},w_t\mid y_{1:t-1})=0$ by independence. The conditional law is Gaussian (linear combination of jointly Gaussian variables), so $\theta_t\mid y_{1:t-1}\sim\mathcal{N}_p(a_t,R_t)$. $\square$

\textbf{3. Step 2 --- predict the observation.}

\boxed{\;Y_t\mid y_{1:t-1}\sim\mathcal{N}_q(f_t,Q_t),\quad f_t=F_t a_t,\quad Q_t=F_t R_t F_t'+V_t.\;}

\emph{Proof.} Same structure as Step 1. $Y_t=F_t\theta_t+v_t$ with $v_t\perp\sigma(\theta_t,y_{1:t-1})$ (by mutual independence). Take conditional moments:
\[
\mathbb{E}[Y_t\mid y_{1:t-1}]=F_t a_t=f_t,
\]
\[
\operatorname{Var}(Y_t\mid y_{1:t-1})=F_t R_t F_t'+V_t=Q_t,
\]
and Gaussianity follows by linearity. $\square$

\textbf{Cross-covariance.} For Step 3 we will also need
\[
\operatorname{Cov}(\theta_t,Y_t\mid y_{1:t-1})=\operatorname{Cov}(\theta_t,F_t\theta_t+v_t\mid y_{1:t-1})=\operatorname{Var}(\theta_t\mid y_{1:t-1})F_t'+\operatorname{Cov}(\theta_t,v_t\mid y_{1:t-1})=R_tF_t'.
\]
(The second term is zero since $v_t\perp\theta_t\mid y_{1:t-1}$.)

\textbf{4. Step 3 --- update via Bayes' rule (the key step).}

The update step is where \emph{Bayes' rule} enters. Treat $\pi(\theta_t\mid y_{1:t-1})$ as the prior on the state and $p(y_t\mid\theta_t)$ as the likelihood of the new observation:
\[
p(\theta_t\mid y_{1:t})=p(\theta_t\mid y_{1:t-1},y_t)\;\propto\;p(y_t\mid\theta_t,y_{1:t-1})\,p(\theta_t\mid y_{1:t-1})=p(y_t\mid\theta_t)\,p(\theta_t\mid y_{1:t-1}),
\]
using $Y_t\perp y_{1:t-1}\mid\theta_t$ (conditional independence of observations in the SSM). The right-hand side is a product of Gaussians, hence Gaussian — but the cleanest derivation uses the joint:

\emph{Joint Gaussian and conditioning.} From Steps 1, 2, 3,
\[
\begin{pmatrix}\theta_t\\ Y_t\end{pmatrix}\;\Big|\;y_{1:t-1}\;\sim\;\mathcal{N}_{p+q}\left(\begin{pmatrix}a_t\\ f_t\end{pmatrix},\;\begin{pmatrix}R_t & R_tF_t'\\ F_tR_t & Q_t\end{pmatrix}\right).
\]
Standard Gaussian conditioning gives: if $(X,Y)\sim\mathcal{N}(\mu_X,\mu_Y,\Sigma_{XX},\Sigma_{XY},\Sigma_{YY})$, then $X\mid Y=y\sim\mathcal{N}(\mu_X+\Sigma_{XY}\Sigma_{YY}^{-1}(y-\mu_Y),\,\Sigma_{XX}-\Sigma_{XY}\Sigma_{YY}^{-1}\Sigma_{YX})$. Applied here with $K_t:=R_tF_t'Q_t^{-1}$:

\boxed{\;\theta_t\mid y_{1:t}\sim\mathcal{N}_p(m_t,C_t),\quad m_t=a_t+K_t(y_t-f_t),\quad C_t=R_t-K_t Q_t K_t',\quad K_t=R_tF_t'Q_t^{-1}.\;}

\emph{Terminology.} $K_t$ is the \emph{Kalman gain}; $y_t-f_t$ is the \emph{innovation} (forecast error); $C_t=R_t-K_t F_t R_t=(I-K_t F_t)R_t$ is an equivalent form.

\textbf{Where is the Bayes step?} In moving from the joint $(θ_t,Y_t)\mid y_{1:t-1}$ to the conditional $\theta_t\mid(y_{1:t-1},Y_t=y_t)=\theta_t\mid y_{1:t}$. Conditioning on the realised value $Y_t=y_t$ is exactly the Bayes update: prior $\pi(\theta_t\mid y_{1:t-1})$ updated by likelihood $p(y_t\mid\theta_t)$, with the joint Gaussian assumption ensuring conjugacy and a closed-form posterior.

\textbf{5. Univariate RW+noise specialisation.}

For $F=G=1$, $V$ and $W$ scalars:
\[
\boxed{\;a_t=m_{t-1},\;R_t=C_{t-1}+W,\;f_t=a_t,\;Q_t=R_t+V,\;K_t=R_t/Q_t\in(0,1),\;m_t=a_t+K_t(y_t-f_t),\;C_t=(1-K_t)R_t=\frac{V R_t}{Q_t}.\;}
\]
The recursion for the filtering variance reduces to the Riccati equation
\[
C_t=\frac{V(C_{t-1}+W)}{C_{t-1}+W+V}.
\]

\textbf{Steady-state.} Solving $C^*=V(C^*+W)/(C^*+W+V)$ gives
\[
C^*=\tfrac12\bigl(-W+\sqrt{W^2+4VW}\bigr)>0,\qquad (W>0).
\]
So \emph{the filtering variance does not converge to $0$} unless $W=0$ (static $\theta$). \emph{Intuition.} Every step injects fresh state noise $w_t\sim\mathcal{N}(0,W)$; even with infinite past data, the most recent innovation is irreducibly unknown until $Y_t$ is observed.

If $W=0$, the state is constant $\theta_t=\theta_0=\theta$. The Riccati becomes $1/C_n=1/C_0+n/V$, i.e.\ $C_n=VC_0/(V+nC_0)\to 0$ as $n\to\infty$, at rate $1/n$ (precision adds up like in i.i.d. Bayes).

\textbf{6. Reading the formulas --- Kalman gain.}

$K_t=R_t F_t'Q_t^{-1}$ measures how much weight to give to the innovation $y_t-f_t$:
\begin{itemize}
\item Small $V$ (precise observations): $Q_t\approx F_t R_t F_t'$, $K_t F_t\approx I$, $m_t\approx y_t$.
\item Large $V$ (noisy observations): $K_t\approx 0$, $m_t\approx a_t$ (trust the prior).
\item Small $W$ (slowly drifting state): $R_t\approx C_{t-1}$, smaller $K_t$, smoother filter.
\item Large $W$ (rapidly drifting state): larger $K_t$, filter tracks data tightly.
\end{itemize}

\textbf{7. Worked micro-example --- RW+noise update.}

$V=W=1$, $m_0=0$, $C_0=1$, $y_1=2$. Step 1: $a_1=0$, $R_1=2$. Step 2: $f_1=0$, $Q_1=3$. Step 3: $K_1=2/3$, $m_1=0+(2/3)(2-0)=1.33$, $C_1=2-(2/3)^2\cdot 3=2/3\approx 0.67$. So $\theta_1\mid y_1=2\sim\mathcal{N}(1.33,0.67)$, with 95\% CI $\approx[-0.27,2.93]$.

\textbf{8. R --- KF on RW+noise.}

```R
library(dlm)
V <- 1; W <- 1
mod <- dlmModPoly(order = 1, dV = V, dW = W, m0 = 0, C0 = 1)

# One-step update by hand
m_prev <- 0; C_prev <- 1; y_t <- 2
a <- m_prev;           R <- C_prev + W
f <- a;                Q <- R + V
K <- R / Q;            m <- a + K * (y_t - f);   C <- (1 - K) * R
c(a = a, R = R, f = f, Q = Q, K = K, m = m, C = C)
# a=0  R=2  f=0  Q=3  K=0.667  m=1.333  C=0.667

# Full KF on a sequence (matches the by-hand result at t=1)
y  <- c(2, 1.5, 1.8, 0.7, 1.2)
kf <- dlmFilter(y, mod)
kf$m              # filtering means m_0,...,m_T
sapply(dlmSvd2var(kf$U.C, kf$D.C), as.numeric)  # filtering variances

# Iterate to the steady state
C <- 1
for (t in 1:200) C <- V*(C + W)/(C + W + V)
C   # ~ 0.5*(-W + sqrt(W^2 + 4*V*W)) for V=W=1 -> ~0.618
```

\textbf{9. Bottom line.}

\boxed{\;\text{KF = Predict state }(a_t,R_t)\,\to\,\text{Predict obs }(f_t,Q_t)\,\to\,\text{Bayes update }(m_t,C_t)\text{ via Gaussian conditioning on }Y_t=y_t.\;}

Bayes' rule is used in the update step; conjugacy of Gaussian-Gaussian gives a closed-form Gaussian posterior. $K_t$ is the Kalman gain; $y_t-f_t$ the innovation; $C_t\to C^*>0$ as $t\to\infty$ when $W>0$, $C_t\to 0$ at rate $1/t$ when $W=0$.

\textbf{10. Exam pointers.}
\begin{itemize}
\item[$\triangleright$] \texttt{exam\_jun\_2025\_q3}: (a) define SSM; (b) "filtering = point estimate?" — NO, it's a distribution; (c) given $\theta_{t-1}\mid y_{1:t-1}\sim\mathcal{N}_p(m_{t-1},C_{t-1})$, derive $\theta_t\mid y_{1:t}\sim\mathcal{N}_p(m_t,C_t)$ via the three steps above. Boxed: $m_t=a_t+K_t(y_t-f_t)$, $C_t=R_t-K_tQ_tK_t'$, $K_t=R_tF'Q_t^{-1}$.
\item[$\triangleright$] \texttt{exam\_may\_2025\_q5}: (a) RW+noise; (b) KF recursions in the scalar RW+noise: $a_t=m_{t-1},R_t=C_{t-1}+W,f_t=a_t,Q_t=R_t+V,K_t=R_t/Q_t,m_t=a_t+K_t(y_t-f_t),C_t=(1-K_t)R_t$. (c) $C_t\to 0$? NO, $C_t\to C^*=\tfrac12(-W+\sqrt{W^2+4VW})>0$ unless $W=0$; intuition = fresh state noise $w_t$ injected each step.
\item[$\triangleright$] \texttt{exam\_may\_2021\_q5}: (a) general DLM; (b) 3-line summary of the KF proof, specifying that Bayes' rule enters at the update step via Gaussian conditioning on the joint $(\theta_t,Y_t)\mid y_{1:t-1}$. (c) Proof of Step 1: $\theta_t=G_t\theta_{t-1}+w_t$, $w_t\perp(\theta_{t-1},y_{1:t-1})$, take conditional moments; Gaussianity by linearity.
\end{itemize}
""",
}


# =============================================================================
# t9a — Filtering vs smoothing — definitions & DAG-based proofs
# =============================================================================
theory_content_ts["t9a"] = {
    "title": "Theory — Filtering vs smoothing — definitions & DAG-based proofs",
    "content": r"""\textbf{\textcolor{red}{THEORY --- Filtering vs smoothing --- definitions \& DAG-based proofs [Topic: T9 --- Kalman smoother]}}

\textbf{1. Three inference targets in an SSM.}

Given a state-space model with state $(\theta_t)_{t\ge 0}$ and observation $(Y_t)_{t\ge 1}$, and the data $y_{1:T}$, three core conditional distributions arise:

\boxed{\;
\begin{aligned}
\text{Filtering:}\quad &\pi(\theta_t\mid y_{1:t})\quad\text{— current state, data \emph{up to now} (online)}.\\
\text{Joint smoothing:}\quad &\pi(\theta_{0:T}\mid y_{1:T})\quad\text{— full latent path, given \emph{all} data (offline)}.\\
\text{Marginal smoothing:}\quad &\pi(\theta_t\mid y_{1:T}),\;t<T\quad\text{— past state, given \emph{all} data, including \emph{after} }t.
\end{aligned}\;}

The fourth, less commonly emphasised in this course, is \emph{prediction} $\pi(\theta_{t+k}\mid y_{1:t})$, $k\ge 1$, or its observation counterpart $\pi(Y_{t+k}\mid y_{1:t})$.

\textbf{Reminder: these are \emph{distributions}, not point estimates.} The conditional mean $\mathbb{E}[\theta_t\mid y_{1:T}]$ is one summary; uncertainty quantification requires the full distribution. In a DLM this distribution is Gaussian and fully captured by $(s_t,S_t)$.

\textbf{2. Online vs offline; information content.}

\emph{Filtering} is the \emph{online} problem: as each new observation arrives, the filtering distribution is updated. Used in real-time tracking, control, Kalman-filter-based forecasting.

\emph{Smoothing} is the \emph{offline} problem: after the entire dataset is in hand, we revise our estimate of past states using \emph{all} information — including data observed \emph{after} time $t$. Used for retrospective analysis (decompose a series into trend + cycle + seasonal; reconstruct latent paths post-hoc).

\textbf{Variance reduction.} Smoothing exploits more data, so it is more informative:
\[
\boxed{\;\operatorname{Var}(\theta_t\mid y_{1:T})\preceq\operatorname{Var}(\theta_t\mid y_{1:t}),\quad\text{i.e.\ }S_t\preceq C_t\;\text{(p.s.d.\ order)}.\;}

\emph{Intuition.} Conditioning on more data cannot increase posterior variance (law of total variance / "conditioning never hurts" for posterior precision). Future observations $Y_{t+1:T}$ carry information about $\theta_t$ via the chain $\theta_t\to\theta_{t+1}\to\dots\to\theta_T$.

\textbf{3. DAG of a state-space model.}

The conditional-independence structure of an SSM is encoded by the DAG
\[
\theta_0\to\theta_1\to\theta_2\to\cdots\to\theta_{T-1}\to\theta_T,\qquad \theta_s\to Y_s\text{ for each }s\in\{1,\dots,T\}.
\]
Every state has the previous state as parent (state chain), and each observation has its contemporaneous state as parent. The joint factorises as
\[
p(\theta_{0:T},y_{1:T})=p(\theta_0)\prod_{s=1}^T p(\theta_s\mid\theta_{s-1})\,p(y_s\mid\theta_s).
\]

\textbf{4. $d$-separation reminder.}

In a DAG, a path between two sets is \emph{blocked} by a conditioning set $C$ if some node $v$ on the path is either
\begin{itemize}
\item a \emph{chain} ($a\to v\to b$) or \emph{fork} ($a\leftarrow v\to b$) with $v\in C$, or
\item a \emph{collider} ($a\to v\leftarrow b$) with neither $v$ nor any descendant of $v$ in $C$.
\end{itemize}
$A\perp B\mid C$ if every path between $A$ and $B$ is blocked. This is the standard graphical criterion for conditional independence.

\textbf{5. Backward-Markov property --- $\theta_t\perp(Y_{t+1},\dots,Y_T)\mid\theta_{t+1}$.}

\boxed{\;\theta_t\perp\!\!\!\perp(Y_{t+1},\dots,Y_T)\mid\theta_{t+1}.\;}

\emph{Proof (DAG / $d$-separation).} Consider any $Y_s$ with $s\ge t+1$. Every directed path from $\theta_t$ to $Y_s$ in the SSM DAG travels along the state chain $\theta_t\to\theta_{t+1}\to\cdots\to\theta_s\to Y_s$ — it \emph{must} pass through $\theta_{t+1}$ (chain/serial node). Conditioning on $\theta_{t+1}$ blocks this path at $\theta_{t+1}$ (chain rule for $d$-separation). All paths from $\theta_t$ to $Y_{t+1:T}$ are blocked $\Rightarrow$ $\theta_t\perp(Y_{t+1},\dots,Y_T)\mid\theta_{t+1}$. $\square$

\emph{Algebraic verification.} Using the factorisation, $p(y_{t+1:T}\mid\theta_t,\theta_{t+1})$ involves only $p(\theta_{s}\mid\theta_{s-1})$ for $s\ge t+2$ and $p(y_s\mid\theta_s)$ for $s\ge t+1$. None of these depend on $\theta_t$ once $\theta_{t+1}$ is given. Hence $p(y_{t+1:T}\mid\theta_t,\theta_{t+1})=p(y_{t+1:T}\mid\theta_{t+1})$.

\textbf{Corollary --- backward Markov for the state.}

\boxed{\;\theta_t\mid(\theta_{t+1},y_{1:T})=\theta_t\mid(\theta_{t+1},y_{1:t}).\;}

\emph{Why.} Given $\theta_{t+1}$, the future observations $y_{t+1:T}$ add no information about $\theta_t$ (just shown). So we can drop them from the conditioning set. This is the engine of the RTS smoother (next).

\textbf{6. The RTS (Rauch–Tung–Striebel) smoother.}

For a DLM, the marginal smoothing distribution is Gaussian: $\theta_t\mid y_{1:T}\sim\mathcal{N}_p(s_t,S_t)$, with $(s_t,S_t)$ obtained by a \emph{backward sweep} after the forward KF.

\boxed{\;
\begin{aligned}
& \text{Initialise: } s_T=m_T,\;S_T=C_T.\\
& \text{For }t=T-1,T-2,\dots,0:\\
&\quad J_t=C_t\,G_{t+1}'\,R_{t+1}^{-1},\\
&\quad s_t=m_t+J_t(s_{t+1}-a_{t+1}),\\
&\quad S_t=C_t-J_t(R_{t+1}-S_{t+1})J_t'.
\end{aligned}\;}

\emph{Proof sketch (DLMwR Prop. 2.4).} Three ingredients.

\emph{(i) Backward Markov:} $\theta_t\mid(\theta_{t+1},y_{1:T})=\theta_t\mid(\theta_{t+1},y_{1:t})$ (just proved).

\emph{(ii) Joint Gaussian and conditioning.} Conditionally on $y_{1:t}$, $(\theta_t,\theta_{t+1})$ is jointly Gaussian (KF predict step + induction):
\[
\begin{pmatrix}\theta_t\\\theta_{t+1}\end{pmatrix}\Big|y_{1:t}\sim\mathcal{N}_{2p}\left(\begin{pmatrix}m_t\\a_{t+1}\end{pmatrix},\begin{pmatrix}C_t & C_tG_{t+1}'\\ G_{t+1}C_t & R_{t+1}\end{pmatrix}\right).
\]
Gaussian conditioning gives, with $J_t=C_tG_{t+1}'R_{t+1}^{-1}$,
\[
\theta_t\mid(\theta_{t+1},y_{1:t})\sim\mathcal{N}_p\bigl(m_t+J_t(\theta_{t+1}-a_{t+1}),\,C_t-J_t R_{t+1} J_t'\bigr).
\]

\emph{(iii) Marginalise against the smoothing of $\theta_{t+1}$.} By backward Markov, $p(\theta_t\mid y_{1:T})=\int p(\theta_t\mid\theta_{t+1},y_{1:t})\,p(\theta_{t+1}\mid y_{1:T})\,d\theta_{t+1}$. Both factors are Gaussian; integrating gives a Gaussian with
\[
s_t=m_t+J_t(s_{t+1}-a_{t+1}),\qquad S_t=C_t-J_t R_{t+1} J_t'+J_t S_{t+1} J_t'=C_t-J_t(R_{t+1}-S_{t+1})J_t'.\;\square
\]

\textbf{7. Recursion to compute the marginal smoothing distribution.}

To obtain $\pi(\theta_t\mid y_{1:T})$ for every $t$:
\begin{enumerate}
\item Forward KF: store $(m_t,C_t)$ and $(a_{t+1},R_{t+1})$ for $t=0,\dots,T-1$, plus $(m_T,C_T)$.
\item Backward RTS: initialise $(s_T,S_T)=(m_T,C_T)$; for $t=T-1,\dots,0$ compute $J_t$, $s_t$, $S_t$.
\end{enumerate}
Cost: $O(Tp^3)$ — same as forward KF.

\textbf{8. Joint smoothing distribution and FFBS.}

The joint smoothing $\pi(\theta_{0:T}\mid y_{1:T})$ factorises by backward Markov:
\[
p(\theta_{0:T}\mid y_{1:T})=p(\theta_T\mid y_{1:T})\prod_{t=0}^{T-1}p(\theta_t\mid\theta_{t+1},y_{1:t}),
\]
each factor Gaussian. Sampling proceeds backward: draw $\theta_T\sim\mathcal{N}(m_T,C_T)$, then for $t=T-1,\dots,0$ draw $\theta_t\sim\mathcal{N}\bigl(m_t+J_t(\theta_{t+1}-a_{t+1}),\,C_t-J_t R_{t+1} J_t'\bigr)$. This is the \emph{Forward-Filter Backward-Sample (FFBS)} algorithm, central to Bayesian inference in DLMs.

\textbf{9. DLM uses --- Gaussian conjugacy summary.}

\begin{itemize}
\item \emph{Filtering}: $\theta_t\mid y_{1:t}\sim\mathcal{N}_p(m_t,C_t)$, forward KF.
\item \emph{Marginal smoothing}: $\theta_t\mid y_{1:T}\sim\mathcal{N}_p(s_t,S_t)$, RTS backward.
\item \emph{Joint smoothing samples}: by FFBS.
\end{itemize}

\textbf{10. Worked micro-example.}

For univariate RW+noise with $V=W=1$, $m_0=0$, $C_0=1$ and observations $y_{1:5}=(2,1.5,1.8,0.7,1.2)$: run KF forward, store $(m_t,C_t)$; then RTS backward gives smoothed $(s_t,S_t)$. Sanity check: $S_t\le C_t$ for $t<T$, with equality at $t=T$ (no future data beyond $T$). Smoothed path $s_{0:5}$ is smoother and tighter than filtered $m_{0:5}$.

\textbf{11. R --- KF + RTS smoother.}

```R
library(dlm)
V <- 1; W <- 1
mod <- dlmModPoly(order = 1, dV = V, dW = W, m0 = 0, C0 = 1)
y   <- c(2, 1.5, 1.8, 0.7, 1.2)

# Forward filter
kf <- dlmFilter(y, mod)
m  <- kf$m;              C  <- sapply(dlmSvd2var(kf$U.C, kf$D.C), as.numeric)

# Backward smoother (RTS)
sm <- dlmSmooth(kf)
s  <- sm$s;              S  <- sapply(dlmSvd2var(sm$U.S, sm$D.S), as.numeric)

# Compare filtered vs smoothed means and variances
cbind(t = 0:length(y), m = m, s = s, C = C, S = S)
# S_t <= C_t for t < T; equality at t = T (smoothing buys no info beyond T)

# Joint smoothing sample (FFBS)
library(dlm)
sample_path <- dlmBSample(kf)   # one draw from p(theta_{0:T} | y_{1:T})

# Diagnostic plot
plot.ts(y, ylab="y_t")
lines(m[-1], col="red")          # filtered mean
lines(s[-1], col="blue")          # smoothed mean
legend("topright", c("filtered m_t","smoothed s_t"), col=c("red","blue"), lty=1)
```

\textbf{12. Bottom line.}

\boxed{\;\text{Filtering: }\pi(\theta_t\mid y_{1:t});\;\;\text{Smoothing (joint): }\pi(\theta_{0:T}\mid y_{1:T});\;\;\text{Smoothing (marginal): }\pi(\theta_t\mid y_{1:T}).\;}

DLM case: KF forward gives $(m_t,C_t)$; RTS backward gives $(s_t,S_t)$; $S_t\preceq C_t$. The backward-Markov property $\theta_t\perp Y_{t+1:T}\mid\theta_{t+1}$ — provable in two lines by $d$-separation through the serial node $\theta_{t+1}$ — is the engine of the RTS recursion.

\textbf{13. Exam pointers.}
\begin{itemize}
\item[$\triangleright$] \texttt{exam\_sep\_2025\_q7}: (a) likelihood by prediction-error decomp.\ (KF gives $\mathcal{N}_q(f_t,Q_t)$ at each step). (b) Definitions: filtering = $\pi(\theta_t\mid y_{1:t})$; joint smoothing = $\pi(\theta_{0:T}\mid y_{1:T})$; marginal smoothing = $\pi(\theta_t\mid y_{1:T})$, $t<T$. (c) RTS recursion + proof hint: backward Markov + Gaussian conditioning + marginalise against $\pi(\theta_{t+1}\mid y_{1:T})$.
\item[$\triangleright$] \texttt{exam\_jun\_2022\_q3}: "Smoothing = providing $\mathbb{E}(\theta_t\mid y_{1:T})$ — correct?" \emph{NO}: smoothing targets the conditional \emph{distribution} $\pi(\theta_t\mid y_{1:T})$ (or joint $\pi(\theta_{0:T}\mid y_{1:T})$); the mean is one summary. In a DLM, $(s_t,S_t)$ from RTS describe the (Gaussian) marginal smoothing distribution.
\item[$\triangleright$] \texttt{exam\_may\_2023\_q5}: prove via DAG that $\theta_t\perp(Y_{t+1},\dots,Y_T)\mid\theta_{t+1}$. DAG: $\theta_0\to\theta_1\to\cdots\to\theta_T$ with $\theta_s\to Y_s$. Every directed path from $\theta_t$ to $Y_s$ ($s\ge t+1$) passes through $\theta_{t+1}$ (serial/chain node). Conditioning on $\theta_{t+1}$ blocks every such path; by $d$-separation, $\theta_t\perp(Y_{t+1},\dots,Y_T)\mid\theta_{t+1}$. This is the backward-Markov property used by the RTS smoother.
\item[$\triangleright$] \texttt{exam\_may\_2022\_q7}: filtering vs smoothing in a general DLM. Filtering = $\pi(\theta_t\mid y_{1:t})$ (online); smoothing = $\pi(\theta_{0:T}\mid y_{1:T})$ joint or $\pi(\theta_t\mid y_{1:T})$ marginal (offline). Smoothing more informative: $\operatorname{Var}(\theta_t\mid y_{1:T})\preceq\operatorname{Var}(\theta_t\mid y_{1:t})$. For DLM: filtering = forward KF; smoothing = RTS backward sweep.
\end{itemize}
""",
}
