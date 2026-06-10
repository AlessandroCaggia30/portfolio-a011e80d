"""Masters wave C — T6b + T7a-d."""
master_exercises_ts = {}

master_exercises_ts["t6b"] = {
    "title": "Master — SSM flexibility — non-stationarity & SV models (is it a DLM?)",
    "content": r"""\textbf{\textcolor{red}{MASTER --- SSM flexibility --- non-stationarity \& SV models (is it a DLM?)}}

\emph{Canonical framework:} a \textbf{state-space model} (SSM) is any pair $(\theta_t, Y_t)$ such that $(\theta_t)$ is a latent Markov chain and $Y_t\mid\theta_t$ is conditionally independent of everything else. A \textbf{DLM} is the linear-Gaussian special case: $\theta_t=G_t\theta_{t-1}+w_t$, $Y_t=F_t\theta_t+v_t$ with Gaussian noises. The key takeaway: \emph{SSMs do not require $(Y_t)$ or $(\theta_t)$ to be stationary}; they merely require Markovianity of the state and conditional independence of the observations. Non-linearity in the observation equation (e.g. multiplicative SV) breaks the DLM template but \emph{not} the SSM template --- the Kalman filter then no longer applies, and one resorts to particle filters, linearisation, or MCMC.

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
    "images": ["images/master/master_t6b_ai.png"]
}

master_exercises_ts["t7a"] = {
    "title": "Master — Random-walk + noise model — definition & independence proofs",
    "content": r"""\textbf{\textcolor{red}{MASTER --- Random-walk + noise model --- definition \& independence proofs}}

\emph{Canonical framework:} the \textbf{random walk plus noise} (a.k.a.\ \emph{local-level}) model is the simplest non-trivial DLM and the workhorse for series with a slowly-drifting level:
\[Y_t=\theta_t+v_t,\quad \theta_t=\theta_{t-1}+w_t,\quad v_t\overset{\text{iid}}{\sim}\mathcal{N}(0,V),\;w_t\overset{\text{iid}}{\sim}\mathcal{N}(0,W),\;\theta_0\sim\mathcal{N}(m_0,C_0),\]
all mutually independent. The two independence results below are routine consequences of the model's \emph{mutual-independence} assumption, plus the fact that the latent state $\theta_s=\theta_0+\sum_{u=1}^{s}w_u$ is a deterministic function of $(\theta_0,w_1,\dots,w_s)$.

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
    "images": ["images/master/master_t7a_ai.png"]
}

master_exercises_ts["t7b"] = {
    "title": "Master — Local linear trend / structural BSM",
    "content": r"""\textbf{\textcolor{red}{MASTER --- Local linear trend / structural BSM}}

\emph{Canonical framework:} the \textbf{local linear trend} (or \emph{linear growth}) DLM extends RW+noise with a stochastic slope, producing a 2-dim.\ state $\theta_t=(\mu_t,\beta_t)^{\top}$ (level + slope). Adding a stochastic-seasonal block gives Harvey's \textbf{Basic Structural Model (BSM)}, which absorbs trend + seasonality directly inside the latent state, so \emph{no detrending or deseasonalising is needed before model-fitting} --- a key advantage of SSMs over ARMA/SARIMA.

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
    "images": ["images/master/master_t7b_ai.png"]
}

master_exercises_ts["t7c"] = {
    "title": "Master — Time-varying-coefficient regression DLM",
    "content": r"""\textbf{\textcolor{red}{MASTER --- Time-varying-coefficient regression DLM}}

\emph{Canonical framework:} a \textbf{time-varying-coefficient (TVC) DLM} embeds an ordinary linear regression $Y_t=\alpha+\beta x_t+\varepsilon_t$ inside a state-space model by letting the coefficients themselves drift over time:
\[\theta_t=(\alpha_t,\beta_t)^{\top},\quad F_t=(1,\;x_t),\quad Y_t=F_t\theta_t+v_t,\quad \theta_t=G\theta_{t-1}+w_t,\;G=I_2.\]
Setting $W=0$ recovers static linear regression; letting $W\succ 0$ allows smooth coefficient drift, capturing non-linearity \textbf{without} specifying a parametric non-linear form. The same template generalises to multiple regressors and to hierarchical pooling across panels.

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
    "images": ["images/master/master_t7c_ai.png"]
}

master_exercises_ts["t7d"] = {
    "title": "Master — Multivariate DLM & dependence between latent series",
    "content": r"""\textbf{\textcolor{red}{MASTER --- Multivariate DLM \& dependence between latent series}}

\emph{Canonical framework:} the \textbf{multivariate DLM} stacks $q$ observed series into a vector $Y_t\in\mathbb{R}^q$ driven by a $p$-dim.\ latent state $\theta_t\in\mathbb{R}^p$; the building blocks $(F_t,G_t,V_t,W_t)$ have appropriate matrix shapes. "Independent random walks plus noise" for $m$ assets is the simplest multivariate building block ($F=G=I_m$, $W,V$ diagonal). \textbf{Dependence} between the latent "ideal price" series can be introduced through three orthogonal channels: correlated state-noise ($W$ off-diagonal), shared latent factors (low-rank $W$), or cross terms in $G$ (VAR-like spillovers).

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
    "images": ["images/master/master_t7d_ai.png"]
}
