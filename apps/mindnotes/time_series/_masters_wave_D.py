"""Masters wave D — T7e + T8 + T9 + T10a."""
master_exercises_ts = {}

master_exercises_ts["t7e"] = {
    "title": "Master — AR(p) as DLM (companion form)",
    "content": r"""\textbf{\textcolor{red}{MASTER --- AR(p) as DLM (companion form)}}

\emph{Canonical framework:} an AR($p$) process can be cast as a DLM by stacking the last $p$ values into a latent state vector $\theta_t=(Y_t,Y_{t-1},\dots,Y_{t-p+1})^{\top}$ and using the \emph{companion matrix} for $G$. This shows AR($p$) is a strict special case of DLMs --- not a recipe for plugging past observations into $F_t$. In the DLM construction $\theta_t$ must be the \emph{latent} process with its own innovation $w_t$, and $F_t$ must not depend on past data (otherwise the conditional independence $Y_t\perp Y_{1:t-1}\mid\theta_t$ fails).

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
    "images": ["images/master/master_t7e_ai.png"]
}

master_exercises_ts["t8a"] = {
    "title": "Master — Filtering distribution: definition, not just a point estimate",
    "content": r"""\textbf{\textcolor{red}{MASTER --- Filtering distribution: definition, not just a point estimate}}

\emph{Canonical framework:} \textbf{Filtering} in a state-space model targets the \emph{distribution} $\pi(\theta_t\mid y_{1:t})$ of the \emph{current} state given data up to now --- \emph{not} merely a point estimate $\mathbb{E}[\theta_t\mid y_{1:t}]$. The conditional mean is just one summary; honest uncertainty quantification (credible intervals, predictive intervals, decisions under risk) requires the whole distribution. In a Gaussian DLM the filtering distribution is Gaussian, so $(m_t,C_t)$ characterise it completely. Outside the Gaussian/linear world (non-linear or non-Gaussian SSMs), $(m_t,C_t)$ are \emph{not} sufficient and the full distribution must be tracked (e.g.\ by particle filters).

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
    "images": ["images/master/master_t8a_ai.png"]
}

master_exercises_ts["t8b"] = {
    "title": "Master — KF predict + update derivation with Bayes step",
    "content": r"""\textbf{\textcolor{red}{MASTER --- KF predict + update derivation with Bayes step}}

\emph{Canonical framework:} the Kalman filter is the recursive update of the Gaussian filtering distribution $\theta_t\mid y_{1:t}\sim\mathcal{N}_p(m_t,C_t)$ in a DLM. Each step has three parts: \textbf{(1) state prediction} via affine + Gaussian closure on the state equation; \textbf{(2) observation prediction} via affine + Gaussian closure on the observation equation; \textbf{(3) update} via \emph{Bayes' rule} on the conditionally Gaussian model, i.e.\ Gaussian conditioning of the joint $(\theta_t,Y_t)\mid y_{1:t-1}$ on the new observation $Y_t=y_t$. The Bayes step is precisely where "the new data $y_t$" enters and turns the prior $\pi(\theta_t\mid y_{1:t-1})$ into the posterior $\pi(\theta_t\mid y_{1:t})$.

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
    "images": ["images/master/master_t8b_ai.png"]
}

master_exercises_ts["t9a"] = {
    "title": "Master — Filtering vs smoothing — definitions & DAG-based proofs",
    "content": r"""\textbf{\textcolor{red}{MASTER --- Filtering vs smoothing — definitions \& DAG-based proofs}}

\emph{Canonical framework:} every SSM has the same DAG: $\theta_0\to\theta_1\to\cdots\to\theta_T$ with each $\theta_s\to Y_s$. \textbf{Filtering} returns $\pi(\theta_t\mid y_{1:t})$ (state given data \emph{up to now}, online). \textbf{Smoothing} returns $\pi(\theta_{0:T}\mid y_{1:T})$ (joint) or its marginals $\pi(\theta_t\mid y_{1:T})$ (state given \emph{all} data including \emph{future} of $t$, offline). DAG / $d$-separation arguments give the key conditional independencies behind both algorithms in 2--3 lines.

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
    "images": ["images/master/master_t9a_ai.png"]
}

master_exercises_ts["t10a"] = {
    "title": "Master — Predictive distribution N(f_t,Q_t) — derivation",
    "content": r"""\textbf{\textcolor{red}{MASTER --- Predictive distribution $\mathcal{N}(f_t,Q_t)$ — derivation}}

\emph{Canonical framework:} the \textbf{one-step-ahead predictive distribution} of $Y_t$ given past data $y_{1:t-1}$ is obtained by plugging the state predictive $\theta_t\mid y_{1:t-1}\sim\mathcal{N}_p(a_t,R_t)$ into the observation equation $Y_t=F_t\theta_t+v_t$. Affine + Gaussian closure gives
\[ Y_t\mid y_{1:t-1}\sim\mathcal{N}_q(f_t,Q_t),\quad f_t=F_t a_t,\quad Q_t=F_t R_t F_t^{\top}+V_t. \]
This is the second step of the Kalman filter; $f_t$ is the point forecast and $Q_t$ is the forecast variance. The same construction is valid for multivariate observations $Y_t\in\mathbb{R}^q$ (DLMs are not restricted to the univariate case).

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
    "images": ["images/master/master_t10a_ai.png"]
}
