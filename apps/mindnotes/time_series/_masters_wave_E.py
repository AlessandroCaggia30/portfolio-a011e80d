"""Masters wave E — T10b + T11 + T12 + T13."""
master_exercises_ts = {}

master_exercises_ts["t10b"] = {
    "title": "Master — Forecast function, k-step intervals, SES & loss functions",
    "content": r"""\textbf{\textcolor{red}{MASTER --- Forecast function, k-step intervals, SES & loss functions}}

\emph{Canonical framework:} in a DLM the predictive distribution $Y_{t+k}\mid y_{1:t}$ is Gaussian; its \emph{mean} is the \textbf{forecast function} $\hat y_{t+k\mid t}=F_{t+k}a_{t+k}$ and its \emph{variance} $Q_{t+k}$ supplies $(1-\alpha)$ credible intervals via $\hat y_{t+k\mid t}\pm z_{1-\alpha/2}\sqrt{Q_{t+k}}$. The point forecast also coincides with the Bayes estimator under quadratic loss (mean), absolute loss (median) --- equal here by Gaussian symmetry. Simple exponential smoothing (SES) is the steady-state KF of the local-level (random walk plus noise) model: it delivers only point forecasts, so any uncertainty quantification must come from the underlying DLM, not the recursion itself.

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
    "images": ["images/master/master_t10b_ai.png"]
}

master_exercises_ts["t11a"] = {
    "title": "Master — Innovations: zero-mean, orthogonality, standardisation",
    "content": r"""\textbf{\textcolor{red}{MASTER --- Innovations: zero-mean, orthogonality, standardisation}}

\emph{Canonical framework:} for a DLM the forecast errors (innovations) $e_t=Y_t-f_t$, with $f_t=\mathbb{E}(Y_t\mid y_{1:t-1})$, form a \emph{martingale-difference sequence}: zero mean, uncorrelated across $t$, conditionally Gaussian with variance $Q_t$ (the one-step-ahead predictive variance). Hence $e_t\sim\mathcal{N}(0,Q_t)$ \emph{not} $\mathcal{N}(0,1)$; the \emph{standardised} innovations $\tilde e_t=Q_t^{-1/2}e_t$ are i.i.d.\ $\mathcal{N}(0,1)$ under correct specification and are the workhorse for model checking (QQ-plot, Ljung--Box, CUSUM).

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
    "images": ["images/master/master_t11a_ai.png"]
}

master_exercises_ts["t12a"] = {
    "title": "Master — Likelihood of phi via prediction-error decomposition (MLE)",
    "content": r"""\textbf{\textcolor{red}{MASTER --- Likelihood of phi via prediction-error decomposition (MLE)}}

\emph{Canonical framework:} in a DLM with unknown parameters $\phi$ (entries of $F,G,V,W$), the joint density $p(y_{1:n}\mid\phi)$ factorises into one-step-ahead Gaussian predictives via the \textbf{prediction-error decomposition}: $p(y_{1:n}\mid\phi)=\prod_t\mathcal{N}_q(y_t;f_t(\phi),Q_t(\phi))$, with $(f_t,Q_t)$ from one Kalman-filter pass. Frequentist: maximise over $\phi$ numerically (BFGS) to get the MLE $\hat\phi$; plug into the KF for predictive distributions (ignoring parameter uncertainty). Bayesian: put a prior $\pi(\phi)$, get the posterior $p(\phi\mid y_{1:n})\propto L(\phi)\pi(\phi)$ via MCMC (Gibbs / Metropolis-Hastings, FFBS for conjugate components), then \emph{integrate out} $\phi$ in the predictive --- yielding a mixture of Gaussians with properly inflated intervals.

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
    "images": ["images/master/master_t12a_ai.png"]
}

master_exercises_ts["t13a"] = {
    "title": "Master — Conjugate Normal-Normal posterior (static theta / Case A)",
    "content": r"""\textbf{\textcolor{red}{MASTER --- Conjugate Normal-Normal posterior (static theta / Case A)}}

\emph{Canonical framework:} the simplest Bayesian time-series problem is i.i.d.\ Gaussian observations $Y_t\mid\theta\overset{\text{iid}}{\sim}\mathcal{N}(\theta,\sigma^2)$ with a \emph{static} (time-invariant) parameter $\theta$ and Normal prior $\theta\sim\mathcal{N}(m_0,C_0)$. The conjugate Normal-Normal posterior is itself Normal: \emph{precisions add}, $1/C_n=1/C_0+n/\sigma^2$, and the posterior mean is the \emph{precision-weighted average} of the prior mean and the sample mean. This is also the limiting case of a random walk plus noise model with zero state-evolution variance ($W=0$): the latent level collapses to a constant and one recovers exactly the static Bayesian update.

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
    "images": ["images/master/master_t13a_ai.png"]
}

master_exercises_ts["t13b"] = {
    "title": "Master — Bayesian predictive integrating out phi (+ MCMC)",
    "content": r"""\textbf{\textcolor{red}{MASTER --- Bayesian predictive integrating out phi (+ MCMC)}}

\emph{Canonical framework:} for a DLM with unknown parameters $\phi$, the Bayesian approach puts a prior $\pi(\phi)$, obtains the posterior $p(\phi\mid y_{1:t})\propto L(\phi)\pi(\phi)$ (with $L(\phi)$ from the prediction-error decomposition), and \emph{marginalises out $\phi$} in every downstream object: filtering $p(\theta_t\mid y_{1:t})$, predictive $p(y_{t+1}\mid y_{1:t})$, smoothing $p(\theta_{0:T}\mid y_{1:T})$. Each becomes a \emph{mixture of Gaussians} (one Kalman-filter run per posterior draw $\phi^{(s)}$). The mixture is non-Gaussian; intervals are \textbf{wider} than plug-in (honest uncertainty). The posterior $p(\phi\mid y_{1:t})$ is sampled by MCMC: Gibbs with FFBS for conditionally-conjugate components, Metropolis-Hastings otherwise. MCMC itself is a Markov chain constructed to have the target as its stationary distribution.

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
    "images": ["images/master/master_t13b_ai.png"]
}
