"""
Theory column entries for sub-topics in topics T4, T5, T6.
"""
theory_content_ts = {}


# =============================================================================
# t4a — Panel transition-count likelihood & MLE
# =============================================================================
theory_content_ts["t4a"] = {
    "title": "Theory — Panel transition-count likelihood & MLE",
    "content": r"""\textbf{\textcolor{red}{THEORY --- Panel transition-count likelihood \& MLE [Topic: T4 — Markov chains — likelihood, MLE \& Anderson--Goodman CI]}}

\textbf{1. Setting — panel of i.i.d.\ homogeneous Markov chains.}

We observe $n$ \emph{independent and identically distributed} categorical time series
\[
\bigl(Y_{k,0},Y_{k,1},\dots,Y_{k,T}\bigr)_{k=1,\dots,n},\qquad Y_{k,t}\in\mathcal Y=\{1,2,\dots,K\}.
\]
Each series is a homogeneous Markov chain with \emph{common} initial law and \emph{common} transition matrix
\[
\mathbf P=[p_{ij}]_{i,j=1}^{K},\qquad p_{ij}=\mathbb{P}(Y_{k,t}=j\mid Y_{k,t-1}=i),\qquad \sum_{j=1}^K p_{ij}=1\;\forall i.
\]
We assume the \emph{initial values} $y_{k,0}$ are \emph{fixed} (known) for every individual $k$, so we only have to estimate $\mathbf P$ and not the initial distribution $\pi$.

\textbf{Free parameters.} $\mathbf P$ has $K^2$ entries but each row sums to $1$, so there are $K(K-1)$ free probabilities (e.g.\ $K=3\Rightarrow 6$ free).

\textbf{2. Transition counts — sufficient statistic.}

Pool the transitions across individuals and time:
\[
\boxed{\;n_{ij}=\sum_{k=1}^n\sum_{t=1}^{T}\mathbf 1\{Y_{k,t-1}=i,\,Y_{k,t}=j\},\qquad n_{i,+}=\sum_{j=1}^{K}n_{ij}.\;}
\]
$n_{i,+}$ is the total number of \emph{visits} to state $i$ that have a recorded next step (the row total of the count matrix). The matrix $N=[n_{ij}]$ is a \emph{sufficient statistic} for $\mathbf P$ conditional on the initial values (Neyman factorisation, point 4).

\textbf{3. Conditional likelihood — derivation.}

Factor each individual path via the Markov property:
\[
\mathbb{P}\bigl(Y_{k,1:T}=y_{k,1:T}\mid Y_{k,0}=y_{k,0};\mathbf P\bigr)=\prod_{t=1}^{T}p_{y_{k,t-1},\,y_{k,t}}.
\]
The $n$ series are independent, so
\[
L(\mathbf P)=\prod_{k=1}^n\prod_{t=1}^{T}p_{y_{k,t-1},y_{k,t}}=\prod_{i=1}^{K}\prod_{j=1}^{K}p_{ij}^{\,n_{ij}}.
\]
The second equality just groups identical factors. The log-likelihood is
\[
\ell(\mathbf P)=\sum_{i,j}n_{ij}\log p_{ij}.
\]

\boxed{\;L(\mathbf P)=\prod_{i,j}p_{ij}^{n_{ij}},\qquad \ell(\mathbf P)=\sum_{i,j}n_{ij}\log p_{ij}.\;}

\emph{(This is exactly the form asked in both linked exams — note we condition on the fixed $y_{k,0}$, hence $\pi$ does not appear.)}

\textbf{4. MLE — row-by-row Lagrangian.}

The objective separates by row $i$: maximise $\sum_j n_{ij}\log p_{ij}$ subject to $\sum_j p_{ij}=1$ and $p_{ij}\ge 0$. Lagrangian
\[
\mathcal L_i=\sum_{j=1}^K n_{ij}\log p_{ij}-\lambda_i\Bigl(\sum_j p_{ij}-1\Bigr),\qquad \frac{\partial\mathcal L_i}{\partial p_{ij}}=\frac{n_{ij}}{p_{ij}}-\lambda_i=0\Rightarrow p_{ij}=\frac{n_{ij}}{\lambda_i}.
\]
The constraint $\sum_j p_{ij}=1$ gives $\lambda_i=\sum_j n_{ij}=n_{i,+}$, hence
\[
\boxed{\;\widehat p_{ij}^{\,\mathrm{MLE}}=\frac{n_{ij}}{n_{i,+}}.\;}
\]
\emph{Interpretation.} $\widehat p_{ij}$ is just the empirical conditional frequency: among all transitions \emph{out of} state $i$, the fraction that landed in state $j$.

\emph{Sanity check (second-order condition).} $\partial^2\mathcal L_i/\partial p_{ij}^2=-n_{ij}/p_{ij}^2<0$ when $n_{ij}>0$; the Hessian is diagonal and negative-definite on the simplex interior, so the stationary point is the global max.

\textbf{5. Conditional multinomial structure (key for inference).}

The crucial fact justifying both the MLE and the Wald CI is:

\textbf{Lemma (Anderson--Goodman).} Conditional on the visit count $n_{i,+}$ (and on the initial values), the row of next-state outcomes
\[
(n_{i,1},n_{i,2},\dots,n_{i,K})\mid n_{i,+}\sim\mathrm{Multinom}\bigl(n_{i,+};\,p_{i,1},p_{i,2},\dots,p_{i,K}\bigr).
\]

\emph{Sketch.} Each transition \emph{out of} state $i$ is, by the Markov property, an independent draw from $\mathrm{Cat}(p_{i,:})$ — \emph{regardless} of when the visit to $i$ occurs in the sample. So the $n_{i,+}$ transitions out of $i$ are an i.i.d.\ multinomial sample. (The rows of $N$ are themselves independent across $i$, conditionally on the vector of row totals.)

\emph{Consequence.} $\widehat p_{ij}=n_{ij}/n_{i,+}$ is just a \emph{sample proportion} from a multinomial — same asymptotics as for an i.i.d.\ Bernoulli with success probability $p_{ij}$ in $n_{i,+}$ trials.

\textbf{6. Properties of the MLE.}

\begin{itemize}
\item \emph{Consistency.} As $n_{i,+}\to\infty$ (large panel / long horizon, irreducible chain), $\widehat p_{ij}\xrightarrow{\mathbb{P}}p_{ij}$ by the LLN applied to the multinomial counts.
\item \emph{Asymptotic normality.} By the multinomial CLT,
\[
\sqrt{n_{i,+}}\bigl(\widehat p_{ij}-p_{ij}\bigr)\xrightarrow{d}\mathcal{N}\bigl(0,\,p_{ij}(1-p_{ij})\bigr).
\]
This is the foundation of the Wald CI (covered in detail in t4b).
\item \emph{Efficiency.} The MLE achieves the Cram\'er--Rao bound for multinomial models — the Fisher information for $p_{ij}$ (with the row constraint) is $n_{i,+}/[p_{ij}(1-p_{ij})]$, exactly the inverse of the asymptotic variance.
\end{itemize}

\textbf{7. Why pooling across $n$ individuals works.}

Because the $n$ chains are i.i.d.\ with \emph{common} $\mathbf P$, every transition $(Y_{k,t-1},Y_{k,t})$ is exchangeable across $k$. So the pooled count $n_{ij}=\sum_{k,t}\mathbf 1\{Y_{k,t-1}=i,Y_{k,t}=j\}$ behaves like the count from a single very long chain — with effective sample size $\sum_k T$ (after subtracting the $n$ initial values). This is what makes panels with short $T$ but large $n$ usable for estimating $\mathbf P$.

\textbf{8. Worked micro-example (Sep 2025 Q6 / May 2024 Q2 style).}

Suppose $K=3$, $n=100$ panel individuals, $T=5$, and the pooled transition counts are
\[
N=\begin{pmatrix}60&50&40\\75&120&55\\30&34&36\end{pmatrix},\qquad n_{1,+}=150,\;n_{2,+}=250,\;n_{3,+}=100.
\]
Then $\widehat p_{11}=60/150=0.40$, $\widehat p_{12}=50/150\approx 0.333$, $\widehat p_{13}=40/150\approx 0.267$, and the row sums to $1$. Similarly $\widehat p_{3,1}=30/100=0.30$.

\textbf{9. R — pool counts then row-normalise.}

```R
K <- 3
N <- matrix(0, K, K)
for (k in 1:n) {
  for (t in 2:(T+1)) {
    i <- Y[k, t-1]; j <- Y[k, t]
    N[i, j] <- N[i, j] + 1
  }
}
Phat <- N / rowSums(N)               # row-by-row MLE
Phat                                  # estimated transition matrix
rowSums(Phat)                         # should be (1, 1, 1)

## Anderson--Goodman SE for each entry:
se <- sqrt(Phat * (1 - Phat) / rowSums(N))
```

\textbf{10. Edge cases worth flagging.}

\begin{itemize}
\item If $n_{i,+}=0$ (state $i$ never visited), the $i$-th row of $\mathbf P$ is unidentified; report \emph{NA}. Common in short panels for rarely-visited states.
\item If $n_{ij}=0$ for some $j$ with $n_{i,+}>0$, the MLE is $\widehat p_{ij}=0$; the Wald SE collapses to $0$ and the CI degenerates. Use Wilson / exact CIs in this regime (not asked in the linked exams but useful to know).
\item Conditional on fixed $y_{k,0}$, the likelihood does \emph{not} involve $\pi$ — the question of estimating the initial law is decoupled and trivially $\widehat\pi_i=\#\{k:y_{k,0}=i\}/n$ if the $y_{k,0}$ are random.
\end{itemize}

\textbf{Exam pointers.}
\begin{itemize}
\item[$\triangleright$] Used in \texttt{exam\_sep\_2025\_q6}: $n$ locations, $T=100$ obs, $K=3$ pollution states; asks for likelihood, MLE and marginal CI for the transition matrix — write $L(\mathbf P)=\prod p_{ij}^{n_{ij}}$, $\widehat p_{ij}=n_{ij}/n_{i,+}$, then the Wald CI as in t4b.
\item[$\triangleright$] Used in \texttt{exam\_may\_2024\_q2}: $n=100$ panel, $T=5$, $K=3$ opinion states with given count table; asks (a) likelihood expression, (b) MLE derivation, (c) $\widehat p_{3,1}=30/100=0.30$ and a $90\%$ Wald CI $[0.224,0.376]$ — verify the derivation flows row-by-row Lagrangian $\Rightarrow$ multinomial $\Rightarrow$ Wald.
\end{itemize}
""",
}


# =============================================================================
# t4b — Wald CI for p_ij + forecasting future percentages
# =============================================================================
theory_content_ts["t4b"] = {
    "title": "Theory — Wald CI for p_ij + forecasting future percentages",
    "content": r"""\textbf{\textcolor{red}{THEORY --- Wald CI for $p_{ij}$ + forecasting future percentages [Topic: T4 — Markov chains — likelihood, MLE \& Anderson--Goodman CI]}}

\textbf{1. Recall: MLE \& Anderson--Goodman sampling distribution.}

From t4a, under the i.i.d.\ panel-of-MCs model with pooled transition counts $n_{ij}$ and row totals $n_{i,+}$, the MLE is $\widehat p_{ij}=n_{ij}/n_{i,+}$, and conditional on $n_{i,+}$,
\[
(n_{i,1},\dots,n_{i,K})\mid n_{i,+}\sim\mathrm{Multinom}\bigl(n_{i,+};\,p_{i,:}\bigr).
\]
Each cell-marginal $n_{ij}\mid n_{i,+}\sim\mathrm{Binomial}(n_{i,+},p_{ij})$ — that is the engine of the Wald CI below.

\textbf{2. Asymptotic distribution of $\widehat p_{ij}$ — derivation steps.}

\emph{(This is the proof the exam asks for in Jun 2025 Q2d.)}

\textbf{Step 1 — multinomial structure.} Conditional on $n_{i,+}$, the next-state outcomes from state $i$ are an i.i.d.\ sample of size $n_{i,+}$ from $\mathrm{Cat}(p_{i,:})$. Define the indicator $X_\ell=\mathbf 1\{\text{the }\ell\text{-th transition out of }i\text{ went to }j\}$. Then $X_\ell\overset{\text{iid}}{\sim}\mathrm{Bernoulli}(p_{ij})$ for $\ell=1,\dots,n_{i,+}$, and $n_{ij}=\sum_\ell X_\ell$.

\textbf{Step 2 — CLT applied to a Bernoulli proportion.} $\widehat p_{ij}=n_{ij}/n_{i,+}=\bar X$ is a sample mean of i.i.d.\ Bernoullis with $\mathbb{E}[X_\ell]=p_{ij}$ and $\operatorname{Var}(X_\ell)=p_{ij}(1-p_{ij})$. By the standard CLT,
\[
\boxed{\;\sqrt{n_{i,+}}\bigl(\widehat p_{ij}-p_{ij}\bigr)\xrightarrow{\,d\,}\mathcal{N}\bigl(0,\,p_{ij}(1-p_{ij})\bigr).\;}
\]

\textbf{Step 3 — Slutsky / plug-in for the SE.} The variance $p_{ij}(1-p_{ij})$ is unknown; replace by $\widehat p_{ij}(1-\widehat p_{ij})$. By consistency of $\widehat p_{ij}$ and Slutsky's theorem, the limiting distribution is preserved with the plug-in variance. (\emph{This is exactly the ``property of the MLE'' the exam asks you to cite: consistency + asymptotic normality.})

\textbf{3. Wald $(1-\alpha)$ confidence interval.}

Solving the asymptotic pivot $|\widehat p_{ij}-p_{ij}|/\mathrm{SE}\le z_{\alpha/2}$ gives
\[
\boxed{\;\widehat p_{ij}\pm z_{\alpha/2}\sqrt{\dfrac{\widehat p_{ij}(1-\widehat p_{ij})}{n_{i,+}}}.\;}
\]
\emph{Common quantiles:} $z_{0.05}=1.645$, $z_{0.025}=1.96$, $z_{0.005}=2.576$ (so the exam's $90\%$ CI uses $1.65$, $95\%$ uses $1.96$).

\emph{Caveats (good to mention).}
\begin{itemize}
\item Valid as $n_{i,+}\to\infty$. Small-sample replacements: Wilson, Clopper--Pearson.
\item ``Marginal'' CI for one entry $p_{ij}$; for joint inference on a whole row, use the full multinomial covariance $\operatorname{Cov}(\widehat p_{ij},\widehat p_{ij'})=-p_{ij}p_{ij'}/n_{i,+}$ and a $\chi^2$-based confidence ellipsoid.
\end{itemize}

\textbf{4. Worked example A (Jun 2025 Q2, $\widehat p_{1,1}$).}

Counts: $n_{1,+}=150$, $n_{1,1}=70$. $\widehat p_{1,1}=70/150\approx 0.467$. $90\%$ Wald CI uses $z_{0.05}=1.65$:
\[
\mathrm{SE}=\sqrt{0.467\cdot 0.533/150}\approx 0.0407,\quad
0.467\pm 1.65\cdot 0.0407=0.467\pm 0.067\Rightarrow[0.400,\,0.534].
\]

\textbf{Worked example B (May 2022 Q5, $\widehat p_{3,1}$).}

Counts: $n_{3,+}=170$, $n_{3,1}=30$. $\widehat p_{3,1}=30/170\approx 0.176$. $95\%$ Wald CI uses $z_{0.025}=1.96$:
\[
\mathrm{SE}=\sqrt{0.176\cdot 0.824/170}\approx 0.0292,\quad
0.176\pm 1.96\cdot 0.0292=0.176\pm 0.057\Rightarrow[0.119,\,0.234].
\]

\textbf{5. Forecasting future percentages — plug-in pipeline.}

\emph{Setting (Jun 2025 Q2e).} Panel of $n$ individuals observed up to month $t=T$. We want to forecast the proportion at $t=T+1$ that will be in state $j_0$ (e.g.\ ``YES vote''). Let $q=\mathbb{P}\bigl(\bar Y_{T+1}>c\bigr)$ for some threshold $c$.

\textbf{Step 1 — predicted marginal probability for one individual at $T+1$.}

If the chain starts at $Y_T=i$ with empirical distribution $\widehat\pi_i^{(T)}=n_{i,+}/\sum_{j}n_{j,+}$ (the share of the panel currently in state $i$), then
\[
\widehat{\mathbb{P}}\bigl(Y_{T+1}=j_0\bigr)=\sum_{i=1}^K\widehat\pi_i^{(T)}\,\widehat p_{i,j_0}.
\]
Each future indicator $\mathbf 1\{Y_{k,T+1}=j_0\}$ is then approximately Bernoulli$(\widehat p_{j_0})$ conditionally on the panel's June state.

\textbf{Step 2 — panel-mean CLT.} The realised proportion $\bar Y_{T+1}=n^{-1}\sum_k\mathbf 1\{Y_{k,T+1}=j_0\}$ is a sample mean of $n$ near-i.i.d.\ Bernoullis (across $k$, conditional on the June states). By CLT,
\[
\bar Y_{T+1}\dot\sim\mathcal{N}\Bigl(\widehat p_{j_0},\,\dfrac{\widehat p_{j_0}(1-\widehat p_{j_0})}{n}\Bigr).
\]

\textbf{Step 3 — plug-in $\widehat q$.}
\[
\widehat q=\mathbb{P}\bigl(\bar Y_{T+1}>c\bigr)\approx 1-\Phi\!\left(\frac{c-\widehat p_{j_0}}{\sqrt{\widehat p_{j_0}(1-\widehat p_{j_0})/n}}\right).
\]

\textbf{Worked example C (Jun 2025 Q2e).} Counts $\rightarrow$ $\widehat\pi^{(5)}\propto(150,250,100)$, $\widehat p_{1,1}=0.467,\widehat p_{2,1}=0.300,\widehat p_{3,1}=0.300$:
\[
\widehat p_{\mathrm{YES}}=\frac{150\cdot 0.467+250\cdot 0.300+100\cdot 0.300}{500}=\frac{175}{500}=0.35.
\]
With $n=1000$ and $c=0.50$: $\mathrm{SE}=\sqrt{0.35\cdot 0.65/1000}\approx 0.0151$, $z=(0.50-0.35)/0.0151\approx 9.93$, hence $\widehat q=1-\Phi(9.93)\approx 0$ (essentially zero probability of a majority).

\emph{Honest caveat.} The plug-in $\widehat q$ ignores the uncertainty in $(\widehat p_{ij},\widehat\pi^{(5)})$. A delta-method or bootstrap CI is more accurate; for the exam, the plug-in point estimate is what's asked.

\textbf{6. ``Is this probability known or estimated?'' (May 2022 Q5a style).}

Path probabilities like $\mathbb{P}(Y_1=3,Y_2=3,Y_3=3,Y_4=1\mid Y_0=1)=p_{1,3}\,p_{3,3}^2\,p_{3,1}$ have a \emph{known} algebraic expression (Markov factorisation), but their \emph{numerical value} depends on the unknown $p_{ij}$ — so they must be \emph{estimated} by plug-in $\widehat p_{1,3}\widehat p_{3,3}^2\widehat p_{3,1}$.

\textbf{7. R — full pipeline (CI + percentage forecast).}

```R
N <- matrix(c(70, 30, 50,
              75,120, 55,
              30, 34, 36), nrow = 3, byrow = TRUE)
rowtot <- rowSums(N)
Phat   <- N / rowtot                                # transition MLE

## Marginal Wald CI for one entry, e.g. (1,1) at 90%:
i <- 1; j <- 1; z <- 1.65
se_ij <- sqrt(Phat[i,j] * (1 - Phat[i,j]) / rowtot[i])
Phat[i,j] + c(-1, 1) * z * se_ij                    # [0.400, 0.534]

## Forecast P(majority YES in July):
pi5    <- rowtot / sum(rowtot)                      # June empirical state
p_yes  <- sum(pi5 * Phat[, 1])                      # 0.35
n_pan  <- 1000
se_bar <- sqrt(p_yes * (1 - p_yes) / n_pan)
q_hat  <- 1 - pnorm((0.5 - p_yes) / se_bar)         # ~ 0
```

\textbf{8. Why ``properties of the MLE'' matter (May 2022 Q5 last sub-question).}

Two MLE properties are explicitly invoked in the Wald CI:
\begin{itemize}
\item \emph{Asymptotic normality} of $\widehat p_{ij}$ (Anderson--Goodman / multinomial CLT) — gives the $\mathcal{N}(0,p_{ij}(1-p_{ij}))$ limit.
\item \emph{Consistency} $\widehat p_{ij}\xrightarrow{\mathbb{P}}p_{ij}$ + \emph{Slutsky's theorem} — allows replacing the unknown variance $p_{ij}(1-p_{ij})$ with the plug-in $\widehat p_{ij}(1-\widehat p_{ij})$ without breaking the asymptotic distribution.
\end{itemize}

\textbf{Exam pointers.}
\begin{itemize}
\item[$\triangleright$] Used in \texttt{exam\_jun\_2025\_q2}: full panel-of-MCs question; (a) $K(K-1)=6$ free parameters in $\mathbf P$; (b) likelihood $\prod p_{ij}^{n_{ij}}$; (c) $\widehat p_{1,1}=0.467$, $90\%$ CI $[0.400,0.534]$; (d) derive the asymptotic normal via multinomial CLT + Slutsky; (e) plug-in $\widehat q\approx 0$ via panel CLT.
\item[$\triangleright$] Used in \texttt{exam\_may\_2022\_q5}: (a) explain that a path probability $p_{13}p_{33}^2 p_{31}$ is algebraically known but numerically estimated; (b) $\widehat p_{3,1}=30/170\approx 0.176$, $95\%$ Wald CI $[0.119,0.234]$, citing asymptotic normality + consistency (Slutsky) of the MLE.
\end{itemize}
""",
}


# =============================================================================
# t5a — HMM definition, parameters & forward-algorithm likelihood
# =============================================================================
theory_content_ts["t5a"] = {
    "title": "Theory — HMM definition, parameters & forward-algorithm likelihood",
    "content": r"""\textbf{\textcolor{red}{THEORY --- HMM definition, parameters \& forward-algorithm likelihood [Topic: T5 — Hidden Markov Models (HMM) — model, likelihood, decoding]}}

\textbf{1. Hidden Markov Model — formal definition.}

A \emph{Hidden Markov Model} (HMM) is a state-space process $\bigl((Y_t,S_t)\bigr)_{t\ge 1}$ where:

\textbf{(i) Latent state chain.} $(S_t)_{t\ge 1}\in\mathcal S=\{1,\dots,k\}$ is a \emph{homogeneous Markov chain} with
\[
S_1\sim\pi=(\pi_1,\dots,\pi_k),\qquad p_{ij}=\mathbb{P}(S_{t+1}=j\mid S_t=i),\qquad \mathbf P=[p_{ij}]\in[0,1]^{k\times k},\;\sum_j p_{ij}=1.
\]
(If $S_0$ is fixed at, say, $S_0=1$, then $\pi$ is degenerate and one less parameter is needed.)

\textbf{(ii) Emission / observation model.} Conditional on the path of $(S_t)$, the observations are conditionally independent and each $Y_t$ depends only on the contemporaneous state:
\[
Y_t\mid S_t=i\sim f(\cdot\mid S_t=i;\,\eta_i),\qquad Y_t\perp\bigl(S_{-t},\,Y_{-t}\bigr)\mid S_t.
\]
The form of $f(\cdot\mid i)$ depends on the application:
\begin{itemize}
\item \emph{Categorical / discrete} (e.g.\ words from a dictionary of size $M$): $e_{i,w}=\mathbb{P}(Y_t=w\mid S_t=i)$, store as $\mathbf E\in\mathbb{R}^{k\times M}$ with $\sum_w e_{i,w}=1$.
\item \emph{Gaussian} (e.g.\ returns / volatility clusters): $Y_t\mid S_t=i\sim\mathcal{N}(\mu_i,\sigma_i^2)$.
\item \emph{Poisson} (e.g.\ counts of calls / arrivals): $Y_t\mid S_t=i\sim\mathrm{Poisson}(\lambda_i)$.
\end{itemize}

\boxed{\;\text{HMM: hidden MC }(S_t)\text{ + state-dependent emission }f(y_t\mid S_t).\;}

\textbf{2. Assumptions made explicit (asked verbatim by May 2025 Q3a).}

\begin{itemize}
\item[(A1)] $(S_t)$ is a homogeneous Markov chain — first-order, time-invariant transitions.
\item[(A2)] Conditional independence of observations given the latent path: $Y_t\perp(S_{-t},Y_{-t})\mid S_t$.
\item[(A3)] Time-invariant emission: $f(y_t\mid S_t=i)$ does not depend on $t$.
\item[(A4)] (Identifiability) The states are distinguished by their emission $f(\cdot\mid i)$ — otherwise the model is only identified up to relabelling. For Gaussian emissions with state-dependent variance one usually enforces $\sigma_1^2<\sigma_2^2<\sigma_3^2$.
\end{itemize}

\textbf{3. Unknown parameters $\phi$ — counting free dimensions.}

In full generality with discrete emissions:
\[
\phi=(\pi,\mathbf P,\mathbf E),\qquad
\underbrace{(k-1)}_{\pi\text{ row}}+\underbrace{k(k-1)}_{\mathbf P\text{ rows}}+\underbrace{k(M-1)}_{\mathbf E\text{ rows}}\text{ free.}
\]

\emph{Common variants asked in exams:}
\begin{itemize}
\item \textbf{Gaussian volatility HMM} $\mathcal{N}(0,\sigma_i^2)$, $k=3$, $S_0=1$ (fixed): $\phi=(\mathbf P,\sigma_1^2,\sigma_2^2,\sigma_3^2)$, $k(k-1)+k=6+3=9$ free (May 2023 Q3, May 2022 Q6).
\item \textbf{Poisson emission HMM} $\mathrm{Poisson}(\lambda_i)$, $k=3$, $S_1\sim\pi$: $\phi=(\pi,\mathbf P,\lambda_1,\lambda_2,\lambda_3)$, $(k-1)+k(k-1)+k=2+6+3=11$ free (May 2021 Q4).
\item \textbf{Topic HMM} for text with vocabulary $M$, $k$ topics, $S_1\sim\pi$: $(k-1)+k(k-1)+k(M-1)$ free (May 2025 Q3).
\end{itemize}

\textbf{4. The likelihood problem — why a naive sum is intractable.}

We want $L(\phi;y_{1:T})=\mathbb{P}(Y_{1:T}=y_{1:T};\phi)$. By the tower / total probability over latent paths,
\[
L(\phi)=\sum_{s_{1:T}\in\{1,\dots,k\}^T}\mathbb{P}(Y_{1:T}=y_{1:T},S_{1:T}=s_{1:T};\phi)
=\sum_{s_{1:T}}\pi_{s_1}\,f(y_1\mid s_1)\prod_{t=2}^T p_{s_{t-1},s_t}\,f(y_t\mid s_t).
\]
There are $k^T$ paths $s_{1:T}$ — already $3^{52}\approx 7\times 10^{24}$ for a year of weekly Gaussian data, hopeless for direct enumeration.

\textbf{5. Forward algorithm — derivation by induction.}

Define the \emph{forward variable}
\[
\boxed{\;\alpha_t(i)=\mathbb{P}\bigl(Y_{1:t}=y_{1:t},\,S_t=i;\,\phi\bigr)\quad\text{for }i=1,\dots,k.\;}
\]
\emph{Initialisation} ($t=1$). $\alpha_1(i)=\mathbb{P}(Y_1=y_1,S_1=i)=\pi_i\,f(y_1\mid i)$.

\emph{Induction step.} For $t\ge 2$,
\begin{align*}
\alpha_t(j)
&=\mathbb{P}(Y_{1:t},S_t=j)
=\sum_{i=1}^k\mathbb{P}(Y_{1:t-1},S_{t-1}=i,S_t=j,Y_t=y_t)\\
&=\sum_{i=1}^k\underbrace{\mathbb{P}(Y_{1:t-1},S_{t-1}=i)}_{\alpha_{t-1}(i)}\;
\underbrace{\mathbb{P}(S_t=j\mid S_{t-1}=i)}_{p_{ij}}\;
\underbrace{\mathbb{P}(Y_t=y_t\mid S_t=j)}_{f(y_t\mid j)}\\
&=\Bigl(\sum_{i=1}^k\alpha_{t-1}(i)\,p_{ij}\Bigr)\,f(y_t\mid j).
\end{align*}
\emph{Where conditional independence is used.} The factorisation in the third line invokes:
(i) Markovianity of $(S_t)$ — $S_t\mid(S_{1:t-1},Y_{1:t-1})$ depends only on $S_{t-1}$ — and
(ii) conditional independence of observations — $Y_t\mid(S_{1:t},Y_{1:t-1})$ depends only on $S_t$.

\emph{Termination.} The total likelihood marginalises over the final state:
\[
\boxed{\;L(\phi;y_{1:T})=\sum_{i=1}^k\alpha_T(i).\;}
\]

\textbf{Complexity.} $O(k^2T)$ multiplications — \emph{linear} in $T$ vs.\ exponential in the naive sum. Numerically, run in log-space or rescale $\alpha_t$ each step to avoid underflow.

\textbf{6. The forward algorithm with specific emissions.}

\emph{Discrete (categorical, $M$ words).} $f(y_t\mid j)=e_{j,y_t}$:
\[
\alpha_1(i)=\pi_i\,e_{i,y_1},\qquad \alpha_t(j)=\Bigl(\sum_i\alpha_{t-1}(i)\,p_{ij}\Bigr)e_{j,y_t}.
\]

\emph{Gaussian volatility (mean $0$, state-dependent variance $\sigma_i^2$).} $f(y_t\mid j)=\phi_\mathrm{N}(y_t;0,\sigma_j^2)$:
\[
\alpha_1(i)=\pi_i\,\phi_\mathrm{N}(y_1;0,\sigma_i^2),\qquad \alpha_t(j)=\Bigl(\sum_i\alpha_{t-1}(i)\,p_{ij}\Bigr)\phi_\mathrm{N}(y_t;0,\sigma_j^2).
\]

\emph{Poisson (rate $\lambda_i$).} $f(y_t\mid j)=e^{-\lambda_j}\lambda_j^{y_t}/y_t!$:
\[
\alpha_1(i)=\pi_i\,e^{-\lambda_i}\lambda_i^{y_1}/y_1!,\qquad \alpha_t(j)=\Bigl(\sum_i\alpha_{t-1}(i)\,p_{ij}\Bigr)e^{-\lambda_j}\lambda_j^{y_t}/y_t!.
\]

\textbf{7. MLE — EM / Baum--Welch.}

Maximising $L(\phi)$ directly is non-convex; the standard route is the EM algorithm specialised to HMMs (Baum--Welch):
\begin{itemize}
\item \emph{E-step}: forward--backward to compute $\gamma_t(i)=\mathbb{P}(S_t=i\mid y_{1:T};\phi)$ and $\xi_t(i,j)=\mathbb{P}(S_t=i,S_{t+1}=j\mid y_{1:T};\phi)$.
\item \emph{M-step}: closed-form updates
\[
\widehat\pi_i=\gamma_1(i),\quad \widehat p_{ij}=\frac{\sum_{t=1}^{T-1}\xi_t(i,j)}{\sum_{t=1}^{T-1}\gamma_t(i)},\quad \widehat e_{i,w}=\frac{\sum_t\gamma_t(i)\mathbf 1\{y_t=w\}}{\sum_t\gamma_t(i)},
\]
with analogous M-step formulae for Gaussian ($\widehat\mu_i,\widehat\sigma_i^2$) and Poisson ($\widehat\lambda_i$) emissions.
\end{itemize}

\textbf{8. Micro-example (Gaussian volatility, $k=2$, $T=3$).}

$\pi=(1,0)$, $\mathbf P=\binom{0.8\;0.2}{0.3\;0.7}$, $\sigma_1=0.5,\sigma_2=2$, $y=(0.1,-3,0.2)$.
\[
\alpha_1=\bigl(\phi_\mathrm{N}(0.1;0,.25),\,0\bigr)\approx(0.738,0).
\]
Then $\alpha_2(1)=(0.738\cdot 0.8+0)\,\phi_\mathrm{N}(-3;0,.25)\approx 0\;$; $\alpha_2(2)=(0.738\cdot 0.2)\,\phi_\mathrm{N}(-3;0,4)\approx 0.148\cdot 0.0648\approx 0.0096$. The large negative $y_2=-3$ pushes the marginal almost entirely to the high-variance state, as one would hope.

\textbf{9. R — fit \& evaluate.}

```R
library(HMM)
hmm <- initHMM(States = 1:k, Symbols = 1:M,
               startProbs = pi0, transProbs = P0, emissionProbs = E0)
fit <- baumWelch(hmm, observation = y)$hmm        # Baum--Welch MLE

## Forward log-probabilities, then the likelihood:
fwd <- forward(fit, y)                            # rows: states; cols: time
logL <- log(sum(exp(fwd[, length(y)])))

## Gaussian / Poisson HMMs: use depmixS4 instead
library(depmixS4)
mod  <- depmix(y ~ 1, nstates = 3, family = gaussian())
fitG <- fit(mod)                                  # EM
logLik(fitG)
```

\textbf{10. Common confusions / gotchas.}

\begin{itemize}
\item ``HMM'' is just an SSM with a discrete state space; the same conditional independence structure as any SSM applies.
\item Without identifiability constraints, MLE is only unique up to permutation of states (re-labelling).
\item EM finds a \emph{local} max — random restarts are standard practice.
\item The forward algorithm computes $\mathbb{P}(y_{1:T})$ exactly under the assumed model; it does \emph{not} provide point estimates of the state path — that's decoding (see t5b).
\end{itemize}

\textbf{Exam pointers.}
\begin{itemize}
\item[$\triangleright$] Used in \texttt{exam\_may\_2025\_q3}: text as a sequence of words; (a) state HMM assumptions (i)--(iii); (b) $\phi=(\pi,\mathbf P,\mathbf E)$ with $(k-1)+k(k-1)+k(M-1)$ free; (c) forward-algorithm likelihood $L=\sum_i\alpha_T(i)$.
\item[$\triangleright$] Used in \texttt{exam\_may\_2023\_q3}: Gaussian volatility HMM, $k=3$, $S_0=1$; (a) write $S_t\mid S_{t-1}=i\sim\mathrm{Cat}(p_{i,:})$, $Y_t\mid S_t=i\sim\mathcal{N}(0,\sigma_i^2)$; (b) $\phi=(\mathbf P,\sigma_{1:3}^2)$, $6+3=9$ free.
\item[$\triangleright$] Used in \texttt{exam\_may\_2022\_q6}: same Gaussian volatility setup but $\pi=(\tfrac13,\tfrac13,\tfrac13)$ uniform; (b) same 9 free parameters (since $\pi$ specified, no extra).
\item[$\triangleright$] Used in \texttt{exam\_may\_2021\_q4}: vaccination-call counts, Poisson emission HMM, $k=3$; (a) write model; (b) $\phi=(\pi,\mathbf P,\lambda_{1:3})$, $2+6+3=11$ free; (c) forward likelihood with Poisson emission.
\end{itemize}
""",
}


# =============================================================================
# t5b — Decoding (Viterbi / forward–backward) + path-probability
# =============================================================================
theory_content_ts["t5b"] = {
    "title": "Theory — Decoding (Viterbi / forward-backward) + path-probability",
    "content": r"""\textbf{\textcolor{red}{THEORY --- Decoding (Viterbi / forward-backward) + path-probability [Topic: T5 — Hidden Markov Models (HMM) — model, likelihood, decoding]}}

\textbf{1. ``Decoding'' — the three problems of HMMs.}

For an HMM $\bigl((Y_t,S_t)\bigr)_{t=1}^T$ with parameters $\phi=(\pi,\mathbf P,\mathbf E)$, three quantitative tasks are routinely solved (Rabiner's trinity):
\begin{itemize}
\item[(P1)] \emph{Evaluation.} Compute $L(\phi)=\mathbb{P}(Y_{1:T}=y_{1:T};\phi)$ — forward algorithm (t5a).
\item[(P2)] \emph{Decoding.} Recover the latent state path $(S_1,\dots,S_T)$ from the observations.
\item[(P3)] \emph{Learning.} Estimate $\phi$ — EM / Baum--Welch (t5a).
\end{itemize}
\textbf{Two flavours of decoding} are standard, and the exam expects both:

\boxed{\;\text{Global decoding: }\widehat s_{1:T}=\arg\max_{s_{1:T}}\mathbb{P}(S_{1:T}=s_{1:T}\mid y_{1:T}).\;}

\boxed{\;\text{Local decoding (marginal): }\widehat s_t=\arg\max_{s\in\{1,\dots,k\}}\gamma_t(s),\;\gamma_t(s)=\mathbb{P}(S_t=s\mid y_{1:T}).\;}

\emph{Conceptual difference.} Global decoding optimises the \emph{whole path} jointly — the MAP path under the posterior. Local decoding maximises each \emph{marginal} state posterior independently — it can produce paths $\widehat s_{1:T}$ that have zero posterior probability (e.g.\ violate transition constraints with $p_{ij}=0$).

\textbf{2. Path probability — basic factorisation (Jun 2024 Q4a, May 2022 Q5a).}

For a homogeneous Markov chain (\emph{no} emissions), conditional on $Y_0=y_0$:
\[
\mathbb{P}(Y_{1:T}=y_{1:T}\mid Y_0=y_0)=\prod_{t=1}^T p_{y_{t-1},y_t}=\prod_{i,j}p_{ij}^{n_{ij}(y_{0:T})},
\]
where $n_{ij}=\#\{t:y_{t-1}=i,y_t=j\}$. \emph{Example (Jun 2024 Q4a)}: with $y_{0:12}=(1,2,1,1,1,2,3,3,2,1,1,2,3)$ and $K=3$, the counts are $n_{11}=3,n_{12}=3,n_{21}=2,n_{23}=2,n_{32}=1,n_{33}=1$ (verify by reading the path), so
\[
\mathbb{P}(y_{1:12}\mid y_0=1;\mathbf P)=p_{11}^3\,p_{12}^3\,p_{21}^2\,p_{23}^2\,p_{32}\,p_{33}.
\]
This is the probability \emph{before} the sample is taken — known algebraically, but with unknown $p_{ij}$ requires plug-in $\widehat p_{ij}$ for a numerical value.

\textbf{3. Global decoding — Viterbi algorithm.}

Define the highest-probability prefix score reaching state $j$ at time $t$:
\[
\boxed{\;\delta_t(j)=\max_{s_{1:t-1}}\mathbb{P}\bigl(Y_{1:t}=y_{1:t},\,S_{1:t-1}=s_{1:t-1},\,S_t=j;\,\phi\bigr).\;}
\]

\emph{Initialisation.} $\delta_1(j)=\pi_j\,f(y_1\mid j)$.

\emph{Recursion (Bellman / dynamic programming).} For $t=2,\dots,T$:
\[
\delta_t(j)=\Bigl[\max_{i\in\{1,\dots,k\}}\delta_{t-1}(i)\,p_{ij}\Bigr]\,f(y_t\mid j),\qquad
\psi_t(j)=\arg\max_{i}\delta_{t-1}(i)\,p_{ij}.
\]
The $\psi_t(j)$ are \emph{back-pointers} storing the best predecessor of state $j$ at time $t$.

\emph{Termination + backtracking.}
\[
\widehat s_T=\arg\max_i\delta_T(i),\qquad \widehat s_t=\psi_{t+1}(\widehat s_{t+1})\;\;(t=T-1,T-2,\dots,1).
\]

\emph{Why this works.} The argmax of a product factorises (Bellman optimality): the most probable path \emph{ending} in $j$ at time $t$ extends the most probable path ending in some $i$ at $t-1$. So a forward pass storing $(\delta_t,\psi_t)$ and a backward backtracking are sufficient.

\emph{Complexity.} $O(k^2T)$ — same as the forward algorithm, just with $\max$ replacing $\sum$.

\emph{Numerical practice.} Work in log-space: $\log\delta_t(j)=\max_i[\log\delta_{t-1}(i)+\log p_{ij}]+\log f(y_t\mid j)$. Avoids underflow for long sequences.

\textbf{4. Local decoding — forward--backward.}

Define the \emph{backward variable}
\[
\beta_t(i)=\mathbb{P}(Y_{t+1:T}=y_{t+1:T}\mid S_t=i;\phi),\quad i=1,\dots,k,
\]
with terminal condition $\beta_T(i)=1$. Backward recursion:
\[
\beta_t(i)=\sum_{j=1}^k p_{ij}\,f(y_{t+1}\mid j)\,\beta_{t+1}(j),\qquad t=T-1,T-2,\dots,1.
\]
The smoothed marginal posterior of $S_t$ given all data is then
\[
\boxed{\;\gamma_t(i)=\mathbb{P}(S_t=i\mid y_{1:T})=\frac{\alpha_t(i)\,\beta_t(i)}{\sum_{j}\alpha_t(j)\,\beta_t(j)}=\frac{\alpha_t(i)\beta_t(i)}{L(\phi)}.\;}
\]
\emph{Why?} By Bayes' rule and the conditional independence $(Y_{1:t},S_t)\perp(Y_{t+1:T})\mid S_t$:
\[
\mathbb{P}(S_t=i,y_{1:T})=\mathbb{P}(S_t=i,y_{1:t})\,\mathbb{P}(y_{t+1:T}\mid S_t=i)=\alpha_t(i)\beta_t(i).
\]
Divide by $\mathbb{P}(y_{1:T})=\sum_j\alpha_T(j)$ to normalise. \emph{Local decoding:} $\widehat s_t=\arg\max_i\gamma_t(i)$.

The forward--backward algorithm also yields the pair posteriors $\xi_t(i,j)=\mathbb{P}(S_t=i,S_{t+1}=j\mid y_{1:T})\propto\alpha_t(i)p_{ij}f(y_{t+1}\mid j)\beta_{t+1}(j)$, which are exactly the E-step quantities for Baum--Welch.

\textbf{5. Global vs.\ local — when do they agree / differ?}

\begin{itemize}
\item For very informative observations (well-separated emission distributions), the posterior on paths concentrates on a single path and Viterbi $\approx$ local-MAP.
\item When emissions overlap, local decoding may prefer one state at $t$ and a \emph{different} state at $t+1$ that have low joint posterior because $p_{ij}\approx 0$. Viterbi avoids this by enforcing path-consistency.
\item Use Viterbi when interpreting the \emph{whole regime sequence} matters (e.g.\ bull/bear regimes in finance, sleep-stage segmentation). Use local for per-time \emph{smoothed marginal} interpretation (e.g.\ ``probability of regime $i$ at time $t$'').
\end{itemize}

\textbf{6. Micro-example (Viterbi by hand, $k=2$, $T=3$).}

$\pi=(0.6,0.4)$, $\mathbf P=\binom{0.7\;0.3}{0.4\;0.6}$, emissions $f(y\mid 1)$ and $f(y\mid 2)$ both fixed; suppose $f(y_t\mid 1)=(0.5,0.4,0.7)$ and $f(y_t\mid 2)=(0.1,0.3,0.2)$ for $t=1,2,3$.
\[
\delta_1=(0.6\cdot 0.5,\;0.4\cdot 0.1)=(0.30,\,0.04).
\]
\[
\delta_2(1)=\max(0.30\cdot 0.7,\,0.04\cdot 0.4)\cdot 0.4=0.21\cdot 0.4=0.084,\;\psi_2(1)=1;
\]
\[
\delta_2(2)=\max(0.30\cdot 0.3,\,0.04\cdot 0.6)\cdot 0.3=0.090\cdot 0.3=0.027,\;\psi_2(2)=1.
\]
\[
\delta_3(1)=\max(0.084\cdot 0.7,\,0.027\cdot 0.4)\cdot 0.7=0.0588\cdot 0.7\approx 0.0412,\;\psi_3(1)=1.
\]
$\delta_3(2)\approx\max(0.084\cdot 0.3,0.027\cdot 0.6)\cdot 0.2=0.0252\cdot 0.2=0.00504$, $\psi_3(2)=1$. Termination: $\widehat s_3=1$, backtrack: $\widehat s_2=\psi_3(1)=1$, $\widehat s_1=\psi_2(1)=1$. MAP path $=(1,1,1)$.

\textbf{7. R — both decoders.}

```R
library(HMM)
hmm <- initHMM(States = 1:k, Symbols = 1:M,
               startProbs = pi0, transProbs = P0, emissionProbs = E0)

## Global decoding (Viterbi):
s_hat_global <- viterbi(hmm, y)

## Local decoding via forward--backward:
fwd <- forward(hmm, y); bwd <- backward(hmm, y)
gamma <- exp(fwd + bwd)
gamma <- sweep(gamma, 2, colSums(gamma), '/')      # normalise each column
s_hat_local  <- apply(gamma, 2, which.max)

## Pair posteriors (E-step quantity for Baum--Welch):
## xi_t(i,j) propto alpha_t(i)*P[i,j]*f(y_{t+1}|j)*beta_{t+1}(j)

## Gaussian / Poisson HMMs:
library(depmixS4)
mod  <- depmix(y ~ 1, nstates = 3, family = gaussian())
fitG <- fit(mod)
s_hat_global <- posterior(fitG)$state             # uses Viterbi internally
```

\textbf{8. Special case: Markov chain (no hidden layer) — ``decoding'' is trivial.}

If we model the observed series directly as a Markov chain (no latent layer), the path is the data — no decoding needed, and the joint probability of any path under given $\mathbf P$ is the product $\prod p_{y_{t-1},y_t}$. This is the setting of Jun 2024 Q4(a) before the HMM extension in (b). The HMM machinery (Viterbi, forward--backward) only becomes relevant once a hidden $S_t$ is introduced.

\textbf{9. Computational sanity checks.}

\begin{itemize}
\item $\sum_i\alpha_T(i)=\sum_i\alpha_t(i)\beta_t(i)/L=\sum_i\gamma_t(i)=1$ for every $t$.
\item Viterbi path must satisfy $p_{\widehat s_{t-1},\widehat s_t}>0$ at every step.
\item In Baum--Welch / forward--backward, rescale $\alpha,\beta$ at each $t$ to prevent underflow on long sequences ($T\gtrsim 100$).
\end{itemize}

\textbf{Exam pointers.}
\begin{itemize}
\item[$\triangleright$] Used in \texttt{exam\_jun\_2024\_q4}: start-up status time series; (a) compute path probability for a fully-observed MC ($\prod p_{y_{t-1},y_t}=p_{11}^3p_{12}^3p_{21}^2p_{23}^2p_{32}p_{33}$); (b.i) define a $k=2$ HMM with 7 free parameters $\phi=(\pi,\mathbf P,\mathbf E)$ ($1+2+4$); (b.ii) write the forward-algorithm likelihood $\alpha_1(i)=\pi_ie_{i,y_1}$, $\alpha_t(j)=(\sum_i\alpha_{t-1}(i)p_{ij})e_{j,y_t}$, $L=\sum_i\alpha_T(i)$; (b.iii) explain decoding — Viterbi $\hat s_{1:T}=\arg\max\mathbb{P}(s_{1:T}\mid y_{1:T})$ via $\delta_t(j)=[\max_i\delta_{t-1}(i)p_{ij}]e_{j,y_t}$ + back-pointers (global), and local $\hat s_t=\arg\max\gamma_t(i)$ from forward--backward.
\end{itemize}
""",
}


# =============================================================================
# t6a — SSM / DLM general definition (univariate & multivariate)
# =============================================================================
theory_content_ts["t6a"] = {
    "title": "Theory — SSM / DLM general definition (univariate & multivariate)",
    "content": r"""\textbf{\textcolor{red}{THEORY --- SSM / DLM general definition (univariate \& multivariate) [Topic: T6 — State-space \& dynamic linear models — general framework]}}

\textbf{1. State-space model (SSM) — general definition.}

A \emph{state-space model} for a (possibly multivariate) time series $(Y_t)_{t\ge 1}$ is a pair of processes $\bigl((Y_t,\theta_t)\bigr)_{t\ge 1}$ where the \emph{latent state} $(\theta_t)_{t\ge 0}$ takes values in $\mathbb{R}^p$ (or a discrete / general space) and:

\textbf{(SS1)} \emph{Initial state.} $\theta_0\sim\pi(\theta_0)$.

\textbf{(SS2)} \emph{Markov state dynamics.} $\theta_t\mid\theta_{0:t-1}\sim f(\theta_t\mid\theta_{t-1})$.

\textbf{(SS3)} \emph{Conditional independence of observations.} Given the latent path, observations are conditionally independent and each $Y_t$ depends only on $\theta_t$:
\[
Y_t\mid(\theta_{0:t},Y_{1:t-1})\sim f(y_t\mid\theta_t).
\]
Equivalently $Y_t\perp(\theta_{-t},Y_{-t})\mid\theta_t$.

\boxed{\;\text{SSM = hidden Markov state }\theta_t\text{ + }Y_t\perp\text{everything else}\mid\theta_t.\;}

\emph{Examples.} (a) HMM: $\theta_t=S_t$ discrete, emission $f(y_t\mid S_t)$ as in t5a. (b) DLM: $\theta_t\in\mathbb{R}^p$ with linear-Gaussian dynamics (definition below). (c) Stochastic volatility: $\theta_t=\log\sigma_t^2$ AR(1) and $Y_t\mid\theta_t\sim\mathcal{N}(0,e^{\theta_t})$ — see t6b.

\textbf{Filtering, smoothing, prediction --- the three SSM problems.}
\begin{itemize}
\item \emph{Filtering}: $\pi(\theta_t\mid y_{1:t})$ — current state given data \emph{up to now}.
\item \emph{Smoothing}: $\pi(\theta_t\mid y_{1:T})$ (marginal) or $\pi(\theta_{0:T}\mid y_{1:T})$ (joint) — past state given \emph{all} data.
\item \emph{Prediction}: $\pi(\theta_{t+k}\mid y_{1:t})$ or $\pi(y_{t+k}\mid y_{1:t})$ — $k$-step ahead.
\end{itemize}
Filtering/smoothing target \emph{distributions}, \emph{not} point estimates --- this is the canonical clarification asked across the exams.

\textbf{2. Dynamic Linear Model (DLM) — definition (multivariate, DLMwR \S 2.3).}

A DLM is an SSM where the latent dynamics and the observation map are both \emph{linear} and the noises are \emph{Gaussian}. For $Y_t\in\mathbb{R}^q$ (observations, $q$-dimensional) and $\theta_t\in\mathbb{R}^p$ (state, $p$-dimensional):

\textbf{(DLM1)} \emph{State equation.}
\[
\boxed{\;\theta_t=G_t\theta_{t-1}+w_t,\qquad w_t\overset{\text{iid}}{\sim}\mathcal{N}_p(0,W_t),\;t\ge 1.\;}
\]

\textbf{(DLM2)} \emph{Observation equation.}
\[
\boxed{\;Y_t=F_t\theta_t+v_t,\qquad v_t\overset{\text{iid}}{\sim}\mathcal{N}_q(0,V_t),\;t\ge 1.\;}
\]

\textbf{(DLM3)} \emph{Initial state.}
\[
\theta_0\sim\mathcal{N}_p(m_0,C_0).
\]

\textbf{(DLM4)} \emph{Mutual independence.} $\theta_0,\{w_t\}_{t\ge 1},\{v_s\}_{s\ge 1}$ are mutually independent.

\emph{Dimensions / role of each matrix.}
\begin{itemize}
\item $G_t\in\mathbb{R}^{p\times p}$ — \emph{state evolution / transition} matrix.
\item $F_t\in\mathbb{R}^{q\times p}$ — \emph{observation / design} matrix (maps state to observation space).
\item $W_t\in\mathbb{R}^{p\times p}$ — covariance of state innovation, symmetric PSD.
\item $V_t\in\mathbb{R}^{q\times q}$ — covariance of observation noise, symmetric PSD.
\end{itemize}
\emph{When all four $(F,G,V,W)$ are time-invariant the DLM is called \emph{time-homogeneous}; if some depend on $t$ (e.g.\ regressor $x_t$ inside $F_t$), the DLM is \emph{time-varying}}.

\boxed{\;\text{DLM = }(G_t,F_t,W_t,V_t,m_0,C_0)\text{ — six ingredients, linear-Gaussian closure.}\;}

\textbf{3. Immediate consequences of the definition.}

\emph{(a) Conditional distribution of $Y_t$ given $(\theta_t,Y_{1:t-1})$.} By (SS3) and the linearity in (DLM2),
\[
Y_t\mid\theta_t,Y_{1:t-1}\sim\mathcal{N}_q(F_t\theta_t,V_t)\quad(\text{Sep 2025 Q4b}).
\]
The conditioning on $Y_{1:t-1}$ is redundant: given the contemporaneous state, observations are conditionally independent of past observations.

\emph{(b) Conditional distribution of $\theta_t$ given $\theta_{0:t-1}$.} By (SS2) Markovianity and (DLM1),
\[
\theta_t\mid\theta_{0:t-1}\sim\mathcal{N}_p(G_t\theta_{t-1},W_t)\quad(\text{Sep 2025 Q4c}).
\]

\emph{(c) DLMs cover multivariate series.} Setting $q>1$ is allowed; $F_t$ is then $q\times p$ and $V_t$ is $q\times q$. There is \emph{no} restriction to univariate observations.

\textbf{4. One-step-ahead state predictive distribution (May 2024 Q5b, Sep 2025 Q4d step 1).}

\emph{Given:} $\theta_{t-1}\mid y_{1:t-1}\sim\mathcal{N}_p(m_{t-1},C_{t-1})$ (this is the filtering output at $t-1$).

\emph{Goal:} derive $\theta_t\mid y_{1:t-1}$.

\emph{Step 1.} $\theta_t=G_t\theta_{t-1}+w_t$. By (DLM4), $w_t$ is independent of $\sigma(\theta_{t-1},y_{1:t-1})$. Hence conditionally on $y_{1:t-1}$, $\theta_t$ is an affine function of a Gaussian $\theta_{t-1}$ plus an independent Gaussian $w_t$.

\emph{Step 2 — moments.}
\begin{align*}
\mathbb{E}[\theta_t\mid y_{1:t-1}]&=G_t\,\mathbb{E}[\theta_{t-1}\mid y_{1:t-1}]+\mathbb{E}[w_t]=G_t m_{t-1}\equiv a_t,\\
\operatorname{Var}(\theta_t\mid y_{1:t-1})&=G_t\operatorname{Var}(\theta_{t-1}\mid y_{1:t-1})G_t'+\operatorname{Var}(w_t)\\
&=G_t C_{t-1}G_t'+W_t\equiv R_t\quad(\text{cross-terms vanish by indep.}).
\end{align*}

\emph{Step 3 — Gaussian closure.} Linear combinations of independent Gaussians are Gaussian, so
\[
\boxed{\;\theta_t\mid y_{1:t-1}\sim\mathcal{N}_p(a_t,R_t),\quad a_t=G_t m_{t-1},\;R_t=G_t C_{t-1}G_t'+W_t.\;}
\]

\textbf{5. One-step-ahead observation predictive distribution (Sep 2025 Q4d step 2).}

\emph{Given:} $\theta_t\mid y_{1:t-1}\sim\mathcal{N}_p(a_t,R_t)$ from step 4.

\emph{Derivation.} $Y_t=F_t\theta_t+v_t$ with $v_t\perp\theta_t\mid y_{1:t-1}$ (by (DLM4)). Same affine-Gaussian argument:
\begin{align*}
\mathbb{E}[Y_t\mid y_{1:t-1}]&=F_t a_t\equiv f_t,\\
\operatorname{Var}(Y_t\mid y_{1:t-1})&=F_t R_t F_t'+V_t\equiv Q_t.
\end{align*}
\[
\boxed{\;Y_t\mid y_{1:t-1}\sim\mathcal{N}_q(f_t,Q_t),\quad f_t=F_t a_t,\;Q_t=F_t R_t F_t'+V_t.\;}
\]
$f_t$ is the \emph{one-step-ahead point forecast} (under quadratic loss); $Q_t$ is the forecast variance; $e_t=y_t-f_t$ is the \emph{forecast error / innovation}.

\emph{Why this matters.} These two formulas together = the predict step of the Kalman filter. They underpin (i) the prediction-error decomposition of the likelihood, (ii) point forecasts, (iii) credible intervals on future observations.

\textbf{6. Univariate special case — random walk + noise.}

The simplest non-trivial DLM (DLMwR \S 2.3.2): $p=q=1$, $F=G=1$, $V,W$ scalar:
\[
Y_t=\theta_t+v_t,\quad \theta_t=\theta_{t-1}+w_t,\quad v_t\overset{\text{iid}}{\sim}\mathcal{N}(0,V),\;w_t\overset{\text{iid}}{\sim}\mathcal{N}(0,W),\;\theta_0\sim\mathcal{N}(m_0,C_0).
\]
Here $a_t=m_{t-1}$, $R_t=C_{t-1}+W$, $f_t=m_{t-1}$, $Q_t=C_{t-1}+W+V$. (Covered in detail in t7/t8.)

\textbf{7. Multivariate example — independent random walks plus noise.}

For $m=2$ assets modelled as i.i.d.\ RW + noise (Jun 2025 Q4 / May 2024 Q5):
\[
F=I_2,\;G=I_2,\;V=\operatorname{diag}(\sigma_{v_1}^2,\sigma_{v_2}^2),\;W=\operatorname{diag}(\sigma_{w_1}^2,\sigma_{w_2}^2).
\]
Putting off-diagonal entries in $W$ (e.g.\ $W_{12}=\rho\sigma_{w_1}\sigma_{w_2}$) introduces correlation between the latent random walks; putting cross-entries in $G$ introduces VAR-style dynamics.

\textbf{8. Why the linear-Gaussian assumptions matter.}

\begin{itemize}
\item \emph{Gaussian closure} — every conditional of a jointly Gaussian vector is Gaussian. So $\theta_t\mid y_{1:t}$, $\theta_t\mid y_{1:t-1}$, $Y_t\mid y_{1:t-1}$ are \emph{all} Gaussian by induction, characterised by their mean + covariance (no other moments needed).
\item \emph{Linearity + independence} — moments propagate via $\mathbb{E}[A\theta+w]=A\mu$, $\operatorname{Var}(A\theta+w)=A\Sigma A'+W$.
\item \emph{Consequence} — the Kalman filter is exact in a DLM. Filtering = forward sweep; smoothing = backward sweep (RTS). For non-Gaussian / non-linear SSMs the same conditional distributions exist abstractly, but their closed-form Gaussian recursions \emph{break}; one then turns to particle filters / extended KF / unscented KF.
\end{itemize}

\textbf{9. R — code-up of the general DLM \& predict step.}

```R
library(dlm)

## Build a generic DLM (FF, GG, V, W, m0, C0) — local level here:
mod <- dlm(FF = matrix(1, 1, 1),
           GG = matrix(1, 1, 1),
           V  = matrix(V_scalar, 1, 1),
           W  = matrix(W_scalar, 1, 1),
           m0 = m0_scalar,
           C0 = matrix(C0_scalar, 1, 1))

## Local linear trend (p=2, q=1) — DLMwR's dlmModPoly(2):
mod2 <- dlmModPoly(order = 2, dV = V_scalar,
                   dW = c(W11_scalar, W22_scalar))

## Kalman filter (gives m_t, C_t, a_t, R_t, f_t, Q_t for all t):
kf <- dlmFilter(y, mod2)
a_t <- kf$a[t, ]                                # state-predict mean
R_t <- with(kf, U.R[[t]] %*% diag(D.R[t, ]^2) %*% t(U.R[[t]]))
f_t <- kf$f[t]                                  # obs.-predict mean
Q_t <- mod2$FF %*% R_t %*% t(mod2$FF) + mod2$V  # obs.-predict variance
```

\textbf{10. Common mistakes / what graders look for.}

\begin{itemize}
\item Don't forget the \emph{initial state distribution} $\theta_0\sim\mathcal{N}_p(m_0,C_0)$ — it's an ingredient of the DLM, not optional.
\item State \emph{independence} of $\{w_t\},\{v_t\},\theta_0$ explicitly; without this the moment derivations of $a_t,R_t,f_t,Q_t$ collapse.
\item Distinguish ``predictive of the state'' $\theta_t\mid y_{1:t-1}\sim\mathcal{N}_p(a_t,R_t)$ from ``predictive of the observation'' $Y_t\mid y_{1:t-1}\sim\mathcal{N}_q(f_t,Q_t)$ — these are two separate but linked formulas.
\item For multivariate observations, $F_t$ is $q\times p$ (not square); $V_t$ is $q\times q$. ``DLMs only handle univariate series'' is \emph{wrong} (Jun 2024 Q6a) — they handle any finite $q$.
\end{itemize}

\textbf{Exam pointers.}
\begin{itemize}
\item[$\triangleright$] Used in \texttt{exam\_sep\_2025\_q4}: defines DLM for $m$-dimensional $Y_t$; (a) write (DLM1)--(DLM4); (b) $Y_t\mid(\theta_t,Y_{1:t-1})\sim\mathcal{N}_q(F_t\theta_t,V_t)$; (c) $\theta_t\mid\theta_{0:t-1}\sim\mathcal{N}_p(G_t\theta_{t-1},W_t)$; (d) one-step-ahead predictive of $Y_t$ from $\theta_{t-1}\mid y_{1:t-1}\sim\mathcal{N}_p(m_{t-1},C_{t-1})$ via the two-step affine-Gaussian derivation $\Rightarrow\mathcal{N}_q(f_t=F_t a_t,Q_t=F_t R_t F_t'+V_t)$ with $a_t=G_t m_{t-1},R_t=G_t C_{t-1}G_t'+W_t$.
\item[$\triangleright$] Used in \texttt{exam\_may\_2024\_q5}: (a) write the general DLM exactly as in (DLM1)--(DLM4); (b) given $\theta_{t-1}\mid y_{1:t-1}\sim\mathcal{N}(m_{t-1},C_{t-1})$, derive the state predictive $\theta_t\mid y_{1:t-1}\sim\mathcal{N}_p(a_t,R_t)$ with $a_t=G_t m_{t-1}$, $R_t=G_t C_{t-1}G_t'+W_t$, citing affine-Gaussian closure + independence of $w_t$.
\end{itemize}
""",
}


# =============================================================================
# t6b — SSM flexibility — non-stationarity & SV models (is it a DLM?)
# =============================================================================
theory_content_ts["t6b"] = {
    "title": "Theory — SSM flexibility — non-stationarity & SV models (is it a DLM?)",
    "content": r"""\textbf{\textcolor{red}{THEORY --- SSM flexibility — non-stationarity \& SV models (is it a DLM?) [Topic: T6 — State-space \& dynamic linear models — general framework]}}

\textbf{1. Big-picture: SSMs do \emph{not} require stationarity.}

A crucial property that distinguishes state-space modelling from classical ARMA modelling:

\boxed{\;\text{SSMs do \emph{not} require stationarity of }(Y_t)\text{ or of }(\theta_t).\;}

\emph{Why?} The Kalman filter / smoother is derived from two ingredients only:
\begin{itemize}
\item[(i)] Markovianity of $(\theta_t)$ — every step is a conditional Gaussian given the past;
\item[(ii)] Conditional Gaussianity of $Y_t\mid\theta_t$ — linear with independent Gaussian noise.
\end{itemize}
Neither (i) nor (ii) involves stationarity. The recursive update of $(m_t,C_t,a_t,R_t,f_t,Q_t)$ goes through for any sequence of matrices $G_t,F_t,W_t,V_t$ — stationary or not.

\textbf{2. Stationarity vs.\ non-stationarity — what each side allows.}

\emph{Classical ARMA setting (for context).}
\begin{itemize}
\item Defining the ACVF as a single-argument function $\gamma(h)$ requires stationarity.
\item Inference (Yule--Walker, conditional MLE, asymptotics) assumes stationarity (+ ergodicity).
\item Trends / seasonality / unit roots must be \emph{removed} (differencing, deseasoning) \emph{before} fitting an ARMA — otherwise the autocovariance has no clean interpretation.
\end{itemize}

\emph{SSM / DLM setting.}
\begin{itemize}
\item Non-stationary trend ($\theta_t=\theta_{t-1}+\beta_{t-1}+w_t$) or random walk ($\theta_t=\theta_{t-1}+w_t$) latent states are \emph{exactly} the canonical examples.
\item Local linear trend + seasonal DLM models levels, slopes and seasonality \emph{jointly} without any preprocessing — see CO$_2$ / airline-passenger examples (Harvey's BSM, DLMwR \S 3.2).
\item Even integrated processes (I$(d)$ with $d\ge 1$) fit naturally: $\theta_t=\theta_{t-1}+w_t$ \emph{is} an I$(1)$ random walk. The KF handles diffuse / improper priors $C_0\to\infty$ as a clean limit.
\end{itemize}

\emph{Multivariate restatement (Sep 2025 Q3).} For an $m$-dim.\ time series $Y_t$ of stock prices: stock prices are typically non-stationary (I$(1)$ random walk). Yes, you can still use an SSM — the canonical model is $\theta_t=\theta_{t-1}+w_t$, $Y_t=\theta_t+v_t$ (local level / RW + noise per asset, possibly with cross-asset correlations in $W$).

\textbf{3. ``YES, because... / NO, because... / YES but only if...'' — answer template.}

For ``can I use an SSM on a non-stationary series?'':

\textbf{YES, because} SSMs do not require stationarity of $(Y_t)$ or $(\theta_t)$. The KF/RTS derivations rest on Markovianity + conditional Gaussianity, not on stationarity. Canonical non-stationary SSMs include local level (I$(1)$), local linear trend ($I(2)$-style), and structural DLMs with seasonal latent components. (\emph{This is the Sep 2025 Q3 answer.})

\textbf{4. Stochastic Volatility (SV) model — formal statement (May 2022 Q4).}

A standard univariate SV model for returns:
\[
\begin{cases}
Y_t=\exp\!\bigl(\tfrac12\theta_t\bigr)v_t, & v_t\overset{\text{iid}}{\sim}\mathcal{N}(0,1),\\
\theta_t=\alpha_1+\alpha_2\theta_{t-1}+w_t, & w_t\overset{\text{iid}}{\sim}\mathcal{N}(0,\sigma^2),\;|\alpha_2|<1.
\end{cases}
\]
Here $(\theta_t)$ is an unobserved Gaussian AR(1) representing log-variance: $\operatorname{Var}(Y_t\mid\theta_t)=e^{\theta_t}$. The model captures volatility clustering: high $\theta_t\Rightarrow$ large $|Y_t|$ likely $\Rightarrow$ next $\theta_{t+1}$ likely high (AR(1) persistence).

\textbf{5. Is the SV model a state-space model? — YES.}

Check (SS1)--(SS3) from t6a:
\begin{itemize}
\item[(SS1)] Initial state: $\theta_0$ drawn from its (stationary or arbitrary) law.
\item[(SS2)] Markov state: $\theta_t\mid\theta_{0:t-1}\sim\mathcal{N}(\alpha_1+\alpha_2\theta_{t-1},\sigma^2)$ — Gaussian AR(1), Markov by construction.
\item[(SS3)] Conditional independence of obs.\ given state: $Y_t\mid\theta_t\sim\mathcal{N}(0,e^{\theta_t})$ depends only on the contemporaneous $\theta_t$.
\end{itemize}
All three SSM axioms hold $\Rightarrow$ the SV model \emph{is} a state-space model.

\textbf{6. Is the SV model a DLM? — NO.}

To be a DLM the observation equation must be \emph{linear in $\theta_t$ with additive Gaussian noise of state-independent variance} (recall (DLM2): $Y_t=F_t\theta_t+v_t$, $v_t\sim\mathcal{N}(0,V_t)$). In the SV model
\[
Y_t=e^{\theta_t/2}\,v_t
\]
\begin{itemize}
\item The observation equation is \emph{multiplicative}, not linear in $\theta_t$ — $e^{\theta_t/2}$ is a non-linear function of the state.
\item Equivalently $Y_t\mid\theta_t\sim\mathcal{N}(0,e^{\theta_t})$: the \emph{variance} depends on the state, not just the mean.
\item Outside the linear-Gaussian template, the Kalman filter does \emph{not} apply directly.
\end{itemize}
Hence SV is a \emph{non-linear} (or \emph{conditionally Gaussian with state-dependent variance}) SSM, but \emph{not} a DLM.

\boxed{\;\text{SV: }\;\text{SSM YES, DLM NO. Reason: }Y_t=e^{\theta_t/2}v_t\text{ is non-linear in }\theta_t.\;}

\textbf{7. Practical workaround — linearisation by log-squaring.}

A standard trick for SV (Kim--Shephard--Chib 1998): square and log the observation equation:
\[
\log Y_t^2=\theta_t+\log v_t^2,\qquad \log v_t^2\sim\log\chi^2_1\;(\text{mean }\approx -1.27,\text{ variance }\approx 4.93).
\]
The state equation is unchanged. The transformed observation equation is now \emph{linear} in $\theta_t$ — but the noise $\log v_t^2$ is \emph{not} Gaussian; it follows a $\log\chi^2_1$ distribution (highly skewed, heavy left tail). Two standard fixes:

\begin{itemize}
\item \emph{Gaussian-mixture approximation.} Approximate $\log\chi^2_1\approx\sum_{j=1}^7 q_j\mathcal{N}(m_j,s_j^2)$ (Kim--Shephard--Chib's 7-component fit). Conditional on the mixture indicator at each $t$, the model \emph{is} a DLM and one runs KF / FFBS within Gibbs.
\item \emph{Particle filter / sequential Monte Carlo.} Approximate $\pi(\theta_t\mid y_{1:t})$ by a weighted particle ensemble; works for any SSM, no Gaussian assumption needed.
\end{itemize}

\textbf{8. Summary table — SSM vs.\ DLM (decision tree the exam tests).}

\emph{(Conceptual; not formatted as a LaTeX tabular here for length.)}

Q1: Latent $\theta_t$ Markov? — If NO, not even an SSM.

Q2: $Y_t\perp\text{everything else}\mid\theta_t$? — If NO, not an SSM (observations depend on past observations directly $\Rightarrow$ ARMA-like).

Q3: State equation $\theta_t=G_t\theta_{t-1}+w_t$, $w_t\sim\mathcal{N}$? — If NO (non-linear state), SSM but not DLM.

Q4: Observation equation $Y_t=F_t\theta_t+v_t$, $v_t\sim\mathcal{N}_q(0,V_t)$ with $V_t$ not depending on $\theta_t$? — If NO (non-linear obs., state-dependent variance), SSM but not DLM.

Q5: Independence of $\{w_t\},\{v_t\},\theta_0$? — Required for KF derivations.

If Q1--Q5 all YES with linear-Gaussian forms $\Rightarrow$ DLM, Kalman filter applies exactly.

\textbf{9. Other ``is it a DLM?'' edge cases (cross-reference).}

\begin{itemize}
\item \emph{AR(2) in companion form (Jun 2024 Q2).} $Y_t=\alpha_1 Y_{t-1}+\alpha_2 Y_{t-2}+\varepsilon_t$. Naively writing $F_t=(Y_{t-1},Y_{t-2})$ violates conditional independence ($F_t$ depends on past observations). Correct DLM (DLMwR \S 3.2.5): $\theta_t=(Y_t,Y_{t-1})'$, $G=\binom{\alpha_1\;\alpha_2}{1\;0}$, $F=(1,0)$, $V=0$, $W=\binom{\sigma^2\;0}{0\;0}$. — IS a DLM in the right parameterisation.
\item \emph{Structural DLMs (May 2021 Q3).} CO$_2$ data with trend + seasonality. Use $Y_t=\mu_t+\gamma_t+v_t$ with local linear trend $\mu_t$ and seasonal $\gamma_t$ as latent components. Non-stationary, but a valid DLM.
\item \emph{Hierarchical / regime-switching SSMs} — generally SSM but not DLM, unless the regime is conditioned on.
\end{itemize}

\textbf{10. R — fitting SV and comparing to a DLM trend model.}

```R
## SV model (NOT a DLM): use the stochvol package
library(stochvol)
draws <- svsample(y, draws = 10000, burnin = 1000,
                  priormu = c(0, 100), priorphi = c(20, 1.5),
                  priorsigma = 0.1)
summary(draws)
plot(draws)                                       # posterior of (alpha_1, alpha_2, sigma^2)

## RW + noise on log-prices (IS a DLM) — local level for non-stationary data:
library(dlm)
mod_ll <- dlmModPoly(order = 1, dV = sigma_v^2, dW = sigma_w^2)
fit_ll <- dlmFilter(log_price, mod_ll)            # KF runs fine on I(1) data
```

\textbf{11. Exam-ready one-liners.}

\begin{itemize}
\item ``\emph{Can I use an SSM for a non-stationary series?}'' $\Rightarrow$ \textbf{YES, because} SSMs do not require stationarity --- KF/RTS only need Markov + cond.\ Gaussian; non-stationary latents (RW, integrated, structural) are the canonical examples.
\item ``\emph{Is the SV model an SSM?}'' $\Rightarrow$ \textbf{YES}: $(\theta_t)$ is a Gaussian AR(1) Markov chain; $Y_t\mid\theta_t\sim\mathcal{N}(0,e^{\theta_t})$ depends only on $\theta_t$.
\item ``\emph{Is the SV model a DLM?}'' $\Rightarrow$ \textbf{NO}: the obs.\ equation $Y_t=e^{\theta_t/2}v_t$ is non-linear in $\theta_t$ (variance depends on state); KF doesn't apply directly. Linearise via $\log Y_t^2=\theta_t+\log v_t^2$ with non-Gaussian noise, then approximate by a Gaussian mixture (KSC) or use a particle filter.
\end{itemize}

\textbf{Exam pointers.}
\begin{itemize}
\item[$\triangleright$] Used in \texttt{exam\_sep\_2025\_q3}: ``$m$-dim.\ non-stationary $(Y_t)$ — can you use an SSM?'' Answer \textbf{YES, because} SSMs do not require stationarity of $(Y_t)$ or $(\theta_t)$ — latent state can be RW / integrated / regime-switching; observations inherit it. KF derived from Markov + cond.\ Gaussian. Canonical $I(1)$ DLM: $\theta_t=\theta_{t-1}+w_t,Y_t=\theta_t+v_t$.
\item[$\triangleright$] Used in \texttt{exam\_may\_2022\_q4}: SV model $Y_t=e^{\theta_t/2}v_t$, $\theta_t=\alpha_1+\alpha_2\theta_{t-1}+w_t$ — (a) IS an SSM (Markov $(\theta_t)$, $Y_t\mid\theta_t\sim\mathcal{N}(0,e^{\theta_t})$ depends only on $\theta_t$); (b) NOT a DLM (multiplicative $e^{\theta_t/2}$, variance depends on state). Standard linearisation $\log Y_t^2=\theta_t+\log v_t^2$ with $\log\chi^2_1$ noise; approximate by Gaussian mixture (KSC) for MCMC.
\end{itemize}
""",
}
