"""Masters wave B — T4 + T5 + T6a."""
master_exercises_ts = {}

master_exercises_ts["t4a"] = {
    "title": "Master — Panel transition-count likelihood & MLE",
    "content": r"""\textbf{\textcolor{red}{MASTER --- Panel transition-count likelihood & MLE}}

\emph{Canonical framework:} a panel of $n$ \emph{i.i.d.} categorical series $(Y_{k,t})_{t=0,\dots,T}$, $k=1,\dots,n$, each modelled as a homogeneous Markov chain on $\{1,\dots,K\}$ with common transition matrix $\mathbf P=[p_{ij}]$. Conditioning on the fixed initial values $y_{k,0}$, by the Markov property each series contributes $\prod_t p_{y_{k,t-1},y_{k,t}}$, and pooling over $(k,t)$ collapses the joint likelihood into a clean product over the transition counts $n_{ij}=\#\{(k,t):y_{k,t-1}=i,\,y_{k,t}=j\}$. The MLE is the Anderson--Goodman row-normalised count; the marginal Wald CI follows from the row-multinomial CLT.

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
    "images": ["images/master/master_t4a_ai.png"]
}

master_exercises_ts["t4b"] = {
    "title": "Master — Wald CI for p_ij + forecasting future percentages",
    "content": r"""\textbf{\textcolor{red}{MASTER --- Wald CI for $p_{ij}$ + forecasting future percentages}}

\emph{Canonical framework:} once the MLE $\widehat p_{ij}=n_{ij}/n_{i,+}$ is in hand, two follow-up tasks arise. \textbf{(i) Inference}: a Wald CI for a single $p_{ij}$ via the multinomial CLT $\sqrt{n_{i,+}}(\widehat p_{ij}-p_{ij})\to\mathcal{N}(0,p_{ij}(1-p_{ij}))$. \textbf{(ii) Forecasting future percentages}: propagate the empirical state distribution one step forward through $\widehat{\mathbf P}$, then use the binomial/panel CLT to evaluate tail probabilities such as $\mathbb{P}(\bar Y_{\text{next}}>0.5)$. A separate "conceptual" bullet asks whether a specific path probability of the form $p_{13}\,p_{33}^2\,p_{31}$ is \emph{known} or must be \emph{estimated}.

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
    "images": ["images/master/master_t4b_ai.png"]
}

master_exercises_ts["t5a"] = {
    "title": "Master — HMM definition, parameters & forward-algorithm likelihood",
    "content": r"""\textbf{\textcolor{red}{MASTER --- HMM definition, parameters \& forward-algorithm likelihood}}

\emph{Canonical framework:} a Hidden Markov Model is a state-space model with \emph{discrete} latent state $(S_t)$ taking values in $\{1,\dots,k\}$. Three building blocks: (i) initial law $S_1\sim\pi$, (ii) homogeneous Markov transition $\mathbf P=[p_{ij}]$, (iii) emission distribution $Y_t\mid S_t=i\sim f_i(\cdot;\theta_i)$ --- Categorical (text), Gaussian (returns), Poisson (counts). The joint $p(y_{1:T},s_{1:T};\phi)=\pi_{s_1}\,e_{s_1,y_1}\prod_{t=2}^{T}p_{s_{t-1},s_t}\,e_{s_t,y_t}$ has $k^T$ terms when marginalised by brute force; the \textbf{forward algorithm} reduces this to $O(k^2 T)$ via the recursion $\alpha_t(j)=\bigl(\sum_i\alpha_{t-1}(i)\,p_{ij}\bigr)e_{j,y_t}$, with likelihood $L(\phi)=\sum_i\alpha_T(i)$. MLE then proceeds by Baum--Welch (EM).

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
    "images": ["images/master/master_t5a_ai.png"]
}

master_exercises_ts["t5b"] = {
    "title": "Master — Decoding (Viterbi / forward-backward) + path-probability",
    "content": r"""\textbf{\textcolor{red}{MASTER --- Decoding (Viterbi / forward--backward) + path-probability}}

\emph{Canonical framework:} once an HMM (or homogeneous MC) is specified, two "path" problems arise. \textbf{(A) Path probability of an observed sequence under a known MC} --- a direct product of transition probabilities by the Markov property. \textbf{(B) Decoding} the latent path of an HMM given $y_{1:T}$, in two flavours: \emph{global / MAP} (most likely \emph{joint} path, by Viterbi) and \emph{marginal / pointwise MAP} (most likely \emph{individual} state at each $t$, by forward--backward).

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
    "images": ["images/master/master_t5b_ai.png"]
}

master_exercises_ts["t6a"] = {
    "title": "Master — SSM / DLM general definition (univariate & multivariate)",
    "content": r"""\textbf{\textcolor{red}{MASTER --- SSM / DLM general definition (univariate \& multivariate)}}

\emph{Canonical framework:} a Dynamic Linear Model couples a $p$-dimensional latent state $\theta_t$ (Markov, linear-Gaussian dynamics) with a $q$-dimensional observation $Y_t$ (linear-Gaussian emission). The defining equations are
\[ \theta_t=G_t\theta_{t-1}+w_t,\;w_t\overset{\text{iid}}{\sim}\mathcal{N}_p(0,W_t);\qquad Y_t=F_t\theta_t+v_t,\;v_t\overset{\text{iid}}{\sim}\mathcal{N}_q(0,V_t), \]
with $\theta_0\sim\mathcal{N}_p(m_0,C_0)$ and $\{w_t\},\{v_s\},\theta_0$ mutually independent. The univariate case ($q=1$) is a special case --- DLMs are intrinsically multivariate. Two conditional distributions read directly off the equations (state and obs.\ Markovianity); the one-step-ahead predictives follow by propagating Gaussians through the linear maps $G_t$ and $F_t$.

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
    "images": ["images/master/master_t6a_ai.png"]
}
