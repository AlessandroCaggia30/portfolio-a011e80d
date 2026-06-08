"""
Past exam snippets — Time Series Analysis

One entry per (exam, question). Generated from TSA_PastExams_Solutions.tex
via 1:1 transcription with macro substitution for the MindNotes KaTeX bridge.
"""

past_exams_ts = {}

past_exams_ts["exam_sep_2025_q1"] = {
    "title": 'Sep 2025 — Q1',
    "content": r"""<span class="exam-question-text">In classic Statistics, a time series is regarded as a stochastic process. Explain:
**(a)** What is a stochastic process?

**(b)** Define a time series as a stochastic process.</span>

---

**Solution.** \textbf{(a)} A stochastic process is a sequence of random vectors $(Y_t)_{t\in T}$ indexed by
time $t\in T$ (e.g.\ $T=\mathbb{N}$). It is fully specified by its finite-dimensional distributions
$\{f_{Y_{t_1},\dots,Y_{t_k}}(y_1,\dots,y_k):k\ge 1,(t_1,\dots,t_k)\in T^k\}$
(Kolmogorov's extension theorem, given consistency).

\textbf{(b)} A time series is a stochastic process $(Y_t)_{t\in T}$ with $Y_t\in\mathcal Y$;
the data $(y_1,\dots,y_T)$ are a single \emph{finite realization} of one path. Inference from
a single path is possible only under structural assumptions (stationarity, parametric
dynamics).""",
    "is_exam": True,
    "topic_hint": "t1a",
    "images": []
}

past_exams_ts["exam_sep_2025_q2"] = {
    "title": 'Sep 2025 — Q2',
    "content": r"""<span class="exam-question-text">Consider a univariate time series $(Y_t)_{t\ge 1}$.
**(a)** Define its mean function and autocovariance function.

**(b)** When is the time series (weakly) stationary?

**(c)** Suppose that the time series is \textbf{not} stationary. Can you use an ARMA model for it?
        \emph{Start your reply with:} \textbf{YES, because\ldots, NO because\ldots, or YES but only if\ldots}</span>

---

**Solution.** \textbf{(a)} $\mu(t)=\mathbb{E}[Y_t]$; $\gamma(s,t)=\operatorname{Cov}(Y_s,Y_t)$, a function of \emph{two} arguments.

\textbf{(b)} Weakly stationary iff $\mathbb{E}[Y_t]$ and $\mathbb{E}[Y_tY_{t+h}]$ are finite and do not depend
on $t$, for every $h$:
$\mu(t)=\mu$, $\sigma^2(t)=\sigma^2$, $\gamma(t,t+h)=\tilde\gamma(h)$.

\textbf{(c) NO}, because ARMA has \emph{constant} coefficients $(\phi_i,\theta_j,\sigma^2)$, hence
constant mean/variance and lag-only ACVF; it cannot describe a non-stationary mean/variance.
\textbf{YES, but only if} the non-stationarity is first removed: differencing $(1-B)^d$ for
unit roots (ARIMA), $(1-B^s)$ for seasonality (SARIMA), or remove trend/seasonality and fit
ARMA to the stationary residual.""",
    "is_exam": True,
    "topic_hint": "t2a",
    "images": []
}

past_exams_ts["exam_sep_2025_q3"] = {
    "title": 'Sep 2025 — Q3',
    "content": r"""<span class="exam-question-text">Consider an $m$-dimensional time series $(Y_t)_{t\ge 1}$, for example describing the prices
of $m$ stocks in a portfolio over time, and suppose that it is \textbf{not} stationary. Can
you use a state--space model for it?
\emph{Start your reply with:} \textbf{YES, because\ldots, NO because\ldots, or YES but only if\ldots}</span>

---

**Solution.** \textbf{YES, because} SSMs do not require $(Y_t)$ to be stationary --- the latent state
$(\theta_t)$ can be non-stationary (random walk, integrated, regime-switching) and the
observations inherit it. The Kalman filter/smoother is derived from Markovianity +
conditional Gaussianity, not from stationarity. Local level $\theta_t=\theta_{t-1}+w_t$,
$Y_t=\theta_t+v_t$ is the canonical SSM for $I(1)$ asset prices.""",
    "is_exam": True,
    "topic_hint": "t6b",
    "images": []
}

past_exams_ts["exam_sep_2025_q4"] = {
    "title": 'Sep 2025 — Q4',
    "content": r"""<span class="exam-question-text">For an $m$-dimensional time series $(Y_t)_{t\ge 1}$,
**(a)** define (precisely) a dynamic linear model (DLM), denoting the $p$-dimensional state
        process by $(\theta_t)_{t\ge 0}$;

**(b)** what is the conditional distribution of $Y_t$ given $(\theta_t,Y_{1:t-1})$?

**(c)** what is the conditional distribution of $\theta_t$ given $\theta_{0:t-1}$?

**(d)** how can one obtain the one-step-ahead predictive distribution of $Y_t$ given data $y_{1:t-1}$?
        Show all steps precisely.</span>

---

**Solution.** \textbf{(a) DLM} (DLMwR \S 2.3; keydef \textbf{16a}).
$\theta_t=G_t\theta_{t-1}+w_t$, $w_t\overset{\text{iid}}{\sim}\Nd_p(0,W_t)$;
$Y_t=F_t\theta_t+v_t$, $v_t\overset{\text{iid}}{\sim}\Nd_q(0,V_t)$;
$\theta_0\sim\Nd_p(m_0,C_0)$; $\{w_t\},\{v_s\},\theta_0$ mutually independent.

\textbf{(b)} $Y_t\perp Y_{1:t-1}\mid\theta_t$ (conditional indep.\ of obs.), so
$Y_t\mid(\theta_t,Y_{1:t-1})\sim\Nd_q(F_t\theta_t,V_t)$.

\textbf{(c)} $(\theta_t)$ Markov, so
$\theta_t\mid\theta_{0:t-1}\sim\Nd_p(G_t\theta_{t-1},W_t)$.

\textbf{(d) Prediction step (DLMwR \S 2.7.2, Prop.\ 2.2).}
Given $\theta_{t-1}\mid y_{1:t-1}\sim\Nd_p(m_{t-1},C_{t-1})$:
$$
\theta_t\mid y_{1:t-1}\sim\Nd_p(a_t,R_t),\quad a_t=G_t m_{t-1},\;R_t=G_t C_{t-1}G_t^{\top}+W_t,
$$
$$
\boxed{\;Y_t\mid y_{1:t-1}\sim\Nd_q(f_t,Q_t),\quad f_t=F_t a_t,\;Q_t=F_t R_t F_t^{\top}+V_t.\;}
$$
Linear combination of independent Gaussians is Gaussian.""",
    "is_exam": True,
    "topic_hint": "t6a",
    "images": []
}

past_exams_ts["exam_sep_2025_q5"] = {
    "title": 'Sep 2025 — Q5',
    "content": r"""<span class="exam-question-text">The following is a popular model for the trend of a univariate time series $(Y_t)_{t\ge 1}$:
$$
Y_t=\mu_t+\varepsilon_t,\qquad \mu_t=\mu_{t-1}+\beta_{t-1}+w_{1,t},\qquad \beta_t=\beta_{t-1}+w_{2,t},
$$
$$
\varepsilon_t\overset{\text{iid}}{\sim}\mathcal{N}(0,\sigma^2),\quad w_{1,t}\overset{\text{iid}}{\sim}\mathcal{N}(0,\sigma_{w_1}^2),\quad
w_{2,t}\overset{\text{iid}}{\sim}\mathcal{N}(0,\sigma_{w_2}^2),
$$
with $(w_{1,t})$ and $(w_{2,t})$ independent, and independent of $(\mu_0,\beta_0)$.
\textbf{Task:} Write the model in the form of a DLM.</span>

---

**Solution.** Local linear trend (DLMwR \S 3.2.2). Take $\theta_t=(\mu_t,\beta_t)^{\top}$:
$$
\boxed{\;
F=(1,\,0),\;V=\sigma^2,\quad
G=\begin{pmatrix}1&1\\0&1\end{pmatrix},\;W=\begin{pmatrix}\sigma_{w_1}^2&0\\0&\sigma_{w_2}^2\end{pmatrix},\;\;
\theta_0=\begin{pmatrix}\mu_0\\\beta_0\end{pmatrix}\sim\Nd_2(m_0,C_0).\;}
$$""",
    "is_exam": True,
    "topic_hint": "t7b",
    "images": []
}

past_exams_ts["exam_sep_2025_q6"] = {
    "title": 'Sep 2025 — Q6',
    "content": r"""<span class="exam-question-text">In a study, the level of air pollution in location $i$ is measured over time as
$1=$\textbf{``low''}, $2=$\textbf{``moderate''}, $3=$\textbf{``high''}, starting from a known
$y_{i0}$ at time $t=0$. A simple model for the resulting time series $(Y_{i,t})_{t\ge 0}$
assumes that it is a homogeneous Markov chain. Now suppose that you have data in $n$ locations,
$i=1,\dots,n$, for $t=0,1,\dots,100$, and for simplicity, you model the time series
$(Y_{i,t})_{t\ge 0}$ as independent and identically distributed across locations
$i=1,\dots,n$. How would you estimate the unknown transition matrix, by maximum likelihood?
Consider both point estimation and (marginal) confidence intervals.</span>

---

**Solution.** \textbf{Likelihood} (cond.\ on $y_{i,0}$; keydef \textbf{12}):
$L(\mathbf P)=\prod_{i,j}p_{ij}^{n_{ij}}$, with $n_{ij}=\sum_{k,t}\mathbf{1}\{Y_{k,t-1}=i,Y_{k,t}=j\}$ the
pooled transition counts and $n_{i,+}=\sum_j n_{ij}$.

\textbf{MLE} (row-by-row Lagrangian, sufficient statistic $\{n_{ij}\}$):
$$
\boxed{\;\widehat p_{ij}=\frac{n_{ij}}{n_{i,+}}.\;}
$$

\textbf{Asymptotic CI} (keydef \textbf{13}, Anderson--Goodman). Conditional on $n_{i,+}$,
$(n_{i,1},n_{i,2},n_{i,3})\sim\mathrm{Multinom}(n_{i,+};p_{i,:})$, so
$\sqrt{n_{i,+}}(\widehat p_{ij}-p_{ij})\xrightarrow{d}\mathcal{N}(0,p_{ij}(1-p_{ij}))$. Wald $(1-\alpha)$ CI:
$$
\boxed{\;\widehat p_{ij}\pm z_{\alpha/2}\sqrt{\widehat p_{ij}(1-\widehat p_{ij})/n_{i,+}}.\;}
$$""",
    "is_exam": True,
    "topic_hint": "t4a",
    "images": []
}

past_exams_ts["exam_sep_2025_q7"] = {
    "title": 'Sep 2025 — Q7',
    "content": r"""<span class="exam-question-text">In applications of DLMs to economic data, the parameters, say $\phi$, of the DLM are
typically unknown, and a common way to proceed is to estimate them by maximum likelihood,
then plug their MLE $\hat\phi$ in the model and proceed by Kalman filter or smoothing.
**(a)** Based on data $y_1,\dots,y_t$, write the expression of the likelihood of $\phi$.

**(b)** What is the difference between filtering and smoothing?
        \emph{Answer as:} The \emph{filtering distribution} is \ldots [its definition, not its
        expression]. The \emph{joint smoothing distribution} is \ldots, and the
        \emph{marginal smoothing distribution} is \ldots [their definitions, not their expressions].

**(c)** \emph{(Optional, +0.5 points)} What are the recursion steps for obtaining (via the
        Kalman smoother) the expression of the marginal smoothing distribution? Can you
        provide hints of the proof?</span>

---

**Solution.** \textbf{(a)} Prediction-error decomposition (DLMwR \S 4.1, eq.\ 4.1, p.\ 144):
$$
L(\phi\mid y_{1:T})=\prod_{t=1}^T \Nd_q\bigl(y_t;f_t(\phi),Q_t(\phi)\bigr),
$$
with $(f_t,Q_t)$ from the Kalman filter at $\phi$. Equivalently
$\ell(\phi)=-\tfrac12\sum_t[\,q\log(2\pi)+\log|Q_t|+e_t^{\top} Q_t^{-1}e_t\,]$.
$\widehat\phi$ numerically (BFGS / EM).

\textbf{(b) Definitions.}
\emph{Filtering:} $\pi(\theta_t\mid y_{1:t})$ --- state given data \emph{up to now}.
\emph{Joint smoothing:} $\pi(\theta_{0:T}\mid y_{1:T})$ --- full latent path given \emph{all} data.
\emph{Marginal smoothing:} $\pi(\theta_t\mid y_{1:T})$, $t<T$ --- past state given all data
(including \emph{after} $t$).

\textbf{(c) RTS smoother} (DLMwR Prop.\ 2.4, p.\ 61). Run KF forward, store $(m_t,C_t)$ and
$(a_{t+1},R_{t+1})$. Backward, with $\theta_t\mid y_{1:T}\sim\Nd_p(s_t,S_t)$:
$$
\boxed{\;
s_T=m_T,\,S_T=C_T;\quad
J_t=C_t G_{t+1}^{\top} R_{t+1}^{-1};\quad
s_t=m_t+J_t(s_{t+1}-a_{t+1}),\;\;
S_t=C_t-J_t(R_{t+1}-S_{t+1})J_t^{\top}.
\;}
$$
\emph{Proof hint.} Backward Markov: $\theta_t\mid(\theta_{t+1},y_{1:T})=\theta_t\mid(\theta_{t+1},y_{1:t})$
(future data add no info on $\theta_t$ once $\theta_{t+1}$ is known). Gaussian conditioning on
the joint $(\theta_t,\theta_{t+1})\mid y_{1:t}$ gives a Gaussian backward kernel with
regression coefficient $J_t$; marginalize against
$p(\theta_{t+1}\mid y_{1:T})=\mathcal{N}(s_{t+1},S_{t+1})$.""",
    "is_exam": True,
    "topic_hint": "t9a",
    "images": []
}

past_exams_ts["exam_sep_2025_q8"] = {
    "title": 'Sep 2025 — Q8',
    "content": r"""<span class="exam-question-text">**(a)** Consider $Y_t=\theta+\varepsilon_t$, $t\ge 1$, where $\varepsilon_t\overset{\text{iid}}{\sim}\mathcal{N}(0,\sigma^2=1)$
        and $\theta$ is unknown. In a Bayesian approach, what is the joint density of
        $(Y_1,\dots,Y_t,\theta)$?

**(b)** \emph{(Optional, +0.5 points)} Given data $y_{1:n}\equiv(Y_1=y_1,\dots,Y_n=y_n)$, with
        $n=20$ and sample mean $\bar y_n=4$, use Bayes' rule and compute the posterior density
        of $\theta$, that is $p(\theta\mid y_{1:n})$. Describe all the steps of the computations.</span>

---

**Solution.** Conjugate prior $\theta\sim\mathcal{N}(m_0,C_0)$ (flat prior $\Leftrightarrow C_0\to\infty$).

\textbf{(a)} Conditional on $\theta$, $Y_t$ are i.i.d.\ $\mathcal{N}(\theta,1)$. So
$$
p(y_{1:t},\theta)=\mathcal{N}(\theta;m_0,C_0)\prod_{s=1}^t\mathcal{N}(y_s;\theta,1).
$$

\textbf{(b) Static-$\theta$ (Case A) update} --- conjugate Normal-Normal:
$$
\boxed{\;\theta\mid y_{1:n}\sim\mathcal{N}(m_n,C_n),\quad
\frac{1}{C_n}=\frac{1}{C_0}+\frac{n}{\sigma^2},\quad m_n=C_n\Bigl(\tfrac{m_0}{C_0}+\tfrac{n\bar y_n}{\sigma^2}\Bigr).\;}
$$
(Precision adds; $m_n$ = precision-weighted average of $m_0$ and $\bar y_n$.)
\emph{Numerics ($\sigma^2=1$, $n=20$, $\bar y_n=4$, flat prior):} $C_n=1/20=0.05$, $m_n=4$, so
$\theta\mid y_{1:20}\sim\mathcal{N}(4,0.05)$; 95\% CI $4\pm 1.96\sqrt{0.05}\approx[3.56,4.44]$.""",
    "is_exam": True,
    "topic_hint": "t13a",
    "images": []
}

past_exams_ts["exam_jun_2025_q1"] = {
    "title": 'Jun 2025 — Q1',
    "content": r"""<span class="exam-question-text">**(a)** Consider a categorical time series $\{Y_t\}$ starting at $Y_0\sim\pi$, with
        $Y_t\in\{1,2,\dots,K\}$. Is it a Markov chain?

**(b)** Using the Directed Acyclic Graph (DAG) representation of the dependence structure of
        a Markov chain, show that
        $$Y_t\perp(Y_1,\dots,Y_{t-2})\mid Y_{t-1}.$$
        That is, $Y_t$ is conditionally independent from $Y_{t-2}$ given $Y_{t-1}$.</span>

---

**Solution.** \textbf{(a) Not in general.} The state space and initial law alone do not imply Markovianity.
$(Y_t)$ is a MC (keydef \textbf{9a}) iff $p(y_t\mid y_{0:t-1})=p(y_t\mid y_{t-1})$ for every $t$.

\textbf{(b)} The MC DAG is the path $Y_0\to Y_1\to\cdots\to Y_t\to\cdots$. Every directed path
from $Y_{1:t-2}$ to $Y_t$ goes through $Y_{t-1}$. Conditioning on $Y_{t-1}$ blocks all such
paths ($Y_{t-1}$ is a serial/chain node), so $Y_t\perp(Y_{1:t-2})\mid Y_{t-1}$ by
$d$-separation. (Algebraically: the Markov property.)""",
    "is_exam": True,
    "topic_hint": "t3a",
    "images": []
}

past_exams_ts["exam_jun_2025_q2"] = {
    "title": 'Jun 2025 — Q2',
    "content": r"""<span class="exam-question-text">Before an election, a study monitors a panel of $n=1000$ individuals, with monthly interviews
from January ($t=0$) to June ($t=5$), where each individual is asked whether she/he is going
to vote ($1=$\textbf{``YES''}), not going ($2=$\textbf{``NO''}), or is still uncertain
($3=$\textbf{``Uncertain''}).
The observed transition counts are reported in the following table
$$
\begin{array}{c|ccc|c}
 & 1 & 2 & 3 & \\\hline
1 & 70 & 30 & 50 & 150 \\
2 & 75 & 120 & 55 & 250 \\
3 & 30 & 34 & 36 & 100
\end{array}
$$
The task is to estimate the probability to have a percentage of voters $>50\%$ at the
elections in the following July. To this aim. let us model the individual time series
$(Y_{k,t})_{t\ge 1}$, with $k=1,\dots,100$ as Independent and identically distributed
homogeneous Markov chains, with common transition matrix $\mathbf P=[p_{i,j}]$.
**(a)** What are the unknown parameters of this model?

**(b)** Write the expression of the likelihood given the data (the initial values at
        $h_{k,0}$ were fixed for all individuals $k=1,\dots,100$, so you can treat the initial
        probabilities as known).

**(c)** Estimate $p_{1,1}$, also providing the asymptotic confidence interval of level 90\%.
        (Use $z_{0.90}=1.28$, $z_{0.95}=1.65$, $z_{0.975}=1.96$.)

**(d)** Explain the steps for obtaining the asymptotic distribution used above.

**(e)** Denote by $q$ the probability that the percentage of people going to vote in July is
        higher than 50\%. Provide a point estimate of $q$.</span>

---

**Solution.** \textbf{(a)} Free parameters of $\mathbf P$: $3\cdot 2=6$ (rows sum to 1).

\textbf{(b)} $L(\mathbf P)=\prod_{i,j}p_{ij}^{n_{ij}}$.

\textbf{(c)} $\widehat p_{1,1}=70/150\approx 0.467$.
$0.467\pm 1.65\sqrt{0.467\cdot 0.533/150}=0.467\pm 1.65\cdot 0.0407=0.467\pm 0.067$,
i.e.\ $\boxed{[0.400,\,0.534]}$.

\textbf{(d)} See keydef \textbf{13}: given visit count $n_{1,+}=150$, the next-state outcomes
are an i.i.d.\ sample from $\mathrm{Multinom}(1;p_{1,:})$, so
$(n_{1,1},n_{1,2},n_{1,3})\mid n_{1,+}\sim\mathrm{Multinom}(n_{1,+};p_{1,:})$. CLT applied to
$\widehat p_{1,1}=n_{11}/n_{1,+}$ gives
$\sqrt{n_{1,+}}(\widehat p_{1,1}-p_{1,1})\xrightarrow{d}\mathcal{N}(0,p_{1,1}(1-p_{1,1}))$.

\textbf{(e) Point estimate of $q$.}
Step 1: estimated July YES probability via plug-in
$\widehat p_{\text{YES}}=\sum_i\widehat\pi_i^{(5)}\,\widehat p_{i,1}$, with
$\widehat\pi^{(5)}\propto(150,250,100)$ the empirical state distribution in June.
This gives $\widehat p_{\text{YES}}=(150\cdot.467+250\cdot.300+100\cdot.300)/500=175/500=0.35$.
Step 2: $\bar Y_{\text{July}}\approx\mathcal{N}(0.35,\,0.35\cdot 0.65/n)$ (panel proportion CLT).
$\sqrt{0.35\cdot 0.65/1000}\approx 0.015$.
$\widehat q=1-\Phi((0.5-0.35)/0.015)=1-\Phi(10)\approx 0$.""",
    "is_exam": True,
    "topic_hint": "t4b",
    "images": []
}

past_exams_ts["exam_jun_2025_q3"] = {
    "title": 'Jun 2025 — Q3',
    "content": r"""<span class="exam-question-text">**(a)** Give the general definition of a state-space model for a time series $(Y_t)_{t\ge 1}$.

**(b)** In state-space models, filtering consists in recursively providing a point estimate
        of the state $\theta_t$ given $y_{1:t}$. Is this correct?
        \begin{itemize}[leftmargin=1.6em]

**(c)** [A.] \textbf{Yes}, because:

**(d)** [B.] \textbf{No}, because:
        \end{itemize}

**(e)** Now consider a Dynamic Linear Model (DLM). If $\theta_t\mid\theta_{t-1}\sim\Nd_p(m_{t-1},C_{t-1})$,
        what is the distribution of $\theta_t\mid y_{1:t}$? Prove your claims.</span>

---

**Solution.** \textbf{(a)} Keydef \textbf{15b}. SSM: $\bigl(Y_t,\theta_t\bigr)_{t\ge 1}$, $\theta_0\sim\pi$,
$\theta_t\mid\theta_{t-1}\sim f(\theta_t\mid\theta_{t-1})$ (Markov state),
$Y_t\mid\theta_t\sim f(y_t\mid\theta_t)$ (conditional indep.\ of obs.\ given state).

\textbf{(b)} \textbf{NO}: filtering is the \emph{distribution} $\pi(\theta_t\mid y_{1:t})$, not
a point estimate. The conditional mean is one summary; uncertainty (variance, credible
region) requires the full distribution.

\textbf{(c) DLM filtering update.} \emph{Predict} (Q4d of Exam 1):
$\theta_t\mid y_{1:t-1}\sim\mathcal{N}(a_t,R_t)$, $a_t=Gm_{t-1}$, $R_t=GC_{t-1}G^{\top}+W$;
$Y_t\mid y_{1:t-1}\sim\mathcal{N}(f_t,Q_t)$, $f_t=Fa_t$, $Q_t=FR_tF^{\top}+V$.
\emph{Update} via Bayes: $p(\theta_t\mid y_{1:t})\propto p(y_t\mid\theta_t)p(\theta_t\mid y_{1:t-1})$;
Gaussian $\times$ Gaussian $=$ Gaussian. The joint $(\theta_t,Y_t)\mid y_{1:t-1}$ is Gaussian
with cross-covariance $R_tF^{\top}$; conditioning on $Y_t=y_t$ gives
$$
\boxed{\;\theta_t\mid y_{1:t}\sim\mathcal{N}(m_t,C_t),\quad
m_t=a_t+K_t(y_t-f_t),\;C_t=R_t-K_tQ_tK_t^{\top},\;K_t=R_tF^{\top} Q_t^{-1}.\;}
$$""",
    "is_exam": True,
    "topic_hint": "t8b",
    "images": []
}

past_exams_ts["exam_jun_2025_q4"] = {
    "title": 'Jun 2025 — Q4',
    "content": r"""<span class="exam-question-text">Consider a time series $\{Y_t=(Y_{1t},\dots,Y_{mt})'\}_{t\ge 1}$, where for $j=1,\dots,m$,
$Y_{jt}$ represents the price of financial asset $j$ in a portfolio at time $t$.
**(a)** Write the general expression of a DLM for the time series $\{Y_t\}_{t\ge 1}$.

**(b)** Now let $m=2$. Suppose one models the $\{Y_t\}_{t\ge 1}$, $2\times 1$, as independent
        random walks plus noise (the random walk would be the assumption in perfect markets).
        What would be the system and covariance matrices of the DLM above?

**(c)** How could you instead introduce a dependence between the two latent random walks (the
        ideal prices in perfect markets) in the model at point (b)?</span>

---

**Solution.** \textbf{(a)} Same as Q4(a) Exam 1.

\textbf{(b)} $\theta_{j,t}=\theta_{j,t-1}+w_{j,t}$, $Y_{j,t}=\theta_{j,t}+v_{j,t}$ for $j=1,2$:
$$
\boxed{\;F=I_2,\;G=I_2,\;V=\mathrm{diag}(\sigma_{v_1}^2,\sigma_{v_2}^2),\;
W=\mathrm{diag}(\sigma_{w_1}^2,\sigma_{w_2}^2).\;}
$$

\textbf{(c)} Three options.
(1) \emph{Correlated state noise:} make $W$ non-diagonal, $W_{12}=\rho_w\sigma_{w_1}\sigma_{w_2}$
--- contemporaneous correlation of increments.
(2) \emph{Common latent factor:} $\theta_t=Af_t$ with $f_t$ a 1-dim.\ RW
(common stochastic trend, factor / cointegration).
(3) \emph{Cross terms in $G$:} $G=\bigl(\begin{smallmatrix}1&\delta\\0&1\end{smallmatrix}\bigr)$
or full $G$ (states evolve as a VAR(1)).""",
    "is_exam": True,
    "topic_hint": "t7d",
    "images": []
}

past_exams_ts["exam_jun_2025_q5"] = {
    "title": 'Jun 2025 — Q5',
    "content": r"""<span class="exam-question-text">Let $\{Y_t\}$ be a univariate time series. Suppose we model $\{Y_t\}$ as a DLM, with no
unknown parameters. Consider the forecast errors
$$
e_t=Y_t-f_t,\qquad f_t=\mathbb{E}(Y_t\mid Y_{t-1}).
$$
Show that $\mathbb{E}(e_t)=0$.</span>

---

**Solution.** By definition of conditional expectation and the tower property:
$\mathbb{E}[e_t]=\mathbb{E}[Y_t-\mathbb{E}(Y_t\mid Y_{1:t-1})]=\mathbb{E}[Y_t]-\mathbb{E}[\mathbb{E}(Y_t\mid Y_{1:t-1})]=\mathbb{E}[Y_t]-\mathbb{E}[Y_t]=0$.
(One line: $\mathbb{E}[e_t\mid\mathcal F_{t-1}]=0$, so unconditionally $\mathbb{E}[e_t]=0$.)""",
    "is_exam": True,
    "topic_hint": "t11a",
    "images": []
}

past_exams_ts["exam_jun_2025_q6"] = {
    "title": 'Jun 2025 — Q6',
    "content": r"""<span class="exam-question-text">Consider a DLM for a univariate time series $\{Y_t\}_{t\ge 1}$, with unknown parameters. How
would you compute the predictive density
$$
p(y_{t+1}\mid y_{1:t})
$$
in a Bayesian approach? Write its expression and comment briefly.</span>

---

**Solution.** Put a prior $\pi(\phi)$. The predictive marginalises both the state \emph{and} the parameters:
$$
\boxed{\;p(y_{t+1}\mid y_{1:t})=\int p(y_{t+1}\mid y_{1:t},\phi)\,p(\phi\mid y_{1:t})\,d\phi.\;}
$$
For each fixed $\phi$, $p(y_{t+1}\mid y_{1:t},\phi)=\mathcal{N}(f_{t+1}(\phi),Q_{t+1}(\phi))$ from the KF.
$p(\phi\mid y_{1:t})\propto L(\phi)\pi(\phi)$ is explored by MCMC. The integral is a
mixture of Gaussians: properly inflated for parameter uncertainty (\emph{wider} than the
plug-in $\mathcal{N}(f_{t+1}(\widehat\phi),Q_{t+1}(\widehat\phi))$).""",
    "is_exam": True,
    "topic_hint": "t13b",
    "images": []
}

past_exams_ts["exam_may_2025_q1"] = {
    "title": 'May 2025 — Q1',
    "content": r"""<span class="exam-question-text">Consider a univariate time series $(Y_t)_{t\ge 1}$.
**(a)** The \emph{autocovariance function} (acf) can be defined only if the time series is stationary.
        \begin{itemize}[leftmargin=1.6em]

**(b)** [A.] \textbf{Yes}, because:

**(c)** [B.] \textbf{No}, because:
        \end{itemize}

**(d)** The correlogram can be used to estimate the acf only if the time series is stationary.
        \begin{itemize}[leftmargin=1.6em]

**(e)** [A.] \textbf{Yes}, because:

**(f)** [B.] \textbf{No}, because:
        \end{itemize}</span>

---

**Solution.** \textbf{(a) NO.} The ACF is defined as $\gamma(s,t)=\operatorname{Cov}(Y_s,Y_t)$ whenever
$\mathbb{E}[Y_t^2]<\infty$. Stationarity merely allows reducing it to a 1-arg function $\gamma(h)$
of the lag.

\textbf{(b) YES} (essentially). The correlogram
$\hat\gamma(h)=T^{-1}\sum_{t}(Y_t-\bar Y)(Y_{t+|h|}-\bar Y)$ \emph{pools} across $t$ to
estimate one function: meaningful only if the true ACVF depends solely on the lag. Without
(weak) stationarity + ergodicity it has no clean interpretation.""",
    "is_exam": True,
    "topic_hint": "t2b",
    "images": []
}

past_exams_ts["exam_may_2025_q2"] = {
    "title": 'May 2025 — Q2',
    "content": r"""<span class="exam-question-text">Consider a stochastic process $(Y_t)_{t\ge 1}$ starting at $Y_0=0$, where
$Y_t=\sum_{i=1}^t Z_i$ and $Z_i$ is i.i.d.\ with $\Prob(Z_i=-1)=p$ and $\Prob(Z_i=1)=1-p$. Is
$(Y_t)_{t\ge 1}$ a Markov process?</span>

---

**Solution.** \textbf{YES.} $Y_t=Y_{t-1}+Z_t$ with $Z_t$ independent of $Y_{0:t-1}$. Hence
$\Prob(Y_t=y_t\mid Y_{0:t-1})=\Prob(Z_t=y_t-Y_{t-1})=\Prob(Y_t\mid Y_{t-1})$
(Markov property; cf.\ Example 1 RW). \emph{It is also non-stationary} ($\operatorname{Var}(Y_t)=4p(1-p)\,t$).""",
    "is_exam": True,
    "topic_hint": "t3a",
    "images": []
}

past_exams_ts["exam_may_2025_q3"] = {
    "title": 'May 2025 — Q3',
    "content": r"""<span class="exam-question-text">Let's regard a text as a sequence of words written over time, and let $Y_t$ be the word
written at time $t$, $t=1,2,\dots$. For simplicity, imagine that the dictionary only includes
$M$ words.

Let us model $(Y_t)_{t\ge 1}$ as a hidden Markov model (HMM), with $k$ latent states
representing $k$ possible topics.
**(a)** Define the model, including stating the assumptions made.

**(b)** What are the unknown parameters of the model?

**(c)** Write the expression of their likelihood, given data $y_{1:t}$.</span>

---

**Solution.** \textbf{(a) HMM} (keydef \textbf{15b}, HMM variant).
Latent topic $(S_t)\in\{1,\dots,k\}$ homogeneous Markov, $S_0\sim\pi$, transition $\mathbf P=[p_{ij}]$.
Observed word $Y_t\in\{1,\dots,M\}$, emission
$e_{i,w}=\Prob(Y_t=w\mid S_t=i)$, $\sum_w e_{i,w}=1$. Conditional indep.\ of obs.\ given the state.

\textbf{(b)} $\phi=(\pi,\mathbf P,\mathbf E)$: $(k-1)+k(k-1)+k(M-1)$ free parameters.

\textbf{(c) Likelihood (forward algorithm).} Define $\alpha_t(i)=\Prob(Y_{1:t}=y_{1:t},S_t=i;\phi)$:
$\alpha_1(i)=\pi_i e_{i,y_1}$; $\alpha_t(j)=\bigl(\sum_i\alpha_{t-1}(i)p_{ij}\bigr)e_{j,y_t}$.
Then
$$
L(\phi;y_{1:t})=\sum_{i=1}^k\alpha_t(i),
$$
computed in $O(k^2 t)$ instead of $O(k^t)$. MLE by EM / Baum--Welch.""",
    "is_exam": True,
    "topic_hint": "t5a",
    "images": []
}

past_exams_ts["exam_may_2025_q4"] = {
    "title": 'May 2025 — Q4',
    "content": r"""<span class="exam-question-text">Consider a clinical trial where data $(x_t,Y_t)$, $t=1,\dots,n$ are collected over time, where
$x_t$ is the dose of drug given at time $t$ to a patient, and $Y_t$ is the response (say, toxicity).

A linear regression model $Y_t=\alpha+\beta x_t+\varepsilon_t$, $\varepsilon_t\overset{\text{iid}}{\sim}\mathcal{N}(0,\sigma^2)$
may fail to capture residual temporal dependence.
**(a)** Explain how you can use DLMs to extend the linear model by allowing time varying
        regression parameters, that may better capture non-linear behaviour. Assume $\sigma^2$
        is known.

**(b)** For inference, you would use the smoothing distribution $(\alpha_t,\beta_t)$.
        Explain: what is the smoothing distribution?

**(c)** How can you proceed if $\sigma^2$ is unknown?

**(d)** Suppose that the same experiment is repeated in another hospital (similar, but not
        identical, to the first one). Can you propose a DLM that allows for borrowing strength,
        by sharing information between the hospitals in the learning process?</span>

---

**Solution.** \textbf{(a)} State $\theta_t=(\alpha_t,\beta_t)^{\top}$; $F_t=(1,x_t)$:
$$
Y_t=F_t\theta_t+v_t,\;v_t\sim\mathcal{N}(0,\sigma^2);\quad
\theta_t=\theta_{t-1}+w_t,\;w_t\sim\Nd_2(0,W).
$$

\textbf{(b)} Smoothing distribution $=\pi(\theta_{0:n}\mid y_{1:n})$ (joint) or
$\pi(\theta_t\mid y_{1:n})=\Nd_2(s_t,S_t)$ (marginal at $t$, by RTS).
Pools \emph{past and future} data, more informative than filtering.

\textbf{(c) $\sigma^2$ unknown.} Either (i) MLE: maximise the prediction-error likelihood
$\ell(\sigma^2,W)$ (Exam 1 Q7a) and plug $\hat\sigma^2$ into the KF; or (ii) Bayesian
conjugate: $\sigma^2\sim\mathrm{IG}$, $W\sim\mathrm{IW}$, FFBS Gibbs on $(\theta_{0:n},\sigma^2,W)$.

\textbf{(d) Hierarchical DLM.} For hospitals $h=1,2$, one DLM each
$\theta_t^{(h)}$, tied via a shared population mean $\theta_0^{(h)}\sim\Nd_2(\mu,T)$,
hyperprior $\mu\sim\Nd_2(0,\Sigma_0)$. Inference shrinks each hospital's coefficient toward
$\mu$; degree of shrinkage learned from data.""",
    "is_exam": True,
    "topic_hint": "t7c",
    "images": []
}

past_exams_ts["exam_may_2025_q5"] = {
    "title": 'May 2025 — Q5',
    "content": r"""<span class="exam-question-text">**(a)** Define the random walk plus noise model.

**(b)** Given
        $$\theta_{t-1}\mid y_{1:t-1}\sim\Nd_p(m_{t-1},C_{t-1}),$$
        what are the steps of the Kalman filter recursions, i.e.\ to obtain that, when a new
        observation $y_t$ becomes available, the filtering distribution of $\theta_t$ is
        $$\theta_t\mid y_{1:t}\sim\Nd_p(m_t,C_t)\,?$$

**(c)** \textbf{Optional question.} Does the variance $C_t$ converge to zero as $t\to\infty$?
        \begin{itemize}[leftmargin=1.6em]

**(d)** [A.] \textbf{YES}, because (give intuitive explanation).

**(e)** [B.] \textbf{NO}, because (give intuitive explanation).
        \end{itemize}</span>

---

**Solution.** \textbf{(a)} $Y_t=\theta_t+v_t$, $\theta_t=\theta_{t-1}+w_t$, $v_t\overset{\text{iid}}{\sim}\mathcal{N}(0,V)$, $w_t\overset{\text{iid}}{\sim}\mathcal{N}(0,W)$,
$\theta_0\sim\mathcal{N}(m_0,C_0)$, indep. (DLMwR \S 2.3.2.)

\textbf{(b) KF steps.}
\emph{Predict}: $a_t=m_{t-1}$, $R_t=C_{t-1}+W$; $f_t=a_t$, $Q_t=R_t+V$.
\emph{Gain}: $K_t=R_t/Q_t$.
\emph{Update}: $m_t=a_t+K_t(y_t-f_t)$, $C_t=(1-K_t)R_t=V R_t/Q_t$.

\textbf{(c) NO} (unless $W=0$). The fixed point of $C^*=V(C^*+W)/(C^*+W+V)$ is
$C^*=\tfrac12(-W+\sqrt{W^2+4VW})>0$. The injection of fresh noise $W$ at every step prevents
$C_t\to 0$. (If $W=0$ the state is static and $C_t=VC_0/(V+tC_0)\to 0$ as $1/t$.)""",
    "is_exam": True,
    "topic_hint": "t8b",
    "images": []
}

past_exams_ts["exam_may_2025_q6"] = {
    "title": 'May 2025 — Q6',
    "content": r"""<span class="exam-question-text">Consider a DLM with unknown parameters $\phi$.
**(a)** How do you treat uncertainty on the unknown parameters in the Bayesian approach?
        Define a state-space process for an $m$-dimensional time series $(Y_t)_{t\ge 1}$.

**(b)** Write the expression of the filtering distribution of $\theta_t$ given $y_{1:t}$.
        Comment briefly.</span>

---

**Solution.** \textbf{(a)} Prior $\pi(\phi)$; $\theta_0\sim\Nd_p(m_0,C_0)$;
$Y_t=F_t\theta_t+v_t$, $v_t\sim\Nd_m(0,V_t(\phi))$;
$\theta_t=G_t\theta_{t-1}+w_t$, $w_t\sim\Nd_p(0,W_t(\phi))$.

\textbf{(b)} $p(\theta_t\mid y_{1:t})=\int p(\theta_t\mid y_{1:t},\phi)p(\phi\mid y_{1:t})d\phi$.
For each $\phi$ fixed, $p(\theta_t\mid y_{1:t},\phi)=\Nd_p(m_t(\phi),C_t(\phi))$ (KF). The
marginal is a \emph{mixture of Gaussians}, generally non-Gaussian. Approximated by MCMC
(draw $\phi^{(s)}$, run KF, average). Wider intervals than the plug-in at $\widehat\phi$
(honest uncertainty).""",
    "is_exam": True,
    "topic_hint": "t13b",
    "images": []
}

past_exams_ts["exam_jun_2024_q1"] = {
    "title": 'Jun 2024 — Q1',
    "content": r"""<span class="exam-question-text">Let $(Y_t)_{t\ge 1}$ be a univariate time series.
**(a)** When is it \emph{(weakly) stationary}?

**(b)** Provide an example of a stationary time series.</span>

---

**Solution.** \textbf{(a)} $\mathbb{E}[Y_t]$ and $\mathbb{E}[Y_tY_{t+h}]$ finite and independent of $t$, $\forall h$:
$\mu(t)=\mu$, $\sigma^2(t)=\sigma^2$, $\gamma(t,t+h)=\tilde\gamma(h)$.

\textbf{(b)} \emph{White noise} $Y_t\overset{\text{iid}}{\sim}\mathcal{N}(0,\sigma^2)$: $\mu=0$, $\gamma(h)=\sigma^2\mathbf{1}\{h=0\}$.
Or \emph{causal AR(1)} $Y_t=\phi Y_{t-1}+\varepsilon_t$, $|\phi|<1$, started in stationary
distribution: $\mu=0$, $\gamma(h)=\sigma^2\phi^{|h|}/(1-\phi^2)$ (Example 3.A; the
``$|\varphi|<1\Rightarrow$ stationary'' proof in the notes).""",
    "is_exam": True,
    "topic_hint": "t2a",
    "images": []
}

past_exams_ts["exam_jun_2024_q2"] = {
    "title": 'Jun 2024 — Q2',
    "content": r"""<span class="exam-question-text">Consider an autoregressive model of order 2, AR(2):
$$
Y_t=\alpha_1 Y_{t-1}+\alpha_2 Y_{t-2}+\varepsilon_t,\qquad \varepsilon_t\overset{\text{iid}}{\sim}\mathcal{N}(0,\sigma^2).
$$
An AR(2) process may be regarded as a special case of a DLM (i.e.\ we can find a DLM with the
same probability law as this AR(2) process).

Would the following be a valid DLM representation of the given AR(2) model?
$$
Y_t=\begin{bmatrix}Y_{t-1}\\Y_{t-2}\end{bmatrix}^{\top}\begin{bmatrix}\alpha_1\\\alpha_2\end{bmatrix}+\varepsilon_t,
\qquad \varepsilon_t\overset{\text{iid}}{\sim}\mathcal{N}(0,\sigma^2),
$$
$$
\alpha_{1,t}=\alpha_1,\qquad \alpha_{2,t}=\alpha_2.
$$
\begin{itemize}[leftmargin=1.6em]
  \item[A.] \textbf{Yes}, because:
  \item[B.] \textbf{No}, because:
\end{itemize}</span>

---

**Solution.** \textbf{NO.} In a DLM $\theta_t$ is the \emph{latent / unobserved} process with its own
dynamics. The proposal puts (constant) coefficients as a non-evolving state and \emph{past
observations} into $F_t$ --- so there is no state innovation $w_t$, and
$F_t=(Y_{t-1},Y_{t-2})$ depends on past data, breaking the conditional indep.\
$Y_t\perp Y_{1:t-1}\mid\theta_t$.

\textbf{Correct DLM (companion form, DLMwR \S 3.2.5).} $\theta_t=(Y_t,Y_{t-1})^{\top}$,
$G=\bigl(\begin{smallmatrix}\alpha_1&\alpha_2\\1&0\end{smallmatrix}\bigr)$, $F=(1,0)$,
$V=0$, $W=\bigl(\begin{smallmatrix}\sigma^2&0\\0&0\end{smallmatrix}\bigr)$.""",
    "is_exam": True,
    "topic_hint": "t7e",
    "images": []
}

past_exams_ts["exam_jun_2024_q3"] = {
    "title": 'Jun 2024 — Q3',
    "content": r"""<span class="exam-question-text">Consider a homogeneous Markov chain $(Y_t)_{t\ge 0}$ with state space $\mathcal Y=\{1,2,3\}$,
initial value $Y_0=1$, and transition matrix:
$$
\begin{array}{c|ccc}
t-1\backslash t & 1 & 2 & 3 \\\hline
1 & .6 & 0 & \square \\
2 & \square & .6 & .3 \\
3 & .3 & \square & .6
\end{array}
$$
**(a)** Complete the transition matrix above.

**(b)** This Markov chain is \emph{aperiodic}. As $n\to\infty$, does $\Prob(Y_n=j\mid Y_0=1)$
        converge for each $j\in\{1,2,3\}$?
        \begin{itemize}[leftmargin=1.6em]

**(c)** [A.] \textbf{Yes}, because:

**(d)** [B.] \textbf{No}, because:

**(e)** [C.] \textbf{Yes}, under the conditions:
        \end{itemize}

**(f)** And how about $\Prob(Y_n=j)$?</span>

---

**Solution.** \textbf{(a)} Rows sum to 1: $P=\bigl(\begin{smallmatrix}.6&0&.4\\.1&.6&.3\\.3&.1&.6\end{smallmatrix}\bigr)$.

\textbf{(b) YES.} $P$ is irreducible on $\{1,2,3\}$ ($1\to3\to2$, $2\to1$, $3\to1$ reach every
state) and aperiodic ($p_{11}>0$). By Theorem 2.1 (keydef \textbf{11d}), there is a unique
stationary $\pi$ and $\Prob(Y_n=j\mid Y_0=i)\to\pi_j$ for every $i,j$.
Solving $\pi P=\pi$: $\pi\approx(0.394,\,0.121,\,0.485)$.

\textbf{(c) YES.} $\Prob(Y_n=j)=\sum_i\nu_i\Prob(Y_n=j\mid Y_0=i)\to\pi_j$ regardless of the
initial law $\nu$.""",
    "is_exam": True,
    "topic_hint": "t3b",
    "images": []
}

past_exams_ts["exam_jun_2024_q4"] = {
    "title": 'Jun 2024 — Q4',
    "content": r"""<span class="exam-question-text">In a study, a start-up is monitored \emph{quarterly} from the first quarter of 2021 to the
first quarter of 2024, and its status is coded as $1=$``healthy'', $2=$``critical'', $3=$``failing''.
Let $Y_t$ be the status of the start-up at time $t$.
**(a)** For simplicity, let us model the time series $(Y_t)_{t\ge 0}$ as a homogeneous Markov
        chain. Set the first quarter of 2021 as $t=0$, and fix $Y_0=1$. The observed data are
        $(y_1,\dots,y_{13})=(2,1,1,1,2,3,3,2,1,1,2,3)$.. Before taking the sample, what was the
        probability of observing such a result?

**(b)** Let us now use a Hidden Markov Model (HMM) with two latent states. Set the first
        quarter of 2021 as $t=1$ and $y_1=1$.
        \begin{enumerate}[label=\roman*.,leftmargin=1.8em]

**(c)** Write the model in a clean and precise way. What are the unknown parameters of the model?

**(d)** Write the expression of the likelihood of the unknown parameters.

**(e)** Then explain how you would solve decoding.
\end{enumerate}</span>

---

**Solution.** \textbf{(a)} Transition counts in the path: $n_{11}=3,n_{12}=3,n_{21}=2,n_{23}=2,n_{32}=1,n_{33}=1$.
$\Prob(y_{1:12}\mid Y_0=1)=\prod p_{y_{t-1},y_t}=p_{11}^3 p_{12}^3 p_{21}^2 p_{23}^2 p_{32}p_{33}$.

\textbf{(b.i) HMM, $k=2$.} $(S_t)\in\{1,2\}$, $\pi\in\Delta^1$, $\mathbf P\in\mathbb{R}^{2\times 2}$;
emission over $\{1,2,3\}$, $\mathbf E\in\mathbb{R}^{2\times 3}$. Parameters $\phi=(\pi,\mathbf P,\mathbf E)$,
$1+2+4=7$ free.

\textbf{(b.ii) Likelihood.} Forward algorithm:
$\alpha_1(i)=\pi_i e_{i,y_1}$, $\alpha_t(j)=(\sum_i\alpha_{t-1}(i)p_{ij})e_{j,y_t}$;
$L(\phi)=\sum_i\alpha_T(i)$.

\textbf{(b.iii) Decoding.} \emph{Global:} Viterbi
$\hat s_{1:T}=\argmax\Prob(S_{1:T}=s_{1:T}\mid y_{1:T})$, via
$\delta_t(j)=[\max_i\delta_{t-1}(i)p_{ij}]e_{j,y_t}$ + back-pointers.
\emph{Local:} $\hat s_t=\argmax\gamma_t(s)=\argmax\Prob(S_t=s\mid y_{1:T})$ from forward--backward.""",
    "is_exam": True,
    "topic_hint": "t5b",
    "images": []
}

past_exams_ts["exam_jun_2024_q5"] = {
    "title": 'Jun 2024 — Q5',
    "content": r"""<span class="exam-question-text">Consider a random walk plus noise model (write it precisely, but without unnecessary details).
**(a)** What is the signal-to-noise ratio?

**(b)** Given data $y_{1:t}$, what is the forecast function? Write it, and then provide the
        expression of the $(1-\alpha)$ credible interval for $Y_{t+2}\mid y_1,\dots,y_t$.</span>

---

**Solution.** \textbf{(a)} $\kappa=W/V$.

\textbf{(b)} From filter, $\theta_t\mid y_{1:t}\sim\mathcal{N}(m_t,C_t)$. Iterate state $k$ times:
$\theta_{t+k}=\theta_t+\sum_{j=1}^k w_{t+j}$, $Y_{t+k}=\theta_{t+k}+v_{t+k}$. Hence
$$
\boxed{\;Y_{t+k}\mid y_{1:t}\sim\mathcal{N}(m_t,C_t+kW+V).\;}
$$
Forecast function is \emph{flat}: $\hat y_{t+k\mid t}=m_t$ for every $k$.
For $k=2$, CI: $m_t\pm z_{1-\alpha/2}\sqrt{C_t+2W+V}$.""",
    "is_exam": True,
    "topic_hint": "t10b",
    "images": []
}

past_exams_ts["exam_jun_2024_q6"] = {
    "title": 'Jun 2024 — Q6',
    "content": r"""<span class="exam-question-text">**(a)** Dynamic linear models (DLMs) can be only defined for univariate time series. Is that correct?
        \begin{itemize}[leftmargin=1.6em]

**(b)** [A.] \textbf{Yes}, because:

**(c)** [B.] \textbf{No}, because:
        \end{itemize}

**(d)** Now consider a DLM with state process $(\theta_t)_{t\ge 0}$. Given data $y_{1:t-1}$,
        we know that
        $$\theta_t\mid y_{1:t-1}\sim\mathcal{N}(a_t,R_t);$$
        explain how one obtains the one-step-ahead predictive distribution of $y_t$ given $y_{1:t-1}$.</span>

---

**Solution.** \textbf{(a) NO.} $Y_t\in\mathbb{R}^q$ general; $F_t$ is $q\times p$.

\textbf{(b)} $Y_t=F_t\theta_t+v_t$, $v_t\perp\theta_t$, $v_t\sim\Nd_q(0,V_t)$. Linear combo:
$\boxed{Y_t\mid y_{1:t-1}\sim\Nd_q(F_t a_t,\,F_t R_t F_t^{\top}+V_t)=\Nd_q(f_t,Q_t)}$.""",
    "is_exam": True,
    "topic_hint": "t10a",
    "images": []
}

past_exams_ts["exam_jun_2024_q7"] = {
    "title": 'Jun 2024 — Q7',
    "content": r"""<span class="exam-question-text">Consider a DLM with unknown parameters $\phi$.
**(a)** What is the expression of the joint density $p(y_1,\dots,y_n)$? Does it depend on $\phi$?

**(b)** In a Bayesian approach, the unknown parameters are treated differently. What is now
        the expression of $p(y_1,\dots,y_n)$?</span>

---

**Solution.** \textbf{(a)} Frequentist: $p(y_{1:n}\mid\phi)=\prod_t\Nd_q(y_t;f_t(\phi),Q_t(\phi))$
(prediction-error decomposition, DLMwR eq.\ 4.1). \emph{Yes, depends on $\phi$.}

\textbf{(b)} Bayesian: prior predictive (evidence)
$$
p(y_{1:n})=\int p(y_{1:n}\mid\phi)\pi(\phi)d\phi,
$$
does \emph{not} depend on $\phi$ (integrated out). Typically intractable; used for Bayes
factors.""",
    "is_exam": True,
    "topic_hint": "t12a",
    "images": []
}

past_exams_ts["exam_may_2024_q1"] = {
    "title": 'May 2024 — Q1',
    "content": r"""<span class="exam-question-text">Consider a univariate stationary time series, $(Y_t)_{t\ge 1}$. Given data $(y_1,y_2,\dots,y_T)$
how can you estimate, non-parametrically, its mean function?</span>

---

**Solution.** By stationarity $\mu(t)\equiv\mu$. Use the sample mean
$\boxed{\hat\mu_T=\bar Y_T=T^{-1}\sum_t Y_t}$. Unbiased under stationarity
($\mathbb{E}[\bar Y_T]=\mu$). Consistent under stationarity + ergodicity (ergodic theorem).
Asymptotic variance involves the long-run variance $\sum_h\gamma(h)$ (use HAC for SE).""",
    "is_exam": True,
    "topic_hint": "t2c",
    "images": []
}

past_exams_ts["exam_may_2024_q2"] = {
    "title": 'May 2024 — Q2',
    "content": r"""<span class="exam-question-text">In a social survey, interviews have been conducted monthly, from January ($t=0$) to June
($t=5$) on a panel of $n=100$ individuals. Each individual is asked to express his/her
opinion on a certain social initiative ($1=$``in favour''; $2=$``negative''; $3=$``uncertain'').
The observed transition counts are reported in the following table.
$$
\begin{array}{c|ccc|c}
 & 1 & 2 & 3 & \\\hline
1 & 60 & 50 & 40 & 150 \\
2 & 75 & 120 & 55 & 250 \\
3 & 30 & 34 & 36 & 100
\end{array}
$$
The data are modelled as a sample from individual series $(Y_{k,t})_t$, $k=0,1,2,\dots,100$
assumed to be independent and identically distributed homogeneous Markov chains, with common
transition matrix $\mathbf P=[p_{i,j}]$. Your task is to estimate the transition probabilities
$p_{i,j}$ based on the available data.

The initial values $y_{k,0}$ were fixed for all individuals $k=0,1,2,\dots,100$ (so that you do
not have to estimate the initial probabilities).
**(a)** Write the expression of the likelihood of the $[p_{i,j}]_{i,j=1,2,3}$

**(b)** Explain how you get their maximum likelihood estimates (MLE).

**(c)** What is the estimated probability that an individual who is uncertain in May will
        become in favor in June? Provide the MLE, together with the asymptotic confidence
        interval of level 90\%. (\emph{The 0.9 quantile of a standard Gaussian distribution is
        1.28, the 0.95 quantile is 1.65, the 0.975 quantile is 1.96}).</span>

---

**Solution.** \textbf{(a)} $L(\mathbf P)=\prod_{ij}p_{ij}^{n_{ij}}$ (conditioned on fixed $y_{i,0}$).

\textbf{(b)} Row-by-row Lagrangian gives $\widehat p_{ij}=n_{ij}/n_{i,+}$.

\textbf{(c)} Required: $\widehat p_{3,1}=30/100=0.30$. Wald 90\% CI:
$0.30\pm 1.65\sqrt{0.30\cdot 0.70/100}=0.30\pm 0.076$, i.e.\ $\boxed{[0.224,\,0.376]}$.""",
    "is_exam": True,
    "topic_hint": "t4a",
    "images": []
}

past_exams_ts["exam_may_2024_q3"] = {
    "title": 'May 2024 — Q3',
    "content": r"""<span class="exam-question-text">Consider a random walk plus noise with observation equation
$$
T_t=\theta_t+v_t
$$
**(a)** Complete the model with the assumptions needed.

**(b)** Prove that $v_t$ and $(\theta_1,\dots,\theta_t)$ are independent.</span>

---

**Solution.** \textbf{(a)} $\theta_t=\theta_{t-1}+w_t$, $v_t\overset{\text{iid}}{\sim}\mathcal{N}(0,V)$, $w_t\overset{\text{iid}}{\sim}\mathcal{N}(0,W)$,
$\theta_0\sim\mathcal{N}(m_0,C_0)$, all mutually independent.

\textbf{(b)} $\theta_s=\theta_0+\sum_{u=1}^s w_u$, so $(\theta_1,\dots,\theta_t)$ is a function
of $(\theta_0,w_1,\dots,w_t)$. By the mutual independence in (a),
$v_t\perp(\theta_0,w_1,\dots,w_t)$, hence $v_t\perp(\theta_1,\dots,\theta_t)$.""",
    "is_exam": True,
    "topic_hint": "t7a",
    "images": []
}

past_exams_ts["exam_may_2024_q4"] = {
    "title": 'May 2024 — Q4',
    "content": r"""<span class="exam-question-text">In state space models, with $(\theta_t)_{t\ge 0}$ denoting the latent state process, the
problem of filtering consists in computing the estimate $\mathbb{E}(\theta_i\mid y_{1:t})$ and
update it as new observations become available. Is that correct?
\begin{itemize}[leftmargin=1.6em]
  \item[A.] \textbf{Yes}, because:
  \item[B.] \textbf{No}, because:
  \item[C.] \textbf{Yes}, under the conditions:
\end{itemize}</span>

---

**Solution.** \textbf{NO}: filtering targets the conditional \emph{distribution} $\pi(\theta_t\mid y_{1:t})$,
not just a point estimate; and it concerns $\theta_t$ specifically (current state), not
$\theta_i$ for arbitrary $i$ (different problem: predicting $i>t$ or smoothing $i<t$).""",
    "is_exam": True,
    "topic_hint": "t8a",
    "images": []
}

past_exams_ts["exam_may_2024_q5"] = {
    "title": 'May 2024 — Q5',
    "content": r"""<span class="exam-question-text">**(a)** Write the general expression of a dynamic linear model (DLM) for a multivariate time series.

**(b)** Given $\theta_{t-1}\mid y_{1:t-1}\sim\mathcal{N}(m_{t-1},C_{t-1})$, explain how you obtain the
        one-step-ahead state predictive distribution of $\theta_t$ given $y_{1:t-1}$.</span>

---

**Solution.** \textbf{(a)} As Exam 1 Q4a (keydef \textbf{16a}).

\textbf{(b)} $\theta_t=G_t\theta_{t-1}+w_t$, $w_t\perp\theta_{t-1}\mid y_{1:t-1}$. So
$$
\boxed{\;\theta_t\mid y_{1:t-1}\sim\Nd_p(a_t,R_t),\quad a_t=G_t m_{t-1},\;R_t=G_t C_{t-1}G_t^{\top}+W_t.\;}
$$""",
    "is_exam": True,
    "topic_hint": "t6a",
    "images": []
}

past_exams_ts["exam_may_2024_q6"] = {
    "title": 'May 2024 — Q6',
    "content": r"""<span class="exam-question-text">Consider a DLM with no unknown parameters. Let $f_t=\mathbb{E}[Y_t\mid y_{1:t-1}]$ be the point
forecasts of $Y_t$ given $y_{1:t-1}$, and let $e_t=Y_t-f_t$ be the forecast error (or innovation).
Suppose $e_t\overset{\text{iid}}{\sim}\mathcal{N}(0,1)$. Is that correct?
\begin{itemize}[leftmargin=1.6em]
  \item[A.] \textbf{Yes}, because:
  \item[B.] \textbf{No}, because:
  \item[C.] \textbf{Yes}, under the conditions:
\end{itemize}</span>

---

**Solution.** \textbf{NO.} The innovations are: zero-mean ($\mathbb{E} e_t=0$), \emph{uncorrelated} across $t$
(orthogonality), Gaussian \emph{conditionally on $y_{1:t-1}$} with variance $Q_t$ \emph{depending
on $t$}. So $e_t\sim\mathcal{N}(0,Q_t)$, not unit variance. \emph{Standardised innovations}
$\tilde e_t=Q_t^{-1/2}e_t\overset{\text{iid}}{\sim}\mathcal{N}(0,1)$ --- that is the object used in model checking
(QQ-plot, Ljung--Box on $\tilde e_t$; see keydef on model checking).""",
    "is_exam": True,
    "topic_hint": "t11a",
    "images": []
}

past_exams_ts["exam_may_2024_q7"] = {
    "title": 'May 2024 — Q7',
    "content": r"""<span class="exam-question-text">Finally, consider a DLM with unknown parameters $\phi$.
**(a)** Given data $(y_1,\dots,y_n)$, write the expression of the likelihood of $\phi$ and
        explain how you would compute the maximum likelihood estimator $\hat\phi$.

**(b)** Now suppose that, at time $t-1$, you have data $y_{1:t-1}$ and have already computed
        the MLE $\hat\phi$. How would you proceed to compute the predictive distribution of
        $Y_t$ given $y_{1:t-1}$?

**(c)** How would you instead proceed, in the Bayesian approach, for inference on $\phi$ and
        for computing the predictive distribution $p(Y_t\mid y_{1:t-1})$?</span>

---

**Solution.** \textbf{(a)} Prediction-error decomposition (DLMwR \S 4.1):
$L(\phi)=\prod_t\Nd_q(y_t;f_t(\phi),Q_t(\phi))$.
$\widehat\phi=\argmax_\phi\ell(\phi)$ by numerical optimization (BFGS), each evaluation = one
KF pass.

\textbf{(b)} Run KF at $\widehat\phi$; one-step ahead
$Y_t\mid y_{1:t-1}\sim\Nd_q(f_t(\widehat\phi),Q_t(\widehat\phi))$. \emph{Ignores parameter
uncertainty}; intervals too narrow.

\textbf{(c)} Bayesian: prior $\pi(\phi)$, posterior by MCMC (Gibbs / FFBS), predictive
$p(y_t\mid y_{1:t-1})=\int p(y_t\mid y_{1:t-1},\phi)p(\phi\mid y_{1:t-1})d\phi$ ---
mixture of Gaussians, properly inflated for parameter uncertainty.""",
    "is_exam": True,
    "topic_hint": "t12a",
    "images": []
}

past_exams_ts["exam_may_2023_q1"] = {
    "title": 'May 2023 — Q1',
    "content": r"""<span class="exam-question-text">What is a time series? An answer could be: ``A time series is a sequence of observations taken
over time.'' Are you happy with this definition?
\begin{itemize}[leftmargin=1.6em]
  \item[A.] \textbf{Yes}, because:
  \item[B.] \textbf{No}, because:
  \item[C.] \textbf{Yes}, under the conditions:
\end{itemize}</span>

---

**Solution.** \textbf{NO} (incomplete). A time series is the \emph{realization} of a stochastic process
$(Y_t)_{t\in T}$ (keydef \textbf{1}--\textbf{2}). The bare ``sequence of observations'' misses
the probabilistic model that gives meaning to mean, variance, ACVF, stationarity, inference.""",
    "is_exam": True,
    "topic_hint": "t1a",
    "images": []
}

past_exams_ts["exam_may_2023_q2"] = {
    "title": 'May 2023 — Q2',
    "content": r"""<span class="exam-question-text">Let $(Y_t)_{t\ge 0}$ be a Markov chain with state space $\mathcal Y=\{1,2,3\}$ and suppose
that each row of the transition matrix is uniform. Write the transition matrix and compute
$\Prob(Y_2=2\mid Y_0=1)$.</span>

---

**Solution.** $\mathbf P=\tfrac13\mathbf 1\mathbf 1^{\top}$ (all entries $1/3$). Then $\mathbf P^k=\mathbf P$ for $k\ge 1$,
so $\Prob(Y_2=2\mid Y_0=1)=(\mathbf P^2)_{12}=1/3$.""",
    "is_exam": True,
    "topic_hint": "t3b",
    "images": []
}

past_exams_ts["exam_may_2023_q3"] = {
    "title": 'May 2023 — Q3',
    "content": r"""<span class="exam-question-text">In finance, returns often show ``volatility clusters''. Thus, an interesting model for returns
is a Hidden Markov Model (HMM) with $k$ latent states and a Gaussian emission distribution
$\mathcal{N}(0,\sigma^2)$, where $\sigma^2$ is state-dependent.
**(a)** Write formally the expression of such a model, assuming $k=3$ and that the latent
        state at time zero is $S_0=1$.

**(b)** What are the unknown parameters to be estimated?</span>

---

**Solution.** \textbf{(a)} $S_0=1$;
$S_t\mid S_{t-1}=i\sim\mathrm{Cat}(p_{i,1},p_{i,2},p_{i,3})$;
$Y_t\mid S_t=i\sim\mathcal{N}(0,\sigma_i^2)$, $i=1,2,3$.

\textbf{(b)} $\phi=(\mathbf P,\sigma_1^2,\sigma_2^2,\sigma_3^2)$, $6+3=9$ free
parameters. ($\pi$ degenerate at 1.) Identification up to relabelling of states (enforce
$\sigma_1^2<\sigma_2^2<\sigma_3^2$).""",
    "is_exam": True,
    "topic_hint": "t5a",
    "images": []
}

past_exams_ts["exam_may_2023_q4"] = {
    "title": 'May 2023 — Q4',
    "content": r"""<span class="exam-question-text">**(a)** Define precisely a random-walk-plus-noise model.

**(b)** Then prove that $w_t$ and $(\theta_1,\dots,\theta_{t-1})$ are independent.</span>

---

**Solution.** \textbf{(a)} As Exam 5 Q3a.

\textbf{(b)} $\theta_s=\theta_0+\sum_{u=1}^s w_u$ for $s<t$, so $(\theta_1,\dots,\theta_{t-1})$
is a function of $(\theta_0,w_1,\dots,w_{t-1})$. By the mutual indep.\ assumption,
$w_t\perp(\theta_0,w_1,\dots,w_{t-1})$, hence $w_t\perp(\theta_1,\dots,\theta_{t-1})$.""",
    "is_exam": True,
    "topic_hint": "t7a",
    "images": []
}

past_exams_ts["exam_may_2023_q5"] = {
    "title": 'May 2023 — Q5',
    "content": r"""<span class="exam-question-text">Using the DAG representation of the conditional-independence structure of a DLM, prove the
following
$$
\theta_t\perp\!\!\!\perp (Y_{t+1},\dots,Y_T)\mid\theta_{t+1}
$$
where the symbol $\perp\!\!\!\perp$ denotes (conditional) independence.
(\emph{At most 2--3 lines should be enough --- be clear in motivating, and to the point}).</span>

---

**Solution.** DLM DAG: $\theta_0\to\theta_1\to\cdots\to\theta_T$ with $\theta_s\to Y_s$ at each $s$. Every
directed path from $\theta_t$ to any $Y_s$ ($s\ge t+1$) passes through $\theta_{t+1}$, which
is a serial/chain node. Conditioning on $\theta_{t+1}$ blocks every such path; by $d$-separation,
$\theta_t\perp(Y_{t+1},\dots,Y_T)\mid\theta_{t+1}$. (This is the backward-Markov property used
in the RTS smoother.)""",
    "is_exam": True,
    "topic_hint": "t9a",
    "images": []
}

past_exams_ts["exam_may_2023_q6"] = {
    "title": 'May 2023 — Q6',
    "content": r"""<span class="exam-question-text">**(a)** Write the general expression of a DLM for a multivariate time series.

**(b)** The Kalman filter provides recursive formulae to compute the filtering estimates
        $\mathbb{E}(\theta_t\mid y_{1:t})$ and the covariance matrices $V(\theta_t\mid y_{1:t})$.
        Are these enough to quantify uncertainty on $\theta_t$ given $y_{1:t}$ through
        credible intervals?</span>

---

**Solution.** \textbf{(a)} As Exam 1 Q4a.

\textbf{(b) YES} in a DLM. By induction, $\theta_t\mid y_{1:t}$ is Gaussian (KF preserves
Gaussianity); $(m_t,C_t)$ then fully characterise the distribution. Marginal CI for component $j$:
$(m_t)_j\pm z_{1-\alpha/2}\sqrt{(C_t)_{jj}}$; joint ellipsoid via $C_t^{-1}$.
\emph{Caveat.} Fails for non-Gaussian SSMs (need particle filter) and for plug-in at $\widehat\phi$
(parameter uncertainty understated).""",
    "is_exam": True,
    "topic_hint": "t8a",
    "images": []
}

past_exams_ts["exam_jun_2022_q1"] = {
    "title": 'Jun 2022 — Q1',
    "content": r"""<span class="exam-question-text">Let $(Y_t)_{t\ge 1}$ be a univariate time series with finite expectations $\mathbb{E}[Y_t]$. Then,
given observations $Y_{1:n}$, we can estimate $\mathbb{E}[Y_n]$ by the sample mean
$$
\bar Y_n=\frac{1}{n}\sum_{t=1}^n Y_t
$$
Is it an appropriate estimator?
\begin{itemize}[leftmargin=1.6em]
  \item[A.] \textbf{Yes}, because:
  \item[B.] \textbf{No}, because:
  \item[C.] \textbf{Yes}, under the conditions:
\end{itemize}</span>

---

**Solution.** \textbf{YES, under conditions}: \emph{stationarity} (so $\mathbb{E}[Y_t]=\mu$ is constant and pooling
across $t$ is meaningful) + \emph{ergodicity} (so $\bar Y_n\asto\mu$). Without stationarity
$\bar Y_n$ targets a mixture $\bar\mu_n=n^{-1}\sum_t\mathbb{E}[Y_t]$, generically different from
$\mathbb{E}[Y_n]$.""",
    "is_exam": True,
    "topic_hint": "t2c",
    "images": []
}

past_exams_ts["exam_jun_2022_q2"] = {
    "title": 'Jun 2022 — Q2',
    "content": r"""<span class="exam-question-text">Consider the filtering problem for a dynamic linear model (DLM). The Kalman filter provides,
recursively, the mean and covariance matrix of the filtering distribution. These two
quantities are enough for characterizing the filtering distribution: is that correct?
\begin{itemize}[leftmargin=1.6em]
  \item[A.] \textbf{Yes}, because:
  \item[B.] \textbf{No}, because:
\end{itemize}</span>

---

**Solution.** \textbf{YES} in a DLM: the filtering distribution is Gaussian (by induction), so $(m_t,C_t)$
suffice. \emph{Not} in general SSMs (non-Gaussian / non-linear): the distribution can be
multimodal/skewed; need particle filters.""",
    "is_exam": True,
    "topic_hint": "t8a",
    "images": []
}

past_exams_ts["exam_jun_2022_q3"] = {
    "title": 'Jun 2022 — Q3',
    "content": r"""<span class="exam-question-text">The problem of smoothing in a state-space model with observation process $(Y_t)_{t\ge 1}$ and
state process $(\theta_t)_{t\ge 0}$ consists in providing the point estimates
$\mathbb{E}(\theta_t\mid y_{1:T})$ for $t\le T$. Is that correct?
\begin{itemize}[leftmargin=1.6em]
  \item[A.] \textbf{Yes}, because:
  \item[B.] \textbf{No}, because:
\end{itemize}</span>

---

**Solution.** \textbf{NO}: same point as for filtering. Smoothing targets the \emph{distribution}
$\pi(\theta_t\mid y_{1:T})$ (or joint $\pi(\theta_{0:T}\mid y_{1:T})$); the mean is just one
summary. In a DLM, $(s_t,S_t)$ from RTS together describe the (Gaussian) marginal smoothing
distribution.""",
    "is_exam": True,
    "topic_hint": "t9a",
    "images": []
}

past_exams_ts["exam_jun_2022_q4"] = {
    "title": 'Jun 2022 — Q4',
    "content": r"""<span class="exam-question-text">**(a)** Define precisely a random walk plus noise model.

**(b)** The one-step-ahead predictive distribution of $Y_t$, given the observations $y_{1:t-1}$, is
        $$\mathcal{N}(f_t,Q_t).$$
        Explain how you obtain the expression of $f_t$ and $Q_t$.

**(c)** What is the point forecast of $Y_t$ given $y_{1:t-1}$ with respect to:
        \begin{enumerate}[label=\roman*.,leftmargin=1.8em]

**(d)** Quadratic loss?

**(e)** Absolute loss $L(y_t,\hat y_t)=|y_t-\hat y_t|$?
\end{enumerate}</span>

---

**Solution.** \textbf{(a)} As Exam 5 Q3a.

\textbf{(b)} KF predict step: $a_t=m_{t-1}$, $R_t=C_{t-1}+W$;
$f_t=a_t=m_{t-1}$, $Q_t=R_t+V=C_{t-1}+W+V$.

\textbf{(c.i) Quadratic loss.} Bayes estimator = conditional mean:
$\hat y_t=\mathbb{E}[Y_t\mid y_{1:t-1}]=f_t=m_{t-1}$.

\textbf{(c.ii) Absolute loss.} Bayes estimator = conditional median.
$Y_t\mid y_{1:t-1}\sim\mathcal{N}(f_t,Q_t)$ symmetric $\Rightarrow$ median $=$ mean,
so $\hat y_t=f_t=m_{t-1}$ as well. (Differ in non-Gaussian / asymmetric predictives.)""",
    "is_exam": True,
    "topic_hint": "t10b",
    "images": []
}

past_exams_ts["exam_jun_2022_q5"] = {
    "title": 'Jun 2022 — Q5',
    "content": r"""<span class="exam-question-text">Suppose that a time series $(Y_t)_{t\ge 1}$ is described as a Hidden Markov Model (HMM) with
latent state process $(S_t)_{t\ge 0}$ taking values in $\{1,2,3\}$, starting at $S_0=1$.
Write the expression of the joint probability
$$
\Prob(S_1=1,S_2=1,S_3=2\mid S_0=1).
$$</span>

---

**Solution.** Only the latent chain enters (no $Y$). By the Markov property,
$\Prob(\cdot\mid S_0=1)=p_{11}\cdot p_{11}\cdot p_{12}=p_{11}^2 p_{12}$.""",
    "is_exam": True,
    "topic_hint": "t3b",
    "images": []
}

past_exams_ts["exam_jun_2022_q6"] = {
    "title": 'Jun 2022 — Q6',
    "content": r"""<span class="exam-question-text">In a pharmaceutical trial on mice, the researcher gives an increasing dose $x_t$ of a drug to
a mouse over time, and observes the corresponding response $y_t$, for $t=1,\dots,100$. From
past experiments, the researcher knows that a linear regression model
$$
y_t=\beta_0+\beta_1 x_t+\varepsilon_t,
$$
under the standard assumptions on the error terms, would not capture the non-linear relationship
over time. An appropriate state-space model might provide a more flexible description.
Which state-space model could you suggest? Write your proposed model and motivate briefly.</span>

---

**Solution.** \emph{Time-varying-coefficients DLM:}
$Y_t=\alpha_t+\beta_t x_t+v_t$; $(\alpha_t,\beta_t)^{\top}=(\alpha_{t-1},\beta_{t-1})^{\top}+w_t$,
$w_t\sim\Nd_2(0,W)$.
State $\theta_t=(\alpha_t,\beta_t)^{\top}$, $F_t=(1,x_t)$, $G=I_2$. \emph{Motivation:} no
parametric non-linear form; coefficients drift smoothly (RW), with $W$ controlling adaptation
speed.""",
    "is_exam": True,
    "topic_hint": "t7c",
    "images": []
}

past_exams_ts["exam_jun_2022_q7"] = {
    "title": 'Jun 2022 — Q7',
    "content": r"""<span class="exam-question-text">Consider a random walk plus noise model, where the variance $\sigma_w^2$ of the evolution
error $w_t$ is \emph{zero}. Given the initial distribution $\mathcal{N}(m_0,C_0)$, and observations
$$
y_{1:n}\equiv(y_1,\dots,y_n),
$$
use Bayes' rule to obtain the conditional distribution of $\theta_n\mid y_{1:n}$.</span>

---

**Solution.** $W=0\Rightarrow\theta_t=\theta_0=:\theta$ constant. Conjugate static-$\theta$ model
(Case A; ``Posterior mean and precision''):
$$
\boxed{\;\theta_n\mid y_{1:n}\sim\mathcal{N}(m_n,C_n),\quad
\tfrac{1}{C_n}=\tfrac{1}{C_0}+\tfrac{n}{V},\quad
m_n=C_n\bigl(\tfrac{m_0}{C_0}+\tfrac{n\bar y_n}{V}\bigr).\;}
$$
(Closed form: $m_n=(Vm_0+nC_0\bar y_n)/(V+nC_0)$, $C_n=C_0V/(V+nC_0)$.)""",
    "is_exam": True,
    "topic_hint": "t13a",
    "images": []
}

past_exams_ts["exam_may_2022_q1"] = {
    "title": 'May 2022 — Q1',
    "content": r"""<span class="exam-question-text">Consider a univariate time series $(Y_t)_{t\ge 1}$.

Simple exponential smoothing is a simple and clever algorithm that recursively provides
one-step-ahead point forecasts $\hat y_{t+1\mid t}$.

Does it provide any quantification of the uncertainty about the point forecasts?
\begin{itemize}[leftmargin=1.6em]
  \item[A.] \textbf{Yes}, because:
  \item[B.] \textbf{No}, because:
\end{itemize}</span>

---

**Solution.** \textbf{NO}, as a stand-alone recursion --- it returns only a point forecast.
\emph{However}, SES is the steady-state KF of the local-level DLM (Case 0 in the
forecasting-algorithms note), so embedding SES in that DLM gives
$Y_{t+1}\mid y_{1:t}\sim\mathcal{N}(\hat y_{t+1\mid t},Q_{t+1})$ with $Q_{t+1}=C_t+W+V$. The DLM
\emph{has} the intervals; the algorithm does not.""",
    "is_exam": True,
    "topic_hint": "t10b",
    "images": []
}

past_exams_ts["exam_may_2022_q2"] = {
    "title": 'May 2022 — Q2',
    "content": r"""<span class="exam-question-text">Let $(Y_t)_{t\ge 1}$ be a univariate time series.

We can define the autocovariance function only if $(Y_t)$ is stationary. Is that correct?
\begin{itemize}[leftmargin=1.6em]
  \item[A.] \textbf{Yes}, because:
  \item[B.] \textbf{No}, because:
\end{itemize}</span>

---

**Solution.** \textbf{NO}, same as Exam 3 Q1a. ACVF $\gamma(s,t)=\operatorname{Cov}(Y_s,Y_t)$ is defined whenever
$\mathbb{E}[Y_t^2]<\infty$.""",
    "is_exam": True,
    "topic_hint": "t2b",
    "images": []
}

past_exams_ts["exam_may_2022_q3"] = {
    "title": 'May 2022 — Q3',
    "content": r"""<span class="exam-question-text">Consider a homogeneous Markov chain $(Y_t)_{t\ge 0}$ with state space $\mathcal Y=\{1,2,3\}$,
initial value $Y_0=1$ and transition matrix
$$
\begin{array}{c|ccc}
t-1\backslash t & 1 & 2 & 3 \\\hline
1 & .6 & 0 & .4 \\
2 & .1 & .6 & .3 \\
3 & .3 & .1 & .6
\end{array}
$$
This Markov chain is aperiodic. For $n\to\infty$, does $\Prob(Y_n=j\mid Y_0=1)$ converge, for
any $j=1,2,3$?
\begin{itemize}[leftmargin=1.6em]
  \item[A.] \textbf{Yes}, because:
  \item[B.] \textbf{No}, because:
  \item[C.] \textbf{Yes}, under the conditions:
\end{itemize}</span>

---

**Solution.** \textbf{YES.} Identical to Exam 4 Q3b: irreducible + aperiodic on a finite state space $\Rightarrow$
ergodic (Theorem 2.1); $\Prob(Y_n=j\mid Y_0=1)\to\pi_j$, with $\pi\approx(0.394,0.121,0.485)$.""",
    "is_exam": True,
    "topic_hint": "t3b",
    "images": []
}

past_exams_ts["exam_may_2022_q4"] = {
    "title": 'May 2022 — Q4',
    "content": r"""<span class="exam-question-text">Let $(Y_t)_{t\ge 1}$ be a univariate time series describing the monthly returns of a financial
asset. Consider the model
$$
\begin{cases}
Y_t=\exp\!\bigl(\tfrac12\theta_t\bigr)v_t & v_t\sim\mathcal{N}(0,1)\\
\theta_t=\alpha_1+\alpha_2\theta_{t-1}+w_t & w_t\sim\mathcal{N}(0,\sigma^2)
\end{cases}
$$
where $(\theta_t)$ is a non-observable AR(1) process.

Motivate (briefly but clearly) your answers.
**(a)** Is this model a state-space process?

**(b)** Is it a DLM?</span>

---

**Solution.** \textbf{(a) YES, SSM.} $(\theta_t)$ is a Markov chain (Gaussian AR(1)) and
$Y_t\mid\theta_t\sim\mathcal{N}(0,e^{\theta_t})$ depends only on $\theta_t$ (conditional indep.\ of
obs.).

\textbf{(b) NO, not a DLM.} The observation equation is non-linear in $\theta_t$ (multiplicative
$e^{\theta_t/2}$); variance depends on the state. Outside the linear-Gaussian template, KF
does not apply. \emph{Standard linearisation:} $\log Y_t^2=\theta_t+\log v_t^2$ (with
$\log v_t^2\sim\log\chi^2_1$ non-Gaussian) --- then approximated by a Gaussian mixture for
MCMC.""",
    "is_exam": True,
    "topic_hint": "t6b",
    "images": []
}

past_exams_ts["exam_may_2022_q5"] = {
    "title": 'May 2022 — Q5',
    "content": r"""<span class="exam-question-text">In view of administrative elections in XXX, a study monitors a panel of $n=100$ individuals;
each individual is periodically asked to express his/her vote intention ($1=$``candidate A'';
$2=$``candidate B''; $3=$``undecided''). The interviews are taken monthly, from January
($t=0$) to May ($t=4$), before the elections in June. The observed transition counts are
$$
\begin{array}{c|ccc|c}
 & 1 & 2 & 3 & \sum\\\hline
1 & 80 & 20 & 30 & 130 \\
2 & 15 & 50 & 35 & 100 \\
3 & 30 & 40 & 100 & 170
\end{array}
$$
To start with, we can model the individual time series $(Y_{k,t})$, $k=1,\dots,100$, as
independent and identically distributed homogeneous Markov chains with common transition
matrix $\mathbf P=[p_{ij}]$.
**(a)** Consider the probability referring to individual 1
        $$\Prob(Y_{1,1}=3,Y_{1,2}=3,\,,Y_{1,3}=3,\,,Y_{1,4}=1\mid Y_{1,0}=1).$$
        Is it known, or is it to be estimated?

**(b)** What is the maximum-likelihood estimate of the probability that an individual who is
        \emph{undecided} in May will vote for candidate A in June?

        Also give a quantification of the uncertainty by providing the asymptotic 95\%
        confidence interval. (\emph{The 0.9 quantile of a standard Gaussian distribution is
        1.28, the 0.95 quantile is 1.65, the 0.975 quantile is 1.96.})

        What properties of the MLE are you using?</span>

---

**Solution.** \textbf{(a)} \emph{Estimated:} the formula
$=p_{13}p_{33}^2 p_{31}$ is known, but its numerical value depends on the unknown $\mathbf P$.

\textbf{(b)} $\widehat p_{3,1}=30/170\approx 0.176$. 95\% CI:
$0.176\pm 1.96\sqrt{0.176\cdot 0.824/170}=0.176\pm 0.057=\boxed{[0.119,0.234]}$.
\emph{MLE properties used:} consistency (plug-in $\widehat p_{3,1}$ in the SE is valid by
Slutsky) and asymptotic normality (Anderson--Goodman, keydef \textbf{13}).""",
    "is_exam": True,
    "topic_hint": "t4b",
    "images": []
}

past_exams_ts["exam_may_2022_q6"] = {
    "title": 'May 2022 — Q6',
    "content": r"""<span class="exam-question-text">In finance, returns are often seen to show ``volatility clusters''.

An interesting model for returns is therefore a Hidden Markov Model (HMM) with $k$ latent
states and a Gaussian emission distribution $\mathcal{N}(0,\sigma^2)$ where $\sigma^2$ is state dependent.
**(a)** Write formally the expression of such a model, assuming a uniform initial distribution
        and $k=3$.

**(b)** What are the unknown parameters to be estimated?</span>

---

**Solution.** \textbf{(a)} $\pi=(\tfrac13,\tfrac13,\tfrac13)$; $S_t\mid S_{t-1}=i\sim\mathrm{Cat}(p_{i,:})$;
$Y_t\mid S_t=i\sim\mathcal{N}(0,\sigma_i^2)$, $i=1,2,3$.

\textbf{(b)} $\phi=(\mathbf P,\sigma_1^2,\sigma_2^2,\sigma_3^2)$, $6+3=9$ free parameters.""",
    "is_exam": True,
    "topic_hint": "t5a",
    "images": []
}

past_exams_ts["exam_may_2022_q7"] = {
    "title": 'May 2022 — Q7',
    "content": r"""<span class="exam-question-text">Define a general DLM $((Y_t,\theta_t))_{t\ge 1}$ starting at $\theta_0\sim\Nd_p(m_0,C_0)$,
where $Y_t$ is $m$-dimensional and $\theta_t$ is $p$-dimensional.

What is the difference between filtering and smoothing?</span>

---

**Solution.** \emph{Filtering:} $\pi(\theta_t\mid y_{1:t})$ --- state given data \emph{up to now} (online).
\emph{Smoothing:} $\pi(\theta_{0:T}\mid y_{1:T})$ (joint) or $\pi(\theta_t\mid y_{1:T})$
(marginal) --- state given \emph{all} sampled data, including \emph{future} of $t$ (offline).
Smoothing is more informative: $\operatorname{Var}(\theta_t\mid y_{1:T})\preceq\operatorname{Var}(\theta_t\mid y_{1:t})$.
For DLM: filtering = forward KF; smoothing = RTS backward sweep.""",
    "is_exam": True,
    "topic_hint": "t9a",
    "images": []
}

past_exams_ts["exam_may_2022_q8"] = {
    "title": 'May 2022 — Q8',
    "content": r"""<span class="exam-question-text">Consider a DLM that includes a vector, say $\phi$, of unknown parameters.
**(a)** In the Bayesian approach, how would you proceed for inference on the unknown
        parameters $\phi$?

        And how would you compute the one-step-ahead predictive distribution $p(y_t\mid y_{1:t-1})$?

**(b)** Markov Chain Monte Carlo (MCMC) is typically used to approximate the conditional
        distribution $p(\theta_{0:T},\phi\mid y_{1:T})$. How is MCMC related to Markov chains?</span>

---

**Solution.** \textbf{(a)} Prior $\pi(\phi)$, posterior $p(\phi\mid y_{1:t-1})\propto L(\phi)\pi(\phi)$ via
MCMC. Predictive:
$$
p(y_t\mid y_{1:t-1})=\int p(y_t\mid y_{1:t-1},\phi)p(\phi\mid y_{1:t-1})d\phi
\approx\tfrac{1}{S}\sum_s\Nd_q(y_t;f_t(\phi^{(s)}),Q_t(\phi^{(s)})),
$$
mixture of Gaussians, properly inflated for $\phi$-uncertainty.

\textbf{(b)} MCMC constructs an ergodic Markov chain on the parameter/latent space whose
\emph{stationary distribution} is the target posterior. Detailed balance
$\pi(x)K(x,y)=\pi(y)K(y,x)$ ensures $\pi$-invariance; ergodicity (irreducibility + aperiodicity)
gives the LLN $\tfrac1S\sum_s h(X^{(s)})\to\Exp_\pi[h(X)]$ (Theorem 2.1 applied to the
sampler's chain). So MCMC \emph{is} a Markov chain, designed to converge to the right limit
distribution.""",
    "is_exam": True,
    "topic_hint": "t13b",
    "images": []
}

past_exams_ts["exam_may_2021_q1"] = {
    "title": 'May 2021 — Q1',
    "content": r"""<span class="exam-question-text">Let $(X_n)_{n\ge 0}$ be a stochastic process, with $X_n\in\{1,2,3\}$. Suppose that
$$
\Prob(X_4=1\mid X_3=3)=0.4\quad\text{and}\quad \Prob(X_2=1\mid X_1=3)=0.5.
$$
Could the process $(X_t)_{t\ge 0}$ be a homogeneous Markov chain?
\begin{itemize}[leftmargin=1.6em]
  \item[A.] \textbf{Yes}, under the following conditions:
  \item[B.] \textbf{No}, because:
\end{itemize}</span>

---

**Solution.** \textbf{NO.} Homogeneous $\Rightarrow$ $\Prob(X_{n+1}=j\mid X_n=i)=p_{ij}$ independent of $n$.
The hypothesis would force $p_{3,1}=0.4=0.5$, contradiction. (Could be inhomogeneous Markov,
or not Markov.)""",
    "is_exam": True,
    "topic_hint": "t3a",
    "images": []
}

past_exams_ts["exam_may_2021_q2"] = {
    "title": 'May 2021 — Q2',
    "content": r"""<span class="exam-question-text">Let $(Y_t)_{t\ge 1}$ be a univariate time series. We can define the autocovariance function
only if $(Y_t)$ is stationary. It that correct?
\begin{itemize}[leftmargin=1.6em]
  \item[A.] \textbf{Yes}, because:
  \item[B.] \textbf{No}, because:
\end{itemize}</span>

---

**Solution.** \textbf{NO}, see Exam 3 Q1a / Exam 8 Q2.""",
    "is_exam": True,
    "topic_hint": "t2b",
    "images": []
}

past_exams_ts["exam_may_2021_q3"] = {
    "title": 'May 2021 — Q3',
    "content": r"""<span class="exam-question-text">The plot below shows monthly measurements of the Co2 level in a certain area:
\begin{center}\emph{[plot: CO\textsubscript{2} 1960--2000, clear upward trend + annual seasonality]}\end{center}
Of course, we cannot assume a stationary model, as we clearly see a trend and a seasonal
component. In fact, a common approach, for example in ARMA modeling, is to remove them, and
model the detrended and deseasonalized data. Could we model the data without any preliminary
transformation, by using a dynamic linear model (DLM)?
\begin{itemize}[leftmargin=1.6em]
  \item[A.] \textbf{Yes}, because:
  \item[B.] \textbf{No}, because:
\end{itemize}</span>

---

**Solution.** \textbf{YES.} Use a \emph{structural} DLM = local linear trend + seasonal component (DLMwR
\S 3.2.2--3.2.3, Harvey's BSM):
$Y_t=\mu_t+\gamma_t+v_t$,
$\mu_t=\mu_{t-1}+\beta_{t-1}+w_{1,t}$, $\beta_t=\beta_{t-1}+w_{2,t}$,
$\gamma_t=-\sum_{j=1}^{s-1}\gamma_{t-j}+w_{3,t}$ ($s=12$).
The latent state carries the non-stationary components; KF + diffuse prior on
non-stationary parts handles them directly. (Recall: SSMs do not need stationarity.)""",
    "is_exam": True,
    "topic_hint": "t7b",
    "images": []
}

past_exams_ts["exam_may_2021_q4"] = {
    "title": 'May 2021 — Q4',
    "content": r"""<span class="exam-question-text">An information center on vaccinations records the number of calls received weekly. Let $Y_t$
be the number of calls received in week $t$, $t=1,\dots,T$. A statistical model for $Y_t$
might be a Poisson distribution with parameter $\lambda$, $\mathrm{Poisson}(\lambda)$. However,
we can envisage that the intensity $\lambda$ varies with time, depending on the state of the
contagion. Let us use a Hidden Markov model, with emission distribution $\mathrm{Poisson}(\lambda_t)$.
**(a)** Write precisely such a model, envisaging $k=3$ latent states.

**(b)** What are the unknown parameters of this model?

**(c)** In order to compute their maximum-likelihood estimates, based on $y_{1:T}$, we need
        the expression of the likelihood: write it.</span>

---

**Solution.** \textbf{(a)} $(S_t)\in\{1,2,3\}$ Markov, $S_1\sim\pi$, transition $\mathbf P$;
$Y_t\mid S_t=i\sim\mathrm{Poisson}(\lambda_i)$, $i=1,2,3$.

\textbf{(b)} $\phi=(\pi,\mathbf P,\lambda_1,\lambda_2,\lambda_3)$: $2+6+3=11$ free.

\textbf{(c)} Forward algorithm:
$\alpha_1(i)=\pi_i e^{-\lambda_i}\lambda_i^{y_1}/y_1!$,\;\;
$\alpha_t(j)=(\sum_i\alpha_{t-1}(i)p_{ij})e^{-\lambda_j}\lambda_j^{y_t}/y_t!$;
$L(\phi)=\sum_i\alpha_T(i)$.""",
    "is_exam": True,
    "topic_hint": "t5a",
    "images": []
}

past_exams_ts["exam_may_2021_q5"] = {
    "title": 'May 2021 — Q5',
    "content": r"""<span class="exam-question-text">Let $Y_t=(Y_{1,t},\dots,Y_{m,t})'$ represent the prices of $m$ assets in a portfolio at time $t$.
**(a)** Write the general expression of a DLM for the time series $(Y_t)$.

**(b)** In a DLM with no unknown parameters, the problem of filtering is solved through the
        celebrated Kalman filter. Explain briefly the steps of the proof of the Kalman
        filter---no more than 3 lines, no proofs are asked here---but do say clearly in which
        step one has to use Bayes' rule, and how it is used.

**(c)** Prove the first step.</span>

---

**Solution.** \textbf{(a)} Keydef \textbf{16a}.

\textbf{(b)} 3 steps. \emph{Step 1 (state prediction)}: $\theta_t\mid y_{1:t-1}\sim\Nd_p(a_t,R_t)$,
$a_t=G_t m_{t-1}$, $R_t=G_t C_{t-1}G_t^{\top}+W_t$ (affine + Gaussian closure).
\emph{Step 2 (obs prediction)}: $Y_t\mid y_{1:t-1}\sim\Nd_q(f_t,Q_t)$, $f_t=F_t a_t$,
$Q_t=F_t R_t F_t^{\top}+V_t$.
\emph{Step 3 (filtering update)}: \textbf{Bayes' rule} on the conditionally Gaussian model
gives $p(\theta_t\mid y_{1:t})\propto p(y_t\mid\theta_t)p(\theta_t\mid y_{1:t-1})$;
Gaussian conditioning on the joint $(\theta_t,Y_t)\mid y_{1:t-1}$ yields
$m_t=a_t+K_t(y_t-f_t)$, $C_t=R_t-K_t Q_t K_t^{\top}$, $K_t=R_t F_t^{\top} Q_t^{-1}$.

\textbf{(c) Proof of Step 1.} $\theta_t=G_t\theta_{t-1}+w_t$ with $w_t\perp\sigma(\theta_{t-1},y_{1:t-1})$.
Conditional moments:
$\mathbb{E}[\theta_t\mid y_{1:t-1}]=G_t m_{t-1}=a_t$;
$\operatorname{Var}(\theta_t\mid y_{1:t-1})=G_t C_{t-1}G_t^{\top}+W_t=R_t$ (independence kills cross terms).
$(\theta_{t-1},w_t)\mid y_{1:t-1}$ jointly Gaussian $\Rightarrow$ linear combination
$\theta_t\mid y_{1:t-1}\sim\Nd_p(a_t,R_t)$. \hfill$\Box$""",
    "is_exam": True,
    "topic_hint": "t8b",
    "images": []
}

past_exams_ts["exam_may_2021_q6"] = {
    "title": 'May 2021 — Q6',
    "content": r"""<span class="exam-question-text">Let $(Y_t)$ be a univariate time series. Suppose we model $(Y_t)$ by a dynamic linear model
(DLM), with known parameters. Consider the forecast errors
$$
e_t=Y_t-f_t,\qquad \text{where }f_t=\mathbb{E}(Y_t\mid Y_{1:t-1}).
$$
The process $(e_t)$ has interesting properties that can be used for model checking. Show that
$\mathbb{E}(e_t)=0$ and that, for any $t>s$, $\operatorname{Cov}(e_t,e_s)=0$.</span>

---

**Solution.** \emph{Mean.} $\mathbb{E}[e_t\mid\mathcal F_{t-1}]=f_t-f_t=0\Rightarrow\mathbb{E}[e_t]=0$.

\emph{Orthogonality} for $t>s$: $e_s$ is $\mathcal F_{t-1}$-measurable (since $s\le t-1$).
By tower + pull out:
$\mathbb{E}[e_t e_s]=\mathbb{E}[\mathbb{E}[e_t\mid\mathcal F_{t-1}]e_s]=\mathbb{E}[0\cdot e_s]=0$.
Hence $\operatorname{Cov}(e_t,e_s)=0$. $(e_t)$ is a martingale-difference sequence; standardised
$\tilde e_t=Q_t^{-1/2}e_t$ are i.i.d.\ $\mathcal{N}(0,1)$ under correct specification --- used in
model checking (QQ plot, Ljung--Box).""",
    "is_exam": True,
    "topic_hint": "t11a",
    "images": []
}
