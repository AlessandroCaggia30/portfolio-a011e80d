"""
TOPICS proposal — Time Series Analysis past exams (61 questions).

13-topic taxonomy clustering all 61 questions from past_exams_content.py.
Each topic minimises within-cluster heterogeneity (one core concept per topic)
and maximises between-cluster distance (filtering vs smoothing vs prediction vs
estimation are kept separate). Sub-topics only created where >=2 questions
share a sharper sub-pattern.

Coverage: every one of the 61 question IDs appears in EXACTLY ONE `questions`
list below. Multi-part questions are assigned to the topic that captures the
primary graded answer (the boxed result / longest proof requested).

Sanity-check: each topic is backable by >=1 existing canvas theory node in
data_timeseries.json -> data.topics[0].subtopics[0].nodes[] (mapping at bottom).
"""

TOPICS = [
    # ----------------------------------------------------------------- T1
    {"id": "T1", "name": "Stochastic processes & time series — foundations", "subtopics": [
        {"sid": "t1a", "name": "What is a stochastic process / time series",
         "questions": ["exam_sep_2025_q1", "exam_may_2023_q1"]},
    ]},

    # ----------------------------------------------------------------- T2
    {"id": "T2", "name": "Stationarity, ACVF & sample mean", "subtopics": [
        {"sid": "t2a", "name": "Weak stationarity — definition & examples",
         "questions": ["exam_sep_2025_q2", "exam_jun_2024_q1"]},
        {"sid": "t2b", "name": "ACVF / correlogram — when defined?",
         "questions": ["exam_may_2025_q1", "exam_may_2022_q2", "exam_may_2021_q2"]},
        {"sid": "t2c", "name": "Sample-mean estimator under stationarity + ergodicity",
         "questions": ["exam_may_2024_q1", "exam_jun_2022_q1"]},
    ]},

    # ----------------------------------------------------------------- T3
    {"id": "T3", "name": "Markov chains — theory (DAG, Markov property, ergodicity)", "subtopics": [
        {"sid": "t3a", "name": "Markov property, DAG & conditional independence",
         "questions": ["exam_jun_2025_q1", "exam_may_2025_q2", "exam_may_2021_q1"]},
        {"sid": "t3b", "name": "Transition-matrix arithmetic & ergodic convergence (Thm 2.1)",
         "questions": ["exam_jun_2024_q3", "exam_may_2023_q2", "exam_may_2022_q3", "exam_jun_2022_q5"]},
    ]},

    # ----------------------------------------------------------------- T4
    {"id": "T4", "name": "Markov chains — likelihood, MLE & Anderson–Goodman CI", "subtopics": [
        {"sid": "t4a", "name": "Panel transition-count likelihood & MLE",
         "questions": ["exam_sep_2025_q6", "exam_may_2024_q2"]},
        {"sid": "t4b", "name": "Wald CI for p_ij + forecasting future percentages",
         "questions": ["exam_jun_2025_q2", "exam_may_2022_q5"]},
    ]},

    # ----------------------------------------------------------------- T5
    {"id": "T5", "name": "Hidden Markov Models (HMM) — model, likelihood, decoding", "subtopics": [
        {"sid": "t5a", "name": "HMM definition, parameters & forward-algorithm likelihood",
         "questions": ["exam_may_2025_q3", "exam_may_2023_q3", "exam_may_2022_q6", "exam_may_2021_q4"]},
        {"sid": "t5b", "name": "Decoding (Viterbi / forward–backward) + path-probability",
         "questions": ["exam_jun_2024_q4"]},
    ]},

    # ----------------------------------------------------------------- T6
    {"id": "T6", "name": "State-space models & DLMs — general definition", "subtopics": [
        {"sid": "t6a", "name": "SSM / DLM general definition (univariate & multivariate)",
         "questions": ["exam_sep_2025_q4", "exam_may_2024_q5"]},
        {"sid": "t6b", "name": "SSM flexibility — non-stationarity & SV models (is it a DLM?)",
         "questions": ["exam_sep_2025_q3", "exam_may_2022_q4"]},
    ]},

    # ----------------------------------------------------------------- T7
    {"id": "T7", "name": "DLM building blocks — RW+noise, structural, regression, AR-as-DLM", "subtopics": [
        {"sid": "t7a", "name": "Random-walk + noise model — definition & independence proofs",
         "questions": ["exam_may_2024_q3", "exam_may_2023_q4"]},
        {"sid": "t7b", "name": "Local linear trend / structural BSM (trend + seasonality)",
         "questions": ["exam_sep_2025_q5", "exam_may_2021_q3"]},
        {"sid": "t7c", "name": "Time-varying-coefficient regression DLM",
         "questions": ["exam_may_2025_q4", "exam_jun_2022_q6"]},
        {"sid": "t7d", "name": "Multivariate DLM & dependence between latent series",
         "questions": ["exam_jun_2025_q4"]},
        {"sid": "t7e", "name": "AR(p) as DLM (companion form)",
         "questions": ["exam_jun_2024_q2"]},
    ]},

    # ----------------------------------------------------------------- T8
    {"id": "T8", "name": "Kalman filter — filtering distribution & update derivation", "subtopics": [
        {"sid": "t8a", "name": "Filtering distribution: definition, not just a point estimate",
         "questions": ["exam_may_2024_q4", "exam_jun_2022_q2", "exam_may_2023_q6"]},
        {"sid": "t8b", "name": "KF predict + update derivation (with Bayes step)",
         "questions": ["exam_jun_2025_q3", "exam_may_2025_q5", "exam_may_2021_q5"]},
    ]},

    # ----------------------------------------------------------------- T9
    {"id": "T9", "name": "Kalman smoother (RTS) & filtering vs smoothing", "subtopics": [
        {"sid": "t9a", "name": "Filtering vs smoothing — definitions & DAG-based proofs",
         "questions": ["exam_sep_2025_q7", "exam_jun_2022_q3", "exam_may_2023_q5", "exam_may_2022_q7"]},
    ]},

    # ----------------------------------------------------------------- T10
    {"id": "T10", "name": "One-step-ahead prediction & forecast function", "subtopics": [
        {"sid": "t10a", "name": "Predictive distribution N(f_t,Q_t) — derivation",
         "questions": ["exam_jun_2024_q6"]},
        {"sid": "t10b", "name": "Forecast function, k-step intervals, SES & loss functions",
         "questions": ["exam_jun_2024_q5", "exam_jun_2022_q4", "exam_may_2022_q1"]},
    ]},

    # ----------------------------------------------------------------- T11
    {"id": "T11", "name": "Forecast errors / innovations & model checking", "subtopics": [
        {"sid": "t11a", "name": "Innovations: zero-mean, orthogonality, standardisation",
         "questions": ["exam_jun_2025_q5", "exam_may_2024_q6", "exam_may_2021_q6"]},
    ]},

    # ----------------------------------------------------------------- T12
    {"id": "T12", "name": "Parameter estimation in DLM — MLE & prediction-error decomp.", "subtopics": [
        {"sid": "t12a", "name": "Likelihood of phi via prediction-error decomposition (MLE)",
         "questions": ["exam_jun_2024_q7", "exam_may_2024_q7"]},
    ]},

    # ----------------------------------------------------------------- T13
    {"id": "T13", "name": "Bayesian inference, conjugate updates & MCMC", "subtopics": [
        {"sid": "t13a", "name": "Conjugate Normal–Normal posterior (static theta / Case A)",
         "questions": ["exam_sep_2025_q8", "exam_jun_2022_q7"]},
        {"sid": "t13b", "name": "Bayesian predictive distribution integrating out phi (+ MCMC)",
         "questions": ["exam_jun_2025_q6", "exam_may_2025_q6", "exam_may_2022_q8"]},
    ]},
]

# ---------------------------------------------------------------------------
# Backing canvas theory nodes (sanity check, all present in data_timeseries.json
# under data.topics[0].subtopics[0].nodes[]):
#
#   T1  <- "Time series as stochastic processes", "Aims of time series analysis"
#   T2  <- "weak Stationarity", "Strict stationarity",
#          "Summaries of a stochastic process",
#          "Stationarity does not imply strict stat."
#   T3  <- "Markov Property",
#          "Example 2: categorical time series (state is observed)",
#          "Conditions of Theorem 2.1 — recurrent / aperiodic / irreducible",
#          "Ergodic properties of a Markov chain (Theorem 2.1)"
#   T4  <- "Estimation", "Proof: the counts ratio is the MLE",
#          "Asymptotics", "Bernoulli vs categorical"
#   T5  <- "Decoding / filtering",
#          "Exercise — weather/activity filtering (state is unobserved)"
#   T6  <- "State space models (SSM)", "Example SSM",
#          "SSM flexibility — no stationarity required",
#          "Example 3: AR(1) (A and B)"
#   T7  <- "Example 1: RW", "Fitting a trend", "Fitting a seasonal component",
#          "Time series decomposition",
#          "How to assign the Probability law of the process"
#   T8  <- "SSM filtering and prediction", "Decoding / filtering"
#   T9  <- "Smoothing and decoding"
#   T10 <- "Forecasting algorithms — Cases 0, 1, 2 (SES / Holt / Holt-Winters)",
#          "tuning alfa in SES", "SSM filtering and prediction"
#   T11 <- "Model checking: forecast errors / innovations"
#   T12 <- "Estimation of unknown parameters --- DLM (prediction-error decomposition)",
#          "Parameter Estimation", "Probability vs Likelihood"
#   T13 <- "Bayesian statistics", "Bayesian vs frequentist inference",
#          "Posterior mean and precision",
#          "Proof for the closed form versions of Ct and mt in the fixed theta case",
#          "Bayesian point estimation: loss functions and the posterior mean"
#
# No gaps: every topic backed by >=1 existing theory node.

# ---------------------------------------------------------------------------
# Self-check helpers (run as `python topics_proposal.py`)
# ---------------------------------------------------------------------------
if __name__ == "__main__":
    seen = {}
    for t in TOPICS:
        for s in t["subtopics"]:
            for q in s["questions"]:
                seen.setdefault(q, []).append(s["sid"])
    dup = {q: sids for q, sids in seen.items() if len(sids) > 1}
    print(f"Unique question IDs assigned: {len(seen)}")
    print(f"Duplicates (assigned to >1 sub-topic): {len(dup)}")
    if dup:
        for q, sids in dup.items():
            print(f"  {q}: {sids}")
    # Compare against past_exams_content
    try:
        import past_exams_content as m
        all_ids = set(m.past_exams_ts.keys())
        assigned = set(seen.keys())
        missing = all_ids - assigned
        extra = assigned - all_ids
        print(f"Total in past_exams_ts: {len(all_ids)}")
        print(f"Missing (not assigned): {sorted(missing)}")
        print(f"Extra (unknown id):     {sorted(extra)}")
    except Exception as e:
        print(f"(could not import past_exams_content: {e})")
