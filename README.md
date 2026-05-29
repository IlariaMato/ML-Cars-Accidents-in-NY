# Road Accident Analysis — New York State (2016–2023)

> **Data Mining · A.A. 2024/2025**  
> Linear and logistic regression modelling on the US-Accidents dataset.

---

## Overview

This project analyses traffic accident data from New York State to model two outcomes:

| Target variable | Model type | Final metric |
|---|---|---|
| **Visibility** (miles) | Linear regression (OLS + robust SE) | R² = 0.851 · AIC = 26,673 |
| **Sunrise/Sunset** (Day vs Night) | Logistic regression (GLM binomial) | AIC = 10,127 |

The dataset is **US-Accidents** (Moosavi et al., Kaggle), covering February 2016 – March 2023.  
After filtering for NY and preprocessing, the working sample is **8,448 observations × 15 variables**.

---

## Repository structure

```
ML-Cars-Accidents-in-NY/
├── DM_code.R          # Full analysis script (sequential, commented)
├── DM_project.pdf     # Polished report (this document)
└── README.md
```

> **Note:** The raw dataset (`US_Accidents_March23.csv`) is not included due to its size (~3 GB).  
> Download it from Kaggle (see [Replication](#replication)) and filter for `State == 'NY'`.

---

## Dataset

| Parameter | Original | After preprocessing |
|---|---|---|
| Observations | 7,730,000 | 8,448 |
| Variables | 43 | 15 |
| Scope | 49 US states | New York (NY) |
| City levels | 477 | 125 |
| Weather_Condition levels | 47 | 11 |

### Variables

| Variable | Type | Role |
|---|---|---|
| `Visibility` | Continuous | **Linear model target** (miles) |
| `Sunrise_Sunset` | Binary | **Logistic model target** (Day / Night) |
| `Distance` | Continuous | Road extent affected (miles) |
| `Temperature` | Continuous | °F |
| `Pressure` | Continuous | Atmospheric pressure (in Hg) |
| `Wind_speed` | Continuous | mph |
| `Severity` | Continuous | Accident severity score (1–4) |
| `City` | Categorical | 125 levels after grouping |
| `Weather_Condition` | Categorical | 11 levels after grouping |
| `Crossing / Junction / Stop / Traffic_signal` | Binary | Infrastructure indicators |
| `Month / Day` | Categorical | Temporal covariates |

---

## Analysis pipeline

### 1. Preprocessing

- **City:** Cities with < 20 accidents grouped into *Other* and removed. Levels: 477 → 125.
- **Weather_Condition:** Optimal grouping via `factorMerger` (GIC criterion). Levels: 47 → 11.
- **Missing values:** Mean imputation for continuous variables, mode imputation for categorical ones (`Hmisc::impute`), verified graphically with `VIM::aggr`.

### 2. Linear model (target: Visibility)

The modelling followed an iterative refinement strategy:

| Step | Action | R² | AIC |
|---|---|---|---|
| Model 1 | Full baseline (all 15 variables) | 0.757 | 33,159 |
| Model 2 | + Interaction `temperature × Sunrise_Sunset` | 0.757 | 33,153 |
| — | Box-Cox (λ = 1.67): rejected — AIC worsens to 64,090 | — | — |
| Model 4 | + Cubic terms for `distance` and `pressure` (GAM/RESET) | 0.760 | 33,083 |
| Model 5 | Bidirectional `stepAIC` selection | 0.760 | 33,079 |
| **Model 6** | **DFFITS outlier removal + White's robust SE** | **0.851** | **26,673** |

**Final model formula:**

```r
modello_final <- lm(
  visibility ~ distance + I(distance^2) + I(distance^3)
             + City
             + pressure + I(pressure^2) + I(pressure^3)
             + wind_speed + Weather_group + Severity
             + month + day
             + temperature * Sunrise_Sunset,
  data = b_filtered
)

# Robust inference
coeftest(modello_final, vcov = vcovHC(modello_final))
```

> R² = 0.851 · R²_adj = 0.836 · AIC = 26,673

**Key modelling decisions:**

- **No Box-Cox transformation** — R² decreases, AIC almost doubles.
- **Cubic terms** for `distance` and `pressure` — confirmed by Loess, Spline fits and RESET test (p < 2.2e-16).
- **DFFITS outlier removal** — criterion |DFFITS| > 2√(p/n); major R² improvement (0.760 → 0.851).
- **Heteroscedasticity** — detected via `ncvTest` and Breusch-Pagan (p = 2.22e-16); corrected with `vcovHC`.
- **Bootstrap (R = 1,999)** — confirms parameter robustness; narrow, symmetric confidence intervals for main effects.

### 3. Logistic model (target: Sunrise/Sunset)

- **Full GLM:** Non-significant predictors: `Junction`, `Traffic_signal`, `visibility`.
- **stepAIC selection:** AIC = 10,127.52. Retained predictors: `Severity`, `distance`, `City`, `Crossing`, `Junction`, `Stop`, `month`, `day`, `wind_speed`, `temperature`, `pressure`, `visibility`, `Weather_group`.
- **LRT (ANOVA):** p = 0.5994 — no significant difference between full and selected model; parsimonious model preferred.

**Selected odds ratios (Day vs Night):**

| Predictor / Group | OR | Interpretation |
|---|---|---|
| Amsterdam, Huntington Station, Fresh Meadows | > 1 | Strongly associated with Day accidents |
| Weather group 6 | > 1 | Associated with Day accidents |
| Hudson, Kirkville, Jordan | < 1 | Associated with Night accidents |
| Reduced visibility | < 1 | Associated with Night accidents |
| Wind speed, Temperature | ≈ 1 | No significant effect |

---

## R packages

| Package | Purpose |
|---|---|
| `dplyr` | Data manipulation and filtering |
| `VIM` | Missing value visualisation (`aggr`) |
| `Hmisc` | Missing value imputation |
| `factorMerger` | Optimal grouping of categorical levels (GIC) |
| `mctest` | VIF / TOL for multicollinearity diagnostics |
| `psych` | Pair panels for exploratory correlation analysis |
| `gam` | Generalised additive models — Loess and Splines |
| `lmtest` | RESET test and Breusch-Pagan test |
| `MASS` | `stepAIC` and Box-Cox transformation |
| `car` | `ncvTest`, bootstrap (`Boot`) |
| `sandwich` | White's robust standard errors (`vcovHC`) |

---

## Replication

1. Download **US-Accidents** from [Kaggle](https://www.kaggle.com/datasets/sobhanmoosavi/us-accidents).
2. Filter for `State == 'NY'` to obtain the New York subset.
3. Set the working directory in `DM_code.R` via `setwd()`.
4. Install all packages listed above.
5. Run `DM_code.R` sequentially — each block is commented to guide the analysis.

---

## References

Moosavi, S., Samavatian, M. H., Parthasarathy, S., Teodorescu, R., & Ramnath, R. (2019).  
*A countrywide traffic accident dataset.* arXiv:1906.05409.

Dataset: [US-Accidents on Kaggle](https://www.kaggle.com/datasets/sobhanmoosavi/us-accidents)
