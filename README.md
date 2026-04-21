# UK SME Credit Access & Loan Outcomes: An Econometric Analysis

## Overview

This project analyses the determinants of SME loan interest rates and default outcomes in the UK using panel econometric methods. It was built to demonstrate applied econometric skills in a financially relevant context — directly applicable to SME lending platforms such as iwoca, Funding Circle and Tide.

The analysis uses synthetic data generated from realistic parameters drawn from publicly available sources including the **Bank of England Credit Conditions Survey**, **FCA Consumer Credit Data** and the **British Business Bank Small Business Finance Markets Report**.

---

## Key Questions

1. Do female-owned SMEs face a **gender premium** in loan interest rates?
2. What firm characteristics drive **credit pricing** and **default risk**?
3. Do results hold after controlling for **unobserved firm heterogeneity** using panel fixed effects?

---

## Econometric Methods

| Method | Purpose |
|--------|---------|
| Probability & normal distribution | Modelling interest rate thresholds |
| Two-sample t-test | Testing gender gap in interest rates |
| Proportion test | Default rate vs industry benchmark |
| Cross-sectional OLS (log-linear) | Baseline determinants of interest rates |
| Heteroskedasticity-robust SE | Correcting for non-constant variance |
| Panel data — entity fixed effects | Controlling for time-invariant firm characteristics |
| Two-way fixed effects (entity + time) | Controlling for macro shocks |
| Hausman test | Choosing between fixed and random effects |
| F-test for time effects | Testing significance of time fixed effects |
| Interaction terms (gender × sector) | Heterogeneous gender effects across sectors |
| Linear hypothesis test | Formal test for gender premium |

---

## Dataset

**Synthetic panel data** — 500 UK SMEs observed annually from 2010 to 2022 (6,500 observations).

| Variable | Description |
|----------|-------------|
| `firm_id` | Unique firm identifier |
| `year` | Year (2010–2022) |
| `sector` | Industry sector |
| `owner_gender` | Gender of primary owner |
| `region` | UK region |
| `firm_age` | Years since founding |
| `annual_revenue` | Annual turnover (£) |
| `credit_score` | Firm credit score (300–850) |
| `loan_amount` | Loan requested (£) |
| `interest_rate` | Interest rate offered (%) |
| `default` | Binary default indicator |
| `employees` | Number of employees |
| `approved` | Loan approval indicator |

---

## Key Findings

- **Gender premium:** Female-owned SMEs are charged approximately 0.3 percentage points higher interest rates, even after controlling for credit score, revenue and sector
- **Credit score** is the strongest predictor of interest rate — a 100-point increase reduces rates by approximately 0.5pp
- **Sector matters:** Hospitality firms face significantly higher rates than retail (baseline)
- **Time fixed effects** are statistically significant — macro conditions (e.g. 2020 COVID shock) affect all firms
- **Hausman test** confirms fixed effects are preferred over random effects

---

## Structure

```
sme_project/
│
├── sme_analysis.R              # Main analysis script
├── sme_lending_data.csv        # Generated panel dataset
├── plot1_interest_rates_by_gender.png
├── plot2_default_by_sector.png
├── plot3_rate_trend_over_time.png
├── plot4_credit_vs_rate.png
└── README.md
```

---

## Requirements

```r
install.packages(c("ggplot2","plm","AER","sandwich",
                   "lmtest","stargazer","dplyr","tidyr"))
```

---

## How to Run

```r
source("sme_analysis.R")
```

---

## Author

**Muhammad Usama**  
MSc Financial Technology (Distinction), University of Exeter  
[LinkedIn](https://www.linkedin.com/in/m-usama-malik-) | [GitHub](https://github.com/MuhammadUsamaMalik26)

---

## Disclaimer

All data in this project is **synthetically generated** from realistic parameters based on published UK SME lending statistics. No real firm or individual data is used. This project is for analytical and portfolio purposes only.
