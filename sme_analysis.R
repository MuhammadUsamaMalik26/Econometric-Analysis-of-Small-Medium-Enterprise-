# ============================================================
# UK SME Credit Access & Loan Outcomes: An Econometric Analysis
# Author: Muhammad Usama
# Date: April 2026
# Description: Panel data analysis of UK SME lending outcomes
#              using synthetic data based on BoE/FCA statistics.
#              Covers probability, OLS, panel fixed effects,
#              interaction terms and hypothesis testing.
# ============================================================

# ── 0. LIBRARIES ─────────────────────────────────────────────
library(ggplot2)
library(plm)
library(AER)
library(sandwich)
library(lmtest)
# library(stargazer) # not available - using summary instead
library(dplyr)
library(tidyr)

# ── 1. GENERATE SYNTHETIC SME PANEL DATA ─────────────────────
# Based on realistic UK SME lending statistics:
# - BoE Credit Conditions Survey
# - FCA Consumer/SME Credit Data 2010-2022
# - British Business Bank Small Business Finance Markets Report

set.seed(2024)

n_firms  <- 500
n_years  <- 13   # 2010 to 2022
N        <- n_firms * n_years

firm_id  <- rep(1:n_firms, each = n_years)
year     <- rep(2010:2022, times = n_firms)

# Firm characteristics (time-invariant)
sector        <- rep(sample(c("Retail","Manufacturing","Tech",
                               "Hospitality","Professional Services"),
                            n_firms, replace = TRUE,
                            prob = c(0.25,0.20,0.20,0.15,0.20)), each = n_years)

owner_gender  <- rep(sample(c("Male","Female"),
                            n_firms, replace = TRUE,
                            prob = c(0.65, 0.35)), each = n_years)

region        <- rep(sample(c("London","South East","North West",
                               "Yorkshire","Scotland","Other"),
                            n_firms, replace = TRUE,
                            prob = c(0.20,0.15,0.15,0.12,0.10,0.28)), each = n_years)

firm_age_base <- rep(sample(1:30, n_firms, replace = TRUE), each = n_years)
firm_age      <- firm_age_base + (year - 2010)

# Time-varying variables
annual_revenue <- 50000 + 30000 * (firm_age / 10) +
                  ifelse(sector == "Tech", 40000, 0) +
                  ifelse(sector == "Hospitality", -15000, 0) +
                  ifelse(region == "London", 25000, 0) +
                  rnorm(N, 0, 20000)

annual_revenue <- pmax(annual_revenue, 10000)

# Credit score 300-850
credit_score <- 450 + 0.8 * (annual_revenue / 10000) +
                2 * firm_age +
                ifelse(owner_gender == "Female", -5, 0) +   # small gap in UK data
                rnorm(N, 0, 60)
credit_score <- pmin(pmax(credit_score, 300), 850)

# Loan amount requested
loan_amount <- 20000 + 0.15 * annual_revenue +
               1000 * firm_age +
               ifelse(sector == "Manufacturing", 15000, 0) +
               rnorm(N, 0, 15000)
loan_amount <- pmax(loan_amount, 5000)

# Interest rate (%) - higher for riskier firms
interest_rate <- 8.5 -
                 0.005 * credit_score +
                 0.02 * (loan_amount / 10000) +
                 ifelse(owner_gender == "Female",  0.3, 0) +  # gender premium
                 ifelse(sector == "Hospitality",   0.8, 0) +
                 ifelse(region == "London",        -0.4, 0) +
                 rnorm(N, 0, 0.8)
interest_rate <- pmax(interest_rate, 2)

# Default probability (logistic)
default_linear <- -3 +
                  0.003 * (interest_rate) -
                  0.004 * (credit_score / 100) +
                  0.002 * (loan_amount / 10000) -
                  0.02  * firm_age +
                  ifelse(sector == "Hospitality", 0.5, 0) +
                  rnorm(N, 0, 0.5)
default_prob <- 1 / (1 + exp(-default_linear))
default      <- rbinom(N, 1, default_prob)

# Number of employees
employees <- round(2 + 0.00005 * annual_revenue +
                   0.3 * firm_age +
                   rnorm(N, 0, 5))
employees <- pmax(employees, 1)

# Loan approved
approval_linear <- 1.5 +
                   0.005 * (credit_score / 100) -
                   0.001 * (loan_amount / 10000) +
                   0.05  * firm_age -
                   ifelse(owner_gender == "Female", 0.15, 0) +
                   rnorm(N, 0, 0.5)
approval_prob <- 1 / (1 + exp(-approval_linear))
approved      <- rbinom(N, 1, approval_prob)

# Assemble data frame
sme_data <- data.frame(
  firm_id, year, sector, owner_gender, region,
  firm_age, annual_revenue, credit_score,
  loan_amount, interest_rate, default, employees, approved
)

# Factor variables
sme_data$sector       <- factor(sme_data$sector,
                                levels = c("Retail","Manufacturing","Tech",
                                           "Hospitality","Professional Services"))
sme_data$owner_gender <- factor(sme_data$owner_gender, levels = c("Male","Female"))
sme_data$region       <- factor(sme_data$region,
                                levels = c("London","South East","North West",
                                           "Yorkshire","Scotland","Other"))

cat("Dataset created:", nrow(sme_data), "observations |",
    n_firms, "firms |", n_years, "years\n")
cat("Default rate:", round(mean(sme_data$default) * 100, 1), "%\n")
cat("Approval rate:", round(mean(sme_data$approved) * 100, 1), "%\n")


# ── 2. DESCRIPTIVE STATISTICS ─────────────────────────────────
cat("\n--- DESCRIPTIVE STATISTICS ---\n")
summary(sme_data[, c("annual_revenue","loan_amount","interest_rate",
                     "credit_score","firm_age","employees")])

# Table: default rate by sector and gender
default_by_sector <- sme_data %>%
  group_by(sector, owner_gender) %>%
  summarise(
    n          = n(),
    default_rt = round(mean(default) * 100, 1),
    avg_rate   = round(mean(interest_rate), 2),
    avg_credit = round(mean(credit_score), 0),
    .groups = "drop"
  )
print(default_by_sector)


# ── 3. PROBABILITY ANALYSIS ───────────────────────────────────
cat("\n--- SECTION 1: PROBABILITY ---\n")

# 3a. Distribution of interest rates
mean_rate <- mean(sme_data$interest_rate)
sd_rate   <- sd(sme_data$interest_rate)
cat("Interest rate: mean =", round(mean_rate, 2),
    "| sd =", round(sd_rate, 2), "\n")

# P(interest rate > 10%) — high cost credit threshold
threshold <- 10
z_score   <- (threshold - mean_rate) / sd_rate
p_high    <- pnorm(z_score, lower.tail = FALSE)
cat("P(interest rate > 10%) =", round(p_high, 4), "\n")

# P(interest rate > 12%)
p_very_high <- pnorm((12 - mean_rate) / sd_rate, lower.tail = FALSE)
cat("P(interest rate > 12%) =", round(p_very_high, 4), "\n")

# Conditional: P(rate > 12% | rate > 10%)
p_conditional <- p_very_high / p_high
cat("P(rate > 12% | rate > 10%) =", round(p_conditional, 4), "\n")


# ── 4. HYPOTHESIS TESTING ─────────────────────────────────────
cat("\n--- SECTION 2: HYPOTHESIS TESTING ---\n")

# H0: mean interest rate for female-owned = male-owned firms
# H1: female-owned firms pay higher rates (one-tailed)
male_rates   <- sme_data$interest_rate[sme_data$owner_gender == "Male"]
female_rates <- sme_data$interest_rate[sme_data$owner_gender == "Female"]

t_test_result <- t.test(female_rates, male_rates,
                         alternative = "greater",
                         var.equal   = FALSE)
print(t_test_result)

cat("\nConclusion: At 5% significance level,",
    ifelse(t_test_result$p.value < 0.05,
           "we REJECT H0 — female-owned firms pay significantly higher interest rates.",
           "we FAIL TO REJECT H0 — no significant difference in rates."), "\n")

# H0: default rate = 10% (industry benchmark)
cat("\n-- One-sample test: default rate vs 10% benchmark --\n")
n_total  <- nrow(sme_data)
defaults <- sum(sme_data$default)
prop_test <- prop.test(defaults, n_total, p = 0.10,
                       alternative = "two.sided")
print(prop_test)


# ── 5. VISUALISATIONS ─────────────────────────────────────────

# Plot 1: Interest rate distribution by gender
p1 <- ggplot(sme_data, aes(x = interest_rate, fill = owner_gender)) +
  geom_histogram(alpha = 0.6, bins = 40, position = "identity") +
  scale_fill_manual(values = c("#2C3E50","#E74C3C"),
                    labels = c("Male-owned","Female-owned")) +
  labs(title    = "Distribution of Loan Interest Rates by Owner Gender",
       subtitle = "UK SME Lending Panel Data, 2010-2022",
       x        = "Interest Rate (%)",
       y        = "Count",
       fill     = "Owner Gender",
       caption  = "Source: Synthetic data based on BoE/FCA SME lending statistics") +
  theme_minimal(base_size = 13) +
  theme(plot.title    = element_text(face = "bold"),
        legend.position = "top")

ggsave("plot1_interest_rates_by_gender.png", p1,
       width = 9, height = 5.5, dpi = 150)
cat("Saved: plot1_interest_rates_by_gender.png\n")

# Plot 2: Default rates by sector
default_sector <- sme_data %>%
  group_by(sector) %>%
  summarise(default_rate = mean(default) * 100, .groups = "drop")

p2 <- ggplot(default_sector, aes(x = reorder(sector, default_rate),
                                  y = default_rate, fill = sector)) +
  geom_col(alpha = 0.85, show.legend = FALSE) +
  geom_text(aes(label = paste0(round(default_rate, 1), "%")),
            hjust = -0.2, size = 4) +
  coord_flip() +
  scale_fill_brewer(palette = "Set2") +
  labs(title    = "SME Loan Default Rates by Sector",
       subtitle = "UK SME Lending Panel Data, 2010-2022",
       x        = NULL,
       y        = "Default Rate (%)",
       caption  = "Source: Synthetic data based on BoE/FCA SME lending statistics") +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold")) +
  ylim(0, max(default_sector$default_rate) * 1.2)

ggsave("plot2_default_by_sector.png", p2,
       width = 9, height = 5.5, dpi = 150)
cat("Saved: plot2_default_by_sector.png\n")

# Plot 3: Interest rate trend over time by gender
trend_data <- sme_data %>%
  group_by(year, owner_gender) %>%
  summarise(avg_rate = mean(interest_rate), .groups = "drop")

p3 <- ggplot(trend_data, aes(x = year, y = avg_rate,
                              colour = owner_gender, group = owner_gender)) +
  geom_line(size = 1.2) +
  geom_point(size = 2.5) +
  scale_colour_manual(values = c("#2C3E50","#E74C3C"),
                      labels = c("Male-owned","Female-owned")) +
  labs(title    = "Average Loan Interest Rate Over Time by Owner Gender",
       subtitle = "UK SME Lending Panel Data, 2010-2022",
       x        = "Year",
       y        = "Average Interest Rate (%)",
       colour   = "Owner Gender",
       caption  = "Source: Synthetic data based on BoE/FCA SME lending statistics") +
  theme_minimal(base_size = 13) +
  theme(plot.title    = element_text(face = "bold"),
        legend.position = "top")

ggsave("plot3_rate_trend_over_time.png", p3,
       width = 9, height = 5.5, dpi = 150)
cat("Saved: plot3_rate_trend_over_time.png\n")

# Plot 4: Credit score vs interest rate
p4 <- ggplot(sme_data %>% sample_n(2000),
             aes(x = credit_score, y = interest_rate, colour = owner_gender)) +
  geom_point(alpha = 0.3, size = 1.5) +
  geom_smooth(method = "lm", se = TRUE) +
  scale_colour_manual(values = c("#2C3E50","#E74C3C"),
                      labels = c("Male-owned","Female-owned")) +
  labs(title    = "Credit Score vs Interest Rate by Owner Gender",
       subtitle = "Sample of 2,000 observations",
       x        = "Credit Score",
       y        = "Interest Rate (%)",
       colour   = "Owner Gender",
       caption  = "Source: Synthetic data based on BoE/FCA SME lending statistics") +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold"),
        legend.position = "top")

ggsave("plot4_credit_vs_rate.png", p4,
       width = 9, height = 5.5, dpi = 150)
cat("Saved: plot4_credit_vs_rate.png\n")


# ── 6. CROSS-SECTIONAL OLS REGRESSION ─────────────────────────
cat("\n--- SECTION 3: CROSS-SECTIONAL OLS REGRESSION (Year = 2020) ---\n")

# Subset to 2020 (cross-section)
data_2020 <- subset(sme_data, year == 2020)
cat("2020 cross-section:", nrow(data_2020), "firms\n")

# Model 1: Baseline
model_cs1 <- lm(log(interest_rate) ~ owner_gender + log(annual_revenue) +
                  credit_score + firm_age,
                data = data_2020)

# Model 2: Add sector controls
model_cs2 <- lm(log(interest_rate) ~ owner_gender + log(annual_revenue) +
                  credit_score + firm_age + sector,
                data = data_2020)

# Model 3: Add region + loan amount
model_cs3 <- lm(log(interest_rate) ~ owner_gender + log(annual_revenue) +
                  credit_score + firm_age + sector + region +
                  log(loan_amount),
                data = data_2020)

# Robust standard errors
coeftest_cs3 <- coeftest(model_cs3, vcov = vcovHC(model_cs3, type = "HC1"))
print(coeftest_cs3)

cat("\n--- Cross-Sectional Summary (Model 3) ---\n")
print(summary(model_cs3))

# ── 7. PANEL DATA: FIXED EFFECTS ──────────────────────────────
cat("\n--- SECTION 4: PANEL DATA ANALYSIS ---\n")

# Convert to pdata.frame
panel_data <- pdata.frame(sme_data,
                          index = c("firm_id","year"))

# Check panel balance
cat("Panel balanced:", is.pbalanced(panel_data), "\n")
cat("Panel dimensions:", pdim(panel_data)$nT$n, "firms x",
    pdim(panel_data)$nT$T, "years\n")

# Model 1: Pooled OLS
model_pool <- plm(log(interest_rate) ~ owner_gender + log(annual_revenue) +
                    credit_score + firm_age + sector + log(loan_amount),
                  data   = panel_data,
                  model  = "pooling")

# Model 2: Entity fixed effects only
model_fe <- plm(log(interest_rate) ~ owner_gender + log(annual_revenue) +
                  credit_score + firm_age + sector + log(loan_amount),
                data   = panel_data,
                model  = "within",
                effect = "individual")

# Model 3: Two-way fixed effects (entity + time)
model_twfe <- plm(log(interest_rate) ~ owner_gender + log(annual_revenue) +
                    credit_score + firm_age + log(loan_amount),
                  data   = panel_data,
                  model  = "within",
                  effect = "twoways")

# Model 4: Two-way FE with interaction: gender x sector
model_interact <- plm(log(interest_rate) ~ owner_gender * sector +
                        log(annual_revenue) + credit_score +
                        firm_age + log(loan_amount),
                      data   = panel_data,
                      model  = "within",
                      effect = "twoways")

# Panel-robust standard errors
robust_twfe     <- coeftest(model_twfe,
                             vcov = vcovHC(model_twfe, type = "HC1",
                                           cluster = "group"))
robust_interact <- coeftest(model_interact,
                             vcov = vcovHC(model_interact, type = "HC1",
                                           cluster = "group"))

cat("\n--- Two-Way Fixed Effects (Robust SE) ---\n")
print(robust_twfe)

cat("\n--- Interaction Model (Robust SE) ---\n")
print(robust_interact)

# Hausman test: FE vs RE
model_re <- plm(log(interest_rate) ~ owner_gender + log(annual_revenue) +
                  credit_score + firm_age + log(loan_amount),
                data   = panel_data,
                model  = "random")

hausman <- phtest(model_fe, model_re)
cat("\n--- Hausman Test (FE vs RE) ---\n")
print(hausman)
cat("Conclusion:",
    ifelse(hausman$p.value < 0.05,
           "Fixed effects preferred (p < 0.05 — reject RE).",
           "Random effects preferred (p >= 0.05)."), "\n")

# F-test for time fixed effects
cat("\n--- F-test: Are time fixed effects significant? ---\n")
pFtest_time <- pFtest(model_twfe, model_fe)
print(pFtest_time)


# ── 8. HYPOTHESIS TEST: GENDER PREMIUM IN LENDING ─────────────
cat("\n--- SECTION 5: HYPOTHESIS TEST — GENDER PREMIUM ---\n")

# H0: No gender premium in interest rates (beta_female = 0)
# H1: Female-owned firms face higher rates (beta_female > 0)
# Using linearHypothesis on pooled model with robust SE

cat("Testing H0: beta(Female) = 0 in pooled OLS\n")
lh <- linearHypothesis(model_cs3,
                       "owner_genderFemale = 0",
                       vcov = vcovHC(model_cs3, type = "HC1"))
print(lh)

cat("\nInterpretation:\n")
fe_coef  <- coef(model_cs3)["owner_genderFemale"]
fe_pct   <- (exp(fe_coef) - 1) * 100
cat("Female-owned firms pay approximately",
    round(fe_pct, 2), "% more in interest rates,",
    ifelse(summary(model_cs3)$coefficients["owner_genderFemale", 4] < 0.05,
           "statistically significant at 5% level.",
           "not statistically significant at 5% level."), "\n")

cat("\n--- Panel Models Summary ---\n")
cat("Pooled OLS:\n"); print(summary(model_pool))
cat("\nTwo-Way FE:\n"); print(summary(model_twfe))

# ── 10. SAVE DATA ─────────────────────────────────────────────
write.csv(sme_data, "sme_lending_data.csv", row.names = FALSE)
cat("\nData saved: sme_lending_data.csv\n")
cat("\n=== ANALYSIS COMPLETE ===\n")
