# ============================================================
# Survey-weighted analysis
# 2022 Ghana Demographic and Health Survey
# ============================================================

library(survey)
library(broom)


# ============================================================
# CREATE SURVEY DESIGN OBJECT
# ============================================================

dhs_design_np <- svydesign(
  ids = ~v021,
  strata = ~v023,
  weights = ~weight,
  data = ghana_ir_np,
  nest = TRUE
)


# ============================================================
# OVERALL WEIGHTED OBESITY PREVALENCE
# ============================================================

overall_obesity_prevalence <- svymean(
  ~obesity,
  dhs_design_np,
  na.rm = TRUE
)

print(overall_obesity_prevalence)


# ============================================================
# WEIGHTED BMI CATEGORY DISTRIBUTION
# ============================================================

weighted_bmi_distribution <- prop.table(
  svytable(
    ~bmi_category,
    dhs_design_np
  )
) * 100

print(weighted_bmi_distribution)


# ============================================================
# SURVEY-WEIGHTED MULTIVARIABLE LOGISTIC REGRESSION
# ============================================================

weighted_model_np <- svyglm(
  obesity ~ age + residence + education + wealth,
  design = dhs_design_np,
  family = quasibinomial()
)

summary(weighted_model_np)


# ============================================================
# ADJUSTED ODDS RATIOS AND 95% CONFIDENCE INTERVALS
# ============================================================

adjusted_odds_ratios <- exp(
  coef(weighted_model_np)
)

adjusted_confidence_intervals <- exp(
  confint(weighted_model_np)
)

print(adjusted_odds_ratios)
print(adjusted_confidence_intervals)


# ============================================================
# CLEAN REGRESSION RESULTS
# Used downstream to produce Table 3 and Figure 4
# ============================================================

regression_results_np <- broom::tidy(
  weighted_model_np,
  exponentiate = TRUE,
  conf.int = TRUE
)

print(regression_results_np)