# ============================================================
# 04_tables_and_figures_updated.R
# Tables 1–3 and Figures 2–3
# 2022 Ghana Demographic and Health Survey
# ============================================================

library(tidyverse)
library(gtsummary)
library(ggplot2)
library(flextable)
library(officer)
library(survey)
library(broom)

# Create output folders -----------------------------------------------------

dir.create("outputs/tables", recursive = TRUE, showWarnings = FALSE)
dir.create("outputs/figures", recursive = TRUE, showWarnings = FALSE)


# ============================================================
# TABLE 1: Survey-weighted participant characteristics
# ============================================================

table1_np <- dhs_design_np %>%
  gtsummary::tbl_svysummary(
    by = obesity,
    include = c(
      age,
      residence,
      education,
      wealth
    ),
    statistic = list(
      all_continuous() ~ "{mean} ({mean.std.error})",
      all_categorical() ~ "{p}%"
    ),
    digits = list(
      all_continuous() ~ 1,
      all_categorical() ~ 1
    ),
    missing = "no",
    label = list(
      age ~ "Age, years",
      residence ~ "Place of residence",
      education ~ "Educational attainment",
      wealth ~ "Household wealth quintile"
    )
  ) %>%
  gtsummary::add_p(
    test = list(
      age ~ "svy.t.test",
      all_categorical() ~ "svy.chisq.test"
    )
  ) %>%
  gtsummary::modify_header(
    label ~ "**Characteristic**",
    stat_1 ~ "**Without obesity**  \n(n = 6,038)",
    stat_2 ~ "**With obesity**  \n(n = 1,018)",
    p.value ~ "**P-value**"
  ) %>%
  gtsummary::bold_labels()

print(table1_np)

table1_flex <- gtsummary::as_flex_table(table1_np)

flextable::save_as_docx(
  table1_flex,
  path = "outputs/tables/table1_participant_characteristics_weighted.docx"
)


# ============================================================
# TABLE 2: Survey-weighted obesity prevalence
# ============================================================

# Create ordered age groups in the analysis dataset.

ghana_ir_np <- ghana_ir_np %>%
  mutate(
    age_group = case_when(
      age >= 15 & age <= 24 ~ "15–24",
      age >= 25 & age <= 34 ~ "25–34",
      age >= 35 & age <= 44 ~ "35–44",
      age >= 45 & age <= 49 ~ "45–49",
      TRUE ~ NA_character_
    ),
    age_group = factor(
      age_group,
      levels = c("15–24", "25–34", "35–44", "45–49")
    )
  )

# Add age group to the existing survey design object.

dhs_design_np <- update(
  dhs_design_np,
  age_group = ghana_ir_np$age_group
)


# Survey-weighted prevalence by age group ----------------------------------

table2_age <- survey::svyby(
  ~obesity,
  ~age_group,
  dhs_design_np,
  survey::svymean,
  na.rm = TRUE
)


# Survey-weighted prevalence by residence ----------------------------------

table2_residence <- survey::svyby(
  ~obesity,
  ~residence,
  dhs_design_np,
  survey::svymean,
  na.rm = TRUE
)


# Survey-weighted prevalence by educational attainment ---------------------

table2_education <- survey::svyby(
  ~obesity,
  ~education,
  dhs_design_np,
  survey::svymean,
  na.rm = TRUE
)


# Survey-weighted prevalence by household wealth quintile ------------------

table2_wealth <- survey::svyby(
  ~obesity,
  ~wealth,
  dhs_design_np,
  survey::svymean,
  na.rm = TRUE
)


# Combine prevalence estimates into Table 2 -------------------------------

table2_np <- bind_rows(
  
  table2_age %>%
    as.data.frame() %>%
    transmute(
      Characteristic = "Age group",
      Category = as.character(age_group),
      Prevalence = obesity * 100,
      SE = se * 100
    ),
  
  table2_residence %>%
    as.data.frame() %>%
    transmute(
      Characteristic = "Place of residence",
      Category = as.character(residence),
      Prevalence = obesity * 100,
      SE = se * 100
    ),
  
  table2_education %>%
    as.data.frame() %>%
    transmute(
      Characteristic = "Educational attainment",
      Category = as.character(education),
      Prevalence = obesity * 100,
      SE = se * 100
    ),
  
  table2_wealth %>%
    as.data.frame() %>%
    transmute(
      Characteristic = "Household wealth quintile",
      Category = as.character(wealth),
      Prevalence = obesity * 100,
      SE = se * 100
    )
) %>%
  mutate(
    Prevalence = round(Prevalence, 1),
    SE = round(SE, 2)
  )

print(table2_np)


# Export Table 2 as CSV.

write.csv(
  table2_np,
  "outputs/tables/table2_weighted_obesity_prevalence.csv",
  row.names = FALSE
)


# Export Table 2 as Word.

table2_flex <- flextable::flextable(table2_np) %>%
  flextable::set_header_labels(
    Characteristic = "Characteristic",
    Category = "Category",
    Prevalence = "Prevalence (%)",
    SE = "SE"
  ) %>%
  flextable::autofit()

flextable::save_as_docx(
  table2_flex,
  path = "outputs/tables/table2_weighted_obesity_prevalence.docx"
)


# ============================================================
# TABLE 3: Survey-weighted multivariable logistic regression
# ============================================================

# weighted_model_np must have been fitted in an earlier analysis script.
# The model should include:
# obesity ~ age + residence + education + wealth

if (!exists("weighted_model_np")) {
  stop(
    "weighted_model_np was not found. Run the regression-analysis script ",
    "before running 04_tables_and_figures_updated.R."
  )
}


# Extract adjusted odds ratios, confidence intervals, and p-values.

table3_np <- broom::tidy(
  weighted_model_np,
  exponentiate = TRUE,
  conf.int = TRUE
) %>%
  filter(term != "(Intercept)") %>%
  mutate(
    estimate = round(estimate, 2),
    conf.low = round(conf.low, 2),
    conf.high = round(conf.high, 2),
    p.value = ifelse(
      p.value < 0.001,
      "<0.001",
      sprintf("%.3f", p.value)
    )
  )


# Create publication-ready labels.

table3_word <- table3_np %>%
  mutate(
    Variable = case_when(
      term == "age" ~ "Age (per one-year increase)",
      term == "residenceRural" ~ "Place of residence: Rural vs urban",
      term == "educationPrimary" ~ "Educational attainment: Primary vs no education",
      term == "educationSecondary" ~ "Educational attainment: Secondary vs no education",
      term == "educationHigher" ~ "Educational attainment: Higher vs no education",
      term == "wealthPoorer" ~ "Household wealth: Poorer vs poorest",
      term == "wealthMiddle" ~ "Household wealth: Middle vs poorest",
      term == "wealthRicher" ~ "Household wealth: Richer vs poorest",
      term == "wealthRichest" ~ "Household wealth: Richest vs poorest",
      TRUE ~ term
    ),
    `AOR` = sprintf("%.2f", estimate),
    `95% CI` = paste0(
      sprintf("%.2f", conf.low),
      "–",
      sprintf("%.2f", conf.high)
    ),
    `P-value` = p.value
  ) %>%
  select(
    Variable,
    AOR,
    `95% CI`,
    `P-value`
  )

print(table3_word)


# Export Table 3 as CSV.

write.csv(
  table3_word,
  "outputs/tables/table3_adjusted_odds_ratios.csv",
  row.names = FALSE
)


# Export Table 3 as Word.

table3_flex <- flextable::flextable(table3_word) %>%
  flextable::autofit()

flextable::save_as_docx(
  table3_flex,
  path = "outputs/tables/table3_adjusted_odds_ratios.docx"
)


# ============================================================
# FIGURE 2: Weighted obesity prevalence by wealth quintile
# ============================================================

# Figure 1 is the study-population flow diagram.
# Figure 2 therefore presents the survey-weighted wealth gradient.

wealth_plot_data_np <- table2_np %>%
  filter(Characteristic == "Household wealth quintile") %>%
  mutate(
    Category = factor(
      Category,
      levels = c(
        "Poorest",
        "Poorer",
        "Middle",
        "Richer",
        "Richest"
      )
    )
  )

wealth_upper_limit <- max(
  wealth_plot_data_np$Prevalence +
    wealth_plot_data_np$SE + 2,
  na.rm = TRUE
)

figure2_np <- ggplot(
  wealth_plot_data_np,
  aes(
    x = Category,
    y = Prevalence
  )
) +
  geom_col() +
  geom_errorbar(
    aes(
      ymin = pmax(Prevalence - SE, 0),
      ymax = Prevalence + SE
    ),
    width = 0.2
  ) +
  geom_text(
    aes(
      y = Prevalence + SE + 0.8,
      label = paste0(
        sprintf("%.1f", Prevalence),
        "%"
      )
    ),
    size = 4
  ) +
  scale_y_continuous(
    limits = c(0, wealth_upper_limit),
    expand = expansion(
      mult = c(0, 0.03)
    )
  ) +
  labs(
    title = "Weighted Obesity Prevalence by Household Wealth Quintile",
    subtitle = "Non-pregnant women aged 15–49 years, 2022 GDHS",
    x = "Household wealth quintile",
    y = "Obesity prevalence (%)",
    caption = "Error bars represent standard errors."
  ) +
  theme_minimal(base_size = 14)

print(figure2_np)

ggsave(
  filename = "outputs/figures/figure2_wealth_prevalence_np.png",
  plot = figure2_np,
  width = 8,
  height = 5,
  dpi = 300
)


# ============================================================
# FIGURE 3: Weighted obesity prevalence by age group
# ============================================================

age_plot_data_np <- table2_np %>%
  filter(Characteristic == "Age group") %>%
  mutate(
    Category = factor(
      Category,
      levels = c(
        "15–24",
        "25–34",
        "35–44",
        "45–49"
      )
    )
  )

age_upper_limit <- max(
  age_plot_data_np$Prevalence +
    age_plot_data_np$SE + 2,
  na.rm = TRUE
)

figure3_np <- ggplot(
  age_plot_data_np,
  aes(
    x = Category,
    y = Prevalence
  )
) +
  geom_col() +
  geom_errorbar(
    aes(
      ymin = pmax(Prevalence - SE, 0),
      ymax = Prevalence + SE
    ),
    width = 0.2
  ) +
  geom_text(
    aes(
      y = Prevalence + SE + 0.8,
      label = paste0(
        sprintf("%.1f", Prevalence),
        "%"
      )
    ),
    size = 4
  ) +
  scale_y_continuous(
    limits = c(0, age_upper_limit),
    expand = expansion(
      mult = c(0, 0.03)
    )
  ) +
  labs(
    title = "Weighted Obesity Prevalence by Age Group",
    subtitle = "Non-pregnant women aged 15–49 years, 2022 GDHS",
    x = "Age group (years)",
    y = "Obesity prevalence (%)",
    caption = "Error bars represent standard errors."
  ) +
  theme_minimal(base_size = 14)

print(figure3_np)

ggsave(
  filename = "outputs/figures/figure3_age_group_prevalence.png",
  plot = figure3_np,
  width = 8,
  height = 5,
  dpi = 300
)