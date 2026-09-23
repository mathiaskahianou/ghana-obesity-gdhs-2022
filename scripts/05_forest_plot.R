# ============================================================
# 05_forest_plot.R
# Figure 4: Adjusted odds ratios for obesity
# 2022 Ghana Demographic and Health Survey
# ============================================================

library(tidyverse)
library(broom)
library(ggplot2)

# Create output folder -----------------------------------------------------

dir.create(
  "outputs/figures",
  recursive = TRUE,
  showWarnings = FALSE
)


# ============================================================
# PREPARE FOREST-PLOT DATA
# ============================================================

# weighted_model_np is created in the survey/regression analysis script.

if (!exists("weighted_model_np")) {
  stop(
    "weighted_model_np was not found. Run the survey/regression ",
    "analysis script before running 05_forest_plot.R."
  )
}


# Extract adjusted odds ratios and 95% confidence intervals
# from the final survey-weighted regression model.

forest_data_np <- broom::tidy(
  weighted_model_np,
  exponentiate = TRUE,
  conf.int = TRUE
) %>%
  filter(term != "(Intercept)") %>%
  mutate(
    Variable = case_when(
      term == "age" ~ "Age (per one-year increase)",
      term == "residenceRural" ~ "Rural residence",
      term == "educationPrimary" ~ "Primary education",
      term == "educationSecondary" ~ "Secondary education",
      term == "educationHigher" ~ "Higher education",
      term == "wealthPoorer" ~ "Poorer wealth quintile",
      term == "wealthMiddle" ~ "Middle wealth quintile",
      term == "wealthRicher" ~ "Richer wealth quintile",
      term == "wealthRichest" ~ "Richest wealth quintile",
      TRUE ~ term
    ),
    Variable = factor(
      Variable,
      levels = c(
        "Richest wealth quintile",
        "Richer wealth quintile",
        "Middle wealth quintile",
        "Poorer wealth quintile",
        "Higher education",
        "Secondary education",
        "Primary education",
        "Rural residence",
        "Age (per one-year increase)"
      )
    )
  )


# ============================================================
# FIGURE 4: FOREST PLOT
# ============================================================

figure4_np <- ggplot(
  forest_data_np,
  aes(
    x = estimate,
    y = Variable
  )
) +
  geom_point(size = 3) +
  geom_errorbarh(
    aes(
      xmin = conf.low,
      xmax = conf.high
    ),
    height = 0.2
  ) +
  geom_vline(
    xintercept = 1,
    linetype = "dashed"
  ) +
  scale_x_log10() +
  labs(
    title = "Adjusted Odds Ratios for Obesity",
    subtitle = paste0(
      "Survey-weighted logistic regression among non-pregnant women ",
      "aged 15–49 years, 2022 GDHS"
    ),
    x = "Adjusted odds ratio (95% CI, log scale)",
    y = NULL,
    caption = paste0(
      "Reference categories: urban residence, no education, ",
      "and the poorest household wealth quintile."
    )
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(
      face = "bold",
      size = 16
    ),
    plot.subtitle = element_text(
      size = 11
    ),
    plot.caption = element_text(
      size = 10,
      hjust = 0
    ),
    axis.text.y = element_text(
      size = 11
    )
  )

print(figure4_np)


# Save Figure 4 ------------------------------------------------------------

ggsave(
  filename = "outputs/figures/figure4_adjusted_odds_ratios_forest_plot.png",
  plot = figure4_np,
  width = 10,
  height = 6,
  units = "in",
  dpi = 300
)