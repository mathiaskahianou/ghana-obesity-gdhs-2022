library(tidyverse)

# Obesity by residence
table(ghana_ir_np$residence, ghana_ir_np$obesity)
prop.table(table(ghana_ir_np$residence, ghana_ir_np$obesity), margin = 1)

# Obesity by education
table(ghana_ir_np$education, ghana_ir_np$obesity)

# Obesity by wealth
table(ghana_ir_np$wealth, ghana_ir_np$obesity)

# BMI category distribution
table(ghana_ir_np$bmi_category, useNA = "ifany")

# Crude obesity prevalence by wealth
wealth_plot_data <- ghana_ir_np %>%
  group_by(wealth) %>%
  summarise(
    obesity_prevalence = mean(obesity, na.rm = TRUE) * 100,
    .groups = "drop"
  )

wealth_plot_data