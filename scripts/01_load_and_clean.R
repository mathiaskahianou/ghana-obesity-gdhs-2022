library(haven)
library(tidyverse)
library(janitor)

ghana_ir <- read_dta("data_raw/GHIR8CFL.DTA")

ghana_ir <- clean_names(ghana_ir)

ghana_ir <- ghana_ir %>%
  mutate(
    bmi = ifelse(v445 >= 9996, NA, v445 / 100),
    
    obesity = case_when(
      bmi >= 30 ~ 1,
      bmi < 30 ~ 0,
      TRUE ~ NA_real_
    ),
    
    overweight_obesity = case_when(
      bmi >= 25 ~ 1,
      bmi < 25 ~ 0,
      TRUE ~ NA_real_
    ),
    
    bmi_category = case_when(
      bmi < 18.5 ~ "Underweight",
      bmi < 25 ~ "Normal",
      bmi < 30 ~ "Overweight",
      bmi >= 30 ~ "Obese",
      TRUE ~ NA_character_
    ),
    
    age = v012,
    
    residence = factor(
      v025,
      levels = c(1, 2),
      labels = c("Urban", "Rural")
    ),
    
    education = factor(
      v106,
      levels = c(0, 1, 2, 3),
      labels = c("No education", "Primary", "Secondary", "Higher")
    ),
    
    wealth = factor(
      v190,
      levels = c(1, 2, 3, 4, 5),
      labels = c("Poorest", "Poorer", "Middle", "Richer", "Richest")
    ),
    
    weight = v005 / 1000000
  )

# Final analytical dataset: non-pregnant women only
ghana_ir_np <- ghana_ir %>%
  filter(v213 == 0)

nrow(ghana_ir)
nrow(ghana_ir_np)

sum(!is.na(ghana_ir$bmi))
sum(!is.na(ghana_ir_np$bmi))