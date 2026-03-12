library(dplyr)
library(readr)
library(tidyr)
library(writexl)

ihme_raw <- read_csv("C:/Users/shara/Desktop/threshold/Multiplier_GBD_raw.csv")
hale_raw <- read_csv("C:/Users/shara/Desktop/threshold/HALE_GBD_raw.csv")
pop_raw <- read_csv("C:/Users/shara/Desktop/threshold/Prevalence_GBD_raw.csv") 

collapse_age <- function(df) {
  df %>%
    filter(!grepl("0-4|5-9|10-14|15-19", age_name)) %>%
    mutate(
      age_10yr = case_when(
        grepl("20-24|25-29", age_name) ~ "20-29",
        grepl("30-34|35-39", age_name) ~ "30-39",
        grepl("40-44|45-49", age_name) ~ "40-49",
        grepl("50-54|55-59", age_name) ~ "50-59",
        grepl("60-64|65-69", age_name) ~ "60-69",
        grepl("70|75|80|85|90|95", age_name) ~ "70+",
        TRUE ~ NA_character_
      )
    ) %>%
    filter(!is.na(age_10yr))
}

ihme_clean <- ihme_raw %>%
  collapse_age() %>%
  mutate(measure_name = case_when(
    grepl("YLD", measure_name) ~ "YLD",
    grepl("YLL", measure_name) ~ "YLL",
    TRUE ~ measure_name
  )) %>%
  group_by(year, sex_name, age_10yr, measure_name) %>%
  summarise(
    val = sum(val, na.rm = TRUE),
    upper = sum(upper, na.rm = TRUE),
    lower = sum(lower, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_wider(
    names_from = measure_name,
    values_from = c(val, upper, lower),
    names_glue = "{measure_name}_{.value}"
  ) %>%
  mutate(M_it = 1 + (YLD_val / YLL_val))

pop_clean <- pop_raw %>%
  collapse_age() %>%
  group_by(year, sex_name, age_10yr) %>%
  summarise(
    Population_val = sum(val, na.rm = TRUE),
    .groups = "drop"
  )

hale_clean <- hale_raw %>%
  collapse_age() %>%
  group_by(year, sex_name, age_10yr) %>%
  summarise(
    HALE_val = mean(val, na.rm = TRUE),
    HALE_upper = mean(upper, na.rm = TRUE),
    HALE_lower = mean(lower, na.rm = TRUE),
    .groups = "drop"
  )

final_panel <- ihme_clean %>%
  left_join(pop_clean, by = c("year", "sex_name", "age_10yr")) %>%
  left_join(hale_clean, by = c("year", "sex_name", "age_10yr")) %>%
  mutate(
    BoD_i = YLD_val / Population_val 
  ) %>%
  select(
    year, sex_name, age_10yr, 
    YLL_val, YLL_lower, YLL_upper,
    YLD_val, YLD_lower, YLD_upper,
    HALE_val, HALE_lower, HALE_upper,
    Population_val, M_it, BoD_i
  ) %>%
  arrange(year, sex_name, age_10yr)

write_xlsx(final_panel, "C:/Users/shara/Desktop/threshold/Final_Demographic_Parameters.xlsx")
write_csv(final_panel, "C:/Users/shara/Desktop/threshold/Final_Demographic_Parameters.csv")

cat("\n--- Final Demographic Parameters Compiled (with Uncertainty Bounds) ---\n")
print(head(final_panel))