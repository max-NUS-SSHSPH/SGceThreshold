library(dplyr)
library(readr)
library(tidyr)
library(writexl)

# 1. Load Data
ihme_raw <- read_csv("C:/Users/shara/Desktop/threshold/Multiplier_GBD_raw.csv")
hale_raw <- read_csv("C:/Users/shara/Desktop/threshold/HALE_GBD_raw.csv")
# Adjust the extension if it is an .xlsx file instead of .csv
pop_raw <- read_csv("C:/Users/shara/Desktop/threshold/Prevalence_GBD_raw.csv") 

# 2. Define a helper function to apply the exact same age-collapsing logic (20+) to all datasets
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

# 3. Process Morbidity and Mortality (YLL and YLD)
ihme_clean <- ihme_raw %>%
  collapse_age() %>%
  group_by(year, sex_name, age_10yr, measure_name) %>%
  summarise(sum_val = sum(val, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = measure_name, values_from = sum_val) %>%
  rename(
    YLD = `YLDs (Years Lived with Disability)`,
    YLL = `YLLs (Years of Life Lost)`
  ) %>%
  # Calculate the Morbidity Multiplier (Mit)
  mutate(M_it = 1 + (YLD / YLL))

# 4. Process Population from the Prevalence/Population GBD file
# Note: Assuming 'val' in this file represents the population count. 
# If there is a specific measure_name for population, add a filter() step here.
pop_clean <- pop_raw %>%
  collapse_age() %>%
  group_by(year, sex_name, age_10yr) %>%
  summarise(Population = sum(val, na.rm = TRUE), .groups = "drop")

# 5. Process HALE
hale_clean <- hale_raw %>%
  collapse_age() %>%
  group_by(year, sex_name, age_10yr) %>%
  summarise(HALE = mean(val, na.rm = TRUE), .groups = "drop")

# 6. Merge Everything into a Final Demographic Panel and Calculate BoDi
final_panel <- ihme_clean %>%
  left_join(pop_clean, by = c("year", "sex_name", "age_10yr")) %>%
  left_join(hale_clean, by = c("year", "sex_name", "age_10yr")) %>%
  mutate(
    # Calculate the Background Disease Burden
    BoD_i = YLD / Population 
  ) %>%
  select(year, sex_name, age_10yr, YLL, YLD, Population, HALE, M_it, BoD_i) %>%
  arrange(year, sex_name, age_10yr)

# 7. Export the Final Unified Dataset
write_xlsx(final_panel, "C:/Users/shara/Desktop/threshold/Final_Demographic_Parameters.xlsx")
write_csv(final_panel, "C:/Users/shara/Desktop/threshold/Final_Demographic_Parameters.csv")

cat("\n--- Final Demographic Parameters Successfully Compiled ---\n")
print(head(final_panel))