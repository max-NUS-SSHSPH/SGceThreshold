library(dplyr)
library(readr)
library(tidyr)
library(writexl)
# Note: stats package is built into base R and contains kmeans()

# 1. Load data
ihme_raw <- read_csv("C:/Users/shara/Desktop/threshold/M_final.csv")

# 2. Mapping Dictionary
mapping_dict <- c(
  "Cardiovascular diseases" = "Cardiovascular disease",
  "Chronic respiratory diseases" = "Chronic respiratory disease",
  "Respiratory infections and TB" = "Respiratory Infection",
  "Enteric infections" = "Infectious and Parasitic Diseases",
  "Other infectious diseases" = "Infectious and Parasitic Diseases",
  "Neoplasms" = "Other neoplasms",
  "Neurological disorders" = "Neurological and sense disorders",
  "Mental disorders" = "Neurological and sense disorders",
  "Diabetes and kidney diseases" = "Endocrine disorders",
  "Musculoskeletal disorders" = "Musculoskeletal disease",
  "Digestive diseases" = "Digestive disease",
  "Skin and subcutaneous diseases" = "Skin diseases",
  "Other non-communicable diseases" = "Oral Diseases",
  "Maternal and newborn disorders" = "Maternal conditions",
  "Nutritional deficiencies" = "Nutritional deficiencies",
  "Self-harm and interpersonal violence" = "Intentional injuries",
  "Unintentional injuries" = "Unintentional injuries"
)

# 3. Clean and Age-Collapse (20+)
ihme_clean <- ihme_raw %>%
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
    ),
    sbod_category = mapping_dict[cause_name]
  ) %>%
  filter(!is.na(age_10yr))

# 4. Pivot and Aggregation
processed_base <- ihme_clean %>%
  group_by(year, sex_name, age_10yr, cause_name, sbod_category, measure_name) %>%
  summarise(sum_val = sum(val, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = measure_name, values_from = sum_val) %>%
  rename(
    YLD = `YLDs (Years Lived with Disability)`,
    YLL = `YLLs (Years of Life Lost)`
  )

# 5. Calculate the Demographic Population Averages
pop_avg <- processed_base %>%
  group_by(year, sex_name, age_10yr) %>%
  summarise(
    avg_yll = sum(YLL, na.rm = TRUE),
    avg_yld = sum(YLD, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(pop_multiplier = 1 + (avg_yld / avg_yll))

# 6. Calculate Raw Disease Multipliers & Morbidity Share (COLLAPSED)
disease_raw <- processed_base %>%
  filter(!is.na(sbod_category)) %>%
  group_by(year, sex_name, age_10yr, sbod_category) %>%
  summarise(
    total_yll = sum(YLL, na.rm = TRUE),
    total_yld = sum(YLD, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    raw_multiplier = 1 + (total_yld / total_yll),
    total_daly = total_yll + total_yld,
    pct_YLD = total_yld / total_daly # The clustering variable
  ) %>%
  # Filter out complete missing data so the clustering doesn't break
  filter(!is.na(pct_YLD))

# 7. Apply 1D K-Means Clustering to find the natural data threshold
# We ask the algorithm to group the pct_YLD into 2 centers (Mortality vs Morbidity)
set.seed(123) # Ensure reproducibility for your thesis
k_model <- kmeans(disease_raw$pct_YLD, centers = 2)

# Identify which cluster represents the "Morbidity-Dominant" group (the one with the higher mean)
morbidity_cluster_id <- which.max(k_model$centers)

# Find the exact dynamic threshold (the minimum pct_YLD of the morbidity cluster)
# This is the exact mathematical boundary the algorithm found.
dynamic_threshold <- min(disease_raw$pct_YLD[k_model$cluster == morbidity_cluster_id])

# 8. Apply the Dynamic Algorithm Rule
disease_multipliers <- disease_raw %>%
  left_join(pop_avg, by = c("year", "sex_name", "age_10yr")) %>%
  mutate(
    adjusted_multiplier_M = case_when(
      !is.finite(raw_multiplier) | pct_YLD >= dynamic_threshold ~ pop_multiplier,
      TRUE ~ raw_multiplier
    ),
    adjustment_flag = case_when(
      !is.finite(raw_multiplier) | pct_YLD >= dynamic_threshold ~ "Adjusted (Algorithmic Cluster)",
      TRUE ~ "None"
    )
  )

# --- AUDIT SECTION ---
high_multiplier_cases <- disease_multipliers %>%
  filter(adjustment_flag == "Adjusted (Algorithmic Cluster)") %>%
  select(year, sex_name, age_10yr, sbod_category, pct_YLD, raw_multiplier, adjusted_multiplier_M) %>%
  arrange(desc(pct_YLD))

cat("\n--- The K-Means Dynamic Threshold is strictly set by the data at:", round(dynamic_threshold * 100, 2), "% ---\n")
print(head(high_multiplier_cases, 10))
write_xlsx(high_multiplier_cases, "C:/Users/shara/Desktop/threshold/Algorithmic_Adjustment_Audit.xlsx")

# 9. Final Combined Table
final_table <- bind_rows(
  disease_multipliers %>% select(year, sex_name, age_10yr, sbod_category, raw_multiplier, adjusted_multiplier_M, adjustment_flag),
  pop_avg %>% mutate(
    sbod_category = "Population_Average",
    raw_multiplier = pop_multiplier,
    adjusted_multiplier_M = pop_multiplier,
    adjustment_flag = "Baseline"
  ) %>% 
    select(year, sex_name, age_10yr, sbod_category, raw_multiplier, adjusted_multiplier_M, adjustment_flag)
) %>%
  arrange(year, sex_name, age_10yr, sbod_category)

# 10. Export
write_xlsx(final_table, "C:/Users/shara/Desktop/threshold/Multipliers_Final_KMeans.xlsx")

# 10. Export for AWS Import Request
write_xlsx(final_table, "C:/Users/shara/Desktop/threshold/Multipliers_Final_KMeans.xlsx")
write_csv(final_table, "C:/Users/shara/Desktop/threshold/Multipliers_Final_KMeans.csv")