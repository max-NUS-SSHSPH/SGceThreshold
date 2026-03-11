library(dplyr)
library(readr)
library(writexl)
# Note: If your file is actually an .xlsx, use library(readxl) and read_excel() instead.

# 1. Load HALE Data
hale_raw <- read_csv("C:/Users/shara/Desktop/threshold/HALE.csv")

# 2. Clean and Age-Collapse (20+)
hale_clean <- hale_raw %>%
  # Filter out pediatric age groups to match your thesis scope
  filter(!grepl("0-4|5-9|10-14|15-19", age_name)) %>%
  mutate(
    # Replicate the exact same 10-year collapse logic
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

# 3. Average the 5-year HALE values into 10-year HALE buckets
hale_final <- hale_clean %>%
  group_by(year, sex_name, age_10yr) %>%
  summarise(
    HALE = mean(val, na.rm = TRUE), 
    .groups = "drop"
  ) %>%
  arrange(year, sex_name, age_10yr)

# 4. Export HALE as a standalone reference file
write_xlsx(hale_final, "C:/Users/shara/Desktop/threshold/HALE_Cleaned.xlsx")
write_csv(hale_final, "C:/Users/shara/Desktop/threshold/HALE_Cleaned.csv")

cat("\n--- Standalone HALE Data Successfully Compiled ---\n")
print(head(hale_final))