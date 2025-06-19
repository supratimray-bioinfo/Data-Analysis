
# Load libraries
library(tidyverse)
library(lubridate)
library(scales)

# Read and Inspect Data
clin_trials <- read.csv("/cloud/project/AERO-BirdsEye-Data.csv")
head(clin_trials)
summary(clin_trials)
glimpse(clin_trials)

# Data Cleaning
clin_trials <- clin_trials %>%
  mutate(
    Start_Year = as.character(Start_Year),
    Start_Month = str_pad(as.character(Start_Month), width = 2, pad = "0"),
    Start_Date = paste0(Start_Year, "-", Start_Month),
    Start_Date = ym(Start_Date)  # Convert to Year-Month
  )

# Checking missing values
colSums(is.na(clin_trials))

# Sponsor Stats
sponsor_stats <- clin_trials %>%
  count(Sponsor, name = "Number_of_Trials") %>%
  mutate(pct_of_Total_Trials = percent(Number_of_Trials / sum(Number_of_Trials), accuracy = 0.1))

# Phase-wise percentages
phase1_pct <- clin_trials %>%
  filter(Phase == "Phase 1") %>%
  count(Sponsor) %>%
  mutate(`pct_of_All_Phase_1` = percent(n / sum(n), accuracy = 0.01)) %>%
  select(Sponsor, `pct_of_All_Phase_1`)

phase2_pct <- clin_trials %>%
  filter(Phase == "Phase 2") %>%
  count(Sponsor) %>%
  mutate(`pct_of_All_Phase_2` = percent(n / sum(n), accuracy = 0.01)) %>%
  select(Sponsor, `pct_of_All_Phase_2`)

phase3_pct <- clin_trials %>%
  filter(Phase == "Phase 3") %>%
  count(Sponsor) %>%
  mutate(`pct_of_All_Phase_3` = percent(n / sum(n), accuracy = 0.01)) %>%
  select(Sponsor, `pct_of_All_Phase_3`)

# Merge all stats
sponsor_stats <- sponsor_stats %>%
  left_join(phase1_pct, by = "Sponsor") %>%
  left_join(phase2_pct, by = "Sponsor") %>%
  left_join(phase3_pct, by = "Sponsor") %>%
  arrange(desc(Number_of_Trials))

# Donut Chart: All Trials by Sponsor
sponsor_stats <- sponsor_stats %>%
  mutate(pct = Number_of_Trials / sum(Number_of_Trials),
         pct_label = percent(pct, accuracy = 0.1))

ggplot(sponsor_stats, aes(x = "", y = pct, fill = Sponsor)) +
  geom_col(width = 1, color = "white") +
  coord_polar(theta = "y") +
  geom_text(aes(label = pct_label), position = position_stack(vjust = 0.5), size = 3) +
  labs(title = "Share of Sponsors for All Trials") +
  theme_void(base_size = 14) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        legend.position = "right")

# Sponsor Valuation Bar Plot
sponsor_value <- data.frame(
  Sponsor = c("AbbVie", "Bayer", "GSK", "Gilead", "JNJ", 
              "Merck", "Novartis", "Pfizer", "Roche", "Sanofi"),
  Valuation = c(152.12, 13.17, 37.43, 75.67, 161.45, 
                107.52, 102.63, 34.07, 36.38, 110.00)
)

ggplot(sponsor_value, aes(x = reorder(Sponsor, Valuation), y = Valuation)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(title = "Sponsor Stock Valuations (as of 2023-09-17)",
       x = "Sponsor", y = "Valuation (USD)") +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

# Top 10 Most Studied Conditions
top10 <- clin_trials %>%
  count(Condition, name = "Total_Trials") %>%
  arrange(Total_Trials) %>%
  slice_tail(n = 10)

ggplot(top10, aes(x = reorder(Condition, Total_Trials), y = Total_Trials)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(title = "The 10 Most Studied Conditions",
       x = "Condition", y = "Total Trials") +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))


# Halted Trials Donut Chart
halt_df <- clin_trials %>%
  filter(Status %in% c("Withdrawn", "Terminated", "Suspended"))

halt_stats <- halt_df %>%
  count(Status) %>%
  mutate(
    pct = n / sum(n),
    label = paste0(Status, " (", percent(pct, accuracy = 0.1), ")")
  )

ggplot(halt_stats, aes(x = 2, y = pct, fill = Status)) +
  geom_col(width = 1, color = "white") +
  coord_polar(theta = "y") +
  xlim(0.5, 2.5) +
  geom_text(aes(label = label), position = position_stack(vjust = 0.5), size = 4) +
  labs(title = "Distribution of Halted Trial Statuses") +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        legend.position = "none")
