# Load Packages
library(tidyverse)
library(readxl)
library(formattable)

# Read the file
kpis <- read.csv("kpis_rounded_v2.1 - Sheet1.csv")

# Convert district data to row level
kpis_cleaned <- kpis %>%
  gather(district, value, -fiscal_year, -sdg, -Keyword, -KPI) %>%
  filter(!is.na(value))

kpis_max_min_xx <- kpis_cleaned %>%
  group_by(fiscal_year, KPI, Keyword) %>%
  # Calculate and format
  summarise(min = min(value),
            max = max(value))

kpis_index_compare <- kpis_cleaned %>%
  left_join(kpis_max_min_xx) %>%
  filter(KPI != "Fund Utilisation") %>%
  mutate(index = (value-min)/(max-min)) %>%
  #mutate(index_1 = if_else((index > 1), 1.00, index)) %>%
  #mutate(index_2 = if_else((index_1 < 0), 0.00, index_1)) %>%
  mutate(index_final = accounting(if_else(KPI == "Average days bid evaluation" |
                                            KPI == "Average difference awarded and published value" |
                                            KPI == "Number of tenders concluded after validity days",
                                          1 - index, index))) %>% 
    select(fiscal_year, sdg, Keyword, KPI, district, value, index_final)
  
write_csv(kpis_index_compare, "kpis_index_compare.csv") 
  
  
  
  
  
  
  