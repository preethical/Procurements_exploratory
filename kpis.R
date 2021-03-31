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

# Create a KPI min/max table
kpis_max_min <- kpis_cleaned %>%
                  group_by(fiscal_year, KPI) %>%
                  # Calculate and format
                  summarise(min = accounting(min(value)),
                            Q10 = accounting(quantile(value, 0.10)),
                            Q90 = accounting(quantile(value, 0.90)),
                            max = accounting(max(value)))

kpis_max_min_xx <- kpis_cleaned %>%
  group_by(fiscal_year, KPI,  Keyword) %>%
  # Calculate and format
  summarise(min = min(value),
            Q10 = quantile(value, 0.10),
            Q90 = quantile(value, 0.90),
            max = max(value))

kpis_max_min_xx_all <- kpis_cleaned %>%
  group_by(fiscal_year,sdg, Keyword, KPI) %>%
  # Calculate and format
  summarise(min = min(value),
            Q10 = quantile(value, 0.10),
            Q90 = quantile(value, 0.90),
            max = max(value))

# KPI Quantiles Matching
kpi_quantiles <- kpis_cleaned %>%
                    left_join(kpis_max_min_xx_all) %>%
                    mutate(flag = if_else((value < Q10 | value > Q90), "N", "Y"))
                    # filter(flag == "Y")

# Box Plot
ggplot(kpi_quantiles, aes(x = fiscal_year, y = value)) +
  geom_boxplot() + #outlier.shape = NA) + 
  # scale_y_continuous(ylim = ) +
  facet_wrap(~ KPI, scales = "free_y")

# Export Max/Mins
#write_csv(kpis_max_min, "Documents/cdl/public_finance/ocp/flag/kpis_max_min.csv")

# Create Index for the data
kpis_index_1 <- kpis_cleaned %>%
  left_join(kpis_max_min_xx_all) %>%
  filter(KPI != "Fund Utilisation") %>%
  mutate(flag = if_else((value < Q10 | value > Q90), "N", "Y"),
         index = (value-Q10)/(Q90-Q10)) %>%
  mutate(index_1 = if_else((index > 1 & flag == "N"), 1.00, index)) %>%
  mutate(index_2 = if_else((index_1 < 0 & flag == "N"), 0.00, index_1)) %>%
  mutate(index_final = accounting(if_else(KPI == "Average days bid evaluation" |
                                 KPI == "Average difference awarded and published value" |
                                 KPI == "Number of tenders concluded after validity days",
                               1 - index_2, index_2))) %>%
  select(fiscal_year, sdg, Keyword, KPI, district, value, flag, index_final)

# Write CSV of indexed data
write_csv(kpis_index_1, "kpis_index_1.csv")
