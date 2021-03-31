library(dplyr)
data_spending <- read.csv("summary_spending_sdg_v2.csv", stringsAsFactors = F)
data_spend_year <- data_spending %>% group_by(fiscal_year,District,sub_minor_desc_sdg,SOE_description) %>% 
  summarise(total = mean(NETPAYMENT, na.rm = TRUE))