library(formattable)

# Read the file
kpis <- read.csv("submission.csv")

# Convert district data to row level
#kpis_cleaned <- kpis %>%
 # gather(district,total,-fiscal_year,-sdg,-Keyword) %>%

kpis_max_min_sub<- kpis %>%
  group_by(fiscal_year) %>%
  # Calculate and format
  summarise(min = min(total),
            max = max(total))

kpis_index_compare_sub <- kpis%>%
  left_join(kpis_max_min_sub) %>%
  #filter(KPI != "Fund Utilisation") %>%
  mutate(index = (total-min)/(max-min)) %>%
  #mutate(index_1 = if_else((index > 1), 1.00, index)) %>%
  #mutate(index_2 = if_else((index_1 < 0), 0.00, index_1)) %>%
  mutate(index_final= as.numeric(1 - index)) %>% 
  select(fiscal_year, sdg, Keyword, total, district,index_final)
boxplot(kpis_index_compare_sub$index_final)

write_csv(kpis_index_compare_sub, "kpis_index_compare_sub.csv") 



