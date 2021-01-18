library(dplyr)
library(formattable)

data <- read.csv("kpis_index_1A.csv")

#first round

agg <- data %>% 
  group_by(fiscal_year,sdg,Keyword, district, KPI) %>% 
  summarise(new_val = mean(value), new_index = mean(index_final))


kpis_max_min_final1<- agg %>%
  group_by(sdg,Keyword, KPI) %>%
  # Calculate and format
  summarise(min = min(new_index),
            max = max(new_index))

kpis_index_compare_final1<- agg %>%
  left_join(kpis_max_min_final1) %>%
  mutate(index = (new_index-min)/(max-min)) %>%
  mutate(index = if_else(is.nan(index), 1.00, index)) %>%
  select(Keyword,sdg,KPI,district,new_val,index)

#second round

agg <- data %>% 
  group_by(Keyword, district, KPI) %>% 
  summarise(new_val = mean(value), new_index = mean(index_final))


kpis_max_min_final1<- agg %>%
  group_by(fiscal_year, Keyword, KPI) %>%
  # Calculate and format
  summarise(min = min(new_index),
            max = max(new_index))

kpis_index_compare_final1<- agg %>%
  left_join(kpis_max_min_final1) %>%
  mutate(index = (new_index-min)/(max-min)) %>%
  mutate(index = if_else(is.nan(index), 1.00, index)) %>%
  select(Keyword,KPI,district,new_val,index)


#second round

usable_indicators <- kpis_index_compare_final1 %>% 
  group_by(district, KPI) %>% 
  summarise(final = mean(new_val), final_index = mean(index))


kpis_max_min_final_3<- usable_indicators %>%
  # Calculate and format
  summarise(min = min(final_index),
            max = max(final_index))

kpis_index_compare_final3<- usable_indicators %>%
  left_join(kpis_max_min_final_3) %>%
  mutate(index = (final_index-min)/(max-min)) %>%
  mutate(index = if_else(is.nan(index), 1.00, index))%>%
  select(KPI, district, final,index)

#write
write.csv(kpis_index_compare_final3, "indicator_list_agg.csv")
