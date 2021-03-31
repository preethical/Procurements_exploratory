library(dplyr)
library(formattable)

data <- read.csv("kpis_index_no_sdg.csv")

#first round

agg <- data %>% 
  group_by(fiscal_year,district, KPI) %>% 
  summarise(new_val = mean(value), new_index = mean(index_final))


kpis_max_min_final1<- agg %>%
  group_by(fiscal_year,KPI) %>%
  # Calculate and format
  summarise(min = min(new_val),
            max = max(new_val))

kpis_index_compare_final1<- agg %>%
  left_join(kpis_max_min_final1) %>%
  mutate(index = (new_val-min)/(max-min)) %>%
  mutate(index = if_else(is.nan(index), 1.00, index)) %>%
  select(KPI,fiscal_year, district,new_val,index)

#second round

#usable_indicators <- kpis_index_compare_final1 %>% 
 # group_by(fiscal_year,district, KPI) %>% 
  #summarise(final = mean(new_val), final_index = mean(index))


#kpis_max_min_final_3<- usable_indicators %>%
 # group_by(fiscal_year,KPI) %>%
  # Calculate and format
#  summarise(min = min(final_index),
 #           max = max(final_index))

#kpis_index_compare_final3<- usable_indicators %>%
 # left_join(kpis_max_min_final_3) %>%
#  mutate(index = (final_index-min)/(max-min)) %>%
#  mutate(index = if_else(is.nan(index), 1.00, index))%>%
#  select(KPI, fiscal_year,district, final,index)

#write
write.csv(kpis_index_compare_final1, "indicator_list_agg_year.csv")
