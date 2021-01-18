library(formattable)
reduced_indicators_year <- read.csv("indicator_list_agg_year.csv", stringsAsFactors = FALSE)

# Efficiency
Efficiency_year_district <- reduced_indicators_year %>%
  filter(KPI == "Average days taken for bid evaluation" | 
           KPI == "Number of tenders concluded after validity days") %>%
  group_by(fiscal_year, district) %>%
  summarise(value = mean(new_val),mean_index = mean(index))

#index
kpis_max_min_efficiency<- Efficiency_year_district %>%
  # Calculate and format
  summarise(min = min(mean_index),
            max = max(mean_index))

kpis_index_efficiency <- Efficiency_year_district %>%
  left_join(kpis_max_min_efficiency) %>%
  mutate(new_index = (mean_index-min)/(max-min)) %>%
  mutate(new_index = if_else(is.nan(new_index),1.00,new_index))%>%
 select(fiscal_year,district,value,new_index)

# Planning
Planning_year_district <- reduced_indicators_year %>%
  filter(KPI == "Average difference awarded and published value" | 
           KPI == "Spending Per Capita") %>%
  group_by(fiscal_year, district) %>%
  summarise(value = mean(new_val),mean_index = mean(index))

#index
kpis_max_min_planning<- Planning_year_district %>%
  # Calculate and format
  summarise(min = min(mean_index),
            max = max(mean_index))

kpis_index_planning <- Planning_year_district %>%
  left_join(kpis_max_min_planning) %>%
  mutate(new_index = (mean_index-min)/(max-min)) %>%
  mutate(new_index = if_else(is.nan(new_index), 1.00, new_index))%>%
  select(fiscal_year,district,value,new_index)

# Participation
Participation_year_district <- reduced_indicators_year %>%
  filter(KPI == "Average days allowed for bid submission" | 
           KPI == "Average number of bids received per tender") %>%
  group_by(fiscal_year, district) %>%
  summarise(value = mean(new_val),mean_index = mean(index))

#index
kpis_max_min_participation <- Participation_year_district %>%
  # Calculate and format
  summarise(min = min(mean_index),
            max = max(mean_index))

kpis_index_participation <- Participation_year_district %>%
  left_join(kpis_max_min_participation) %>%
  mutate(new_index = (mean_index-min)/(max-min)) %>%
  mutate(new_index = if_else(is.nan(new_index), 1.00, new_index))%>%
  select(fiscal_year,district,value,new_index)

write.csv(kpis_index_efficiency, "efficiency_year.csv")
write.csv(kpis_index_planning, "planning_year.csv")
write.csv(kpis_index_participation,"participation_year.csv")
