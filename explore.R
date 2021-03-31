library(dplyr)


Data_final <- read.csv("summary_hp_health_spending_expenditure_fy_2017_18.csv", na ="",
                       stringsAsFactors = FALSE)

Data_final$sub_minor_desc_sdg <- ifelse(is.na(Data_final$sub_minor_desc_sdg), Data_final$minor_desc_sdg, Data_final$sub_minor_desc_sdg)
Data_final$sub_minor_desc_sdg <- ifelse(is.na(Data_final$sub_minor_desc_sdg), Data_final$sub_major_desc_sdg, Data_final$sub_minor_desc_sdg)


write.csv(Data_final, "summary_hp_health_spending_2017-18_V6.csv")