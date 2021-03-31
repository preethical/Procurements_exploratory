library (dplyr)
library(readr)
library(ggplot2) 
library(stringr)
library(tidyverse)
aoc_all <- read.csv("aoc_all_records.csv", stringsAsFactors = FALSE)
aoc_all_1 <-  read.csv("aoc_all_records_1.csv", stringsAsFactors = FALSE)
aoc_all_2 <- read.csv("aoc_all_records_2.csv", stringsAsFactors = FALSE)
aoc_all_3 <- read.csv("aoc_all_records_3.csv", stringsAsFactors = FALSE)

aoc_merge <- rbind(aoc_all,aoc_all_1,aoc_all_2,aoc_all_3)
aoc_merge[!duplicated(aoc_merge), ]

aoc_merge$AOC.Date <- as.Date(aoc_merge$AOC.Date, format = "%d-%b-%Y")
aoc_merge$e.Published.Date <- as.Date(aoc_merge$e.Published.Date, format = "%d-%b-%Y")

aoc_merge_state <- aoc_merge %>% group_by(State.Name, Tender.Type)%>% summarise(total = sum(Contract.Value, na.rm = TRUE))

plot_1<- aoc_merge_state %>% top_n(10, total) %>% ggplot() + aes(x = State.Name, y=total, fill = Tender.Type)+ geom_bar(stat = "identity", position = "dodge",width = 0.7)

print(plot_1)



