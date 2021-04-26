library (dplyr)
library(readr)
library(ggplot2) 
library(stringr)
library(tidyverse)
activetenders <- read.csv("Active_tender_health_2021_26_04 - Sheet2.csv", stringsAsFactors = FALSE)

#unidff <- activetenders[duplicated(activetenders$Title.Ref.No..Tender.Id), ]
nonunidiff <- activetenders[!duplicated(activetenders$Title.Ref.No..Tender.Id), ]
#activetend <- unique(activetenders[ , 5])
#distinct_data <- distinct(activetenders)

#newdata <- setdiff(distinct_data, nonunidiff)

nonunidiff$e.Published.Date <- as.Date(nonunidiff$e.Published.Date, format = "%d-%b-%Y")
nonunidiff$Bid.Submission.Closing.Date <- as.Date(nonunidiff$Bid.Submission.Closing.Date, format = "%d-%b-%Y")
nonunidiff$Tender.Opening.Date<- as.Date(nonunidiff$Tender.Opening.Date, format = "%d-%b-%Y")

##Clean the tender description into two parts - tender name and ref number and org id
nonunidiff <- nonunidiff %>% separate (Title.Ref.No..Tender.Id, into = c("Title_name","Ref_ID"), sep = "\\/", extra = "merge")
nonunidiff <- nonunidiff %>% 
  separate(Ref_ID, into = c("Ref", "Tender ID"), sep="/(?=[^/]+$)")


nonunidiff_state<- nonunidiff %>% group_by(State.Name)%>% tally()
plot1 <- nonunidiff %>% ggplot(aes(x = State.Name))+ 
  geom_bar ()+
  theme(axis.text.x = element_text(size = 6, angle = 90))
print (plot1)



#Here we have calculated the number of days between the published and awarded date
nonunidiff$number_days_submission<-
  as.numeric(difftime (nonunidiff$Bid.Submission.Closing.Date, 
                       nonunidiff$e.Published.Date,units = c("days")))


nonunidiff$number_days_opening <- as.numeric(difftime (nonunidiff$Tender.Opening.Date, 
                                                  nonunidiff$e.Published.Date,units = c("days")))


plot2 <- nonunidiff %>% ggplot(aes(x=State.Name, fill = number_days_submission))+  
  geom_boxplot()+
  theme(axis.text.x = element_text(size = 6, angle = 90))

print(plot2)
write.csv(nonunidiff, "Activetender_26_04_2021.csv")