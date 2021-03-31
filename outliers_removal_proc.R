library(dplyr)
library(psych)
data <- read.csv("procurment_water_health - procurment_water_health.csv", stringsAsFactors = FALSE)

bid_dat <- data %>% 
  group_by(fiscal_year,sdg,Keyword, district)

#bid submission days

outliers <- boxplot(bid_dat$number_of_bid_submission_days)$out

bid_sub <- bid_dat[-which(bid_dat$number_of_bid_submission_days %in% outliers),]

boxplot(bid_sub$number_of_bid_submission_days) 

submission_Days <- bid_sub %>% group_by(fiscal_year,sdg,Keyword, district) %>% summarise(total = geometric.mean(number_of_bid_submission_days, na.rm = TRUE))

write_csv(submission_Days, "submission.csv")

#bid evaluation days

outliers_eval <- boxplot(bid_dat$number_of_bid_evaluation_days)$out 

bid_eval<- bid_dat[-which(bid_dat$number_of_bid_evaluation_days %in% outliers),]

boxplot(bid_eval$number_of_bid_evaluation_days)

evaluation_days <- bid_eval %>% group_by(fiscal_year,sdg,Keyword, district) %>% 
  summarise(total = geometric.mean(number_of_bid_evaluation_days, na.rm = TRUE))

#bid validity days

bid_dat$invalidnumber <- ifelse(bid_dat$number_of_days_after_validity_date >= 0, "1", "0") 

validity <- bid_dat %>% group_by(fiscal_year,sdg,Keyword, district) %>% summarise(total = sum(as.numeric(invalidnumber)))

#difference in value

bid_dat$difference <- bid_dat$contract_value - bid_dat$tender_value

outliers_bidvalue <- boxplot(bid_dat$difference)$out 

bid_value<- bid_dat[-which(bid_dat$difference %in% outliers),]

boxplot(bid_value$difference)

value <- bid_dat %>% group_by(fiscal_year,sdg, Keyword, district) %>% summarise(total = geometric.mean(difference, na.rm = TRUE))

#number of bids recieved

outliers <- boxplot(bid_dat$Number.of.bids.received)$out

bid_num <- bid_dat[-which(bid_dat$Number.of.bids.received %in% outliers),]

number <- bid_num %>% group_by(fiscal_year,sdg,Keyword, district) %>% summarise(total = geometric.mean(Number.of.bids.received, na.rm = TRUE))                                                                             
                                                                               
boxplot(bid_num$Number.of.bids.received)                                                                                














#outliers_ <- boxplot(bid_dat$number_of_bid_evaluation_days)$out 

#bid_eval<- bid_dat[-which(bid_dat$number_of_bid_evaluation_days %in% outliers),]

#boxplot(bid_eval$number_of_bid_evaluation_days)





#remove_outliers <- function(x, na.rm = TRUE, ...) {
 # qnt <- quantile(x, probs=c(.25, .75), na.rm = na.rm, ...)
#  H <- 1.5 * IQR(x, na.rm = na.rm)
#  y <- x
#  y[x < (qnt[1] - H)] <- NA
#  y[x > (qnt[2] + H)] <- NA
#  y
#}

#set.seed(1)

#y <- remove_outliers(bid_sub$number_of_bid_submission_days)

#boxplot(y)

