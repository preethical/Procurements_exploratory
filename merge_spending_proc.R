proc <- read.csv("data_proc_group - data_proc_group.csv", stringsAsFactors = FALSE)
spending <- read.csv("data_spen.csv", stringsAsFactors = FALSE)



newdat <- merge(proc,spending, by.x = c("fiscal_year","sdg", "Keyword","District"), by.y = c("fiscal_year","sdg", "Keyword","District"), all.x = TRUE, all.y = TRUE)


