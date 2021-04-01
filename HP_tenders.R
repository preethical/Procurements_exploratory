##HP geospatial stuff

library(dplyr)
library(stringr)
HPtenders <- read.csv("combined_tenders_results_with_repeats.csv",row.names = NULL,
                      stringsAsFactors = FALSE)

HPawards <- read.csv("health_award.csv",row.names = NULL,
                     stringsAsFactors = FALSE)

HPtend <- read.csv("final_merged.csv",row.names = NULL,
                   stringsAsFactors = FALSE)

tehsil <- read.csv("tehsil.csv", row.names = NULL,
                   stringsAsFactors = FALSE )

patterns <-data.frame(word = c("Medical", "Hospital", "PHC", "CHC", "covid", "COVID", "medicine", "drugs", "liquid", 
                               "ambulance",  "microscope", "chemistry", "radiograph", "biomedical", "laboratory", "dispensary", 
                               "Anganwadi","DHS","subcentre", "ANM", "sub centre", "HWC", "RBSK", "JSSK", "KASP","crematorium",
                               "mortem","paediatric","^MCH","Smashanbhumi","Shmashanbhumi","smashan bhumi","HFW","ICDS","RT-PCR",
                               "PPE","corona","anganwari","NHM","Advia","Oxygen","^HCG","AWC$","mask"))

#pattern_match <- data.frame(word = c("Medical","Hospital","PHC","CHC","Health","covid","medicine","drugs","liquid", "ambulance","microscope","chemistry", "radiograph","biomedical","lab","laboratory","dispensary","Anganwadi","DHS","subcentre", "ANM", "sub centre", "HWC", "RBSK", "JSSK", "KASP", "crematorium", "DNASTAR","mortem", "paediatric", "HWC", "MCH", "Smashanbhumi", "Shmashanbhumi", "smashan bhumi", "OPD", "HFW", "ICDS","RT-PCR","corona","anganwari","NHM", "TCL","Advia","Oxygen","HCG","AWC","DGPCS"), matching = c("1l", "2", "3", "4","5", "6", "7", "8", "9", "10","11","12","13","14","15","16","17","18","19","20","21","22","23","24","25","26","27","28","29","30","21","32","33","34","35","36", "37", "38","39","40","41", "42", "43", "44", "45", "46", "47", "48"))


newtable <- HPawards%>% 
  filter_all(any_vars(str_detect(.,paste(patterns$word, collapse = "|"))))

for(i in seq_len(nrow(tehsil))) {
  want <- grepl(tehsil[i, "X.1"], newtable[,"Title.and.Ref.No..Tender.Id"],ignore.case = T)
  newtable[want, "X.1"] <- tehsil[i, "X.1"]
}   



#newtable <- newtable %>% 
#  filter(!grepl("Vet",Title_description))
