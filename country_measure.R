# setting WD ----
setwd("~/Documents/Study/FSU MSc/Thesis/OECD_REGPAT_202208")


# Loading library ----
library(tidyverse)




# raw .txt loading ----
EPO_Inv <- read.csv("202308_EPO_inv_reg.txt", header = TRUE, sep = "|")
EPO_IPC <- read.csv("202308_EPO_IPC.txt", header = TRUE, sep = "|")

# Saving as .rbd extension for better compatibility ----
save(EPO_Inv, file = "com_EPO_Inv.rdb")
save(EPO_IPC, file = "com_EPO_IPC.rdb")

# Dropping big DFs 
rm(EPO_Inv)
rm(EPO_IPC)

# Loading the .rdb ----
load("com_EPO_Inv.rdb")
load("com_EPO_IPC.rdb")


# creating datasets ----
EPO_IPC_new <- EPO_IPC %>% 
  filter(substr(IPC, 1, 4) == "F03D") 

j_epo <- right_join(EPO_IPC_new, EPO_Inv, by = "appln_id") # joining two of these DFs using "appln_id" for 
colnames(j_epo)



j_epo <- j_epo %>% 
  select(-prio_year,
         -person_id,
         -address,
         -reg_code,
         -reg_share,
         -inv_share)

View(head(j_epo, 10))


new <- j_epo %>% 
  group_by(ctry_code) %>% 
  summarise(count = n()) %>%  # filtering to get the top 5 countries
  arrange(desc(count))



j_epo <- j_epo %>% 
  filter(ctry_code == c("DE", "FR", "GB", "IT", "NL")) # filtering the countries after getting the top 5 countries



save(j_epo, file = "j_epo.rdb") # saving as .rdb to utilize in the main analysis in "test_eu_f03d.R"

# Dropping DFs
rm(EPO_Inv)
rm(EPO_IPC)
rm(EPO_IPC_new)
rm(j_epo)
rm(new)

