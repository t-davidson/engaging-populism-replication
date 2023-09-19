# Calculating descriptive discussed in paper (comparing populism measures) and appendix B

library(tidyverse)
`%notin%` <- Negate(`%in%`) # Defining a helper function 

# Loading final datasets
data.fb <- read_csv("../../data/model_data/model_dataset_fb.csv")
data.tw <- read_csv("../../data/model_data/model_dataset_tw.csv")

# Proportion of party-years featuring a populist party
round(sum(data.fb$populist)/nrow(data.fb),3)
round(sum(data.tw$populist)/nrow(data.tw),3)

# Getting party-level data
parties.fb <- data.fb %>% group_by(parlgov_id) %>% select(populist, gps_populism, poppa_populism_mean) %>%
    distinct(.keep_all = T)
parties.tw <- data.tw %>% group_by(parlgov_id) %>% select(populist, gps_populism, poppa_populism_mean) %>%
    distinct(.keep_all = T)

# Getting all parties across both datasets
parties.all <- bind_rows(parties.fb, parties.tw) %>% distinct(.keep_all = T)

# Association between PopuList and other metrics
# These calculations automatically ignore NAs
print(t.test(poppa_populism_mean ~ populist, data = parties.all)) # 3.19 vs 7.56
print(t.test(gps_populism ~ populist, data = parties.all)) # 4.2 vs 8.16
print(cor.test(parties.all$poppa_populism_mean, parties.all$gps_populism)) # r = 0.86

# Proportion of parties considered populist (PopuList)
# Subtracting 1 to avoid double-counting Sinn Fein
round( (sum(parties.fb$populist)-1) / (nrow(parties.fb)-1) ,2)
round( (sum(parties.tw$populist)-1)/ (nrow(parties.tw)-1) ,2)

# Calculating average populism scores by dataset
# Excluding parties missing scores
round(mean(parties.fb$poppa_populism_mean, na.rm = T),1)
round(mean(parties.fb$gps_populism, na.rm = T),1)
round(mean(parties.tw$poppa_populism_mean, na.rm = T),1)
round(mean(parties.tw$gps_populism, na.rm = T),1)
