library(tidyverse)
`%notin%` <- Negate(`%in%`) 

# Loading data
data.fb <- read_csv("../../data/model_data/model_dataset_fb_posts.csv")
data.tw <- read_csv("../../data/model_data/model_dataset_tw_tweets.csv")

###################################
#       DATA PROCESSING           #
###################################

# Data merged in full_merge_script.R
# Data cleaned in final_data_cleaning.R
# Final filtering here is dependent on outcomes

# Setting year to factor (some post-model analyses break if this is done in the model formula)
data.fb$year.f <- as.factor(data.fb$year)
data.tw$year.f <- as.factor(data.tw$year)

# Setting country and handle as factors
data.fb$country	<- as.factor(data.fb$country)
data.tw$country	<- as.factor(data.tw$country)
data.fb$handle <- as.factor(data.fb$handle)
data.tw$twitter_handle <- as.factor(data.tw$twitter_handle)

# Adding country-years
data.fb <- data.fb %>%
    unite(country_year, c(country, year.f),sep = "_", remove = FALSE) %>%
    mutate(country_year = as.factor(country_year))

data.tw <- data.tw %>%
    unite(country_year, c(country, year.f),sep = "_", remove = FALSE) %>%
    mutate(country_year = as.factor(country_year))

# Setting other dummies as factors
data.fb$populist <- as.factor(data.fb$populist)
data.fb$cabinet_party_max <- as.factor(data.fb$cabinet_party_max)
data.fb$prime_minister_max <- as.factor(data.fb$prime_minister_max)
data.fb$election_year <- as.factor(data.fb$election_year)
data.tw$populist <- as.factor(data.tw$populist)
data.tw$cabinet_party_max <- as.factor(data.tw$cabinet_party_max)
data.tw$prime_minister_max <- as.factor(data.tw$prime_minister_max)
data.tw$election_year <- as.factor(data.tw$election_year)

# Ensuring correct ref category for categorical populism measure (PopuList)
data.fb$populist_cat <- relevel(as.factor(data.fb$populist_cat), ref = "np")
data.tw$populist_cat <- relevel(as.factor(data.tw$populist_cat), ref = "np")

# Ensuring correct ref category for categorical populism measure (PopuList)
data.fb$post_type <- relevel(as.factor(data.fb$post_type), ref = "status")
data.tw$contains_url <- relevel(as.factor(data.tw$contains_url), ref = "TRUE")

# Filtering specific to outcomes
# FB comments and likes are fine, as are retweets, but other outcomes not used much early on.
data.fb.shares <- data.fb %>% filter(year > 2012) # Shares added November 2012
data.tw.replies <- data.tw %>% filter(year > 2010) # I couldn't find exactly when reply button introduced but they are very rare in 2010

