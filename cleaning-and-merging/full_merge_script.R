###########################
### FINAL MERGE SCRIPT  ###
### Last updated 9/8/23 ###
###########################

# This script merges social media data with party and country covariates to create
# a party-year level dataset. final_data_cleaning.R should be run afterwards to
# produce datasets used in analyses.

# Loading packages
library(tidyverse)
`%notin%` <- Negate(`%in%`) # Defining a helper function                   

# Defining the set of countries to be used in the analysis
COUNTRIES <- c("AUT", "BEL", "BGR", "HRV", "CZE", "DNK", "EST", "FIN", "FRA", "DEU", "GRC",
               "HUN", "ITA", "LVA", "LTU", "LUX", "NLD", "NOR", "POL", "PRT", "ROU", "SVK", "SVN",
               "ESP", "SWE", "CHE", "GBR", "MLT", "IRL", "NIR")

#######################################################################
##                                                                   ##
##                      READING DATA FILES                           ##
##                                                                   ##
#######################################################################

#Facebook
fb <- read.csv("../data/facebook_data_cleaned/facebook_yearly.csv") %>% 
  dplyr::select(facebook_id = page_id, handle, year, total_posts, total_likes, total_shares, total_comments)

#Twitter
tweets <- read.csv("../data/twitter_data_cleaned/twitter_yearly.csv")

#Party Info (Manually created dataset)
partyinfo <- read.csv("../data/merge_table/social_media_partyfacts_mergetable_revised_v2.csv") %>% 
  dplyr::select(country, partyfacts_id, name_short,
                handle, page_id,
                twitter_handle, author_id)

# PartyFacts lookup
partyfacts <- read.csv("../data/partyfacts_data/partyfacts-external-parties.csv") %>% dplyr::filter(dataset_key == "parlgov") %>%
  dplyr::select(partyfacts_id, parlgov_id = dataset_party_id, 
                partyfacts_name = name_english, partyfacts_country = country) %>%
  filter(partyfacts_country %in% COUNTRIES) # Filtering out countries not considered

# This file includes some additional information to merge parties otherwise missed due to mismatching identifiers
partyfacts.additional <- read.csv("../data/partyfacts_data/partyfacts_parlgov_missing_merge.csv") %>% filter(!is.na(as.numeric(partyfacts_id))) %>%
  filter(partyfacts_id > 0) %>% filter(!is.na(as.numeric(parlgov_id)))  %>%
  filter(partyfacts_country %in% COUNTRIES)

#ParlGov
path = "../data/parlgov_data/" 
files.data <- list.files(path, pattern = glob2rx("view_*.csv"), full.names = TRUE)
files.data.short <- gsub(".*_", "\\", files.data)
for (i in 1:length(files.data)) assign(files.data.short[i], read.csv(files.data[i]))

# PopuList Data (unedited, converted to CSV)
populist <- read_csv("../data/populism_data/populist-version-2-20200626.csv") #enter path for PopuList csv 

# Country Info
country_data <- read_csv("../data/country_data/final_imputed_country_data.csv")

#Read POPPA dataset - Includes ParlGov IDs
poppa_means <- read_csv("../data/populism_data/poppa_corrected.csv")

#Read GPS dataset (.csv) - Includes PartyFacts and ParlGov IDs
# Manually corrected to add missing IDs
gps <- read_csv("../data/populism_data/gps_corrected.csv")

#######################################################################
##                                                                   ##
##                   MERGING DATASETS                                ##
##                                                                   ##
#######################################################################

##############################
# PART 1. Party information ##
##############################

# Filtering elections dataset in order to obtain correct ParlGov IDs from PartyFacts
elections <- election.csv %>% filter(election_type == "parliament") %>%  # only select parliamentary elections
  select(election_id, party_id, country_id, country_name_short, party_name_short, election_date, vote_share, seats, seats_total) %>%
  dplyr::filter(country_name_short %in% COUNTRIES)

partyfacts <- partyfacts %>% filter(partyfacts_id %notin% partyfacts.additional$partyfacts_id) # Drop any redundant entries where a partyfacts_id already exists but without appropriate parlgov_id
partyfacts <- bind_rows(partyfacts, partyfacts.additional %>% select(partyfacts_id, parlgov_id, partyfacts_name, partyfacts_country)) # Merging missing parties into merge table

# Checking for missingness. This should be low now because most legitimate misses have been addressed in partyfacts.additional (see comments there for additional misses)
partyinfo.missing <- partyinfo %>% anti_join(partyfacts, by = "partyfacts_id") %>% dplyr::select(partyfacts_id, partyfacts_name = name_short, partyfacts_country = country, facebook_handle = handle, twitter_handle)

# Note: There are some parties which have partyfacts_id that does not have a match in the partyfacts table (to join to parlgov)
# Some of these parties do not seem to have a valid partyfacts_id
# (e.g. M5S in Italy has a PF ID (2046) and appears to be in parlgov (2155), but does not appear in the partyfacts dataset
# For these cases we will have to manually add rows to the lookup table (see examples below)
# These can then be appended to partyfacts
partyfacts <- rbind(partyfacts, c(2046, 2155, "M5S", "ITA"))
partyfacts <- rbind(partyfacts, c(1388, 659, "Liberal Democrats", "GBR"))
partyfacts <- partyfacts %>% filter(partyfacts_id != 859) # Incorrect mapping
partyfacts <- rbind(partyfacts, c(859, 1202, "Munkaspart", "HUN")) # Correct mapping
partyfacts <- partyfacts %>% filter(partyfacts_id != 365) # Removing minor Italian party with conflict
partyfacts <- partyfacts %>% filter(partyfacts_id != 110) # Removing Estonian party with 1 year of data and conflict
partyfacts <- partyfacts %>% filter(partyfacts_id != 1212) # Removing two conflicting versions of Italian party 
partyfacts <- partyfacts %>% filter(partyfacts_id != 8246) 

# Adding Northern Ireland parties with IDs to merge to election data
# Dropping existing entries for NIR and for NIR parties in UK
partyfacts <- partyfacts %>% filter(partyfacts_country != "NIR")
# PF codes correspond to NI parties but PG to UK (no separate code for NI)
partyfacts <- rbind(partyfacts, c(266, 1023, "SDLP", "NIR"))
partyfacts <- rbind(partyfacts, c(667, 319, "DUP","NIR"))
#partyfacts <- rbind(partyfacts, c(694, 5003, "NI Conservatives","NIR")) # Does not qualify due to low electoral perf and no PG data
partyfacts <- rbind(partyfacts, c(1257, 689, "Sinn Fein","NIR"))
partyfacts <- rbind(partyfacts, c(1763, 1210, "UUP","NIR"))
partyfacts <- rbind(partyfacts, c(3257, 2731, "Alliance","NIR"))
#partyfacts <- rbind(partyfacts, c(642, 5007, "PUP","NIR")) # No PG data

# Filtering out any duplicates that may have been introduced
partyfacts$partyfacts_id <- as.numeric(partyfacts$partyfacts_id)
partyfacts$parlgov_id <- as.numeric(partyfacts$parlgov_id)

# Keep aside any with just one mapping
partyfacts.solo <- partyfacts %>% group_by(partyfacts_id) %>%
  mutate(count = n()) %>% filter(count == 1) %>% select(-count)

# Find those with multiple mappings
partyfacts.multiple <- partyfacts %>% group_by(partyfacts_id) %>%
  mutate(count = n()) %>% filter(count > 1) %>% select(-count) %>%
  mutate(parlgov_id = as.numeric(parlgov_id))

# If there is a match in elections, then just keep the most recent
partyfacts.multiple.elect <- partyfacts.multiple %>%
  inner_join(elections, by = c("parlgov_id" = "party_id")) %>%
  group_by(partyfacts_id) %>% slice(which.max(as.Date(election_date, '%Y-%m-%d'))) %>%
  select(partyfacts_id, parlgov_id, partyfacts_name, partyfacts_country)

# Filter multiple to drop those in elections
partyfacts.multiple.non.elect <- partyfacts.multiple %>%
  filter(partyfacts_id %notin% partyfacts.multiple.elect$partyfacts_id) 

# Merge all three back together
partyfacts <- bind_rows(partyfacts.solo, partyfacts.multiple.elect, partyfacts.multiple.non.elect)

# Joining partyinfo and partyfacts on partyfacts_id
partyfacts$partyfacts_id <- as.numeric(partyfacts$partyfacts_id) # setting ID field to numeric
partyinfo.pf <- partyinfo %>% left_join(partyfacts, by = "partyfacts_id")
partyinfo.pf$handle <- str_trim(tolower(partyinfo.pf$handle)) # ensuring case does not interfere with joins
partyinfo.pf$twitter_handle <- str_trim(tolower(partyinfo.pf$twitter_handle))

# Dropping any parties with missing partyfacts (either NA or by missingness code) 
partyinfo.pf <- partyinfo.pf %>% drop_na(partyfacts_id) %>% filter(partyfacts_id >= 1) %>% filter(country %in% COUNTRIES)

####################################
# PART 2. Social media information #
####################################

# Modifying handles for joining
fb$handle <- str_trim(tolower(fb$handle))
#tweets <- tweets %>% rename(twitter_handle = handle) # renaming before join # No longer needed due to change in script
tweets$twitter_handle <- str_trim(tolower(tweets$twitter_handle))

# Missing data - run these lines to identify any accounts which exist in partyinfo but not the collected social media data.
missing.fb <- partyinfo.pf %>% anti_join(fb, by = "handle") # Note two fewer now whitespace tripped.
missing.twitter <- partyinfo.pf %>% anti_join(tweets, by = "twitter_handle")  %>% drop_na(twitter_handle) %>% mutate(twitter_handle_length = nchar(twitter_handle)) %>% filter(twitter_handle_length > 1)

# Join partyinfo to FB and Twitter individually
# Inner join between the handles in partyinfo and each social media dataset
fb.join <- partyinfo.pf %>% 
  select(partyfacts_id, parlgov_id, country, handle) %>% 
  inner_join(fb, by = "handle")

# There are a few parties which we could not get FB data for, despite obtaining handles
not_joined_fb <- partyinfo.pf %>% 
  select(partyfacts_id, parlgov_id, country, handle) %>% 
  anti_join(fb, by = "handle")

twitter.join <- partyinfo.pf %>%
  select(partyfacts_id, parlgov_id, country, twitter_handle) %>%
  inner_join(tweets, by = "twitter_handle") %>% distinct() # distinct prevents

# There are many more without Twitter, mostly due to missing handles, but some that could not be downloaded
not_joined_tw <- partyinfo.pf %>%
  select(partyfacts_id, parlgov_id, country, twitter_handle) %>%
  anti_join(tweets, by = "twitter_handle") %>% distinct()

# Join both datasets together by partyfacts_id and year, then removing duplicates with distinct
facebook.twitter <- fb.join %>% full_join(twitter.join, by = c('partyfacts_id', 'parlgov_id', 'country', 'year')) %>% distinct()

facebook.twitter$parlgov_id <- as.integer(facebook.twitter$parlgov_id) # converting to integer for later merge # 2023: Note some are missing

# Address issue with "pvdabelgie"  "ptbbelgique" - These parties have the same ID but different Twitter and Facebook profiles due to translation from Flemish
# Solution is to consider both pages/accounts together
# There are four entries for each year due to the full join. Need to group by year, sum, then divide by two
belgium.fix <- facebook.twitter %>% filter(handle %in% c("pvdabelgie", "ptbbelgique"))
facebook.twitter <- facebook.twitter %>% filter(handle %notin% c("pvdabelgie", "ptbbelgique"))
belgium.fix$handle <- "pvdabelgie" # standardizing handles and IDs
belgium.fix$twitter_handle <- "pvdabelgie"
belgium.fix$facebook_id <- 350392
belgium.fix$author_id <- 85941439 
belgium.fix <- belgium.fix %>% group_by(year) %>% reframe(partyfacts_id, parlgov_id, country, handle, facebook_id, 
                                                            total_posts = sum(total_posts)/2, total_likes = sum(total_likes)/2, 
                                                            total_shares = sum(total_shares)/2, total_comments = sum(total_comments)/2,
                                                            twitter_handle, author_id, total_tweets = sum(total_tweets)/2, total_likes_tw = sum(total_likes_tw)/2,
                                                            total_retweets = sum(total_retweets)/2, total_replies = sum(total_replies)/2)  %>% distinct()

# Removing rows from main dataset (with condition to prevent removal of NAs) and adding transformed data back in
facebook.twitter <- facebook.twitter %>% bind_rows(belgium.fix) %>%
  filter(country %in% COUNTRIES)

###############################################
# PART 3. ParlGov elections, cabinet, parties #
###############################################

# Get subset of relevant countries
elections$election_year <- year(elections$election_date) # Adding year variable

# Filtering cabinet dataset
# Taking maximum values for each binary indicator to account for changes between elections
# Ignoring cabinet_id, previous_cabinet_id, and cabinet name
cabinet <- cabinet.csv %>% select(election_id, party_id, country_id, party_name_short, 
                                  cabinet_party, prime_minister) %>% 
  group_by(election_id, party_id, country_id) %>%
  summarize(cabinet_party_max = max(cabinet_party),
            prime_minister_max = max(prime_minister)) %>%
  distinct() # Drop duplicate rows

# Joining elections with cabinet data by parlgov_id ("party_id"), election_id, and country_code
# replacing missing cabinet info so maximum values to be calculated in all cases
elections <- elections %>%
  left_join(cabinet, by = c("party_id", "election_id", "country_id")) %>% 
  replace_na(list(cabinet_party = 0,
                  prime_minister = 0))


# Spain 2019, Greece 2012, 2015 had multiple elections in the same year
# Taking the mean seats and vote share then dropping out unique election info
# Note that cabinet maximums need to be recalculated to account for changes in the above countries
elections <- elections %>% group_by(party_id, country_id, election_year) %>% 
  mutate(mean_vote_share = mean(vote_share), mean_seats = mean(seats), cabinet_party_max = max(cabinet_party_max), 
         prime_minister_max = max(prime_minister_max)) %>%
  ungroup() %>%
  select(party_id, country_name_short, party_name_short, election_year, mean_vote_share, mean_seats, seats_total,
         cabinet_party_max, prime_minister_max) %>%
  distinct()

# Adding Northern Ireland parties election results
nir.elections <- read_csv("../data/misc/ni_elections.csv")

# Current results correspond to percentages in UK parliamentary elections
# Removing corresponding rows
elections <- elections %>%
    filter(!(country_name_short == "GBR" & party_id %in% c(1023, 319, 689, 1210, 2731)))

# Transforming Northern Ireland data for compatibility
# Number of seats changed following 2017 election
nir.elections <- nir.elections %>% mutate(seats_total = ifelse(year > 2017, 90, 108))
# Standardizing columns to same format as elections table
nir.elections.merge <- nir.elections %>% select(party_id = parlgov_id, election_year = year, 
                                               mean_vote_share = vote_share, mean_seats = seats,
                                               seats_total, cabinet_party_max = cabinet_party, 
                                               prime_minister_max = prime_minister)

# Dropping three columns so dimensions equivalent
elections <- elections %>% select(-country_name_short, -party_name_short)

# Appending NI data to elections table
elections <- bind_rows(elections, nir.elections.merge)

# Selecting relevant party level variables
party <- party.csv %>% dplyr::select(party_id, left_right)

#################################
# PART 4. The populism datasets #
#################################

# Time-varying classifications do not apply to any of the parties in the period studied here, so we can just use the standard PopuList labels
populist <- populist %>% select(populist, farright, farleft,
                                parlgov_id) %>% distinct()

gps <- gps %>% 
  select(parlgov_id, gps_populism) %>%
  drop_na(parlgov_id)

poppa_means <- poppa_means %>% rename(poppa_populism_mean = populism) %>% 
  select(parlgov_id, poppa_populism_mean) %>% drop_na()

# Note: Sinn Fein has different IDs in NIR and IRE in these datasets.
# Adding new rows to each with the missing IDs and corresponding scores
# poppa_means <- rbind(poppa_means, c(689, 6.23))
# gps <- rbind(gps, c(2217, 7.25))
# Update: NIR not in POPPA and IRL not in GPS
# so no need to only include a single party in each country, it will happen automatically

# Joining time-invariant party and popuList datasets
party.p <- party %>% left_join(populist, by = c("party_id" = "parlgov_id")) %>%
  left_join(poppa_means, by = c("party_id" = "parlgov_id")) %>%
  #left_join(poppa_medians, by = c("party_id" = "parlgov_id")) %>%
  left_join(gps, by = c("party_id" = "parlgov_id"))

##################################
# PART 6. Merging final datasets #
##################################

# Joins social media data to the elections data using parlgov ID and year
# This is a full join because we need earlier data (pre-2010) to backfill some temporal data 
final <- facebook.twitter %>%
  full_join(elections, by = c("parlgov_id" = "party_id", "year" = "election_year")) %>%
  left_join(party.p, by=c("parlgov_id" = "party_id")) %>%  # merging in populist
  drop_na("parlgov_id") %>% # dropping rows without parlgov IDs (these need to be found)
  left_join(country_data, by=c("country", "year")) # Joining country data

final <- final %>% filter(parlgov_id != 1727) # Dropping CSU/CDU alliance pages for Germany, just retaining main parties
final <- final %>% filter(parlgov_id != 2389) # Mistake, added wrong handle to a SVK party

# Filling in missing values
# Seats, vote share, and government status backfilled using earlier values
# Missing popuList data coded as zero
# Finally, filtering rows without social media data and outside relevant data range
# and dropping any duplicated rows
final.filled <- final %>% group_by(parlgov_id) %>% 
  arrange(year, .by_group = TRUE) %>%
  #fill(c("family_name", "family_id", "left_right", "state_market", "liberty_authority", "eu_anti_pro"), .direction="downup") %>%
  mutate(election_year = ifelse(is.na(mean_vote_share), 0,1)) %>% # dummy for election year
  fill(c("mean_seats", "mean_vote_share", "cabinet_party_max", "prime_minister_max", "seats_total"), .direction="down") %>%
  # Note that the election values are null until a party competes in an election, even if they have a social media presence. Replacing these with zeros
  replace_na(list(mean_seats = 0, mean_vote_share = 0, cabinet_party_max = 0, prime_minister_max = 0, seats_total = 0)) %>%
  replace_na(list(populist = 0, farright = 0, farleft = 0)) %>% ungroup() %>%
  mutate(keep = ifelse(is.na(handle), 0,1) + ifelse(is.na(twitter_handle), 0, 1)) %>% # Keep is 1 if we have data for one platform in a given year and 2 if we have both
  filter(keep > 0) %>% filter(year <= 2020) %>% distinct(.keep_all = TRUE)

## Finding parties with duplicate parlgov IDs. These should be handled appropriately
dup.parlgov.ids <- final.filled %>% group_by(parlgov_id) %>% summarize(n = length(unique(partyfacts_id))) %>% filter(n > 1)

dup.parlgov <- final.filled %>% filter(parlgov_id %in% dup.parlgov.ids$parlgov_id)
# The two remaining duplicates pertain to coalitions where both parties are active on social media.
# These are currently added as separate entries. Each has a unique partyfacts_id

# Filtering out any NI parties listed in UK
final.filled <- final.filled %>%
    filter(!(country == "GBR" & parlgov_id %in% c(1023, 319, 689, 1210, 2731)))

write_csv(final.filled, "../data/final_merged_data/final_merged_dataset.csv")
