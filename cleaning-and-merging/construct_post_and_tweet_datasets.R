# Constructing post/tweet level datasets for modeling
library(tidyverse)

# Note: This script should only be run after the final modeling datasets are created.
# These party-year level data are used to filter posts/tweets to ensure comparability
# and obtain party and country covariates

# Loading FB data then filtering out missing
# Note: Note all posts have a valid message
fb_year <- read_csv("../data/model_data/model_dataset_fb.csv") %>%
    select(partyfacts_id, parlgov_id, country, handle, facebook_id, year,
           mean_vote_share, cabinet_party_max, prime_minister_max, populist, populist_cat,
           gps_populism_s, poppa_populism_s, left_right_s,
           pop_s, gdp_s, everyday_social_networks_imputed_s, election_year
           )
posts <- read_csv("../data/facebook_data_cleaned/facebook_posts.csv") %>%
    left_join(fb_year, by = c("handle", "year")) %>% drop_na(populist)

tw_year <- read_csv("../data/model_data/model_dataset_tw.csv") %>%
    select(partyfacts_id, parlgov_id, country, twitter_handle, author_id, year,
           mean_vote_share, cabinet_party_max, prime_minister_max, populist, populist_cat,
           gps_populism_s, poppa_populism_s, left_right_s,
           pop_s, gdp_s, everyday_social_networks_imputed_s, election_year)
tweets <- read_csv("../data/twitter_data_cleaned/tweets.csv") %>% filter(is_retweet == F) %>%
    left_join(tw_year, by = c("twitter_handle_lower" = "twitter_handle", "year")) %>% drop_na(populist)

tweets <- tweets %>% rename(twitter_handle = twitter_handle_lower)

# Merge with yearly covariates, drop any missing (i.e. we do not consider a party in a given year)

# Word counts (after removing URL) then dropping text column
posts <- posts %>% mutate(contains_url = str_detect(message, "https?://\\S+")) %>%
    mutate(message = gsub("\\bhttps?://\\S+\\b", "", message)) %>%
    mutate(word_count = str_count(message, "\\S+")) %>%
    select(-message) %>%
    replace_na(list(word_count = 0))

tweets <- tweets %>% mutate(contains_url = str_detect(text, "https?://\\S+")) %>% 
    mutate(text = gsub("\\bhttps?://\\S+\\b", "", text)) %>%
    mutate(word_count = str_count(text, "\\S+")) %>%
    select(-text) %>%
    replace_na(list(word_count = 0))

# Now to get time zone in hours

# Turns out it is nontrivial to just infer timezone from country
# Reading in manually created file containing timezones for each country.
tzs <- read_csv("../data/misc/timezone_information.csv")

# Using with_tz to convert UTC timestamp to local time
# Note: Do not confuse with force_tz, which will "correct" timezone but keep UTC time constant
# Must do rowwise as with_tz does not work with vector timezones

posts <- posts %>%
     left_join(tzs, by = "country") %>%
     rowwise() %>% # Must do this as with_tz does not work if tzone = timezone argument is a vector
     mutate(date_added_local = with_tz(date_added, tzone = timezone))

tweets <- tweets %>% # Repeating for tweets
     left_join(tzs, by = "country") %>%
     rowwise() %>% #
     mutate(created_at_local = with_tz(created_at, tzone = timezone))

# Creating final transformed variables
posts <- posts %>%
     mutate(hr = hour(date_added_local)) %>%
     mutate(hr_sq = hr^2,
            word_count_sq = word_count^2)

tweets <- tweets %>%
     mutate(hr = hour(created_at_local)) %>%
     mutate(hr_sq = hr^2,
            word_count_sq = word_count^2)

# Standardizing FB post types    
posts <- posts %>% mutate(post_type = ifelse(
    type %in% c("video", "live_video_scheduled", "live_video_complete", "youtube", "native_video", "vine", "live_video"), 
                                             "video", 
        ifelse(type %in% c("photo", "album"), 
               "photo", type))) # Remaining types are "link" and "status" so just use these

write_csv(posts, "../data/model_data/model_dataset_fb_posts.csv")
write_csv(tweets, "../data/model_data/model_dataset_tw_tweets.csv")
