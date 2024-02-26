# Final data cleaning to prepare dataset for modeling

library(tidyverse)
`%notin%` <- Negate(`%in%`) # Defining a helper function

# This script performs several assorted functions on the merged data:
# 1. Imputing missing ideology (mean)
# 2. Scaling independent variables
# 3. Processing parties with gaps in activity (Lines 56-196)
# 4. Additional filtering, including removing extremely low activity/engagement parties
# 5. Constructing categorical populism measures
# 6. Addressing Sinn Fein account overlap
# 7. Creating dataset with country-year info (used to help plot results)
# 8. Outputting final datasets used for modeling

data <- read_csv("../data/final_merged_data/final_merged_dataset.csv") %>% filter(year >= 2010) %>%
    filter(year <= 2020) %>% distinct(.keep_all = TRUE) # Just get years between 2010 and 2020

# Imputing missing ideology
# These parties are either centrist populists, regional, or special issue
data <- data %>% replace_na(list(left_right = 5))

# Defining scaling function
scale2sd <- function (x) {return( (x-mean(x, na.rm = TRUE))  / (2*sd(x, na.rm = TRUE)) )}

# Apply standardization for all relevant variables before dropping anything
# Since observations are repeated, we need to group them for scaling
# Party-level factors take party mean
# Note that all of these are time-invariant within party
party_scaled <- data %>% group_by(parlgov_id) %>%
    summarize(left_right = mean(left_right),
              poppa_populism_mean = mean(poppa_populism_mean),
              gps_populism = mean(gps_populism)) %>%
    mutate(left_right_s = as.numeric(scale2sd(left_right)),
           poppa_populism_s = as.numeric(scale2sd(poppa_populism_mean)),
           gps_populism_s = as.numeric(scale2sd(gps_populism))) %>%
    select(parlgov_id, left_right_s, poppa_populism_s, gps_populism_s)

# Country-year level factors, take country-year mean
# Note that all are time-invariant within country-year
cy_scaled <- data %>% group_by(country, year) %>%
    summarize(gdp = mean(gdp),
              pop = mean(pop),
              everyday_social_networks_imputed = mean(everyday_social_networks_imputed),
              gdp_growth = mean(gdp_growth),
              net_migr = mean(net_migr),
              refugee = mean(refugee),
              refugee_change = mean(refugee_change)) %>%
    mutate(gdp_s = as.numeric(scale2sd(gdp)),
           pop_s = as.numeric(scale2sd(pop)),
           everyday_social_networks_imputed_s = as.numeric(scale2sd(everyday_social_networks_imputed)),
           gdp_growth_s = as.numeric(scale2sd(gdp_growth)),
           net_migr_s =  as.numeric(scale2sd(net_migr)),
           refugee_s = as.numeric(scale2sd(refugee)),
           refugee_change_s = as.numeric(scale2sd(refugee_change))) %>%
    select(country, year, gdp_s, pop_s, everyday_social_networks_imputed_s, gdp_growth_s, net_migr_s, refugee_s,
           refugee_change_s)

data <- data %>% left_join(party_scaled, by = "parlgov_id") %>% left_join(cy_scaled, by = c("country", "year"))


data$seats_per <- (data$mean_seats/data$seats_total)*100
data$handle <- as.factor(data$handle)
data$twitter_handle <- as.factor(data$twitter_handle)

# Replacing missing handles (i.e. party has a row where it has Twitter handle but not FB because account opened later)
data <- data %>% group_by(parlgov_id) %>% fill(c("handle", "twitter_handle", "facebook_id", "author_id"), .direction = "updown") %>%
    ungroup()

# Finding missing values in the middle of each series (e.g. 2014, 2015, NA, 2015) as these cause problem
# Find min and max year where active on Twitter. Count total years active. Find cases where total years active is less than max-min.
data$total_tweets <- replace_na(data$total_tweets, -1)
data$total_posts <- replace_na(data$total_posts, -1)
data$twitter_active <- ifelse(data$total_tweets >= 1, 1, 0) # Dummy indicating whether active (must have posted or tweeted at least once a year)
data$facebook_active <- ifelse(data$total_posts >= 1, 1, 0) # Dummy indicating whether active

# If we drop based on active threshold then we simplify the problem. We drop first and last values below threshold
# We just need to see if the duration is lower than expected
# Defining separate datasets
data.fb <- data %>% filter(facebook_active == 1)
data.tw <- data %>% filter(twitter_active == 1)

# Finding activity within these
twitter.active <- data.tw %>% drop_na(twitter_handle) %>% filter(twitter_active == 1) %>% group_by(twitter_handle) %>%
    summarize(total_active = sum(twitter_active),
              duration = 1+max(year) - min(year)) %>% mutate(diff = total_active - duration)
facebook.active <- data.fb %>% drop_na(handle) %>% filter(facebook_active == 1) %>% group_by(handle) %>%
    summarize(total_active = sum(facebook_active),
              duration = 1+max(year) - min(year)) %>% mutate(diff = total_active - duration)

# These parties have gaps in their Twitter activity.
mm.tw <- twitter.active %>% filter(diff < 0)
data.tw.mm <- data.tw %>% filter(twitter_handle %in% mm.tw$twitter_handle) # Shows activity

# These parties have gaps in their FB activity.
mm.fb <- facebook.active %>% filter(diff < 0)
data.fb.mm <- data.fb %>% filter(handle %in% mm.fb$handle) # Shows activity

data.tw.mm.gap <- data.tw.mm %>% select(twitter_handle, total_tweets, year, twitter_active) %>%
    group_by(twitter_handle) %>% mutate(min_year = min(year),
                                                   max_year = max(year),
                                                   observed_years = cumsum(twitter_active),
                                                   duration = year-min(year)+1)

data.tw.before <- data.tw.mm.gap %>% filter(duration == observed_years)
data.tw.after <- data.tw.mm.gap  %>% filter(duration > observed_years)

# Summaries for each period
s1 <- data.tw.before %>% group_by(twitter_handle) %>% summarize(active_years_1 = sum(twitter_active),
                                                          activity_1 = sum(total_tweets))
s2 <- data.tw.after %>% group_by(twitter_handle) %>% summarize(active_years_2 = sum(twitter_active),
                                                                activity_2 = sum(total_tweets))
s <- s1 %>% left_join(s2, by = "twitter_handle") %>%
    mutate(longer_after = ifelse(active_years_2 > active_years_1, TRUE, FALSE),
           more_active_after = ifelse(activity_2 > activity_1, TRUE, FALSE),
           agreement = longer_after == more_active_after)

s_keep_after <- s %>% filter(longer_after == TRUE & agreement == TRUE)
s_keep_before <- s %>% filter(longer_after == FALSE & agreement == TRUE)
s_disagree <- s %>% filter(agreement == FALSE)
# For each party, determine whether before or after is longer / has more activity

# If s_keep_before, then drop rows not in before
# If s_keep_after or s_disagree, drop dows not in after

data.tw.mm.gap.2.kb <- data.tw.mm.gap %>% filter(twitter_handle %in% s_keep_before$twitter_handle) %>%
    filter(duration == observed_years) %>%  # Filtering and repeating. There should no longer be any gaps unless there were multiple gaps
    mutate(min_year = min(year),
           max_year = max(year),
           observed_years = cumsum(twitter_active),
           duration = year-min(year)+1)

data.tw.mm.gap.2.ka <- data.tw.mm.gap %>% filter(twitter_handle %notin% s_keep_before$twitter_handle) %>%
    filter(duration > observed_years) %>%  # Filtering and repeating. There should no longer be any gaps unless there were multiple gaps
    mutate(min_year = min(year),
           max_year = max(year),
           observed_years = cumsum(twitter_active),
           duration = year-min(year)+1)


data.tw.mm.gap.2 <- bind_rows(data.tw.mm.gap.2.kb, data.tw.mm.gap.2.ka)

# Now to merge
# 1. Drop the parties with gaps
data.tw <- data.tw %>% filter(twitter_handle %notin% mm.tw$twitter_handle)
# 2. Filter the gap table using the final non-gap table
data.tw.mm <- data.tw.mm %>% semi_join(data.tw.mm.gap.2, by = c("year", "twitter_handle"))
# 3. Add the rows back in
data.tw <- bind_rows(data.tw, data.tw.mm)

# Repeating for FB
data.fb.mm.gap <- data.fb.mm %>% select(handle, total_posts, year, facebook_active) %>%
    group_by(handle) %>% mutate(min_year = min(year),
                                        max_year = max(year),
                                        observed_years = cumsum(facebook_active),
                                        duration = year-min(year)+1)

# Adding additional filtering steps
data.fb.before <- data.fb.mm.gap %>% filter(duration == observed_years)
data.fb.after <- data.fb.mm.gap  %>% filter(duration > observed_years)

# Summaries for each period
s1 <- data.fb.before %>% group_by(handle) %>% summarize(active_years_1 = sum(facebook_active),
                                                                activity_1 = sum(total_posts))
s2 <- data.fb.after %>% group_by(handle) %>% summarize(active_years_2 = sum(facebook_active),
                                                               activity_2 = sum(total_posts))
s <- s1 %>% left_join(s2, by = "handle") %>%
    mutate(longer_after = ifelse(active_years_2 > active_years_1, TRUE, FALSE),
           more_active_after = ifelse(activity_2 > activity_1, TRUE, FALSE),
           agreement = longer_after == more_active_after)

s_keep_after <- s %>% filter(longer_after == TRUE & agreement == TRUE)
s_keep_before <- s %>% filter(longer_after == FALSE & agreement == TRUE)
s_disagree <- s %>% filter(agreement == FALSE)
# For each party, determine whether before or after is longer / has more activity

# If s_keep_before, then drop rows not in before
# If s_keep_after or s_disagree, drop dows not in after

data.fb.mm.gap.2.kb <- data.fb.mm.gap %>% filter(handle %in% s_keep_before$handle) %>%
    filter(duration == observed_years) %>%  # Filtering and repeating. There should no longer be any gaps unless there were multiple gaps
    mutate(min_year = min(year),
           max_year = max(year),
           observed_years = cumsum(facebook_active),
           duration = year-min(year)+1)

data.fb.mm.gap.2.ka <- data.fb.mm.gap %>% filter(handle %notin% s_keep_before$handle) %>%
    filter(duration > observed_years) %>%  # Filtering and repeating. There should no longer be any gaps unless there were multiple gaps
    mutate(min_year = min(year),
           max_year = max(year),
           observed_years = cumsum(facebook_active),
           duration = year-min(year)+1)

data.fb.mm.gap.2 <- bind_rows(data.fb.mm.gap.2.kb, data.fb.mm.gap.2.ka)

# Now to merge
# 1. Drop the parties with gaps
data.fb <- data.fb %>% filter(handle %notin% data.fb.mm.gap.2$handle)
# 2. Filter the gap table using the final non-gap table
data.fb.mm <- data.fb.mm %>% semi_join(data.fb.mm.gap.2, by = c("year", "handle"))
# 3. Add the rows back in
data.fb <- bind_rows(data.fb, data.fb.mm)

# Adding a variable tracking years active, overwriting total duration variable
data.fb <- data.fb %>% group_by(handle) %>%
    mutate(duration = year + 1 - min(year),
           years_observed = n_distinct(year)) %>% ungroup()
data.tw <- data.tw %>% group_by(handle) %>%
    mutate(duration = year+ 1 - min(year),
           years_observed = n_distinct(year)) %>% ungroup()

# Filtering rows where handle is either incorrect, duplicate, or removed due to data issues
data.fb <- data.fb %>% filter(handle %notin% c("esquerda.net", "bulgariabezcenzurabs",
                                               "uniapolitykirealnej", "mysme99percent", "ceuscoalition",
                                               "elpce", # Duplicate PG
                                               "kommaellinonkinigon")) # Drop 23 rows
data.fb <- data.fb %>% filter(parlgov_id != 2211) # Removing duplicate BGR party with incorrect PG ID
data.tw <- data.tw %>% filter(twitter_handle %notin% c("upr_org", "vermelhaestrela",
                                               "afd", "ceuscoalition", "elcpe"))
data.fb <- data.fb %>% filter(!(country == "LVA" & year == 2011)) # ~0 activity in this CY

# Filtering out parties to require at least two years of observations
data.fb <- data.fb %>% filter(years_observed >= 2)
data.tw <- data.tw %>% filter(years_observed >= 2)

# Filtering by activity and engagement. Dropped if less than 100 total posts & less than 100 total likes
# This removes a handful of parties which set up social media but were never active
low.activity.fb <- data.fb %>% group_by(handle) %>% mutate(post_sum = sum(total_posts),
                                                           like_sum = sum(total_likes)) %>%
    filter(post_sum < 100 & like_sum < 100)

low.activity.tw <- data.tw %>% group_by(twitter_handle) %>% mutate(tweet_sum = sum(total_tweets),
                                                                   like_sum = sum(total_likes_tw)) %>%
    filter(tweet_sum < 100 & like_sum < 100)

data.fb <- data.fb %>% filter(handle %notin% low.activity.fb$handle)
data.tw <- data.tw %>% filter(twitter_handle %notin% low.activity.tw$twitter_handle)

# Defining populism by ideology independent variable (PopuList only)
data.fb <- data.fb %>% mutate(populist_cat = ifelse(populist == 1 & farright == 1, "rwp", ifelse(
    populist == 1 & farleft == 1, "lwp", ifelse(
        populist == 1, "cp", "np"
    )
)))

data.tw <- data.tw %>% mutate(populist_cat = ifelse(populist == 1 & farright == 1, "rwp", ifelse(
    populist == 1 & farleft == 1, "lwp", ifelse(
        populist == 1, "cp", "np"
    )
)))

# Writing version of data without Sinn Fein adjustments for robustness tests
write_csv(data.fb, "../robustness/sinnfein_analysis/model_dataset_fb_sf.csv")
write_csv(data.tw, "../robustness/sinnfein_analysis/model_dataset_tw_sf.csv")

# Performing modifications to re-weight Sinn Fein observations
pop.data <- data.tw %>% filter(country %in% c("IRL", "NIR") & twitter_handle == "sinnfeinireland") %>% group_by(country, year) %>% select(country, year, pop, mean_vote_share) %>% distinct(.keep_all = T)
pop.data <- pop.data %>% mutate(sf_voters = pop * (mean_vote_share/100))
nir <- pop.data %>% filter(country == "NIR")
irl <- pop.data %>% filter(country == "IRL")
nir$prop <- round(nir$sf_voters / (nir$sf_voters + irl$sf_voters),2)
sf_prop <- nir %>% ungroup() %>% select(year, prop)
print(sf_prop)

# Now to transform the data
# For SF observations on either platform
# Outcome for NIR is X/prop and IRL is X/(1-prop) where prop varies each year
data.fb <- data.fb %>%
    left_join(sf_prop, by = "year") %>%
    mutate(
        #total_posts = ifelse(parlgov_id %in% c(689, 2217) & country == "NIR", total_posts * prop,
        #                     ifelse(parlgov_id %in% c(689, 2217) & country == "IRL", total_posts* (1 - prop), total_posts)),
        total_likes = ifelse(parlgov_id %in% c(689, 2217) & country == "NIR", total_likes * prop,
                             ifelse(parlgov_id %in% c(689, 2217) & country == "IRL", total_likes * (1 - prop), total_likes)),
        total_shares = ifelse(parlgov_id %in% c(689, 2217) & country == "NIR", total_shares * prop,
                              ifelse(parlgov_id %in% c(689, 2217) & country == "IRL", total_shares * (1 - prop), total_shares)),
        total_comments = ifelse(parlgov_id %in% c(689, 2217) & country == "NIR", total_comments * prop,
                                ifelse(parlgov_id %in% c(689, 2217) & country == "IRL", total_comments * (1 - prop), total_comments))
    ) %>%
    select(-prop)  # Remove the 'prop' column if you don't need it anymore

data.tw <- data.tw %>%
    left_join(sf_prop, by = "year") %>%
    mutate(
        # Avoid doing this to tweets, just modify engagements
        #total_tweets = ifelse(parlgov_id %in% c(689, 2217) & country == "NIR", total_tweets * prop,
        #                      ifelse(parlgov_id %in% c(689, 2217) & country == "IRL", total_tweets * (1 - prop), total_tweets)),
        total_likes_tw = ifelse(parlgov_id %in% c(689, 2217) & country == "NIR", total_likes_tw * prop,
                                ifelse(parlgov_id %in% c(689, 2217) & country == "IRL", total_likes_tw * (1 - prop), total_likes_tw)),
        total_retweets = ifelse(parlgov_id %in% c(689, 2217) & country == "NIR", total_retweets * prop,
                                ifelse(parlgov_id %in% c(689, 2217) & country == "IRL", total_retweets * (1 - prop), total_retweets)),
        total_replies = ifelse(parlgov_id %in% c(689, 2217) & country == "NIR", total_replies * prop,
                               ifelse(parlgov_id %in% c(689, 2217) & country == "IRL", total_replies * (1 - prop), total_replies)),
        total_tweets_rt = ifelse(parlgov_id %in% c(689, 2217) & country == "NIR", total_tweets_rt * prop,
                                 ifelse(parlgov_id %in% c(689, 2217) & country == "IRL", total_tweets_rt * (1 - prop), total_tweets_rt))
    ) %>%
    select(-prop)  # Remove the 'prop' column if you don't need it anymore

# Ensuring values rounded
data.fb$total_likes <- round(data.fb$total_likes)
data.fb$total_shares <- round(data.fb$total_shares)
data.fb$total_comments <- round(data.fb$total_comments)
data.tw$total_likes_tw <- round(data.tw$total_likes_tw)
data.tw$total_retweets <- round(data.tw$total_retweets)
data.tw$total_replies <- round(data.tw$total_replies)

# Storing datasets containing country-year counts of populist parties (PopuList)
# used later on to aid visualization
cy.populist.fb <- data.fb %>% group_by(country, year) %>%
    summarize(total_populist = sum(populist),
              total_cp = sum(populist_cat == "cp"),
              total_rwp = sum(populist_cat == "rwp"),
              total_lwp = sum(populist_cat == "lwp"))
cy.populist.tw <- data.tw %>% group_by(country, year) %>%
    summarize(total_populist = sum(populist),
              total_cp = sum(populist_cat == "cp"),
              total_rwp = sum(populist_cat == "rwp"),
              total_lwp = sum(populist_cat == "lwp"))

write_csv(cy.populist.fb, "../data/misc/active_populist_country_year_fb.csv")
write_csv(cy.populist.tw, "../data/misc/active_populist_country_year_tw.csv")

# Storing final version of data used for modeling
write_csv(data.fb, "../data/model_data/model_dataset_fb.csv")
write_csv(data.tw, "../data/model_data/model_dataset_tw.csv")
