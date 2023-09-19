source("load-data-local.R")
library(modelsummary)

# Begin by filtering datasets to select necessary variables
data.fb <- data.fb %>% select(populist, poppa_populism_mean, gps_populism,
                              total_likes, total_shares, total_comments,
                              total_posts, mean_vote_share,
                              left_right, cabinet_party_max, prime_minister_max,
                              pop, gdp, everyday_social_networks_imputed,
                              election_year, year.f, country, country_year,
                              handle) %>% mutate(populist = as.numeric(trimws(populist)),
                                                 Cabinet = as.numeric(trimws(cabinet_party_max)),
                                                 Prime_minister = as.numeric(trimws(prime_minister_max)),
                                                 election_year = as.numeric(trimws(election_year))) %>%
    rename(Posts = total_posts, 
           Likes = total_likes,
           Shares  = total_shares,
           Comments = total_comments,
           PopuList = populist,
           Left_Right = left_right,
           Election = election_year,
           GDP = gdp,
           Population = pop,
           Social_networks = everyday_social_networks_imputed,
           Vote = mean_vote_share)

data.fb.shares <- data.fb.shares %>% select(populist, poppa_populism_mean, gps_populism,
                              total_likes, total_shares, total_comments,
                              total_posts, mean_vote_share,
                              left_right, cabinet_party_max, prime_minister_max,
                              pop, gdp, everyday_social_networks_imputed,
                              election_year, year.f, country, country_year,
                              handle) %>% mutate(populist = as.numeric(trimws(populist)),
                                                 Cabinet = as.numeric(trimws(cabinet_party_max)),
                                                 Prime_minister = as.numeric(trimws(prime_minister_max)),
                                                 election_year = as.numeric(trimws(election_year))) %>%
    rename(Posts = total_posts, 
           Likes = total_likes,
           Shares  = total_shares,
           Comments = total_comments,
           PopuList = populist,
           Left_Right = left_right,
           Election = election_year,
           GDP = gdp,
           Population = pop,
           Social_networks = everyday_social_networks_imputed,
           Vote = mean_vote_share)


data.tw <- data.tw %>% select(populist, poppa_populism_mean, gps_populism,
                                            total_likes_tw, total_retweets, total_replies,
                                            total_tweets, mean_vote_share,
                                            left_right, cabinet_party_max, prime_minister_max,
                                            pop, gdp, everyday_social_networks_imputed,
                                            election_year, year.f, country, country_year,
                                            twitter_handle) %>% mutate(populist = as.numeric(trimws(populist)),
                                                                       Cabinet = as.numeric(trimws(cabinet_party_max)),
                                                                       Prime_minister = as.numeric(trimws(prime_minister_max)),
                                                                       election_year = as.numeric(trimws(election_year))) %>%
    rename(Tweets = total_tweets, 
           Likes = total_likes_tw,
           Retweets  = total_retweets,
           Replies = total_replies,
           PopuList = populist,
           Left_Right = left_right,
           Election = election_year,
           GDP = gdp,
           Population = pop,
           Social_networks = everyday_social_networks_imputed,
           Vote = mean_vote_share)

data.tw.replies <- data.tw.replies %>% select(populist, poppa_populism_mean, gps_populism,
                              total_likes_tw, total_retweets, total_replies,
                              total_tweets, mean_vote_share,
                              left_right, cabinet_party_max, prime_minister_max,
                              pop, gdp, everyday_social_networks_imputed,
                              election_year, year.f, country, country_year,
                              twitter_handle) %>% mutate(populist = as.numeric(trimws(populist)),
                                                         Cabinet = as.numeric(trimws(cabinet_party_max)),
                                                         Prime_minister = as.numeric(trimws(prime_minister_max)),
                                                         election_year = as.numeric(trimws(election_year))) %>%
    rename(Tweets = total_tweets, 
           Likes = total_likes_tw,
           Retweets  = total_retweets,
           Replies = total_replies,
           PopuList = populist,
           Left_Right = left_right,
           Election = election_year,
           GDP = gdp,
           Population = pop,
           Social_networks = everyday_social_networks_imputed,
           Vote = mean_vote_share)

########################
## Rendering tables   ##
########################

# Note: No straightforward way to combine these within R
# Storing a table for each platform and populism measure (6)
# then manually combining to create descriptive table in appendix.

# Defining N rows
party.N <- length(unique(data.fb$handle))
country.N <- length(unique(data.fb$country))
years.N <- length(unique(data.fb$year.f))
country.year.N <- length(unique(data.fb$country_year))
new_rows <- as.data.frame(rbind(c("Parties","","",party.N), c("Countries", "", "", country.N),
                                c("Years", "", "", years.N),
                                c("Country-years", "", "", country.year.N),
                                c("Observations", "", "", dim(data.fb)[[1]])))
datasummary(Posts + Likes + Shares + Comments + PopuList +
                Left_Right + Vote + Cabinet + Prime_minister + GDP  + Population + Social_networks + Election ~ Mean + SD + N, data = as.data.frame(data.fb),
            add_rows = new_rows,
            output = "descriptive_tables/facebook_populist.docx")

party.N <- length(unique(data.fb.shares$handle))
country.N <- length(unique(data.fb.shares$country))
years.N <- length(unique(data.fb.shares$year.f))
country.year.N <- length(unique(data.fb.shares$country_year))
new_rows <- as.data.frame(rbind(c("Parties","","",party.N), c("Countries", "", "", country.N),
                                c("Years", "", "", years.N),
                                c("Country-years", "", "", country.year.N),
                                c("Observations", "", "", dim(data.fb.shares)[[1]])))
datasummary(Posts + Likes + Shares + Comments + PopuList +
                Left_Right + Vote + Cabinet + Prime_minister + GDP  + Population + Social_networks + Election ~ Mean + SD + N, data = as.data.frame(data.fb.shares),
            add_rows = new_rows,
            output = "descriptive_tables/facebook_shares_populist.docx")
# Shares parties = 395, years = 8, country-years = 224, obs = 2674. Most stats slightly higher. Negligible diff for populist/left right...

# Twitter/Populist
party.N <- length(unique(data.tw$twitter_handle))
country.N <- length(unique(data.tw$country))
years.N <- length(unique(data.tw$year.f))
country.year.N <- length(unique(data.tw$country_year))
new_rows <- as.data.frame(rbind(c("Parties","","",party.N), c("Countries", "", "", country.N),
                                c("Years", "", "", years.N),
                                c("Country-years", "", "", country.year.N),
                                c("Observations", "", "", dim(data.tw)[[1]])))
datasummary(Tweets + Likes + Retweets + Replies + PopuList +
                Left_Right + Vote + Cabinet + Prime_minister + GDP  + Population + Social_networks + Election ~ Mean + SD + N, data = as.data.frame(data.tw),
            add_rows = new_rows,
            output = "descriptive_tables/twitter_populist.docx")

# Repeating for POPPA
data.fb.poppa <- data.fb %>% drop_na(poppa_populism_mean)
# Defining N rows
party.N <- length(unique(data.fb.poppa$handle))
country.N <- length(unique(data.fb.poppa$country))
years.N <- length(unique(data.fb.poppa$year.f))
country.year.N <- length(unique(data.fb.poppa$country_year))
new_rows <- as.data.frame(rbind(c("Parties","","",party.N), c("Countries", "", "", country.N),
                                c("Years", "", "", years.N),
                                c("Country-years", "", "", country.year.N),
                                c("Observations", "", "", dim(data.fb.poppa)[[1]])))
datasummary(Posts + Likes + Shares + Comments + poppa_populism_mean +
                Left_Right + Vote + Cabinet + Prime_minister + GDP  + Population + Social_networks + Election ~ Mean + SD + N, data = as.data.frame(data.fb.poppa),
            add_rows = new_rows,
            output = "descriptive_tables/facebook_poppa.docx")

# Twitter/POPPA
data.tw.poppa <- data.tw %>% drop_na(poppa_populism_mean)

party.N <- length(unique(data.tw.poppa$twitter_handle))
country.N <- length(unique(data.tw.poppa$country))
years.N <- length(unique(data.tw.poppa$year.f))
country.year.N <- length(unique(data.tw.poppa$country_year))
new_rows <- as.data.frame(rbind(c("Parties","","",party.N), c("Countries", "", "", country.N),
                                c("Years", "", "", years.N),
                                c("Country-years", "", "", country.year.N),
                                c("Observations", "", "", dim(data.tw.poppa)[[1]])))
datasummary(Tweets + Likes + Retweets + Replies + poppa_populism_mean +
                Left_Right + Vote + Cabinet + Prime_minister + GDP  + Population + Social_networks + Election ~ Mean + SD + N, data = as.data.frame(data.tw.poppa),
            add_rows = new_rows,
            output = "descriptive_tables/twitter_poppa.docx")

# Repeating for GPS
data.fb.gps <- data.fb %>% drop_na(gps_populism)
# Defining N rows
party.N <- length(unique(data.fb.gps$handle))
country.N <- length(unique(data.fb.gps$country))
years.N <- length(unique(data.fb.gps$year.f))
country.year.N <- length(unique(data.fb.gps$country_year))
new_rows <- as.data.frame(rbind(c("Parties","","",party.N), c("Countries", "", "", country.N),
                                c("Years", "", "", years.N),
                                c("Country-years", "", "", country.year.N),
                                c("Observations", "", "", dim(data.fb.gps)[[1]])))
datasummary(Posts + Likes + Shares + Comments + gps_populism +
                Left_Right + Vote + Cabinet + Prime_minister + GDP  + Population + Social_networks + Election ~ Mean + SD + N, data = as.data.frame(data.fb.gps),
            add_rows = new_rows,
            output = "descriptive_tables/facebook_gps.docx")

# Twitter/gps
data.tw.gps <- data.tw %>% drop_na(gps_populism)

party.N <- length(unique(data.tw.gps$twitter_handle))
country.N <- length(unique(data.tw.gps$country))
years.N <- length(unique(data.tw.gps$year.f))
country.year.N <- length(unique(data.tw.gps$country_year))
new_rows <- as.data.frame(rbind(c("Parties","","",party.N), c("Countries", "", "", country.N),
                                c("Years", "", "", years.N),
                                c("Country-years", "", "", country.year.N),
                                c("Observations", "", "", dim(data.tw.gps)[[1]])))
datasummary(Tweets + Likes + Retweets + Replies + gps_populism +
                Left_Right + Vote + Cabinet + Prime_minister + GDP  + Population + Social_networks + Election ~ Mean + SD + N, data = as.data.frame(data.tw.gps),
            add_rows = new_rows,
            output = "descriptive_tables/twitter_gps.docx")
