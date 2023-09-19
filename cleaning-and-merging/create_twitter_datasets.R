library(tidyverse)

tweets <- read_csv("../data/twitter_data_raw/twitter_dump_full_unique.csv")

party.info <- read_csv("../data/merge_table/social_media_partyfacts_mergetable_revised_v2.csv") %>%
    select(twitter_handle_lower, author_id) %>% distinct(.keep_all = TRUE) %>% drop_na()

# Merging to drop any tweets that do not correspond to parties in our dataset
merged <- tweets %>% left_join(party.info, by = c("author_id"))
# N = 6,582,316

merged <- merged %>% filter(!is.na(twitter_handle_lower))
# N = 6,530,350

# Constructing retweet and year variables
merged <- merged %>% mutate(is_retweet = ifelse(str_starts(text, "RT "), T, F))
merged <- merged %>% mutate(year = year(created_at))

# Storing tweet level dataset
write_csv(merged, "../data/twitter_data_cleaned/tweets.csv")

# Storing handle-year aggregate dataset
dy <- merged %>% filter(!is_retweet) %>%
    group_by(author_id, twitter_handle = user_username, year) %>%
    summarize(total_tweets = n(),
              total_likes_tw = sum(like_count),
              total_retweets = sum(retweet_count),
              total_replies = sum(reply_count))

dy.rt <- merged %>% filter(is_retweet) %>%
    group_by(author_id, twitter_handle = user_username, year) %>%
    summarize(total_tweets_rt = n())

dy <- dy %>% left_join(dy.rt, by = c("author_id", "twitter_handle", "year"))

write_csv(dy, "../data/twitter_data_cleaned/twitter_yearly.csv")
