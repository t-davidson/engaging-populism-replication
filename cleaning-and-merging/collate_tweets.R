# The Twitter data are contained in three different directories, each of which contains many JSON
# files containing raw data downloaded using Academic Twitter API

# This script loads these files using helper functions from academictwitteR and compiles a single dataset
# of unique tweets

# IMPORTANT: The bind_tweets function does not return reply_count, see https://github.com/cjbarrie/academictwitteR/issues/294
# Must install modified version using the following line for this to work (uncomment and run the following line)
# devtools::install_github("t-davidson/academictwitteR", build_vignettes = F)

library(academictwitteR)
library(tidyverse)

future::plan("multisession") # Running in parallel for loading raw Twitter data, otherwise this takes all day
tw1 <- bind_tweets("../../Twitter-Data-Script/data/", output_format = "tidy") %>%
    select(tweet_id, user_username, author_id, text, created_at, like_count, retweet_count, reply_count, quote_count)
write_csv(tw2, "../data/twitter_data_raw/twitter_dump_1.csv")

tw2 <- bind_tweets("../../Twitter-Data-Script/missing_twitter_data/", output_format = "tidy") %>%
    select(tweet_id, user_username, author_id, text, created_at, like_count, retweet_count, reply_count, quote_count)
write_csv(tw2, "../data/twitter_data_raw/twitter_dump_2.csv")

tw3 <- bind_tweets("../../Twitter-Data-Script/missing_twitter_data_2/", output_format = "tidy") %>%
    select(tweet_id, user_username, author_id, text, created_at, like_count, retweet_count, reply_count, quote_count)
write_csv(tw3, "../data/twitter_data_raw/twitter_dump_3.csv")

future::plan("sequential")
all_tweets <- bind_rows(tw1, tw2, tw3) # N = 7029506

write_csv(all_tweets, "../data/twitter_data_raw/twitter_dump_full.csv")

# Dropping duplicates after sorting by likes
# This should ensure most recent version (i.e. most likes) is retained
all_tweets <- all_tweets %>%
    arrange(desc(like_count)) %>%  # Arrange in descending order of like_count
    distinct(tweet_id, .keep_all = TRUE) # keep distinct by tweet_id

# N = 6582316
write_csv(all_tweets, "../data/twitter_data_raw/twitter_dump_full_unique.csv")
