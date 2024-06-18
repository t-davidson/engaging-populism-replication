library(tidyverse)
library(dplyr)
library(lubridate)
library(jsonlite)
library(academictwitteR)

# This script collects all data from Twitter handles
# It requires access to the Twitter Academic API
# This was shut down after Elon Musk took over the company

# Read Twitter handles from CSV file
handles <- read_csv("../data/merge_table/social_media_partyfacts_mergetable_revised_v2.csv")

twitter_handles <- handles %>% drop_na(twitter_handle)

# Load API credentials
creds <- read_json("twitter_creds.json")

# For each handle, pull entire timeline and store
for (h in twitter_handles$twitter_handle) {
    tweets <- get_all_tweets(users = h,
                             start_tweets = "2010-01-01T00:00:00Z",
                             end_tweets = "2021-01-01T00:00:00Z",
                         bearer_token = get_bearer(),
                         data_path = "../data/twitter_data_raw/", n=2000000)
}


