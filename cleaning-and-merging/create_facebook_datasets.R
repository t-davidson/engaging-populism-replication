library(tidyverse)

# Loading Facebook data collected from CrowdTangle
# Two versions: Original data collection and parties missed on first pass
d <- read_csv("../data/facebook_data_raw/facebook_dump_2.csv")
d2 <- read_csv("../data/facebook_data_raw/facebook_dump_missing.csv")

# Some unused columns are slightly different across two versions so need to ensure common names/types
# before merging the dataframes together

# Selecting compatible columns with d2
d <- d %>% select(page_id, handle, name, post_id, date_added, type, link, likes, shares, comments, message)
d2 <- d2 %>% select(page_id, handle, name, post_id, date_added, type, link, likes, shares, comments, message)

d <- bind_rows(d, d2)
# N = 2,561,356

# Creating new primary key since some posts are missing post_id or link but all have at least one
d$unique <- paste0(d$post_id, d$link)

d <- d %>% distinct(unique, .keep_all = T)
# N = 2,511,356

d <- d %>% mutate(year = year(date_added))

d <- d %>% mutate(handle = tolower(handle))

pages <- d %>% group_by(handle) %>% summarize(n())

# Some of the handles are missing
# This means some rows dropped when merging
# Loading mapping file to complete this merge

keys <- read_csv("../data/facebook_data_raw/keys.csv") %>%
    mutate(handle = tolower(handle))

d.missing_handle <- d %>% filter(is.na(handle) & !is.na(page_id))
missing.pages <- d.missing_handle %>% group_by(page_id) %>% summarize(missing_rows = n()) %>%
    left_join(keys, by = "page_id") %>% filter(!is.na(handle))

d <- d %>% mutate(handle = ifelse(page_id == 1552684, "50pluspartij", handle))
d <- d %>% mutate(handle = ifelse(page_id == 2831386, "zemniekusavieniba", handle))
d <- d %>% mutate(handle = ifelse(page_id == 2831699, "vienti-latvijai", handle))
d <- d %>% mutate(handle = ifelse(page_id == 5697150, "robertsvecsho", handle))
d <- d %>% mutate(handle = ifelse(page_id == 6410829, "fremskridtspartiet", handle))
d <- d %>% mutate(handle = ifelse(page_id == 7743200, "ant", handle))
d <- d %>% mutate(handle = ifelse(page_id == 11959526, "fkgp", handle))
d <- d %>% mutate(handle = ifelse(page_id == 16511470, "ekd", handle))

# The remaining missing pages do not have handles in my list and cannot be joined
# Note that some of the data returned by CrowdTangle corresponded to other pages so this
# is expected

# 2.5M posts

# Merging with party-info

party.info <- read_csv("../data/merge_table/social_media_partyfacts_mergetable_revised.csv") %>%
    mutate(handle = tolower(handle)) %>% select(page_id, handle) %>% distinct(handle, .keep_all = TRUE)
# Note: Some handles occur multiple times if associated with more than one party ID (e.g. Sinn Fein)
# Just matching on handle here

# Note: Some parties missing handles in dump and some missing page_id in party.info
# Need to get common format or we lose parties

d <- d %>% filter(handle %in% party.info$handle)
# 2.31M posts with matching handles

# Storing post-level dataset
write_csv(d, "../data/facebook_data_cleaned/facebook_posts.csv")

# Storing handle-year aggregate dataset
dy <- d %>% group_by(handle, page_id, year) %>%
    summarize(total_posts = n(),
              total_likes = sum(likes),
              total_shares = sum(shares),
              total_comments = sum(comments))

write_csv(dy, "../data/facebook_data_cleaned/facebook_yearly.csv")
