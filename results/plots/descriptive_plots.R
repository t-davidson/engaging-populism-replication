library(tidyverse)
library(ggplot2)
library(ggrepel)
library(viridis)
library(lubridate)
library(ggpubr)
library(lemon)
library(scales)
`%notin%` <- Negate(`%in%`) # Defining a helper function 

source("load-data-local.R")

###########################
## BAR CHARTS - POPULIST ##
###########################
data.fb$like_rate <- data.fb$total_likes/data.fb$total_posts
data.fb$share_rate <- data.fb$total_shares/data.fb$total_posts
data.fb$comment_rate <- data.fb$total_comments/data.fb$total_posts

data.fb$populist <- as.factor(data.fb$populist)
data.fb$populist <- recode_factor(data.fb$populist, `1` = "Populist", `0` = "Non-populist")
data.tw$populist <- as.factor(data.tw$populist)
data.tw$populist <- recode_factor(data.tw$populist, `1` = "Populist", `0` = "Non-populist")

# Setting rate to 0 when undefined.
data.fb <- data.fb %>% mutate(share_rate = ifelse(year >= 2013, share_rate, NA))

# GRID of four plots for posts, likes/posts, shares/posts, comments
p <- ggplot(data.fb %>% group_by(year.f, populist) %>% summarize(mean_posts = median(total_posts))  , 
            aes(fill = populist, x = year.f, y= mean_posts)) + 
    geom_bar(position = "dodge", stat = "identity", color = "grey10") +
    #geom_col() +
    scale_fill_grey() + theme_classic() +
    labs(x = "", y = "Posts", title = "Median posts", fill = "Party type") + 
    theme(axis.text.x = element_blank())

# Like rate by type
l <- ggplot(data.fb %>% group_by(year.f, populist) %>% summarize(mean_like_rate = median(like_rate)),
            aes(fill = populist, x = year.f, y= mean_like_rate)) + 
    geom_bar(position = "dodge", stat = "identity", color = "grey10") +
    #geom_col(position = "dodge") +
    scale_fill_grey() + theme_classic() +
    labs(x = "", y = "Likes / posts", title = "Median like rate",
         fill = "Party type")+ theme(axis.text.x = element_blank())

# Share rate by type
s <- ggplot(data.fb %>% group_by(year.f, populist) %>% summarize(mean_share_rate = median(share_rate)),
            aes(fill = populist, x = year.f, y= mean_share_rate)) + 
    geom_bar(position = "dodge", stat = "identity", color = "grey10") +
    #geom_col(position = "dodge") +
    scale_fill_grey() + theme_classic() +
    labs(x = "", y = "Shares / posts", title = "Median share rate",
         fill = "Party type")+ theme(axis.text.x = element_text(angle = 90))

# Comment rate by type
c <- ggplot(data.fb %>% group_by(year.f, populist) %>% summarize(mean_comment_rate = median(comment_rate)),
            aes(fill = populist, x = year.f, y= mean_comment_rate)) + 
    geom_bar(position = "dodge", stat = "identity", color = "grey10") +
    #geom_col(position = "dodge") +
    scale_fill_grey() + theme_classic() +
    labs(x = "", y = "Comments / posts", title = "Median comment rate",
         fill = "Party type")+ theme(axis.text.x = element_text(angle = 90))

g <- ggarrange(p,l,s,c, nrow=2, ncol=2, common.legend = T, legend="bottom")
#annotate_figure(g, top = text_grob("Facebook activity and engagement by populism (PopuList)",
#                                   color = "grey10", face = "bold", size = 14))
g
ggsave("figures/populist_fb_barchart_grey.pdf", width = 8.3, height =6)

# Twitter version
# Like rate by type
data.tw$like_rate <- data.tw$total_likes_tw/data.tw$total_tweets
data.tw$retweet_rate <- data.tw$total_retweets/data.tw$total_tweets
data.tw$reply_rate <- data.tw$total_replies/data.tw$total_tweets

# Set 2010 to zero since no replies
data.tw <- data.tw %>% mutate(reply_rate = ifelse(year > 2010, reply_rate, NA))


pt <- ggplot(data.tw %>% group_by(year.f, populist) %>% summarize(mean_posts = median(total_tweets))  , 
            aes(fill = populist, x = year.f, y= mean_posts)) + 
    geom_bar(position = "dodge", stat = "identity", color = "grey10") +
    #geom_col() +
    scale_fill_grey() + theme_classic() +
    labs(x = "", y = "Tweets", title = "Median tweets", fill = "Party type") + 
    theme(axis.text.x = element_blank())




lt <- ggplot(data.tw %>% group_by(year.f, populist) %>% summarize(mean_like_rate = median(like_rate)),
            aes(fill = populist, x = year.f, y= mean_like_rate)) + 
    geom_bar(position = "dodge", stat = "identity", color = "grey10") +
    #geom_col(position = "dodge") +
    scale_fill_grey() + theme_classic() +
    labs(x = "", y = "Likes / tweets", title = "Median like rate",
         fill = "Party type")+ theme(axis.text.x = element_blank())

# Share rate by type
st <- ggplot(data.tw %>% group_by(year.f, populist) %>% summarize(mean_share_rate = median(retweet_rate)),
            aes(fill = populist, x = year.f, y= mean_share_rate)) + 
    geom_bar(position = "dodge", stat = "identity", color = "grey10") +
    #geom_col(position = "dodge") +
    scale_fill_grey() + theme_classic() +
    labs(x = "", y = "Retweets / tweets", title = "Median retweet rate",
         fill = "Party type")+ theme(axis.text.x = element_text(angle = 90))

# Comment rate by type
ct <- ggplot(data.tw %>% group_by(year.f, populist) %>% summarize(mean_comment_rate = median(reply_rate)),
            aes(fill = populist, x = year.f, y= mean_comment_rate)) + 
    geom_bar(position = "dodge", stat = "identity", color = "grey10") +
    #geom_col(position = "dodge") +
    scale_fill_grey() + theme_classic() +
    labs(x = "", y = "Replies / tweets", title = "Median reply rate",
         fill = "Party type")+ theme(axis.text.x = element_text(angle = 90))

g2 <- ggarrange(pt,lt,st,ct, nrow=2, ncol=2, common.legend = T, legend="bottom")
g2
ggsave("figures/populist_tw_barchart_grey.pdf", width = 8.3, height =6)

# REPEATING FOR CATEGORICAL

# Modifying categories for plot
data.fb <- data.fb %>% mutate(populist_cat = as.factor(populist_cat))
levels(data.fb$populist_cat)  <- list(RWP = "rwp",
                              LWP = "lwp",
                              CP = "cp",
                              NP = "np")
data.fb$populist_cat <- fct_relevel(data.fb$populist_cat, "RWP", "LWP", "CP", "NP")

# GRID of four plots for posts, likes/posts, shares/posts, comments
# Using scale_fill_manual(values = c("#1B9E77", "#D95F02", "#7570B3", "grey60"))but for three parties, setting NP to grey
p <- ggplot(data.fb %>% group_by(year.f, populist_cat) %>% summarize(mean_posts = median(total_posts))  , 
            aes(fill = populist_cat, x = year.f, y= mean_posts)) + 
    geom_bar(position = "dodge", stat = "identity", color = "grey10") +
    scale_fill_manual(values = c("#1B9E77", "#D95F02", "#7570B3", "grey60")) + theme_classic() +
    labs(x = "", y = "Posts", title = "Median posts", fill = "Party type") + 
    theme(axis.text.x = element_blank())

# Like rate by type
l <- ggplot(data.fb %>% group_by(year.f, populist_cat) %>% summarize(mean_like_rate = median(like_rate)),
            aes(fill = populist_cat, x = year.f, y= mean_like_rate)) + 
    geom_bar(position = "dodge", stat = "identity", color = "grey10") +
    #geom_col(position = "dodge") +
    scale_fill_manual(values = c("#1B9E77", "#D95F02", "#7570B3", "grey60"))+ theme_classic() +
    labs(x = "", y = "Likes / posts", title = "Median like rate",
         fill = "Party type")+ theme(axis.text.x = element_blank())

# Share rate by type
s <- ggplot(data.fb %>% group_by(year.f, populist_cat) %>% summarize(mean_share_rate = median(share_rate)),
            aes(fill = populist_cat, x = year.f, y= mean_share_rate)) + 
    geom_bar(position = "dodge", stat = "identity", color = "grey10") +
    #geom_col(position = "dodge") +
    scale_fill_manual(values = c("#1B9E77", "#D95F02", "#7570B3", "grey60"))+ theme_classic() +
    labs(x = "", y = "Shares / posts", title = "Median share rate",
         fill = "Party type")+ theme(axis.text.x = element_text(angle = 90))

# Comment rate by type
c <- ggplot(data.fb %>% group_by(year.f, populist_cat) %>% summarize(mean_comment_rate = median(comment_rate)),
            aes(fill = populist_cat, x = year.f, y= mean_comment_rate)) + 
    geom_bar(position = "dodge", stat = "identity", color = "grey10") +
    #geom_col(position = "dodge") +
    scale_fill_manual(values = c("#1B9E77", "#D95F02", "#7570B3", "grey60"))+ theme_classic() +
    labs(x = "", y = "Comments / posts", title = "Median comment rate",
         fill = "Party type")+ theme(axis.text.x = element_text(angle = 90))

g <- ggarrange(p,l,s,c, nrow=2, ncol=2, common.legend = T, legend="bottom")
#annotate_figure(g, top = text_grob("Facebook activity and engagement by populism (PopuList)",
#                                   color = "grey10", face = "bold", size = 14))
g
ggsave("figures/populist_fb_barchart_by_ideology.pdf", width = 8.3, height =6)

# Twitter version

data.tw <- data.tw %>% mutate(populist_cat = as.factor(populist_cat))
levels(data.tw$populist_cat)  <- list(RWP = "rwp",
                                      LWP = "lwp",
                                      CP = "cp",
                                      NP = "np")
data.tw$populist_cat <- fct_relevel(data.tw$populist_cat, "RWP", "LWP", "CP", "NP")

pt <- ggplot(data.tw %>% group_by(year.f, populist_cat) %>% summarize(mean_posts = median(total_tweets))  , 
             aes(fill = populist_cat, x = year.f, y= mean_posts)) + 
    geom_bar(position = "dodge", stat = "identity", color = "grey10") +
    #geom_col() +
    scale_fill_manual(values = c("#1B9E77", "#D95F02", "#7570B3", "grey60"))+ theme_classic() +
    labs(x = "", y = "Tweets", title = "Median tweets", fill = "Party type") + 
    theme(axis.text.x = element_blank())




lt <- ggplot(data.tw %>% group_by(year.f, populist_cat) %>% summarize(mean_like_rate = median(like_rate)),
             aes(fill = populist_cat, x = year.f, y= mean_like_rate)) + 
    geom_bar(position = "dodge", stat = "identity", color = "grey10") +
    #geom_col(position = "dodge") +
    scale_fill_manual(values = c("#1B9E77", "#D95F02", "#7570B3", "grey60"))+ theme_classic() +
    labs(x = "", y = "Likes / tweets", title = "Median like rate",
         fill = "Party type")+ theme(axis.text.x = element_blank())

# Share rate by type
st <- ggplot(data.tw %>% group_by(year.f, populist_cat) %>% summarize(mean_share_rate = median(retweet_rate)),
             aes(fill = populist_cat, x = year.f, y= mean_share_rate)) + 
    geom_bar(position = "dodge", stat = "identity", color = "grey10") +
    #geom_col(position = "dodge") +
    scale_fill_manual(values = c("#1B9E77", "#D95F02", "#7570B3", "grey60"))+ theme_classic() +
    labs(x = "", y = "Retweets / tweets", title = "Median retweet rate",
         fill = "Party type")+ theme(axis.text.x = element_text(angle = 90))

# Comment rate by type
ct <- ggplot(data.tw %>% group_by(year.f, populist_cat) %>% summarize(mean_comment_rate = median(reply_rate)),
             aes(fill = populist_cat, x = year.f, y= mean_comment_rate)) + 
    geom_bar(position = "dodge", stat = "identity", color = "grey10") +
    #geom_col(position = "dodge") +
    scale_fill_manual(values = c("#1B9E77", "#D95F02", "#7570B3", "grey60"))+ theme_classic() +
    labs(x = "", y = "Replies / tweets", title = "Median reply rate",
         fill = "Party type")+ theme(axis.text.x = element_text(angle = 90))

g2 <- ggarrange(pt,lt,st,ct, nrow=2, ncol=2, common.legend = T, legend="bottom")
g2
ggsave("figures/populist_tw_barchart_by_ideology.pdf")

