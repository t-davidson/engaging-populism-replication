source("load-data-disaggregated-local.R")

library(fixest)
library(tidyverse)
library(forcats)
library(viridis)
library(cowplot)
library(latex2exp)
library(scales)


extract.info <- function(s, i = 1) {
    return(c(s$coefficients[[i]], s$se[[i]]))
}


se_type <- "cluster" # Specifying standard error type

# Running models for Facebook
outcomes <- c("likes", "shares", "comments")
ivs <- c("populist", "poppa_populism_s", "gps_populism_s")
results <- tibble()

# Estimating models for all combinations of outcomes and independent variables
for (outcome in outcomes) {
    for (populism_measure in ivs) {
        
        if (outcome == "shares") {
            tmp <- data.fb.shares
        } else {
            tmp <- data.fb
        }
        
        tmp$outcome <- as.matrix(tmp[c(outcome)])
        tmp$P <- as.matrix(tmp[c(populism_measure)])
        
        if (populism_measure != "populist") {
            # Drop missing
            tmp <- tmp %>% drop_na(P)
        }
        
        m <- feols(outcome ~ P + post_type + hr + hr_sq + word_count + word_count_sq +
                        left_right_s +
                        cabinet_party_max + prime_minister_max + mean_vote_share +
                        gdp_s + pop_s + everyday_social_networks_imputed_s +
                        election_year  | country + year.f,
                    panel.id = ~handle + year.f,  tmp)
        d <- extract.info(summary(m, se_type))
        results <- bind_rows(results, as.data.frame(t(c(outcome, populism_measure, d[1], d[2]))))
        
    }
}
results.fb <- results
results.fb$Platform <- "Facebook"
colnames(results.fb) <- c("Outcome", "Populism", "Coef", "SE", "Platform")



# Repeating for Twitter
results <- tibble()
outcomes <- c("like_count", "retweet_count", "reply_count")
for (outcome in outcomes) {
    for (populism_measure in ivs) {
        
        if (outcome == "replies") {
            tmp <- data.tw.replies
        } else {
            tmp <- data.tw
        }
        
        tmp$outcome <- as.matrix(tmp[c(outcome)])
        tmp$P <- as.matrix(tmp[c(populism_measure)])
        
        if (populism_measure != "populist") {
            # Drop missing and scale by 2 sd
            tmp <- tmp %>% drop_na(P)
        }
        
        # + country factors
        m <- feols(outcome ~ P + contains_url  + hr + hr_sq + word_count + word_count_sq + 
                       left_right_s +
                        cabinet_party_max + prime_minister_max + mean_vote_share +
                        gdp_s + pop_s + everyday_social_networks_imputed_s +
                        election_year  | country + year.f,
                    panel.id = ~twitter_handle + year.f,  tmp)
        d <- extract.info(summary(m, se_type))
        results <- bind_rows(results, as.data.frame(t(c(outcome, populism_measure, d[1], d[2]))))
        
    }
}
results$Platform <- "Twitter"
colnames(results) <- c("Outcome", "Populism", "Coef", "SE", "Platform")
results <- bind_rows(results, results.fb)


results$Coef <- as.numeric(results$Coef)
results$SE <- as.numeric(results$SE)
results <- results %>% mutate(lower = Coef-(1.96*SE),
                              upper = Coef+(1.96*SE))

results$Populism <- as_factor(results$Populism)

levels(results$Populism) <- list(PopuList = "populist",
                                 POPPA = "poppa_populism_s",
                                 GPS = "gps_populism_s")

results$Populism <- fct_relevel(results$Populism, "PopuList", "POPPA", "GPS")

results$Outcome <- as_factor(results$Outcome)

levels(results$Outcome) <- list(Likes = "likes",
                                Shares = "shares",
                                Comments = "comments",
                                "Likes " = "like_count",
                                Retweets = "retweet_count",
                                Replies = "reply_count")

# Creating plot showing estimates for all models
options(scipen=10000)
p <- ggplot(aes(y = fct_rev(Populism), x = Coef), data = results) +
    geom_point(position = position_dodge(0.5)) +
    geom_errorbar(aes(xmin = lower, xmax = upper, width = 0), position = position_dodge(0.5)) +
    scale_y_discrete() +
    geom_vline(xintercept = 0, linetype = "dashed") +  theme_minimal() +
    facet_wrap(. ~ Outcome, ncol = 3, scales = "free_x") + 
    theme(axis.text.x = element_text(angle = 45), panel.spacing = unit(1, "lines")) +
    labs(y = "", x = TeX("$\\hat{\\beta}_{populism}$"))
ggdraw(p) + draw_label("Facebook", x = 0.08, y = 0.97) + draw_label("Twitter", x = 0.08, y = 0.5)
ggsave2("figures/ols_fixed_effects_post_tweet.pdf", width = 8.3, height = 5)

