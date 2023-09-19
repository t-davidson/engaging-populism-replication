source("load-data-local.R")

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

### 



se_type <- "cluster"
outcomes <- c("total_likes", "total_shares", "total_comments")
ivs <- c("populist", "poppa_populism_s", "gps_populism_s")
results <- tibble()

for (outcome in outcomes) {
    for (populism_measure in ivs) {
        
        if (outcome == "total_shares") {
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
        
        m0 <- feols(outcome ~ P, panel.id = ~ handle + year.f, data = tmp)
        d0 <- extract.info(summary(m0), i = 2)
        results <- bind_rows(results, as.data.frame(t(c(outcome, populism_measure, "1. Bivariate (Pooled)", d0[1], d0[2]))))
        
        # Bivariate FE
        m1 <- feols(outcome ~ P | country + year.f, panel.id = ~ handle + year.f,  tmp)
        d1 <- extract.info(summary(m1, se_type)) # first coef since no intercept provided
        results <- bind_rows(results, as.data.frame(t(c(outcome, populism_measure, "2. Bivariate (FE)", d1[1], d1[2]))))
        
        # + posts
        m2 <- feols(outcome ~ P + log(total_posts) | country + year.f, panel.id = ~handle + year.f,  tmp)
        d2 <- extract.info(summary(m2, se_type))
        results <- bind_rows(results, as.data.frame(t(c(outcome, populism_measure, "3. + log(Posts/Tweets)", d2[1], d2[2]))))
        
        # + ideology
        m3 <- feols(outcome ~ P + log(total_posts) + left_right_s | country + year.f, panel.id = ~handle + year.f,  tmp)
        d3 <- extract.info(summary(m3, se_type))
        results <- bind_rows(results, as.data.frame(t(c(outcome, populism_measure, "4. + Ideology", d3[1], d3[2]))))
        
        # + ideology, support & incumbency
        m5 <- feols(outcome ~ P + log(total_posts) + left_right_s +
                        cabinet_party_max + prime_minister_max +
                        mean_vote_share | country + year.f, panel.id = ~handle + year.f,  tmp)
        d5 <- extract.info(summary(m5, se_type))
        results <- bind_rows(results, as.data.frame(t(c(outcome, populism_measure, "5. + Incumbency", d5[1], d5[2]))))
        
        # + country factors
        m6 <- feols(outcome ~ P + log(total_posts) + left_right_s +
                        cabinet_party_max + prime_minister_max + mean_vote_share +
                        gdp_s + pop_s + everyday_social_networks_imputed_s +
                        election_year  | country + year.f,
                    panel.id = ~handle + year.f,  tmp)
        d6 <- extract.info(summary(m6, se_type))
        results <- bind_rows(results, as.data.frame(t(c(outcome, populism_measure, "6. + Country", d6[1], d6[2]))))
        
    }
}

results.fb <- results
results.fb$platform <- "Facebook"

# Twitter
results <- tibble()
outcomes <- c("total_likes_tw", "total_retweets", "total_replies")
for (outcome in outcomes) {
    for (populism_measure in ivs) {
        
        if (outcome == "total_replies") {
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
        
        m0 <- feols(outcome ~ P, panel.id = ~ twitter_handle + year.f, data = tmp)
        d0 <- extract.info(summary(m0), i = 2)
        results <- bind_rows(results, as.data.frame(t(c(outcome, populism_measure, "1. Bivariate (Pooled)", d0[1], d0[2]))))
        
        # Bivariate FE
        m1 <- feols(outcome ~ P | country + year.f, panel.id = ~twitter_handle + year.f,  tmp)
        d1 <- extract.info(summary(m1, se_type)) # first coef since no intercept provided
        results <- bind_rows(results, as.data.frame(t(c(outcome, populism_measure, "2. Bivariate (FE)", d1[1], d1[2]))))
        
        # + posts
        m2 <- feols(outcome ~ P + log(total_tweets) | country + year.f, panel.id = ~twitter_handle + year.f,  tmp)
        d2 <- extract.info(summary(m2, se_type))
        results <- bind_rows(results, as.data.frame(t(c(outcome, populism_measure, "3. + log(Posts/Tweets)", d2[1], d2[2]))))
        
        # + ideology
        m3 <- feols(outcome ~ P + log(total_tweets) + left_right_s | country + year.f, panel.id = ~twitter_handle + year.f,  tmp)
        d3 <- extract.info(summary(m3, se_type))
        results <- bind_rows(results, as.data.frame(t(c(outcome, populism_measure, "4. + Ideology", d3[1], d3[2]))))
        
        # + ideology, support & incumbency
        m5 <- feols(outcome ~ P + log(total_tweets) + left_right_s +
                        cabinet_party_max + prime_minister_max +
                        mean_vote_share | country + year.f, panel.id = ~twitter_handle + year.f,  tmp)
        d5 <- extract.info(summary(m5, se_type))
        results <- bind_rows(results, as.data.frame(t(c(outcome, populism_measure, "5. + Incumbency", d5[1], d5[2]))))
        
        # + country factors
        m6 <- feols(outcome ~ P + log(total_tweets) + left_right_s +
                        cabinet_party_max + prime_minister_max + mean_vote_share +
                        gdp_s + pop_s + everyday_social_networks_imputed_s +
                        election_year  | country + year.f,
                    panel.id = ~twitter_handle + year.f,  tmp)
        d6 <- extract.info(summary(m6, se_type))
        results <- bind_rows(results, as.data.frame(t(c(outcome, populism_measure, "6. + Country", d6[1], d6[2]))))
        
    }
}

results$platform <- "Twitter"

results <- bind_rows(results, results.fb)
colnames(results) <- c("Outcome", "Populism", "Model", "Coef", "SE", "Platform")

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

levels(results$Outcome) <- list(Likes = "total_likes",
                                Shares = "total_shares",
                                Comments = "total_comments",
                                "Likes " = "total_likes_tw",
                                Retweets = "total_replies",
                                Replies = "total_retweets")

#results$Outcome <- fct_relevel(results$Outcome, "Likes", "Shares", "Comments", "Likes ", "Retweets", "Replies")

# Fixed-effects models include two-way standard errors by party and time
options(scipen=10000)
p <- ggplot(aes(y = fct_rev(Populism), x = Coef, group = fct_rev(Model), color = Model), data = results) +
    geom_point(position = position_dodge(0.5)) +
    geom_errorbar(aes(xmin = lower, xmax = upper, width = 0), position = position_dodge(0.5)) +
    scale_y_discrete() +
    geom_vline(xintercept = 0, linetype = "dashed") +  theme_minimal() +
    scale_color_viridis_d(option = "viridis") + facet_wrap(. ~ Outcome, ncol = 3, scales = "free_x") + 
    theme(axis.text.x = element_text(angle = 45), panel.spacing = unit(1, "lines")) +
    labs(y = "", x = TeX("$\\hat{\\beta}_{populism}$"))
ggdraw(p) + draw_label("Facebook", x = 0.08, y = 0.97) + draw_label("Twitter", x = 0.08, y = 0.5)
ggsave2("figures/ols_baseline_fixed_effects.pdf", width = 8.3, height = 7)

