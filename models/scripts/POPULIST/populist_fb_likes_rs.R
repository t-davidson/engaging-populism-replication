source("load-data.R") # Loading data

#############################
#          MODELS           #
#############################

library(brms)
seed <- 1485308901

# Outcome: Facebook likes
# Populism measure: PopuList

# Year fixed effects + full random effects + populism random slope for country and year
m <- brm(
    total_likes ~ log(total_posts) +
      populist +
      # ideology
      left_right_s +
      # party
      cabinet_party_max + prime_minister_max + mean_vote_share +
      # country
      gdp_s + pop_s + everyday_social_networks_imputed_s +
      # election
      election_year +
      # random part
      (1 |handle) + (1 + populist |country) + (1 + populist |year.f),
    prior = c(prior(normal(0,1), class = "b"),
              prior(normal(0,10), class = "Intercept"),
              prior(exponential(1), class = "shape"),
              prior(exponential(1), class = "sd"),
              prior(lkj_corr_cholesky(2), class = "L")),
    data = data.fb, family = negbinomial,
    iter = 11000, warmup = 1000, chains = 1,
    control = list(
        adapt_delta = 0.95),
    file = 'model_results/fb_likes_populist_rs',
    seed = seed,
    backend = "cmdstanr")
