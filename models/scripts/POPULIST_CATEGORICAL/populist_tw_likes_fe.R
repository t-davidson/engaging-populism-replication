source("load-data.R") # Loading data

#############################
#          MODELS           #
#############################

library(brms)
seed <- 1485308901

# Outcome: Twitter likes
# Populism measure: populist_cat

# Year fixed effects
m <- brm(
    total_likes_tw ~ log(total_tweets) + year.f + country +
        populist_cat +
        # ideology
        left_right_s +
        # party
        cabinet_party_max + prime_minister_max + mean_vote_share +
        # country
        gdp_s + pop_s + everyday_social_networks_imputed_s +
        # election
        election_year,
    prior = c(prior(normal(0,1), class = "b"),
              prior(normal(0,10), class = "Intercept"),
              prior(exponential(1), class = "shape")),
    data = data.tw, family = negbinomial,
    iter = 11000, warmup = 1000, chains = 1,
    control = list(
    adapt_delta = 0.95),
    file = 'model_results/tw_likes_populist_cat_fe',
    seed = seed,
    backend = "cmdstanr")

print("Model completed.")
