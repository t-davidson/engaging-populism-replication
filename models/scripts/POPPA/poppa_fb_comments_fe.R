source("load-data.R") # Loading data

#############################
#          MODELS           #
#############################

library(brms)
seed <- 1485308901

# Outcome: Facebook comments
# Populism measure: POPPA

# Year fixed effects
m <- brm(
    total_comments ~ log(total_posts) + year.f + country +
        poppa_populism_s +
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
    data = data.fb, family = negbinomial,
    iter = 11000, warmup = 1000, chains = 1,
    control = list(
    adapt_delta = 0.95),
    file = 'model_results/fb_comments_poppa_fe',
    seed = seed,
    backend = "cmdstanr")

print("Model completed.")
