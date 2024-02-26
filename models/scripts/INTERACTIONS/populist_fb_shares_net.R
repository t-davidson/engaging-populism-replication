source("load-data.R") # Loading data

#############################
#          MODELS           #
#############################

library(brms)
seed <- 1485308901

# Outcome: Facebook shares
# Populism measure: populist_cat

# Year fixed effects
m <- brm(
    total_shares ~ log(total_posts) + year.f + country +
        populist_cat +
        # ideology
        left_right_s +
        # party
        cabinet_party_max + prime_minister_max + mean_vote_share +
        # 6 Internet adoption
        gdp_s + pop_s + 
        gdp_growth + unemp_ilo + 
        net_migr_s + refugee_change_s +
        everyday_social_networks_imputed + internet_adoption  +
        election_year +
        internet_adoption*populist_cat,
    prior = c(prior(normal(0,1), class = "b"),
              prior(normal(0,10), class = "Intercept"),
              prior(exponential(1), class = "shape")),
    data = data.fb.shares, family = negbinomial,
    iter = 11000, warmup = 1000, chains = 1,
    control = list(
    adapt_delta = 0.95),
    file = 'model_results_interactions/fb_shares_populist_cat_net',
    seed = seed,
    backend = "cmdstanr")

print("Model completed.")
