source("load-data.R") # Loading data

#############################
#          MODELS           #
#############################

library(brms)
seed <- 1485308901

# Outcome: Facebook likes
# Populism measure: populist_cat

# Year fixed effects
m <- brm(
    total_likes ~ log(total_posts) + year.f + country +
        populist_cat +
        # ideology
        left_right_s +
        # party
        cabinet_party_max + prime_minister_max + mean_vote_share +
        # 3 Net migration
        gdp_s + pop_s + 
        gdp_growth + unemp_ilo + 
        net_migr_s + refugee_change_s +
        everyday_social_networks_imputed + internet_adoption  +
        election_year +
        net_migr_s*populist_cat,
    prior = c(prior(normal(0,1), class = "b"),
              prior(normal(0,10), class = "Intercept"),
              prior(exponential(1), class = "shape")),
    data = data.fb, family = negbinomial,
    iter = 11000, warmup = 1000, chains = 1,
    control = list(
    adapt_delta = 0.95),
    file = 'model_results_interactions/fb_likes_populist_cat_immgr',
    seed = seed,
    backend = "cmdstanr")

print("Model completed.")
