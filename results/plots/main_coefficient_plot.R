# Bayesian Model visualization
library(brms)
library(tidyverse)
library(viridis)
library(grid)
library(gridExtra)
library(cowplot)
library(scales)
library(latex2exp)
library(egg)
library(tidybayes)

path <- "../output/"
files <- list.files(path = path) %>% str_subset(".rds") # list of model files

files <- files[!grepl("_cat_", files)] # Focusing on main results (not split by category)
files <- files[!grepl("_post_", files)] 
files <- files[!grepl("_tweet_", files)] 

# Loading models and extract info
final_results <- tibble()
for (f in files) {
    m <- readRDS(paste(path, f, sep = ""))
    name <- str_split(f, pattern = ".rds")[[1]]
    info <- str_split(name[1], pattern = "_") %>% unlist()
    print(info)
    platform <- info[1]
    outcome <- info[2]
    if (platform == "tw" & outcome == "likes") {
        outcome <- "tw_likes" # Avoid class with Facebook
    }
    populism <- info[3]
    model <- info[4]
    if (length(info) == 5) {
        model <- info[5]
    }
    d <- fixef(m) # Get model coefs
    vars <- rownames(d) # Get var names
    d <- d %>% as_tibble() %>% mutate(var = vars)
    
    # Select effect of populism
    if (populism == "gps") {
        result <- m %>% gather_draws(b_gps_populism_s) %>%
            median_hdi(.width = c(.95, .68))
    } else if (populism == "poppa") {
        result <- m %>% gather_draws(b_poppa_populism_s) %>%
            median_hdi(.width = c(.95, .68))
    } else {result <- m %>% gather_draws(b_populist1) %>%
        median_hdi(.width = c(.95, .68)) 
    }
    
    result$Platform <- platform
    result$Outcome <- outcome
    result$Model <- model
    result$Populism = populism
    
    final_results <- bind_rows(final_results, result)
}


# Recoding populism
final_results$Populism <- as_factor(final_results$Populism)

levels(final_results$Populism) <- list(PopuList = "populist",
                                       POPPA = "poppa",
                                       GPS = "gps")

final_results$Populism <- fct_relevel(final_results$Populism, "PopuList", "POPPA", "GPS")

# Recoding outcomes

final_results$Outcome <- as_factor(final_results$Outcome)

levels(final_results$Outcome) <- list(Likes = "likes",
                                      Shares = "shares",
                                      Comments = "comments",
                                      "Likes " = "tw_likes",
                                      Retweets = "retweets",
                                      Replies = "replies")
final_results$Outcome <- fct_relevel(final_results$Outcome, "Likes", "Shares", "Comments", "Likes ", "Retweets", "Replies")

# Model
final_results$Model <- as_factor(final_results$Model)
levels(final_results$Model) <- list(FE = "fe",
                                    RE = "re",
                                    RS = "rs",
                                    "RS (CY)" = "cy")
# Reorder
final_results$Model <- fct_relevel(final_results$Model, "FE", "RE", "RS", "RS (CY)")

labs <- list("FE" = TeX("$FE_{c,y}$"),
             "RE" = TeX("$RI_{p,c,y}$"),
             "RS" = TeX("$RI_{p,c,y} + RC_{c,y}$"),
             "RS (CY)" = TeX("$RI_{p,c,y,cy} + RC_{c,y,cy}$"))



# Running for final CY model only
results_cy <- final_results %>% filter(Model == "RS (CY)")
p <- results_cy %>% ggplot(aes(y = fct_rev(Populism), x = exp(.value), xmin = exp(.lower), xmax = exp(.upper))) +
    geom_pointinterval(position = position_dodge(0.5), interval_size_range = c(0.5,1), point_size = 1)  + # scale best for printing
    scale_y_discrete() + 
    geom_vline(xintercept = 1, linetype = "dashed") +  theme_minimal() +
    facet_wrap(. ~ Outcome, ncol = 3) + 
    theme(panel.spacing = unit(1, "lines")) +
    labs(y = "", x = TeX("$exp(\\hat{\\beta}_{populism})$")) + theme(panel.grid.major.y = element_blank()) # I originally had a "free" x but this works better
ggdraw(p) + draw_label("Facebook", x = 0.08, y = 0.97) + draw_label("Twitter", x = 0.08, y = 0.5)
ggsave2(filename = "figures/figure3.pdf", width = 6.3, height = 5)

# Supplementary figure with all models
p.all <- final_results %>% ggplot(aes(y = fct_rev(Populism), x = exp(.value), xmin = exp(.lower), xmax = exp(.upper), group = fct_rev(Model), color = Model)) +
    geom_pointinterval(position = position_dodge(0.5), interval_size_range = c(0.5,1), point_size = 1)  + # scale best for printing
    scale_y_discrete() + 
    geom_vline(xintercept = 1, linetype = "dashed") +  theme_minimal() +
    scale_color_viridis_d() +
    facet_wrap(. ~ Outcome, ncol = 3) + 
    theme(panel.spacing = unit(1, "lines")) +
    labs(y = "", x = TeX("$exp(\\hat{\\beta}_{populism})$")) + theme(panel.grid.major.y = element_blank()) # I originally had a "free" x but this works better
ggdraw(p.all) + draw_label("Facebook", x = 0.08, y = 0.97) + draw_label("Twitter", x = 0.08, y = 0.5)
ggsave2(filename = "figures/figure3-supplementary.pdf", width = 6.3, height = 5)
