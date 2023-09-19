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
set.seed(08540)

path <- "../output/"
files <- list.files(path = path) %>% str_subset(".rds") # list of model files

files <- files[grepl("_cat_", files)] # Focusing on main results (not split by category)
files <- files[grepl("_rs_cy.rds", files)] # And only final models

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
    
    result <- m %>% gather_draws(b_populist_catrwp, b_populist_catlwp, b_populist_catcp) %>%
        median_hdi(.width = c(.95, .68)) 
    
    result$platform <- platform
    result$outcome <- outcome
    
    final_results <- bind_rows(final_results, result)
}

# Recoding populism
final_results$Populism <- as_factor(final_results$.variable)

levels(final_results$Populism) <- list(RWP = "b_populist_catrwp",
                                       LWP = "b_populist_catlwp",
                                       CP = "b_populist_catcp")

final_results$Populism <- fct_relevel(final_results$Populism, "RWP", "LWP", "CP")

# Recoding outcomes

final_results$Outcome <- as_factor(final_results$outcome)

levels(final_results$Outcome) <- list(Likes = "likes",
                                      Shares = "shares",
                                      Comments = "comments",
                                      "Likes " = "tw_likes",
                                      Retweets = "retweets",
                                      Replies = "replies")
final_results$Outcome <- fct_relevel(final_results$Outcome, "Likes", "Shares", "Comments", "Likes ", "Retweets", "Replies")



# Repeat final version with only CY model
p <- final_results %>% ggplot(aes(y = fct_rev(Populism), x = exp(.value), xmin = exp(.lower), xmax = exp(.upper), color = Populism, group = Populism)) +
    geom_pointinterval(interval_size_range = c(0.5,1), point_size = 1)  + # scale best for printing
    scale_y_discrete() + 
    geom_vline(xintercept = 1, linetype = "dashed") +  theme_minimal() +
    facet_wrap(. ~ Outcome, ncol = 3) + scale_color_brewer(palette = "Dark2") +#scale_color_viridis_d(option = "magma") +
    theme(panel.spacing = unit(1, "lines")) +
    labs(y = "", x = TeX("$exp(\\hat{\\beta}_{populism})$")) + theme(panel.grid.major.y = element_blank()) 
ggdraw(p) + draw_label("Facebook", x = 0.08, y = 0.98) + draw_label("Twitter", x = 0.08, y = 0.5)
ggsave2(filename = "figures/figure4.pdf", width = 6.3, height = 5)
