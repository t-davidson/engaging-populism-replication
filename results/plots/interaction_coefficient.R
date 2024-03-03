# Bayesian Model visualization for interaction models
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

path <- "../output_interactions/"
files <- list.files(path = path) %>% str_subset(".rds") # list of model files

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

    if (info[5] == "elect") {
        result <- m %>% gather_draws(`b_populist_catcp:election_year1`, `b_populist_catlwp:election_year1`, `b_populist_catrwp:election_year1`) %>%
            median_hdi(.width = c(.95, .68))
    }
    if (info[5] == "gdp") {
        result <- m %>% gather_draws(`b_populist_catcp:gdp_growth`, `b_populist_catlwp:gdp_growth`, `b_populist_catrwp:gdp_growth`) %>%
            median_hdi(.width = c(.95, .68))
    }
    if (info[5] == "unemp") {
        result <- m %>% gather_draws(`b_populist_catcp:unemp_ilo`, `b_populist_catlwp:unemp_ilo`, `b_populist_catrwp:unemp_ilo`) %>%
            median_hdi(.width = c(.95, .68))
    }
    if (info[5] == "immgr") {
        result <- m %>% gather_draws(`b_populist_catcp:net_migr_s`, `b_populist_catlwp:net_migr_s`, `b_populist_catrwp:net_migr_s`) %>%
            median_hdi(.width = c(.95, .68))
    }
    if (info[5] == "refg") {
        result <- m %>% gather_draws(`b_populist_catcp:refugee_change_s`, `b_populist_catlwp:refugee_change_s`, `b_populist_catrwp:refugee_change_s`) %>%
            median_hdi(.width = c(.95, .68))
    }
    if (info[5] == "socmed") {
        result <- m %>% gather_draws(`b_populist_catcp:everyday_social_networks_imputed`, `b_populist_catlwp:everyday_social_networks_imputed`, `b_populist_catrwp:everyday_social_networks_imputed`) %>%
            median_hdi(.width = c(.95, .68))
    }
    if (info[5] == "net") {
        result <- m %>% gather_draws(`b_populist_catcp:internet_adoption`, `b_populist_catlwp:internet_adoption`, `b_populist_catrwp:internet_adoption`) %>%
            median_hdi(.width = c(.95, .68))
    }


    result$platform <- platform
    result$outcome <- outcome
    result$interaction <- info[5]

    final_results <- bind_rows(final_results, result)
}

# Recoding interactions
final_results <- final_results %>%
    # Separate 'variable' into intermediate columns
    separate(.variable, into = c("Populism_part", "Covariate"), sep = ":") %>%
    # Create 'Populism' with specific matches
    mutate(Populism = case_when(
        str_detect(Populism_part, "cp$") ~ "CP",
        str_detect(Populism_part, "lwp$") ~ "LWP",
        str_detect(Populism_part, "rwp$") ~ "RWP",
        TRUE ~ as.character(NA)  # Handle unexpected cases
    )) %>%
    # Convert 'Populism' and 'Covariate' to factors
    mutate(Populism = factor(Populism)) %>%
    # Optionally, remove the intermediate 'Populism_part' column
    select(-Populism_part)


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

# Recoding interaction variables

final_results$Covariate <- as_factor(final_results$Covariate)

levels(final_results$Covariate) <- list("GDP change\n(%)" = "gdp_growth",
                                      "Unemp. rate\n(%)" = "unemp_ilo",
                                      "Net migration\n(2SD)"  = "net_migr_s",
                                      "Refugee pop.\nchange (2SD)" = "refugee_change_s",
                                      Election = "election_year1",
                                      "Social media\nadoption (%)" = "everyday_social_networks_imputed",
                                      "Internet\nadoption (%)" = "internet_adoption"
                                      )

# Making plot showing all coefficients
p <- final_results %>% ggplot(aes(y = fct_rev(Outcome), x = exp(.value), xmin = exp(.lower), xmax = exp(.upper), color = Populism, group = Populism)) +
    geom_pointinterval(interval_size_range = c(0.5,1), point_size = 1, position = position_dodge(width = 0.35))  + # scale best for printing
    scale_y_discrete() +
    geom_vline(xintercept = 1, linetype = "dashed") +  theme_bw() +
    facet_grid(. ~ Covariate, scales = "free") + scale_color_brewer(palette = "Dark2") +#scale_color_viridis_d(option = "magma") +
    theme(panel.spacing = unit(0.5, "lines"),
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
          axis.ticks.length = unit(1, "mm")) +
    labs(y = "", x = "Estimate") + theme(panel.grid.major.y = element_blank())  + geom_hline(yintercept = 3.5)
ggdraw(p) + draw_label("FB", x = 0.05, y = 0.90) + draw_label("TW", x = 0.05, y = 0.5)
ggsave2(filename = "figures/figure_int.pdf")


# Making plot only for likes for main paper since results are similar
p2 <- final_results %>% filter(str_detect(Outcome, "Like")) %>%
    mutate(platform = case_when(
        platform == "fb" ~ "Likes (FB)",
        platform == "tw" ~ "Likes (Tw)")) %>%
    ggplot(aes(y = fct_rev(platform), x = exp(.value), xmin = exp(.lower), xmax = exp(.upper), color = Populism, group = Populism)) +
    geom_pointinterval(interval_size_range = c(0.5,1), point_size = 1, position = position_dodge(width = 0.35))  + # scale best for printing
    scale_y_discrete() +
    geom_vline(xintercept = 1, linetype = "dashed") +  theme_bw() +
    facet_grid(. ~ Covariate, scales = "free") +
    scale_color_brewer(palette = "Dark2") +
    theme(panel.spacing = unit(0.5, "lines"),
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
          axis.ticks.length = unit(1, "mm")) +
    labs(y = "", x = "Estimate") + theme(panel.grid.major.y = element_blank(),
                                         legend.position = "bottom") +
    geom_hline(yintercept = 1.5)
ggdraw(p2)
ggsave2(filename = "figures/figure_int_likes.pdf")#, width = 6.3, height = 4)
