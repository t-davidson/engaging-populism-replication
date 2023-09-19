#source("load-data-local.R")
library(tidyverse)
library(tidybayes)
library(ggplot2)
library(latex2exp)
library(cowplot)
library(ggpubr)
set.seed(08901)

m_populist <- readRDS("../output/fb_shares_populist_cat_rs_cy.rds")

# Main effects (4000 draws of each)
# Creating variable to map onto "term" in the random slopes results
slopes.rwp <- m_populist %>% gather_draws(b_populist_catrwp) %>%
    mutate(term = str_remove(.variable, "b_"))
slopes.lwp <- m_populist %>% gather_draws(b_populist_catlwp) %>%
    mutate(term = str_remove(.variable, "b_"))
slopes.cp <- m_populist %>% gather_draws(b_populist_catcp) %>%
    mutate(term = str_remove(.variable, "b_"))

slopes.b <- bind_rows(slopes.rwp, slopes.lwp, slopes.cp)

# Getting random slopes for each variable
slopes.y <- m_populist %>%
    spread_draws(r_year.f[year,term]) %>%
    filter(term != "Intercept")

slopes.c <- m_populist %>%
    spread_draws(r_country[country,term]) %>%
    filter(term != "Intercept")

slopes.cy <- m_populist %>%
    spread_draws(r_country_year[cy, term]) %>%
    filter(term != "Intercept") %>%
    mutate(country = sub("_(\\d+)$", "", cy),  # Extract country code
           year = as.integer(sub(".*_(\\d+)$", "\\1", cy)))
    

slopes <- slopes.cy %>% left_join(slopes.c, by = c("term", ".chain", ".iteration", ".draw", "country")) %>%
    left_join(slopes.y, by = c("term", ".chain", ".iteration", ".draw", "year")) %>%
    left_join(slopes.b, by = c("term", ".chain", ".iteration", ".draw"))

slopes <- slopes %>% mutate(effect = .value + r_year.f + r_country + r_country_year)

######### Preliminary analysis of random slopes

country.intervals <- slopes %>% group_by(country, term) %>% mutate(effect = r_country) %>% median_qi(effect, .width = c(.90))

country.intervals %>% ggplot(aes(x = effect, y = country, xmin = .lower, xmax = .upper, group = term, color = term)) +
    geom_pointinterval(position = position_dodge(0.5), interval_size_range = c(0.5,1), point_size = 1 )  +
    theme_minimal()

year.intervals <- slopes %>% group_by(year, term) %>% mutate(effect = r_year.f) %>% median_qi(effect, .width = c(.90))

year.intervals %>% ggplot(aes(y = effect, x = as.factor(year), ymin = .lower, ymax = .upper, group = term, color = term)) +
    geom_line() +
    geom_pointinterval(position = position_dodge(0.5), interval_size_range = c(0.5,1), point_size = 1 )  +
    theme_minimal()

cy.intervals <- slopes %>% group_by(country, year, term) %>% mutate(effect = r_country_year) %>% median_qi(effect, .width = c(.90))

cy.intervals %>% ggplot(aes(x = year, y = effect, ymin = .lower, ymax = .upper, group = term, color = term)) +
    geom_pointinterval(position = position_dodge(0.5), interval_size_range = c(0.5,1), point_size = 1 )  +
    geom_line() +
    theme_minimal() +
    facet_wrap(. ~ country, ncol = 5)

#########

results <- slopes %>% group_by(term, country, year, cy) %>% mutate(effect = exp(effect)) %>% median_qi(effect, .width = c(.68, .90))

# For each CY, determine whether the 90% interval overlaps 1 (after exponentiating)
# Use this to create a variable to join back on to set whether we should consider substantial evidence of a populist advantage

overlap <- results %>% mutate(overlap = ifelse(.lower <= 1, 1, 0)) %>%
    group_by(country, year, term) %>% summarize(ol = sum(overlap)) %>%
    mutate(ol = ifelse(ol >= 1, T, F))

results <- results %>% left_join(overlap, by = c("country", "year", "term"))


# Filtering results such that we only include country years
# where each type of populist party was active
fb_activity <- read_csv("../../data/misc/active_populist_country_year_fb.csv")
tw_activity <- read_csv("../../data/misc/active_populist_country_year_tw.csv")

results <- results %>% left_join(fb_activity, by = c("country", "year"))
results.rwp <- results %>% filter(term == "populist_catrwp") %>% mutate(Populism = "RWP") %>%
    filter(total_rwp >= 1)
results.lwp <- results %>% filter(term == "populist_catlwp") %>% mutate(Populism = "LWP") %>%
    filter(total_lwp >= 1)
results.cp <- results %>% filter(term == "populist_catcp") %>% mutate(Populism = "CP") %>%
    filter(total_cp >= 1)

results <- bind_rows(results.rwp, results.lwp, results.cp)

results <- results %>% mutate(Populism = as.factor(Populism),
                              ol = ifelse(ol, "Interval includes 1", "Interval greater than 1"))
results$Populism <- fct_relevel(results$Populism, "RWP", "LWP", "CP")


p <- results %>% ggplot(aes(y = effect, x = year, ymin = .lower, ymax = .upper, group = term, color = Populism)) +
    geom_line() +
    geom_pointinterval(aes(shape = ol), position = position_dodge(0.9), interval_size_range = c(0.5,0.8), point_size = 1)  +
    geom_hline(yintercept = 1, linetype = "dashed") +
    theme_minimal() +
    scale_color_brewer(palette = "Dark2") +
    scale_shape_manual(values = c(19,4)) +
    theme(axis.text.x = element_text(angle = 90), panel.spacing = unit(1, "lines"),
          legend.direction = 'horizontal', legend.position = "bottom") + 
    labs(y = TeX("$exp(\\hat{\\beta_{populism}_{ijk}}$)"), x = "", shape = "", color = "Populism type") +
    xlim(2010,2020) +
    scale_x_continuous(breaks = seq(2010, 2020, 2), labels = seq(2010, 2020, 2)) +
    facet_wrap(. ~ country, ncol = 5, scales = "free_y")
p
ggsave2(filename = "figures/figure6.pdf", width = 8.3, height = 9.7)