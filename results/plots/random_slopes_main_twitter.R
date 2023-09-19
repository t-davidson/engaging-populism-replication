#source("load-data-local.R")
library(tidyverse)
library(tidybayes)
library(ggridges)
library(viridis)
library(shades)
library(ggplot2)
library(latex2exp)
library(cowplot)
library(ggpubr)
set.seed(08901)

m_poppa <- readRDS("../output/tw_likes_poppa_rs_cy.rds")
m_populist <- readRDS("../output/tw_likes_populist_rs_cy.rds")
m_gps <- readRDS("../output/tw_likes_gps_rs_cy.rds")

# Main effects (4000 draws of each)
# Creating variable to map onto "term" in the random slopes results
slopes.p <- m_populist %>% gather_draws(b_populist1) %>%
    mutate(term = str_remove(.variable, "b_"))

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
    left_join(slopes.p, by = c("term", ".chain", ".iteration", ".draw"))

rs.populist <- slopes %>% mutate(effect = .value + r_year.f + r_country + r_country_year)

# Note that we are not observing full posterior since only 1000 draws obtained
# Any more and memory error occurs even at maximum
# Using 90% interval rather than 95% as recommended by Kruscke 2014
rs.populist.int <- rs.populist %>% mutate(effect = exp(effect)) %>% group_by(country, year, cy) %>% median_qi(effect, .width = c(.68, .90))

# For each CY, determine whether the 95% interval overlaps 1 (after exponentiating)
# Use this to create a variable to join back on to set whether we should consider substantial evidence of a populist advantage

overlap <- rs.populist.int %>% mutate(overlap = ifelse(.lower <= 1, 1, 0)) %>%
    group_by(country, year) %>% summarize(ol = sum(overlap)) %>%
    mutate(ol = ifelse(ol >= 1, T, F))

rs.populist.int <- rs.populist.int %>% left_join(overlap, by = c("country", "year"))

p <- rs.populist.int %>% ggplot(aes(y = effect, x = year, ymin = .lower, ymax = .upper)) +
    geom_line(color = "darkgrey") +
    geom_pointinterval(aes(color = ol), position = position_dodge(0.5), interval_size_range = c(0.5,1), point_size = 1 )  +
    geom_hline(yintercept = 1, linetype = "dashed") +
    theme_minimal() +
    scale_colour_manual(values = c("grey10", "grey50")) + #c("", "#7FBC41")) +
    theme(axis.text.x = element_text(angle = 90), panel.spacing = unit(1, "lines")) + 
    labs(y = TeX("$exp(\\hat{\\beta_{populism}_{ijk}}$)"), x = "") +
    theme(legend.position = "none") +
    xlim(2010,2020) +
    scale_x_continuous(breaks = seq(2010, 2020, 2), labels = seq(2010, 2020, 2)) +
    facet_wrap(. ~ country, ncol = 5, scales = "free_y")
p
ggsave2(filename = "figures/figure5_twitter.pdf", width = 8.3, height = 9.7)

#######

slopes.p <- m_poppa %>% gather_draws(b_poppa_populism_s) %>%
    mutate(term = str_remove(.variable, "b_"))

# Getting random slopes for each variable
slopes.y <- m_poppa %>%
    spread_draws(r_year.f[year,term]) %>%
    filter(term != "Intercept")

slopes.c <- m_poppa %>%
    spread_draws(r_country[country,term]) %>%
    filter(term != "Intercept")

slopes.cy <- m_poppa %>%
    spread_draws(r_country_year[cy, term]) %>%
    filter(term != "Intercept") %>%
    mutate(country = sub("_(\\d+)$", "", cy),  # Extract country code
           year = as.integer(sub(".*_(\\d+)$", "\\1", cy)))


slopes <- slopes.cy %>% left_join(slopes.c, by = c("term", ".chain", ".iteration", ".draw", "country")) %>%
    left_join(slopes.y, by = c("term", ".chain", ".iteration", ".draw", "year")) %>%
    left_join(slopes.p, by = c("term", ".chain", ".iteration", ".draw"))

rs.poppa <- slopes %>% mutate(effect = .value + r_year.f + r_country + r_country_year)

rs.poppa.int <- rs.poppa %>% mutate(effect = exp(effect)) %>% group_by(country, year, cy) %>% median_qi(effect, .width = c(.68, .90))
overlap2 <- rs.poppa.int %>% mutate(overlap = ifelse(.lower <= 1, 1, 0)) %>%
    group_by(country, year) %>% summarize(ol = sum(overlap)) %>%
    mutate(ol = ifelse(ol >= 1, T, F))
rs.poppa.int <- rs.poppa.int %>% left_join(overlap2, by = c("country", "year"))

p2 <- rs.poppa.int %>% ggplot(aes(y = effect, x = year, ymin = .lower, ymax = .upper)) +
    geom_line(color = "darkgrey") +
    geom_pointinterval(aes(color = ol), position = position_dodge(0.5), interval_size_range = c(0.5,1), point_size = 1 )  +
    geom_hline(yintercept = 1, linetype = "dashed") +
    theme_minimal() +
    scale_colour_manual(values = c("grey10", "grey50")) + #c("", "#7FBC41")) +
    theme(axis.text.x = element_text(angle = 90), panel.spacing = unit(1, "lines")) + 
    labs(y = TeX("$exp(\\hat{\\beta_{populism}_{ijk}}$)"), x = "") +
    theme(legend.position = "none") +
    xlim(2010,2020) +
    scale_x_continuous(breaks = seq(2010, 2020, 2), labels = seq(2010, 2020, 2)) +
    facet_wrap(. ~ country, ncol = 5, scales = "free_y")
p2
ggsave2(filename = "figures/figure5_twitter_poppa.pdf", width = 8.3, height = 9.7)

### Repeating

slopes.p <- m_gps %>% gather_draws(b_gps_populism_s) %>%
    mutate(term = str_remove(.variable, "b_"))

# Getting random slopes for each variable
slopes.y <- m_gps %>%
    spread_draws(r_year.f[year,term]) %>%
    filter(term != "Intercept")

slopes.c <- m_gps %>%
    spread_draws(r_country[country,term]) %>%
    filter(term != "Intercept")

slopes.cy <- m_gps %>%
    spread_draws(r_country_year[cy, term]) %>%
    filter(term != "Intercept") %>%
    mutate(country = sub("_(\\d+)$", "", cy),  # Extract country code
           year = as.integer(sub(".*_(\\d+)$", "\\1", cy)))


slopes <- slopes.cy %>% left_join(slopes.c, by = c("term", ".chain", ".iteration", ".draw", "country")) %>%
    left_join(slopes.y, by = c("term", ".chain", ".iteration", ".draw", "year")) %>%
    left_join(slopes.p, by = c("term", ".chain", ".iteration", ".draw"))

rs.gps <- slopes %>% mutate(effect = .value + r_year.f + r_country + r_country_year)


rs.gps.int <- rs.gps %>% mutate(effect = exp(effect)) %>% group_by(country, year, cy) %>% median_qi(effect, .width = c(.68, .90))
overlap3 <- rs.gps.int %>% mutate(overlap = ifelse(.lower <= 1, 1, 0)) %>%
    group_by(country, year) %>% summarize(ol = sum(overlap)) %>%
    mutate(ol = ifelse(ol >= 1, T, F))
rs.gps.int <- rs.gps.int %>% left_join(overlap3, by = c("country", "year"))

p3 <- rs.gps.int %>% ggplot(aes(y = effect, x = year, ymin = .lower, ymax = .upper)) +
    geom_line(color = "darkgrey") +
    geom_pointinterval(aes(color = ol), position = position_dodge(0.5), interval_size_range = c(0.5,1), point_size = 1 )  +
    geom_hline(yintercept = 1, linetype = "dashed") +
    theme_minimal() +
    scale_colour_manual(values = c("grey10", "grey50")) + #c("", "#7FBC41")) +
    theme(axis.text.x = element_text(angle = 90), panel.spacing = unit(1, "lines")) + 
    labs(y = TeX("$exp(\\hat{\\beta_{populism}_{ijk}}$)"), x = "") +
    theme(legend.position = "none") +
    xlim(2010,2020) +
    scale_x_continuous(breaks = seq(2010, 2020, 2), labels = seq(2010, 2020, 2)) +
    facet_wrap(. ~ country, ncol = 5, scales = "free_y")
p3
ggsave2(filename = "figures/figure5_twitter_gps.pdf", width = 8.3, height = 9.7)
