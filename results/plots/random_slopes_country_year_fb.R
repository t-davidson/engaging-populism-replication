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

m_poppa <- readRDS("../output/fb_likes_poppa_rs_cy.rds")
m_populist <- readRDS("../output/fb_likes_populist_rs_cy.rds")
m_gps <- readRDS("../output/fb_likes_gps_rs_cy.rds")

# Main effects (4000 draws of each)
# Creating variable to map onto "term" in the random slopes results
slopes.p <- m_populist %>% gather_draws(b_populist1) %>%
    mutate(term = str_remove(.variable, "b_"))

# Getting random slopes for each variable
slopes.y <- m_populist %>%
    spread_draws(r_year.f[year,term]) %>%
    filter(term != "Intercept") %>%
    left_join(slopes.p, by = c("term", ".chain", ".iteration", ".draw")) %>%
    mutate(effect = exp(.value + r_year.f))

slopes.c <- m_populist %>%
    spread_draws(r_country[country,term]) %>%
    filter(term != "Intercept") %>%
    left_join(slopes.p, by = c("term", ".chain", ".iteration", ".draw")) %>%
    mutate(effect = exp(.value + r_country))

c <- ggplot(slopes.c, aes(x = effect, y = fct_reorder(country, effect, .fun = median), fill = after_stat(abs(x) > 1))) +
    stat_halfeye(.width = c(0.68, 0.90), slab_alpha = 0.5, position = position_dodge(0.5), interval_size_range = c(0.5,1), point_size = 1 ) +
    geom_vline(xintercept = 1, linetype = "dashed") + 
    scale_fill_manual(values = c("gray50", "#B35806")) +
    theme_tidybayes() + theme(legend.position = "none") + xlim(0, 5) +
    labs(y="", x = TeX("$exp(\\hat{\\beta_1}_{Populism} + \\hat{\\gamma_1}_{Country})$"))
c

y <- ggplot(slopes.y, aes(x = effect, y = as.factor(year), fill = after_stat(abs(x) > 1))) +
    stat_halfeye(.width = c(0.68, 0.90), slab_alpha = 0.5, position = position_dodge(0.5), interval_size_range = c(0.5,1), point_size = 1 ) +
    scale_fill_manual(values = c("gray50", "#B35806")) +
    geom_vline(xintercept = 1, linetype = "dashed") + 
    theme_tidybayes() + theme(legend.position = "none") + 
    labs(y="", x = TeX("$exp(\\hat{\\beta_1}_{Populism} + \\hat{\\gamma_1}_{Year})$")) + xlim(0, 4)
y

ggarrange(c, y, labels = c("A", "B"))
ggsave2(filename = "figures/country_year_slopes_fb_populist_likes.pdf", width = 8.3, height = 10)


# Repeat but show (a) year slopes for all three datasets, (b) country slopes for all three datasets, by FB and TW (4 plots total)

# Running for POPPA and GPS FB models
slopes.p.poppa <- m_poppa %>% gather_draws(b_poppa_populism_s) %>%
    mutate(term = str_remove(.variable, "b_"))

slopes.y.poppa <- m_poppa %>%
    spread_draws(r_year.f[year,term]) %>%
    filter(term != "Intercept") %>%
    left_join(slopes.p.poppa, by = c("term", ".chain", ".iteration", ".draw")) %>%
    mutate(effect = exp(.value + r_year.f))

slopes.c.poppa <- m_poppa %>%
    spread_draws(r_country[country,term]) %>%
    filter(term != "Intercept") %>%
    left_join(slopes.p.poppa, by = c("term", ".chain", ".iteration", ".draw")) %>%
    mutate(effect = exp(.value + r_country))

slopes.p.gps <- m_gps %>% gather_draws(b_gps_populism_s) %>%
    mutate(term = str_remove(.variable, "b_"))

# Getting random slopes for each variable
slopes.y.gps <- m_gps %>%
    spread_draws(r_year.f[year,term]) %>%
    filter(term != "Intercept") %>%
    left_join(slopes.p.gps, by = c("term", ".chain", ".iteration", ".draw")) %>%
    mutate(effect = exp(.value + r_year.f))

slopes.c.gps <- m_gps %>%
    spread_draws(r_country[country,term]) %>%
    filter(term != "Intercept") %>%
    left_join(slopes.p.gps, by = c("term", ".chain", ".iteration", ".draw")) %>%
    mutate(effect = exp(.value + r_country))


# Making plots
c.populist <- ggplot(slopes.c, aes(x = effect, y = fct_reorder(country, effect, .fun = median), fill = after_stat(abs(x) > 1))) + # fill = fct_reorder(country, effect, .fun = median)
    stat_halfeye(.width = c(0.68, 0.90), slab_alpha = 0.5, position = position_dodge(0.5), interval_size_range = c(0.5,1), point_size = 1 ) +
    geom_vline(xintercept = 1, linetype = "dashed") + 
    scale_fill_manual(values = c("gray50", "#B35806")) +
    theme_tidybayes() + theme(legend.position = "none") + xlim(0, 5) +
    labs(y="", x = "")

y.populist <- ggplot(slopes.y, aes(x = effect, y = as.factor(year), fill = after_stat(abs(x) > 1))) +
    stat_halfeye(.width = c(0.68, 0.90), slab_alpha = 0.5, position = position_dodge(0.5), interval_size_range = c(0.5,1), point_size = 1 ) +
    scale_fill_manual(values = c("gray50", "#B35806")) +
    geom_vline(xintercept = 1, linetype = "dashed") + 
    theme_tidybayes() + theme(legend.position = "none") + 
    labs(y="", x = "") + xlim(0, 4)

c.poppa <- ggplot(slopes.c.poppa, aes(x = effect, y = fct_reorder(country, effect, .fun = median), fill = after_stat(abs(x) > 1))) + # fill = fct_reorder(country, effect, .fun = median)
    stat_halfeye(.width = c(0.68, 0.90), slab_alpha = 0.5, position = position_dodge(0.5), interval_size_range = c(0.5,1), point_size = 1 ) +
    geom_vline(xintercept = 1, linetype = "dashed") + 
    scale_fill_manual(values = c("gray50", "#B35806")) +
    theme_tidybayes() + theme(legend.position = "none") + xlim(0, 5) +
    labs(y="", x = "")


y.poppa <- ggplot(slopes.y.poppa, aes(x = effect, y = as.factor(year), fill = after_stat(abs(x) > 1))) +
    stat_halfeye(.width = c(0.68, 0.90), slab_alpha = 0.5, position = position_dodge(0.5), interval_size_range = c(0.5,1), point_size = 1 ) +
    scale_fill_manual(values = c("gray50", "#B35806")) +
    geom_vline(xintercept = 1, linetype = "dashed") + 
    theme_tidybayes() + theme(legend.position = "none") + 
    labs(y="", x = "") + xlim(0, 4)

c.gps <- ggplot(slopes.c.gps, aes(x = effect, y = fct_reorder(country, effect, .fun = median), fill = after_stat(abs(x) > 1))) + # fill = fct_reorder(country, effect, .fun = median)
    stat_halfeye(.width = c(0.68, 0.90), slab_alpha = 0.5, position = position_dodge(0.5), interval_size_range = c(0.5,1), point_size = 1 ) +
    geom_vline(xintercept = 1, linetype = "dashed") + 
    scale_fill_manual(values = c("gray50", "#B35806")) +
    theme_tidybayes() + theme(legend.position = "none") + xlim(0, 5) +
    labs(y="", x = "")


y.gps <- ggplot(slopes.y.gps, aes(x = effect, y = as.factor(year), fill = after_stat(abs(x) > 1))) +
    stat_halfeye(.width = c(0.68, 0.90), slab_alpha = 0.5, position = position_dodge(0.5), interval_size_range = c(0.5,1), point_size = 1 ) +
    scale_fill_manual(values = c("gray50", "#B35806")) +
    geom_vline(xintercept = 1, linetype = "dashed") + 
    theme_tidybayes() + theme(legend.position = "none") + 
    labs(y="", x = "") + xlim(0, 4)


# 
pc <- plot_grid(c.populist, c.poppa, c.gps, labels = c("A", "B", "C"),
                ncol = 3)
ggdraw(add_sub(pc, TeX("$exp(\\hat{\\beta_1}_{Populism} + \\hat{\\gamma_1}_{Country})"), vpadding=grid::unit(0,"lines"),y=6, x=0.5, vjust=5.5))
ggsave2(filename = "figures/country_slopes_fb_likes.pdf", width = 8.3, height = 9)

py <- plot_grid(y.populist, y.poppa, y.gps, labels = c("A", "B", "C"),
               ncol = 3)
ggdraw(add_sub(py, TeX("$exp(\\hat{\\beta_1}_{Populism} + \\hat{\\gamma_1}_{Year})"), vpadding=grid::unit(0,"lines"),y=6, x=0.5, vjust=5.5))

ggsave2(filename = "figures/year_slopes_fb_likes.pdf", width = 8.3, height = 9)
