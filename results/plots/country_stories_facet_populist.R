# The goal of this script is to pull out the party-level predictions
# for a particular country and to plot them over time, along with the raw 
# data. This will see how much of the trajectory is captured by the Bayesian
# models. 
library(tidyverse)
library(ggplot2)
library(viridis)
library(scales)
library(cowplot)

source("load-data-local.R") # Loading data
m <- read_rds("../output/fb_likes_populist_cat_rs_cy.rds")

# If using populist or GPS, drop missing

dat <- data.fb

# Note: if using shares need to manually filter all missing country years from data
preds <- predict(m, dat,re_formula = NULL, probs = c(.16, .84))
colnames(preds) <- c("Estimate", "Error", "Lower", "Upper")

# Use dodge to address overlap
dodge <- 0.5

# Merging
d <- bind_cols(dat, preds)

# Parties
gb_list <- c('conservatives', 'labourparty', 'libdems','ukip')
de_list <- c("spd", "csu", "cdu", "alternativefuerde", "linkspartei") # Ignoring these, otherwise too many lines,"b90diegruenen"  "fdp")
it_list <- c("partitodemocratico", "legasalvinipremier", "fdi.paginaufficiale", "forzaitaliaufficiale", "movimento5stelle")
fr_list <- c("rassemblementnational", "enmarche", "partisocialiste", "les.republicains.fr")
sw_list <- c("socialdemokraterna", "moderaterna", "sverigedemokraterna", "centerpartiet", "vansterpartiet")
parties <- c(gb_list, de_list, it_list, fr_list, sw_list)

d <- d %>% filter(handle %in% parties)

# Plot
# Pallete from https://stackoverflow.com/questions/57153428/r-plot-color-combinations-that-are-colorblind-accessible
safe_colorblind_palette <- c("#88CCEE", "#CC6677", "#DDCC77", "#117733", "#332288", "#AA4499", 
                             "#44AA99", "#999933", "#882255", "#661100", "#6699CC", "#888888")

# Simple names for faceted plot
d$handle <- recode_factor(d$handle, 
                          conservatives = "CON", labourparty = "LAB",
                          libdems = "LIB", ukip = "UKIP",
                          spd = "SPD", csu = "CSU",cdu = "CDU",
                          b90diegruenen = "B90/DG", alternativefuerde = "AfD",
                          linkspartei = "DL", fdp = "FDP",
                          partitodemocratico = "PD", legasalvinipremier = "LN",
                          fdi.paginaufficiale = "FdI", forzaitaliaufficiale = "FI",
                          movimento5stelle = "M5S", 
                          rassemblementnational = "RN/FN", enmarche = "REM", 
                          partisocialiste = "PS", les.republicains.fr = "LR",
                          socialdemokraterna = "S/SAP", moderaterna = "M",
                          centerpartiet = "C", sverigedemokraterna = "SD",
                          vansterpartiet = "V"
                          
)

d$alpha <- ifelse(d$handle %in% c("UKIP", "AfD", "DL", "M5S", "LN", "RN/FN", "SD"), 1,  .5)
amin <- 0.8 # Minimum alpha
dodge <- 0.6

# Note: It seems like dodge leads to some points being dropped by xlim. Setting limits to include 2021 seems to fix this
p1 <- ggplot(aes(y = Estimate, x = year, color = handle, group = handle, alpha = alpha), data = d %>% filter(country == "DEU")) +
    #geom_point(position = position_dodge(dodge)) +
    geom_line(aes(linetype = populist), position = position_dodge(dodge), show.legend = FALSE) +
    geom_point(aes(y = total_likes, x = year), position = position_dodge(dodge), shape = 8) +
    geom_errorbar(aes(ymin = Lower, ymax = Upper, width = 0), position = position_dodge(dodge)) +
    labs(x = "", y = "", color = "") + scale_x_continuous(limits = c(2009.75,2020.25), breaks = seq(2010, 2020, by=1)) +
    scale_y_log10(labels = label_log()) + scale_color_manual(values = c("SPD" = "#CC6677", "CSU" = "black", "CDU"= "#6699CC",
                                                     "AfD" = "#88CCEE",
                                                    "DL" = "#882255")) +  # Cut: "FDP" = "#DDCC77" "B90/DG" = "#117733"
    scale_alpha_continuous(range = c(amin, 1)) +  guides(alpha = "none") + # Necessary, otherwise alpha scaling is fofhttps://stackoverflow.com/questions/61200151/how-to-adjust-relative-transparency-of-ggplot2-points
    theme_minimal() + theme(axis.text.x=element_text(angle = 90, hjust = 1, vjust = 0.5), legend.justification = c(0,1))

p2 <- ggplot(aes(y = Estimate, x = year, color = handle, group = handle, alpha = alpha), data = d %>% filter(country == "ITA")) +
    #geom_point(position = position_dodge(dodge)) +
    geom_line(aes(linetype = populist),position = position_dodge(dodge), show.legend = FALSE) +
    geom_point(aes(y = total_likes, x = year), position = position_dodge(dodge), shape = 8) +
    geom_errorbar(aes(ymin = Lower, ymax = Upper, width = 0), position = position_dodge(dodge)) +
    labs(x = "", y = "", color = "") + scale_x_continuous(limits = c(2009.75,2020.25), breaks = seq(2010, 2020, by=1)) +
    scale_y_log10(labels = label_log()) + scale_color_manual(values = c("PD" = "#CC6677", 
                                                    "LN" = "#117733",
                                                    "FdI" = "#332288",
                                                    "FI" = "#6699CC",
                                                    "M5S" = "#DDCC77")) +
    scale_alpha_continuous(range = c(amin, 1)) +  guides(alpha = "none") +
    theme_minimal() + theme(axis.text.x=element_text(angle = 90, hjust = 1, vjust = 0.5), legend.justification = c(0,1))

p3 <- ggplot(aes(y = Estimate, x = year, color = handle, group = handle, alpha = alpha), data = d %>% filter(country == "FRA")) +
    #geom_point(position = position_dodge(dodge)) +
    geom_line(aes(linetype = populist),position = position_dodge(dodge), show.legend = FALSE) +
    geom_point(aes(y = total_likes, x = year), position = position_dodge(dodge), shape = 8) +
    geom_errorbar(aes(ymin = Lower, ymax = Upper, width = 0), position = position_dodge(dodge)) +
    labs(x = "", y = "", color = "") + scale_x_continuous(limits = c(2009.75,2020.25), breaks = seq(2010, 2020, by=1)) +
    scale_y_log10(labels = label_log()) + scale_color_manual(values = c("RN/FN" = "#332288", 
                                                    "REM" = "#DDCC77",
                                                    "LR" = "#6699CC",
                                                    "PS" = "#CC6677")) + 
    scale_alpha_continuous(range = c(amin, 1)) +  guides(alpha = "none") +
    theme_minimal() + theme(axis.text.x=element_text(angle = 90, hjust = 1, vjust = 0.5), legend.justification = c(0,1))

p4 <- ggplot(aes(y = Estimate, x = year, color = handle, group = handle, alpha = alpha), data = d %>% filter(country == "GBR")) +
    #geom_point(position = position_dodge(dodge)) +
    geom_line(aes(linetype = populist),position = position_dodge(dodge), show.legend = FALSE) +
    geom_point(aes(y = total_likes, x = year), position = position_dodge(dodge), shape = 8) +
    geom_errorbar(aes(ymin = Lower, ymax = Upper, width = 0), position = position_dodge(dodge)) +
    labs(x = "", y = "", color = "") + scale_x_continuous(limits = c(2009.75,2020.25), breaks = seq(2010, 2020, by=1)) +
    scale_y_log10(labels = label_log()) + scale_color_manual(values = c("CON" = "#6699CC", "LAB" = "#CC6677", "LIB" = "#DDCC77",
                                                    "UKIP" = "#AA4499")) + 
    scale_alpha_continuous(range = c(amin, 1)) +  guides(alpha = "none") +
    theme_minimal() + theme(axis.text.x=element_text(angle = 90, hjust = 1, vjust = 0.5), legend.justification = c(0,1))

p5 <- ggplot(aes(y = Estimate, x = year, color = handle, group = handle, alpha = alpha), data = d %>% filter(country == "SWE")) +
    #geom_point(position = position_dodge(dodge)) +
    geom_line(aes(linetype = populist),position = position_dodge(dodge), show.legend = FALSE) +
    geom_point(aes(y = total_likes, x = year), position = position_dodge(dodge), shape = 8) +
    geom_errorbar(aes(ymin = Lower, ymax = Upper, width = 0), position = position_dodge(dodge)) +
    labs(x = "", y = "", color = "") + scale_x_continuous(limits = c(2009.75,2020.25), breaks = seq(2010, 2020, by=1)) +
    scale_y_log10(labels = label_log()) + scale_color_manual(values = c("S/SAP" = "#CC6677", 
                                                    "M" = "#88CCEE",
                                                    "SD" = "#DDCC77",
                                                    "C" = "#117733",
                                                    "V" = "#661100")) + 
    scale_alpha_continuous(range = c(amin, 1)) +  guides(alpha = "none") +
    theme_minimal() + theme(axis.text.x=element_text(angle = 90, hjust = 1, vjust = 0.5), legend.justification = c(0,1))



# This only works if colors are specified uniquely for each country....

#plot_grid(p1, p2, p3, p4, p5, ncol = 1, align = "v")

plot_grid(p1, p2, p3, p5, ncol = 2, nrow = 2, align = "vh") + draw_label("Likes", x=  0, y=0.5, vjust= 1.5, angle=90)
ggsave2("figures/country_predictions_populist_likes_rs_cy_subset.pdf", width = 8.3, height = 9.7)

### Plotting many countries using populist
preds2 <- predict(m, dat, re_formula = NULL, probs = c(.16, .84)) # Not supplying data to get all relevant preds
colnames(preds2) <- c("Estimate", "Error", "Lower", "Upper")

d2 <- bind_cols(dat, preds2)

# General plot for many countries
d2$alpha_p <- ifelse(d2$populist == 1, 1, 0.2)

d2$populism_type <- as.factor(ifelse(d2$populist == 1 & d2$farleft, "LWP", 
                           ifelse(d2$populist == 1 & d2$farright, "RWP",
                                  ifelse(d2$populist == 1, "CP", "Non-populist"
                                  ))))

d2$populism_type <- fct_relevel(d2$populism_type, "RWP", "LWP", "CP", "Non-populist")


ggplot(aes(y = Estimate, x = year.f, color = populism_type, group = handle, alpha = alpha_p), data = d2) +
    geom_line(position = position_dodge(0.8)) +
    #geom_point(aes(y = total_likes, x = year.f), position = position_dodge(dodge), shape = 8) +
    geom_errorbar(aes(ymin = Lower+1, ymax = Upper+1, width = 0), position = position_dodge(dodge)) +
    labs(x = "", y = "Predicted likes", color = "") + 
    scale_y_log10(labels = label_log()) +
    theme_minimal() +
    scale_alpha_continuous(range = c(0.5, 1)) +  guides(alpha = "none") +
    scale_x_discrete(breaks = c(2010, 2012, 2014, 2016, 2018, 2020)) +
    scale_color_manual(values = c("LWP" = "#D95F02", "RWP" = "#1B9E77", "CP" = "#7570B3", "Non-populist" = "#888888")) + 
    facet_wrap(. ~ country, ncol = 5, scales = "free_y") + 
    theme(axis.text.x=element_text(angle = 90, hjust = 1, vjust = 0.5),
          legend.direction = 'horizontal', legend.position = "bottom")
ggsave2(filename = "figures/figure7.pdf", width = 8.3, height = 9.7)
