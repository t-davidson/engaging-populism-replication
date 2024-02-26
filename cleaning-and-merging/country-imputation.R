### Cleaning up country-level data
### Created: 06/20/21
### Modified: 12/30/21, 8/24/23
### Final: 2/11/24

library(tidyverse)
library(Amelia)

set.seed(08540)

# Read in country-level data
country_data <- read.csv("../data/country_data/merged_country_data.csv")

COUNTRIES <- c("AUT", "BEL", "BGR", "HRV", "CZE", "DNK", "EST", "FIN", "FRA", "DEU", "GRC",
               "HUN", "ITA", "LVA", "LTU", "LUX", "MLT", "NLD", "NOR", "POL", "PRT", "ROU", "SVK", "SVN",
               "ESP", "SWE", "CHE", "GBR", "IRL", "NIR")

country_data <- country_data %>% filter(country %in% COUNTRIES) %>% # Verifying countries
    filter(year <= 2020) # Dropping pre-2020 data

colnames(country_data) <- c("country", "year", "gdp_growth", "pop_growth", "pop", "net_migr",
                            "unemp_nat", "unemp_ilo", "refugee", "tax_gdp", "income_l20",
                            "trade_gdp", "gdp", "internet_adoption", "socialmedia", "refugee_change")

# Only using a small subset that will be used later
country_data_impute <- country_data %>% select(country, year, gdp, pop,
                                               unemp_ilo, tax_gdp, income_l20, trade_gdp,
                                               internet_adoption,socialmedia)

###########################################
##                                       ##
##       ########################        ##
##          Multiple Imputation          ##
##       ########################        ##
##                                       ##
###########################################

# Using min and max vals of non-missing data to specify some reasonable bounds for imputations to avoid zeros
# Getting minima and maxima for all observed values
min_vals <- country_data_impute %>% drop_na %>% select_if(is.numeric) %>% select(-year) %>% summarize_all(min)
max_vals <- country_data_impute %>% drop_na %>% select_if(is.numeric) %>% select(-year) %>% summarize_all(max)
print(min_vals)
print(max_vals)

# Set bounds:
##"A three column matrix to hold logical bounds on the imputations. Each row of the matrix should be of the form c(column.number, lower.bound,upper.bound)"
column.number <- 1:(length(min_vals)) + 2
lower.bound <- unlist(min_vals - abs((min_vals / 20))) # Specifying reasonable upper and lower bounds based on observed data
upper.bound <- unlist(max_vals + (max_vals / 20))# +/- 5% of observed min and max

bounds <- bind_cols(column.number = column.number,
                    lower.bound = lower.bound,
                    upper.bound = upper.bound) %>% as.matrix()
# Run imputation

a <- amelia(country_data_impute, m = 1000, ts = "year", cs = "country", # 1000 imputed datasets
            bounds = bounds, # Adding bounds for variables
            polytime=2, # Includes second order polynomial for time
            intercs=F, # Denotes whether time polynomial can vary across countries
            logs = c("gdp", "pop"),# Specifies which variables to log transform
            lags = c("gdp", "pop", "internet_adoption", "socialmedia", "unemp_ilo", "income_l20", "trade_gdp", "tax_gdp"), # Adding lags of each variable
            parallel = "multicore",
            ncpus = 4,
            p2s = 1 # Denotes detail of output printed
            )


# Notes
#When I set it to Polytime=3 (indicates cubic time effects) my R crashes. Set to 1 or 2 it's fine. 2 seems to yield a pretty good estimation in terms of social media usage.

# Adding lags greatly improves the fit of the model as shown on overimpute plots

compare.density(a, var = c(10))

overimpute(a, var = 10)
# The model appears to work well for socialmedia, the main variable we are imputing. Note the bimodal distribution is a function of the imputed data (most missingness is at beginning and end of period)

for (c in COUNTRIES) {
  tscsPlot(a, cs = c, var = "socialmedia")
}

# Saving a dataset containing all imputations
write.amelia(obj=a, separate = F, file.stem = "imputed_country_data", orig.data = F)

final <- read_csv('imputed_country_data.csv') %>%
  group_by(country, year) %>% summarize(everyday_social_networks_imputed = mean(socialmedia))

# Just keeping gdp, pop, and imputed Eurobarometer
country_data_final <- country_data %>% left_join(final, by=c("country", "year")) %>% as.data.frame()

write.csv(country_data_final, '../data/country_data/final_imputed_country_data.csv')


######################
### VISUALIZATIONS ###
######################

# Further assessing imputations using some plots

country_data_final$survey_missing <- ifelse(is.na(country_data$socialmedia),"Imputed", "Unimputed")

# Setting up manual colors
color_group <- rep("black", 31)
color_flag <- c("red", "grey")


# Lines for each country
ggplot(country_data_final %>% filter(year > 2009), aes(x=year, y=everyday_social_networks_imputed))  +
  geom_smooth(aes(color=country), method = "loess", se = F) +
  geom_point(aes(fill=factor(survey_missing)), size=2, shape=21, stroke=0) +
  scale_fill_manual(values=color_flag) +
  scale_colour_manual(values=color_group, guide = "none") +
  theme_bw() + scale_x_continuous(breaks=c(2010, 2012, 2014, 2016, 2018, 2020)) +
  labs(fill="", colour="Group", y = "% Use social networks everyday/almost everyday (Eurobarometer)", x="",
       title = "Social media usage by country, 2010-2020", caption = "LOESS regression line for each country.")

# Single LOESS plot
ggplot(country_data_final %>% filter(year > 2009), aes(x=year, y=everyday_social_networks_imputed))  +
  geom_smooth(method = "loess", se = T) +
  geom_point(aes(fill=factor(survey_missing)), size=2, shape=21, stroke=0) +
  scale_fill_manual(values=color_flag) +
  #scale_colour_manual(values=color_group, guide = "none") +
  theme_bw() + scale_x_continuous(breaks=c(2010, 2012, 2014, 2016, 2018, 2020)) +
  labs(fill="", colour="Group", y = "% Use social networks everyday/almost everyday (Eurobarometer)", x="",
       title="Social media usage, 2010-2020", caption = "Imputed values represent the mean imputed value over 1000 Amelia II runs.")