### Cleaning up country-level data
### Created: 06/20/21
### Modified: 12/30/21
### Final: 8/24/23

library(tidyverse)
library(Amelia)

set.seed(08540)

# Read in country-level files

eurobarometer <- read.csv("../data/eurobarometer_data/Eurobarometer.csv")
country_stats <- read.csv("../data/country_data/country_stats.csv")

# Rename columns for consistency and clarity
eurobarometer_edit <- rename(eurobarometer, 
                                year = Year,
                                eb_no = EB_no,
                                ebq1_1 = EBQ1_1,
                                ebq1_2 = EBQ1_2,
                                ebq1_3 = EBQ1_3,
                                ebq1_4 = EBQ1_4,
                                ebq1_5 = EBQ1_5,
                                ebq1_6 = EBQ1_6,
                                ebq1_7 = EBQ1_7,
                                ebq2_1 = EBQ2_1,
                                ebq2_2 = EBQ2_2,
                                ebq2_3 = EBQ2_3,
                                ebq2_4 = EBQ2_4,
                                country = country_right)

country_stats_edit <- rename(country_stats,
                                country = country_right,
                                year = Year,
                                unemp = Unemployment.rate..OECD.data.,
                                pop = Population.count,
                                gdp = GDP.per.capita..USD..current.prices..current.PPPs..OECD.data.unless.highlighted.in.yellow.for.World.Bank.data.)

# Select only the columns we want to include in final dataset
country_stats_final <- country_stats_edit %>% select(country, year, gdp, pop, unemp)
eurobarometer_final <- eurobarometer_edit %>% select(country, year, ebq1_1)

# Merging the two files
country_data <- country_stats_final %>% left_join(eurobarometer_final, by=c("country", "year")) %>% as.data.frame()

#country_data$gdp <- as.numeric(country_data$gdp)

COUNTRIES <- c("AUT", "BEL", "BGR", "HRV", "CYP", "CZE", "DNK", "EST", "FIN", "FRA", "DEU", "GRC",
               "HUN", "ITA", "LVA", "LTU", "LUX", "MLT", "NLD", "NOR", "POL", "PRT", "ROU", "SVK", "SVN",
               "ESP", "SWE", "CHE", "GBR", "IRL", "NIR")

country_data <- country_data %>% filter(country %in% COUNTRIES)

# Saving
write_csv(country_data, "../data/country_data/cleaned_country_data.csv")

###########################################
##                                       ##
##       ########################        ##
##          Multiple Imputation          ##
##       ########################        ##
##                                       ##
###########################################

# Using min and max vals of non-missing data to specify some reasonable bounds for imputations to avoid zeros
# Getting minima and maxima for all observed values
min_vals <- country_data %>% drop_na %>% select(gdp, pop, unemp, ebq1_1) %>% summarize_all(min)
max_vals <- country_data %>% drop_na %>% select(gdp, pop, unemp, ebq1_1) %>% summarize_all(max)
print(min_vals)
print(max_vals)

# Set bounds:
##"A three column matrix to hold logical bounds on the imputations. Each row of the matrix should be of the form c(column.number, lower.bound,upper.bound)" 
column.number <- c(3:6)
lower.bound <- c(10000,300000,1,1) # Specifying reasonable upper and lower bounds based on observed data
upper.bound <- c(140000, 90000000, 35, 80) 

bounds <- as.matrix(cbind(column.number, lower.bound, upper.bound)) #Put these three columns into 16x3 matrix with each of these vector names as column names
# Run imputation

a <- amelia(country_data, m = 1000, ts = "year", cs = "country", # 1000 imputed datasets
            bounds = bounds, # Adding bounds for variables
            polytime=2, # Includes second order polynomial for time
            intercs=F, # Denotes whether time polynomial can vary across countries
            logs = c("gdp", "pop"),# Specifies which variables to log transform, including immigration and asylum caused problems (immigration imputed as infinite)
            lags = c("gdp", "pop", "unemp", "ebq1_1"), # Adding lags of each variable
            parallel = "multicore",
            ncpus = 16,
            p2s = 1 # Denotes detail of output printed
            ) 


# Notes
#When I set it to Polytime=3 (indicates cubic time effects) my R crashes. Set to 1 or 2 it's fine. 2 seems to yield a pretty good estimation in terms of social media usage.

# Adding lags greatly improves the fit of the model as shown on overimpute plots

compare.density(a, var = c(6))

overimpute(a, var = 6)
# The model appears to work well for ebq1_1, the main variable we are imputing. Note the bimodal distribution is a function of the imputed data (most missingness is at beginning and end of period)

for (c in COUNTRIES) {
  tscsPlot(a, cs = c, var = "ebq1_1")
}


# Saving a dataset containing all imputations
write.amelia(obj=a, separate = F, file.stem = "imputed_country_data", orig.data = F)

final <- read_csv('imputed_country_data.csv') %>% select(country, year, gdp, pop, ebq1_1) %>% 
  group_by(country, year) %>% summarize(everyday_social_networks_imputed = mean(ebq1_1))

# Just keeping gdp, pop, and imputed Eurobarometer
country_data_final <- country_data %>% select(country, year, gdp, pop, unemp) %>% left_join(final, by=c("country", "year")) %>% as.data.frame()

write.csv(country_data_final, '../data/country_data/final_imputed_country_data.csv')


######################
### VISUALIZATIONS ###
######################

# Further assessing imputations using some plots

country_data_final$survey_missing <- ifelse(is.na(country_data$ebq1_1),"Imputed", "Unimputed")

# Setting up manual colors
color_group <- rep("black", 31)
color_flag <- c("red", "grey")


ggplot(country_data_final %>% filter(year > 2009), aes(x = year, y= everyday_social_networks_imputed, color = country)) + 
  geom_point(alpha=1) +
  scale_color_viridis_d(option="magma") +
  geom_smooth(method = "loess", se = F) + 
  theme_bw()


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
       title="Social media usage, 2010-2020", caption = "Imputed values represent the mean imputed value over 100 Amelia II runs.")

