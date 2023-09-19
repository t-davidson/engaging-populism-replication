library(tidyverse)
library(parameters)
library(brms)
library(kableExtra)
set.seed(08540)

options(knitr.kable.NA = '')

# Note: This script creates custom regression tables
# The current approach is unfortunately rather brittle
# The script will require modification if the number of variables
# or order of variables is changed

path <- "../output/"
files <- list.files(path = path) %>% str_subset(".rds") # list of model files


# Loading all model files into environnment
for (f in files) {
    m <- readRDS(paste(path, f, sep = ""))
    name <- str_split(f, pattern = ".rds")[[1]]
    assign(name, m)
}

extract.info <- function(model, rounding = 2) {
    results <- model %>% model_parameters(effects = "all", group_level = FALSE, test = NULL, diagnostic = NULL, exponentiate = FALSE, ci_method = "hdi") %>%
        mutate(Median = ifelse(startsWith(Parameter, "b_") & Parameter != "b_Intercept", exp(Median), Median),
               CI_low = ifelse(startsWith(Parameter, "b_") & Parameter != "b_Intercept", exp(CI_low), CI_low),
               CI_high = ifelse(startsWith(Parameter, "b_") & Parameter != "b_Intercept", exp(CI_high), CI_high)) %>%
        mutate(Median = round(Median,rounding),
               CI_low = round(CI_low,rounding),
               CI_high = round(CI_high,rounding)) %>% select(Parameter, Median, CI_low, CI_high)
    return(results)
}

process.results <- function(results) {
    # Separate tables for each part to help with ordering.
    fixed.intercept <- tibble()
    fixed <- tibble()
    random.int <- tibble()
    random.slope <- tibble()
    corr <- tibble()
    shape <- tibble()
    for (i in 1:dim(results)[[1]]) {
        row <- results[i,]
        # Ignore fixed effects for country or year
        if (!startsWith(row$Parameter, "b_country") & !startsWith(row$Parameter, "b_year")) {
            
            if (startsWith(row$Parameter, "b_")) {
                if (row$Parameter == "b_Intercept") {
                    fixed.intercept <- bind_rows(fixed.intercept, list("Coefficient" = row$Parameter, "Estimate" = as.character(row$Median), "Type" = paste0(row$Parameter, "_coef")))
                    fixed.intercept <- bind_rows(fixed.intercept, list("Coefficient" = "", "Estimate" = paste0("(",row$CI_low, ", ", row$CI_high, ")"), "Type" = paste0(row$Parameter, "_ci")))
                } else {fixed <- bind_rows(fixed, list("Coefficient" = row$Parameter, "Estimate" = as.character(row$Median), "Type" = paste0(row$Parameter, "_coef")))
                        fixed <- bind_rows(fixed, list("Coefficient" = "", "Estimate" = paste0("(",row$CI_low, ", ", row$CI_high, ")"), "Type" = paste0(row$Parameter, "_ci")))
                }
                 } else if (startsWith(row$Parameter, "sd_")) {
                if (endsWith(row$Parameter, "Intercept")) {
                    random.int <- bind_rows(random.int, list("Coefficient" = row$Parameter, "Estimate" = as.character(row$Median), "Type" = paste0(row$Parameter, "_coef")))
                    random.int <- bind_rows(random.int, list("Coefficient" = "", "Estimate" = paste0("(",row$CI_low, ", ", row$CI_high, ")"), "Type" = paste0(row$Parameter, "_ci")))
                } else {
                    random.slope <- bind_rows(random.slope, list("Coefficient" = row$Parameter, "Estimate" = as.character(row$Median), "Type" = paste0(row$Parameter, "_coef")))
                    random.slope <- bind_rows(random.slope, list("Coefficient" = "", "Estimate" = paste0("(",row$CI_low, ", ", row$CI_high, ")"), "Type" = paste0(row$Parameter, "_ci")))
                }
            } else if(startsWith(row$Parameter, "cor")) {
                corr <- bind_rows(corr, list("Coefficient" = row$Parameter, "Estimate" = as.character(row$Median), "Type" = paste0(row$Parameter, "_coef")))
                corr <- bind_rows(corr, list("Coefficient" = "", "Estimate" = paste0("(",row$CI_low, ", ", row$CI_high, ")"), "Type" = paste0(row$Parameter, "_ci")))
            } else {
                shape <- bind_rows(shape, list("Coefficient" = row$Parameter, "Estimate" = as.character(row$Median), "Type" = paste0(row$Parameter, "_coef")))
                shape <- bind_rows(shape, list("Coefficient" = "", "Estimate" = paste0("(",row$CI_low, ", ", row$CI_high, ")"), "Type" = paste0(row$Parameter, "_ci")))
                
            }
            
        }
    }
    # Sorting rows
    if (dim(random.int)[1] >= 1) {
        random.int <- sort.rows(random.int)
        if (dim(random.slope)[1] >= 1) {
            random.slope <- sort.rows(random.slope) 
            corr <- sort.rows(corr)
        }
    }
    
    new <- bind_rows(fixed, fixed.intercept, random.int, random.slope, corr, shape)
    return(new)
}

sort.rows <- function(tab) {
    # A short function to sort rows in a subtable
    # Type allows both rows to be sorted (coef and CI)
    tab <- tab %>% mutate(level = ifelse(str_detect(Type, "country_year"), 4,
                                         ifelse(str_detect(Type, "year"), 3,
                                                ifelse(str_detect(Type, "country"), 2, 1)
                                         )))
    
    tab <- tab %>% arrange(level) %>% select(Coefficient, Estimate, Type)
    
    return(tab)
}

correct.names.latex <- c("\\textit{Fixed part}", "Log(Activity)", "", "Populism", "", "Left-Right", "",
                         "Cabinet", "", "Prime minister", "", "Vote \\%", "",
                         "GDP per capita", "", "Population", "", "Social networks \\%", "",
                         "Election", "", "$\\alpha$","", "\\textit{Random part}",
                         # Random intercepts
                         "$\\sigma_{\\alpha_P}$", "", "$\\sigma_{\\alpha_C}$", "", "$\\sigma_{\\alpha_Y}$", "", "$\\sigma_{\\alpha_{CY}$}", "",
                         # Random slopes
                         "$\\sigma_{\\gamma_{1C}}$", "", "$\\sigma_{\\gamma_{1Y}}$", "", "$\\sigma_{\\gamma_{1CY}}$", "",
                         # Correlations
                         "$\\rho_{\\alpha_C, \\gamma_{1C}}$", "", "$\\rho_{\\alpha_Y, \\gamma_{1Y}}$", "", "$\\rho_{\\alpha_{CY}, \\gamma_{1CY}}$", "",
                         # Shape
                         "$\\phi$", ""
)

make.table <- function(models, name) {
    # Inputs consist of 3 models in order from FE to RE and name of outcome
    r1 <- extract.info(models[[1]]) %>% as.data.frame() %>% process.results() %>%
        rename(M1 = Estimate)
    r2 <- extract.info(models[[2]]) %>% as.data.frame() %>% process.results() %>%
        rename(M2 = Estimate)
    r3 <- extract.info(models[[3]]) %>% as.data.frame() %>% process.results() %>%
        rename(M3 = Estimate)
    
    # Merging columns from left to right to keep all rows (longest first)
    # Then using select to reverse order
    reg.tab <- r3 %>% left_join(r2, by = "Type") %>% left_join(r1, by = "Type") %>%
        select(Coefficient.x, M1, M2, M3)
    
    # Adding title rows, renaming rows and columns
    reg.tab <- add_row(reg.tab, .before = 1)
    reg.tab <- add_row(reg.tab,.before = 24)
    reg.tab$Coefficient.x <- correct.names.latex
    colnames(reg.tab) <- c("", "Model 1", "Model 2", "Model 3")
    return(reg.tab)
    
}

add.n <- function(models) {
    N <- list("N",0,0,0)
    i <- 2
    for (m in models) {
        s <- summary(m)
        N[i] <- as.character(s$nobs)
        i <- i + 1
    }
    N <- as.data.frame(N)
    colnames(N) <- c("", "Model 1", "Model 2", "Model 3")
    return(N)
}

render.table <- function(reg.tab, models, name, col.names) {
    reg.tab <- bind_rows(reg.tab, add.n(models))
    colnames(reg.tab) <- col.names
    kable(reg.tab, "latex", escape = FALSE, booktabs = TRUE, align = "lcccc") %>%
        kable_styling(full_width = TRUE) %>%
        column_spec(1, width = "5cm") %>%
        column_spec(2, width = "3cm") %>%
        column_spec(3, width = "3cm") %>%
        column_spec(4, width = "3cm") %>%
        column_spec(5, width = "3cm") %>%
        as_image(file = paste0("final_tables/", name, ".pdf"))
}

models.test <- list(fb_likes_gps_rs_cy, fb_shares_gps_rs_cy, fb_comments_gps_rs_cy)
name.test <- "fb_gps"
#names <- c("likes", "shares", "comments")
t2 <- make.table(models.test, name.test)
col.names <- c("", "Likes", "Shares", "Comments")
render.table(t2, models.test, name.test, col.names)


# Now to make 6 lists and names
# 3 for FB
fb.populist <- list(fb_likes_populist_rs_cy, fb_shares_populist_rs_cy, fb_comments_populist_rs_cy)
fb.poppa <- list(fb_likes_poppa_rs_cy, fb_shares_poppa_rs_cy, fb_comments_poppa_rs_cy)
fb.gps <- list(fb_likes_gps_rs_cy, fb_shares_gps_rs_cy, fb_comments_gps_rs_cy)

tw.populist <- list(tw_likes_populist_rs_cy, tw_retweets_populist_rs_cy, tw_replies_populist_rs_cy)
tw.poppa <- list(tw_likes_poppa_rs_cy, tw_retweets_poppa_rs_cy, tw_replies_poppa_rs_cy)
tw.gps <- list(tw_likes_gps_rs_cy, tw_retweets_gps_rs_cy, tw_replies_gps_rs_cy)


models <- list(fb.populist, fb.poppa, fb.gps, tw.populist, tw.poppa, tw.gps)

names <- c("fb_populist", "fb_poppa", "fb_gps",
           "tw_populist", "tw_poppa", "tw_gps")

for (i in 1:length(names)) {
    print(names[i])
    tb <- make.table(models[[i]], names[i])
    if (i <= 3) {
        render.table(tb, models[[i]], names[i], c("", "Likes", "Shares", "Comments"))
    } else {
        render.table(tb, models[[i]], names[i], c("", "Likes", "Retweets", "Replies"))
    }
    
}

# Captions won't work so just made without titles and named within Word doc

# Running for categorical models
# Requires a change to the names file

# Only using simplified version to do large number of auxillary parameters
correct.names.latex <- c("Log(Activity)", "", 
                         "Centrist populism", "", "Left-wing populism", "", "Right-wing populism", "",
                         "Left-Right", "",
                         "Cabinet", "", "Prime minister", "", "Vote \\%", "",
                         "GDP per capita", "", "Population", "", "Social networks \\%", "",
                         "Election", "", "$\\alpha$","",
                         # Shape
                         "$\\phi$", ""
)

# Modifying make.table function for categorical populism measure
make.table <- function(models, name) {
    # Inputs consist of 3 models in order from FE to RE and name of outcome
    r1 <- extract.info(models[[1]]) %>% as.data.frame() %>% process.results() %>%
        rename(M1 = Estimate)
    r2 <- extract.info(models[[2]]) %>% as.data.frame() %>% process.results() %>%
        rename(M2 = Estimate)
    r3 <- extract.info(models[[3]]) %>% as.data.frame() %>% process.results() %>%
        rename(M3 = Estimate)
    
    # Merging columns from left to right to keep all rows (longest first)
    # Then using select to reverse order
    reg.tab <- r3 %>% left_join(r2, by = "Type") %>% left_join(r1, by = "Type") %>%
        select(Coefficient.x, M1, M2, M3)
    
    # Slice out rows 1-27 and 91-92 to avoid having to print all the random parameters in a table
    colnames(reg.tab) <- c("a", "b", "c", "d") # Will not work with empty name so assigning placeholder
    reg.tab <- reg.tab %>% mutate(row_num = row_number()) %>%
        filter(row_num %in% c(1:26, 89:90)) %>%
        select(-row_num)
    
    # Adding title rows, renaming rows and columns
    reg.tab$a <- correct.names.latex
    colnames(reg.tab) <- c("", "Model 1", "Model 2", "Model 3")
    return(reg.tab)
    
}

models.fb <- list(fb_likes_populist_cat_rs_cy, fb_shares_populist_cat_rs_cy, fb_comments_populist_cat_rs_cy)
name.fb <- "fb_cat"
tfb <- make.table(models.fb, name.fb)
col.names <- c("", "Likes", "Shares", "Comments")
render.table(tfb, models.fb, name.fb, col.names)   

models.tw <- list(tw_likes_populist_cat_rs_cy, tw_retweets_populist_cat_rs_cy, tw_replies_populist_cat_rs_cy)
name.tw <- "tw_cat"
#names <- c("likes", "shares", "comments")
ttw <- make.table(models.tw, name.tw)
col.names <- c("", "Likes", "Retweets", "Replies")
render.table(ttw, models.tw, name.tw, col.names)