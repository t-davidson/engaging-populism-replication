# Replication materials for Engaging Populism

Last edited 6/17/2024

This repository contains replication materials for Thomas Davidson and Jenny Enos. Forthcoming. "Engaging Populism? The Popularity of European Populist Political Parties on Facebook and Twitter, 2010-2020." *Political Communication* doi: https://doi.org/10.1080/10584609.2024.2369118

Please get in touch via email if you have any questions.

# Version information

Most analyses were performed on a Macbook Pro laptop using R and are compatible with the latest version as of publication (4.1.2). The main estimation procedures were executed using the [Amarel high-performance computing cluster](https://oarc.rutgers.edu/resources/amarel/) at Rutgers University, a Linux-based environment consisting of a large gluster of compute nodes that can be used to execute processes in parallel.

# Codebase
The following sections detail the organization of the data and processes for data cleaning and merging, the estimation of the models, and the construction of the results.

At a high-level, our analyses can be replicated by running the scripts to process and merge the data (`cleaning-and-merging`), estimating the models (`models`), then finally generating the figures and tables (`results`). Since we are providing the full output from our model estimation, the final results can be reproduced simply by using these files to produce the figures and tables without needing to perform the intermediate steps.

## Data

The following describes the data sources used in the analyses. See the following section for instructions on running the scripts to generate the final analysis data. All data are stored in the `data` folder. 

### Social media data

The Facebook and Twitter posts were collected from the respective APIs. These APIs require authorization and the Twitter Academic API is no longer active, but example scripts to show how these APIs were queried are contained in `data-collection`.

Tables containing raw social media data collected from the APIs are stored in `facebook_data_raw` and `twitter_data_raw`, respectively. The versions of these data used in the final country-year analyses are contained in `facebook_data_cleaned` and `twitter_data_cleaned`. The process for construct this data are described below.

### Party-level data

The data on political parties are stored in three different places. `populism_data` contains the populism measures from the PopuList, GPS, and POPPA. `partyfacts_data` and `parlgov_data` contain the PartyFacts and ParlGov datasets respectively.

### Country-level data
There are two main sources of country-level data. `country_data` contains a table on country covariates from the World Bank and another table with information on Northern Ireland from the UK Office of National statistics. The measure of social media use from the Eurobarometer is included in the `eurobarometer_data` directory.

## Building the analysis datasets

The analysis dataset is build by running several different components. The scripts are contained in the `cleaning-and-merging` directory.

### Processing social media data
The raw social media data are combined into cleaned versions by running `create_facebook_datasets.R` and `create_twitter_datasets.R`. These scripts yield two versions of each dataset: an aggegated country-year dataset used for the main analyses and a post-level dataset used for a robustness check. The original data are not provided with this replication package but these scripts show the logic of the approach used to prepare the data.

### Processing country data
The different country-level datasets are combined by running the script `country-data-compilation.R`. This merges together the World Bank, Northern Ireland, and Eurobarometer datasets.

Next, the `country-imputation.R` script can be run to load the output from the previous step and execute the procedure to impute missing observations for the Eurobarometer data. This script yields the final country dataset, `data/country_data/final_imputed_country_data.csv`.

### Merging data sources
Once the yearly social media data and country-level data have been compiled, `full_merge_script.R` can be run to merge together the party-level and country-level covariates. This script is the most complicated part of the process as it involves joining together several different sources. The output is stored as `data/final_merged_data/final_merged_dataset.csv`.

### Final data cleaning
Some additional cleaning steps are used to prepare the merged data for modeling. The `final_data_cleaning.R` includes additional steps to filter the analysis dataset and construct additional variables. The outputs from this script are two datasets: `/data/model_data/model_dataset_fb.csv` and `/data/model_data/model_dataset_tw.csv`. There are also some supplementary datasets created for robustness checks (different versions of Irish data) and to provide information for visualization.

## Estimating the models

All models reported in the paper were estimated using the [`brms`](https://paul-buerkner.github.io/brms/) R package and [RStan](https://mc-stan.org/users/interfaces/rstan).  The analysis was performed using a SLURM high-performance compute cluster, which allows multiple models to be estimated simultaneously rather than in linear order, significantly reducing the compute time.

This infrastructure necessitates each model to be stored in a separate script, such that the SLURM controller can run separate scripts. As such, our models are organized in the following way:

The `models` directory contains all code. The `scripts` directory inside this contains subdirectories for different parts of the analysis.

`POPULIST`, `GPS`, and `POPPA` include scripts for the main three outcomes and `POPULIST_CATEGORICAL` models for the categorical measure from PopuList. In each case, the following convention is used to name the files.

Dataset + Platform + Outcome + Model. The following codes are used for models: `fe` = Fixed effects, `re` = Random intercepts, `rs` = Random intercepts and slopes, `rs_cy` = Random slopes and intercepts including at country-year (main specification). For example, `populist_tw_retweets_rs_cy.R` runs the model to estimate the relationship between number of retweets and populism using the Populist measure.

The `INTERACTIONS` subdirectory contains models to estimate interaction terms. In this case, the last part of the name indicates the interacting variable. 

Each script does the following. First, the line `source(load-data.R)` is run to load the final datasets into memory. This script also performs some adjustments to ensure that variables are correctly represented and some additional filtering. Next, the `brm` function from the package `brms` is used to estimate each model using Bayesian estimation. The model is then stored in a directory called model results.

Note: For our analyses, these models were run on a remote server and the results were then downloaded. The scripts in the replication package have been modified to ensure that results are stored in the correct place to render plots (`results/output` and `results/output_interactions`). This means that these models could be re-run locally rather than requiring a SLURM controller.

## Analyzing the results

The final models are stored as `.RData` objects output by `brms`. These can be read into memory to extract various information used to produce the figures and tables. Our final fitted models are included with this replication package, so it is not necessary to re-estimate the models in order to reproduce our results. These models are too large to be stored on Github but can be downloaded using this [Google Drive](https://drive.google.com/drive/folders/1DNpAeBIZ-jlQuwhWPZ0RjqpcGcrod9KJ?usp=sharing) link. Please contact the lead author if the link does not work.

The `results` folder contains scripts used to produce results in the paper as well as directories containing various figures and tables. 

### Main results

`descriptive` contains a file `descriptive_analysis.R` that reproduces Figure 1 and Figure 2.

The remaining figures are produced by running the following scripts in `results/plots/` and all output is stored in `results/plots/figures`.

`main_coefficient_plot.R` reproduces Figure 3.

`categorical_populism_coefficient.R` reproduces Figure 4.

`random_slopes_main.R` reproduces Figure 5.

`random_slopes_by_ideology` reproduces Figure 6.

`country_stories_facet_populist.R` reproduces Figure 6.

`interaction_coefficient.R` reproduces Figure 8.

### Additional results and robustness checks

These scripts can also be used to reproduce the various analyses reported in the Appendix.

The descriptive table is produced by `tables/final_tables/descriptive_table.R`. Note that this table required some additional formatting to render correctly in the document, whereas other tables are stored as PDFs.

The full regression tables for our main results are reproduced by the script `tables/final_tables/render_regression_tables.R`.

Additional figures for random slopes analysis for Twitter are produced by `random_slopes_main_twitter.R` and `random_slopes_by_ideology_twitter.R`. The figures are stored in the same place.

The figure for the post-level regression plot is reproduced by running `plots/post_tweet_regression.R`. This will run the frequentist models and produce the plot.
