# 0. Dependencies ------------------------------------------------------------
set.seed(12345)
library(here)
library(data.table)
library(ggplot2)   # For plotting
library(patchwork) # For arranging multiple plots
library(Hmisc)     # For bootstrapped flexible PMM imputation
library(stan4bart)
# library(naniar) # For missing exploration
# library(splines)
# library(rlang)
options(max.print = 300L)

# Plotting configuration
source(here::here("scripts", "config", "tvp_ggplot_theme.R"))
ggplot2::theme_set(tvp_theme(base_size = 17L, no_grid = TRUE))
large_plot      <- c(8L, 15L) # Inches
medium_plot     <- c(6L, 11L)
alpha_plot      <- 0.6      # Transparency for some geom
wrapping_length <- 60L      # How many characters before wrapping for titles and subtitles
linewidth_plot  <- 1L
manuscript_mode <- TRUE # Removes title, subtitle and caption off figures
print_all       <- TRUE
save_all        <- TRUE
output_format   <- "pdf"

# Define path where HPC cluster folder is: RDS objects and R scripts need to be there
cluster_path <- "M:\\p2149-tomasfv\\bash\\definite\\"

# Functions
  # Wrapper function to save plot as a file with sensible defaults
ggsaver <- function(plot, 
                    filename,
                    output_format = "pdf",
                    path = here::here("output", "plots"),
                    size = c("large", "medium", "custom"),
                    custom_plot = NULL,
                    large_plot = c(8L, 15L),
                    medium_plot = c(6L, 11L),
                    manuscript_mode = FALSE,
                    manuscript_shrink_factor = 0.80){
  
  if (length(size) == 1) {
    if (size == "custom") {
      stopifnot("If using a custom size,
            you must give a numerical vector
            `c(height, width)` (inches) to the `custom_plot` argument." =
              !is.null(custom_plot)) 
    }
  }

  if(is.character(size)){
    size <- switch(match.arg(size), "large" = large_plot,
                   "medium" = medium_plot, "custom" = custom_plot)
  }

  if (manuscript_mode == TRUE) {
  size <- size * manuscript_shrink_factor
    
  plot  <- plot + theme(plot.title      = element_blank(),
                 plot.subtitle   = element_blank(),
                 plot.caption    = element_blank())
  }
  ggplot2::ggsave(
    plot = plot,
    filename = if (manuscript_mode == TRUE) {glue::glue("{filename}_mm.{output_format}")} else 
      {glue::glue("{filename}.{output_format}")} ,
    path = path,
    device = output_format,
    height = size[1], width = size[2],
    units = "in"
  )
}

  # Extract two-character domain string from variable name
identify_domain <- function(input_string) { 
  pattern <- c("MA", "EN", "RE") # Order matters, as this is will stop with the first one that finds
  found_substring <- character(0)
  
  for (p in pattern) {
    if (grepl(p, input_string, ignore.case = FALSE)) {
      found_substring <- p
      break
    }
  }
  
  return(found_substring)
}

#   # Get percent of missing postest per domain only for eligible (non-missing) pretests
# percent_missing_postest <- function(domain, # could vectorize
#                                     named_list = outcome_meta_list,
#                                     data = dt_merged){
#   
#   if (is.numeric(domain)) {
#     domain <- switch(domain, "numeracy", "reading", "english")
#   }  
#   
#   elligible_pretest <- data[!is.na(get(outcome_meta_list[[domain]]$pretest))] |> nrow()
#   missing_postest   <- data[!is.na(get(outcome_meta_list[[domain]]$pretest)) &
#                               is.na(get(outcome_meta_list[[domain]]$posttest))] |> nrow()
#   z <- 100 - 100*((elligible_pretest - missing_postest) / elligible_pretest) # 90.64%
#   names(z) <- domain
#   return(z)
# }

# 1. Data loading and preparation -----------------------------------------

# Create RDS objects from .Rdata files given by G.B.
# Load RDS objects

source(here::here("scripts", "00_load_data.R"))

# Assign NA to 1st dispensions after posttest
# Create age.1st.dispension variable
# Replace 0 with NA for test scores
# Address 0 of school average test scores
# Creates exemption variables per outcome for eligible participants
# Creates outcome_time_list ($time  [vector of names])
# Creates outcome_meta_list   ($domain$time)
# Creates avg_school_scores_names character vector for all school test averages columns
# Define exposure variable: `initiated`
# Creates binary variable for medication before time zero: `medicated.pt0`
# Creates dt_eligible, dataset that excludes pupils with zero pretests
# Creates dt_complete case, dataset with complete data for outcome covariates
# Standardizes national test scores relative to general cohort (including non-ADHD children)
# Creates vector list `imputation_covariates`
# Creates vector list `outcome_covariates`
# Reduces data frames to only variables in the imputation and outcome vectors


standardized_scores <- TRUE # Posttest scores standardized relative to the general population of children for each birth year 
descriptive_dataset <- TRUE # To create dataset for table 1 and table 2-type descriptives
reduce_columns      <- FALSE
# reduction_exclusions <- c("NCE") # Comment out if not needed. Var names not part of outcome_covariates nor imputation_covariates that you want to keep. 

source(here::here("scripts", "01_prepare_data.R"))

# 2. Description and exploration ------------------------------------------

# Inform choice on time-zero choice, grace period and post-exam diagnosis window
# Determine correlations of known confounders for bias analysis
# Examine adherence trajectories
# Check exemption patterns for scores and GPA

source(here::here("scripts", "02_descriptives.R"))
# source(here::here("scripts", "03_exploratory_analyses.R"))

# 3. Imputation ----------------------------------------------------------------

# Save objects loaded unto HPC cluster before imputation
saveRDS(dt_merged, file = glue::glue("{cluster_path}dt_merged.RDS"))
saveRDS(imputation_covariates, file = glue::glue("{cluster_path}imputation_covariates.RDS"))
saveRDS(outcome_covariates, file = glue::glue("{cluster_path}outcome_covariates.RDS"))
file.copy(from = here::here("scripts", "04_pmm_imputation.R"), to = glue::glue("{cluster_path}"), overwrite = TRUE)

# Returns a pmm_imputed_list of `num_imputations` datasets each 
# with unique imputed values for each outcome from each domain
# and if x1 is in the name, with imputed `congenial_covariates`
  # Submit job script:
# sbatch pmm_imputation_script.sh

# 4. BART analysis -------------------------------------------------------------

# 4.1 :: Fitting ----------------------------------------------------------

# Copy objects unto HPC cluster before fitting
# saveRDS(imputation_covariates, file = glue::glue("{cluster_path}imputation_covariates.RDS"))
saveRDS(eligible_IDs, file = glue::glue("{cluster_path}eligible_IDs.RDS"))
saveRDS(outcome_covariates, file = glue::glue("{cluster_path}outcome_covariates.RDS"))
file.copy(from = here::here("output", "objects", "pmm_imputation", "pmm_x_imputed_list.RDS"), to = glue::glue("{cluster_path}"), overwrite = TRUE)
file.copy(from = here::here("scripts", "06_BART_definite.R"), to = glue::glue("{cluster_path}"), overwrite = TRUE)
file.copy(from = here::here("scripts", "06c_bart_diagnostics.R"), to = glue::glue("{cluster_path}"), overwrite = TRUE)

# Outputs a full folder of RDS objects for each analyzed imputed dataset for each domain for each sample (analytic or washout)
# ~ 150 MB each file, num_imputations/thinning * 3 (Domains) * 2 (Samples) files
  # Submit job script with command line arguments:
# sbatch bart_job.sh $thinning $domain $washout


# :: 4.2 Processing -------------------------------------------------------

# Output a `definite_washout_meaneffects_list` and `definite_meaneffects_list`
  # of three domains and three estimands with posterior samples for the
  # analytic and washout samples, respectively.
# Output the samples in a long format, in data.tables `long_dt` and
  # `washout_long_dt`
# Output a list `summarized_posteriors`containing summary (mean and 
  # confidence interval) for both samples, three estimands and three domains

# source(here::here("scripts", "06b_bart_colossus_output_processing.R"))


# # 5. Joint imputation and multivariate model -----------------------------------
# 
# # Copy objects unto HPC cluster before fitting
# saveRDS(dt_eligible_std, file = glue::glue("{cluster_path}dt_eligible_std.RDS")) # Must be reduced dataset!
# file.copy(from = here::here("scripts", "07_joint_cmdstanr_clean.R"), to = glue::glue("{cluster_path}"), overwrite = TRUE)
# file.copy(from = here::here("scripts", "07b_joint_cmdstanr_utils.R"), to = glue::glue("{cluster_path}"), overwrite = TRUE)
# 
# 
# # sbatch joint_imputation_job.sh

# 6. Effect summaries ----------------------------------------------------------

# Generate and save table with effect summaries across model specifications: `figure3_effects.RDS`
# Save ggplot object of effect summaries: `fig3_ggplot.RDS`
# Save ggplot figure of effect summaries: `figure3_effects.pdf`
# Prepare and clean table for manuscript: `effect_table.RDS` and `latex_effects.tex`

source(here::here("scripts", "08_effect_summaries.R"))

# 7. Keilow replication --------------------------------------------------------

# Do an informal replication attempt of the results of Keilow et al (2018) but on
# our Norwegian sample
# Store results in small data.table objects: 
  # `keilow_prev.RDS` for prevalence of treatment patterns
  # `keilow_effects.RDS`  for estimated effects for teacher-evaluated GPA

source(here::here("scripts", "09_keilow_replication.R"))

# 8. Programmatic text generation ----------------------------------------------

# Programmaticaly generate text that depends on specific numeric values
# from previously generated objects. 
# Generate one paragraph per topic in a text file already escaping characters
# for proper pasting into LaTeX document:
  # `programmatic_text.txt`

source(here::here("scripts", "10_programmatic_text.R"))



# > sessionInfo()
# 28-02-2024
# R version 4.2.3 (2023-03-15 ucrt)
# Platform: x86_64-w64-mingw32/x64 (64-bit)
# Running under: Windows Server x64 (build 17763)
# 
# Matrix products: default
# 
# locale:
#   [1] LC_COLLATE=Norwegian Bokmål_Norway.1252  LC_CTYPE=Norwegian Bokmål_Norway.1252    LC_MONETARY=Norwegian Bokmål_Norway.1252 LC_NUMERIC=C                            
# [5] LC_TIME=Norwegian Bokmål_Norway.1252    
# 
# attached base packages:
#   [1] stats     graphics  grDevices utils     datasets  methods   base     
# 
# other attached packages:
#   [1] MatchIt_4.5.2     stan4bart_0.0-6   dbarts_0.9-23     Hmisc_5.0-1       patchwork_1.1.2   ggplot2_3.4.1     data.table_1.14.8 here_1.0.1       
# 
# loaded via a namespace (and not attached):
#   [1] tidyselect_1.2.0   xfun_0.38          lattice_0.20-45    colorspace_2.1-0   vctrs_0.6.1        generics_0.1.3     htmltools_0.5.5    base64enc_0.1-3    utf8_1.2.3        
# [10] rlang_1.1.0        pillar_1.9.0       foreign_0.8-84     glue_1.6.2         withr_2.5.0        lifecycle_1.0.3    stringr_1.5.0      munsell_0.5.0      gtable_0.3.3      
# [19] ragg_1.2.5         htmlwidgets_1.6.2  evaluate_0.20      labeling_0.4.2     knitr_1.42         forcats_1.0.0      fastmap_1.1.1      parallel_4.2.3     fansi_1.0.4       
# [28] htmlTable_2.4.1    Rcpp_1.0.10        scales_1.2.1       backports_1.4.1    checkmate_2.1.0    RcppParallel_5.1.7 parallelly_1.35.0  systemfonts_1.0.4  farver_2.1.1      
# [37] textshaping_0.3.6  gridExtra_2.3      digest_0.6.31      stringi_1.7.12     dplyr_1.1.1        stargazer_5.2.3    grid_4.2.3         rprojroot_2.0.3    khroma_1.9.0      
# [46] cli_3.6.1          tools_4.2.3        magrittr_2.0.3     tibble_3.2.1       Formula_1.2-5      cluster_2.1.4      pkgconfig_2.0.3    Matrix_1.5-3       timechange_0.2.0  
# [55] lubridate_1.9.2    rmarkdown_2.21     rstudioapi_0.14    R6_2.5.1           rpart_4.1.19       nnet_7.3-18        compiler_4.2.3 
