# Preliminary exploration ------------------------------------------------------

# na_tmp <- Hmisc::naclus(dt_merged[, .SD, .SDcols = c(imputation_covariates,
#                                                      "p.EN_8",
#                                                      "p.MA_8",
#                                                      "p.RE_8",
#                                                      "p.MA_9",
#                                                      "p.RE_9")])
# 
# Hmisc::naplot(na_tmp, 'na per var')
# plot(na_tmp)

# Predictive Mean Matching -----------------------------------------------------

# If in TSD, load RDS objects and vectors with variable names
if (Sys.info()["sysname"] != "Windows") {
  dt_merged             <- readRDS(file = "dt_merged.RDS")
  imputation_covariates <- readRDS(file = "imputation_covariates.RDS")
  outcome_covariates    <- readRDS(file = "outcome_covariates.RDS")
  
  posttest_names <- c("p.EN_8", "p.RE_8", "p.MA_8")
  posttest_names_std <- paste0(posttest_names, "_std")
  
  # Check command line argument to see whether covariates will be imputed
  impute_X <- commandArgs(trailingOnly = TRUE) |> as.integer() |> as.logical()
  print(impute_X)
  
  
  # Check if the outcome in the data.table includes the "std" substring
  standardized_scores <- grep(pattern = "p.EN_8",
                              colnames(dt_merged),
                              value = TRUE) |> grepl(pattern = "std",
                                                     x = _) |>
    any() # Since there will be exempt indicators and raw scores as well that match the first pattern
  
}

start_time <- Sys.time()

set.seed(123)

num_imputations <- 250L
num_knots <- 3L

# Formula of predictors in imputation model
formula_tmp <- paste(" ~ ", paste(imputation_covariates,
                                             collapse = " + "))
# Call for PMM imputation
pmm_impute_output <- Hmisc::aregImpute(
  formula = as.formula(formula_tmp),
  data = dt_merged[,
                   .SD,
                   .SDcols = c(imputation_covariates)],
  nk = num_knots, # Use 0:3 To get CV and bootstraps. 
  tlinear = FALSE, # Default is TRUE if nk > 0, restricting target to have no transformation
  type = "pmm",
  match = "weighted",
  pmmtype = 3, # We do not have a small number of variables so we choose to go with ABB
  # nperm = 5, # Note the algorithm is not order-invariant
  n.impute = num_imputations,
  plotTrans = FALSE)

end_time <- Sys.time()
print(start_time - end_time)

# Save output
  # Save path for Windows
if (Sys.info()["sysname"] == "Windows") {
  save.image(file = here::here("output", glue::glue("definite_{num_imputations}_imputation{batch}.Rdata")))
  saveRDS(pmm_impute_output, here::here("output", glue::glue("pmm_impute_output{num_imputations}_{batch}.RDS")))
}

  # Save path for TSD, we do not need batches in the HPC
if (Sys.info()["sysname"] != "Windows") {
  save.image(file = here::here(glue::glue("finished_imputation.Rdata")))
  saveRDS(pmm_impute_output, here::here(glue::glue("pmm_impute_output.RDS")))
}

  # Troubleshooting
load(file = choose.files())



  # Reduce dataset to only variables used in imputation or outcome model + id
dt_merged_reduced <- dt_merged[, mget(union(c("id", outcome_covariates),
                                            imputation_covariates))]
  # Create list with `num_imputations` copies of the original data.table
pmm_imputed_list <- replicate(num_imputations,
                              data.table::copy(dt_merged_reduced),
                              FALSE)

  # Create vector of variable names that are common to imputation and outcome vectors
congenial_covariates <- intersect(imputation_covariates, outcome_covariates)

  # If baseline covariates are to be imputed
if (impute_X == TRUE) {
  # Will do ugly loop to go over all congenial_covariates
  for (h in 1:length(congenial_covariates)) {
    for (i in 1:num_imputations) {
      pmm_imputed_list[[i]][is.na(get(congenial_covariates[1])), c(congenial_covariates[1]) :=
                              pmm_impute_output$imputed[[congenial_covariates[1]]][, i]]
      pmm_imputed_list[[i]][is.na(get(congenial_covariates[2])), c(congenial_covariates[2]) :=
                              pmm_impute_output$imputed[[congenial_covariates[2]]][, i]]
      pmm_imputed_list[[i]][is.na(get(congenial_covariates[3])), c(congenial_covariates[3]) :=
                              pmm_impute_output$imputed[[congenial_covariates[3]]][, i]]
      print(glue::glue("Imputation {i}/{num_imputations} of {h}/{length(congenial_covariates)} variables"))
    }
  }
}

  # Could clean so that posttest_names == posttest_names_std in the standardized_scores case
if (standardized_scores == TRUE) {
  # For unstandardized variables
  for (i in 1:num_imputations) {
    pmm_imputed_list[[i]][is.na(get(posttest_names_std[1])), 
                          c(posttest_names_std[1]) :=
                            pmm_impute_output$imputed[[posttest_names_std[1]]][, i]]
    pmm_imputed_list[[i]][is.na(get(posttest_names_std[2])), 
                          c(posttest_names_std[2]) :=
                            pmm_impute_output$imputed[[posttest_names_std[2]]][, i]]
    pmm_imputed_list[[i]][is.na(get(posttest_names_std[3])), 
                          c(posttest_names_std[3]) :=
                            pmm_impute_output$imputed[[posttest_names_std[3]]][, i]]
    print(glue::glue("Imputation {i}/{num_imputations}"))
  }
}

if (standardized_scores == FALSE) {
  # For unstandardized variables
  for (i in 1:num_imputations) {
    pmm_imputed_list[[i]][is.na(get(posttest_names[1])), 
                          c(posttest_names[1]) :=
                            pmm_impute_output$imputed[[posttest_names[1]]][, i]]
    pmm_imputed_list[[i]][is.na(get(posttest_names[2])), 
                          c(posttest_names[2]) :=
                            pmm_impute_output$imputed[[posttest_names[2]]][, i]]
    pmm_imputed_list[[i]][is.na(get(posttest_names[3])), 
                          c(posttest_names[3]) :=
                            pmm_impute_output$imputed[[posttest_names[3]]][, i]]
    print(glue::glue("Imputation {i}/{num_imputations}"))
  }
}

# Prepare it for BART format by making treatment numeric instead of factor 
for (i in 1:num_imputations) {
  pmm_imputed_list[[i]][, initiated := (as.numeric(initiated) - 1L)]
  print(glue::glue("Iteration {i} updated"))
}



# Save path for Windows
if (Sys.info()["sysname"] == "Windows") {
  batch = 0L
  # Save list with all data.tables with observed + imputed unstandardized posttests
  saveRDS(pmm_imputed_list, file = here::here("output",
                                              "objects",
                                              "pmm_imputation",
                                              glue::glue("pmm_x{as.integer(impute_X)}_imputed_list{num_imputations}_{batch}.RDS")))
}

# Save path for TSD
if (Sys.info()["sysname"] != "Windows") {
  saveRDS(pmm_imputed_list, file = "pmm_x{as.integer(impute_X)}_imputed_list.RDS")
}


#:: Notes :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

# Notes on aregImpute class object, "aI":
  # aI$na is a list of J variables, with each element being a vector of integer
    # indices of which values are missing

# Regarding convergence diagnostics
# # Paul Brücke:
# In the summary output, we notice that some
# Rhat values are higher than 1.1
# indicating possible convergence problems.
# For models based on multiple imputed data sets,
# this is often a false positive: 
#   Chains of different submodels may not overlay
# each other exactly, since there were fitted to different data.
# Accordingly, we have to investigate the convergence of
# the submodels separately, which we can do by looking at
# round(fit_imp1$rhats, 2)

# Notes on the aregImpute algorithm:

# > `aregImpute` takes all aspects of uncertainty in the imputations into
#  account by using the bootstrap to approximate the process of drawing
#   predicted values from a full Bayesian predictive distribution.
# 
# The typical approach is to fit one single imputation model and then to
#  draw imputed values from its predictive distribution.
# 
# The imputation model is fitted to a sample of the observed data which could
#  have gone differently though. Thus `aregImpute` bootstraps the original
#   sample for each imputed dataset.
# 
# > the target variable is predicted using a sample with replacement
#  from the observations with non-missing target variable
# 
# 
# There are different steps. 
# 
# `areg` functions are only about fitting the imputation models. 
# Will use cross validation to choose number of knots if `nk` is a vector.
# 
# 
# aregImpute algorithm:
#   
#   I. Initialize (impute) missing values of variables with a random sample.
#  II. Repeat the following steps `burnin + number_imputations` times:
#     II.1. For each target variable with missing value X, draw a sample with 
#     replacement of size (length(X_obs)) of all the variables in the dataset.
#           II.1.a. Fit a flexible additive model to this sample, to predict 
#           the target (or different transformations of it)
#           II.1.b. Use this flexible additive model to predict both observed 
#           and missing values of the target variable X
#           II.1.c. For each predicted missing value of X, use its distance to
#            predicted observed values of X to draw an observed value to impute
#             with (Predictive Mean Matching, match = "weighted").
#           II.1.d. Keep the only the last `number_imputations` iterations
# III. Use the imputed values from variable X when X is used as a predictor for
#  another target variable with missing values X'.

