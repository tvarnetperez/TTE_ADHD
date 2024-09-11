# 0. Dependencies ---------------------------------------------------------
set.seed(123456)
library(boot)

if (Sys.info()["sysname"] == "Windows"){
  saveRDS(pretest_names_std, file = "pretest_names_std.RDS")
  saveRDS(dt_completecase, file = "dt_completecase.RDS")
  saveRDS(outcome_covariates, file = "outcome_covariates.RDS")
}

if (Sys.info()["sysname"] != "Windows"){
  readRDS(file = "pretest_names_std.RDS")
  readRDS(file = "dt_completecase.RDS")
  readRDS(file = "outcome_covariates.RDS")
}



# 1. Partition variables --------------------------------------------------

  # Treatment variable
treatment <- "initiated"

  # Negative control exposure
    # This is the post-outcome med initiation
NCE <- "NCE"
  # Negative control outcome
    # This are the pre-exposure test scores
NCOs <- pretest_names_std

  # Remove school indicators
outcome_covariates <- setdiff(outcome_covariates, c("s.id5", "s.id8"))

endogenous_vars <- NCOs
exogenous_vars <- c(outcome_covariates, treatment)
instruments <- NCE


# 2. Fit model ------------------------------------------------------------

  # Pre-allocate list of outcome-specific complete case data.tables
complete_dts <- list("EN" = NA, "RE" = NA, "MA" = NA)

# Exclude missing posttest observations of that respective outcome
for (posttest in posttest_names_std) {
  domain <- identify_domain(posttest)
  complete_dts[[domain]] <- dt_completecase[!is.na(get(posttest))]
}

# Change names so they match the article's terminology
Ws <- paste0("W", 1:length(NCOs))
Ys <- paste0("Y", 1:length(posttest_names_std))

# Create vector of new one-letter names
new_vars <- c(Ys, 
              "Z",
              "A",
              Ws
)

# Update by reference
sapply(complete_dts,
       function(x) setnames(x,
                            old = c(posttest_names_std,
                                    "NCE",
                                    "initiated",
                                    NCOs
                            ),
                            new = new_vars)
)

  # Add names to the vector for traceability
names(Ys) <- c("EN", "RE", "MA")

# 2.1. Example specification -------------------------------------------------

# formula_iv <- glue::glue("Y ~ {eta_X} + A + {glue::glue_collapse(Ws, sep = (' + '))} |
#                          A + {eta_X} + Z") |> as.formula()
# 
# iv_fit <- ivreg(formula = formula_iv, data = dt_completecase_Y)
# 
# summary(iv_fit)

# 2.2 Manual Proximal 2SLS ----------------------------------------------------


# Extract domain
# domain <- identify_domain(posttest)

# First use this to check the code works, then loop and store for all 3

# Define formula components
  # Ws: NCOs
  # Z: NCE
  # X: covariates
  # A: treatment

eta_X <- paste0(setdiff(outcome_covariates, NCOs), collapse = " + ") 

formula_W <- glue::glue("cbind({glue::glue_collapse(Ws, sep = ', ')}) ~ A + Z + {eta_X}") |> as.formula()
formula_Y <- glue::glue("Y ~ A + {eta_X} +
                        {glue::glue_collapse(Ws, sep = ' + ')}") |> as.formula()

# Pre-allocate
targets <- rep(NA_real_, length(posttest_names_std))
names(targets) <- c("EN", "RE", "MA")

# :: 2.2.1 Model for W conditional on A, Z, X ----------------------------------
start <- Sys.time()

boot_p2sls <- function(data, index){
  # Sample with replacement
data <- data[index, ]
  # Fit first_stage
  first_stage <- lm(formula_W,
              data = data)

  # Predictions for W
W_hats <- predict(first_stage, newdata =  data) |> as.data.table()
#  # Create reduced data.table copy
# dt_pred <- data[, .SD, .SDcols = c(new_vars)] 
  # Replace original values with predicted values
data[, c("W1", "W2", "W3") := W_hats]
  # Fit second stage
second_stage <- lm(formula_Y, 
   data = data)

target <- second_stage$coefficients[["AInitiated"]]

return(target)}

  # Pre-allocate
bootstraps <- list()

# 5k resamples was not enough as the mean was too sensitive to a single observation
for(i in 1:length(complete_dts)){
  # Name the "active" outcome as Y instead of Y1, Y2 or Y3
  setnames(complete_dts[[i]], old = Ys[i], new = "Y")
  # Run bootstrap
  bootstraps[[i]] <- boot(data = complete_dts[[i]], statistic = boot_p2sls, R = 500000L,
                          parallel = "multicore")
}
rm(complete_dts)

if (Sys.info()["sysname"] != "Windows"){
  # Store output
  saveRDS(bootstraps, file = "bootstraps_P2SLS.RDS")
}

if (Sys.info()["sysname"] == "Windows"){
  # Store output
  saveRDS(bootstraps, file = here::here("output", "objects", "bootstraps_P2SLS.RDS"))
}



# 2.3 Bootstrapped --------------------------------------------------------

# This is run in the cluster with boot_job.sh
if (Sys.info()["sysname"] != "Windows") {
  start <- Sys.time()
  
  boot_p2sls <- function(data, index){
    # Sample with replacement
    data <- data[index, ]
    # Fit first_stage
    first_stage <- lm(formula_W,
                      data = data)
    
    # Predictions for W
    W_hats <- predict(first_stage, newdata =  data) |> as.data.table()
    #  # Create reduced data.table copy
    # dt_pred <- data[, .SD, .SDcols = c(new_vars)] 
    # Replace original values with predicted values
    data[, c("W1", "W2", "W3") := W_hats]
    # Fit second stage
    second_stage <- lm(formula_Y, 
                       data = data)
    
    target <- second_stage$coefficients[["AInitiated"]]
    
    return(target)}
  
  # Pre-allocate
  bootstraps <- list()
  
  # 5k resamples was not enough as the mean was too sensitive to a single observation
  # 250k is max else we get integer overflow
  for(i in 1:length(complete_dts)){
    # Name the "active" outcome as Y instead of Y1, Y2 or Y3
    setnames(complete_dts[[i]], old = Ys[i], new = "Y")
    # Run bootstrap
    bootstraps[[i]] <- boot(data = complete_dts[[i]], statistic = boot_p2sls, R = 200000L,
                            parallel = "multicore")
  }
  rm(complete_dts)
  
  if (Sys.info()["sysname"] != "Windows"){
    # Store output
    saveRDS(bootstraps, file = "bootstraps_P2SLS.RDS")
  }
  
  if (Sys.info()["sysname"] == "Windows"){
    # Store output
    saveRDS(bootstraps, file = here::here("output", "objects", "bootstraps_P2SLS.RDS"))
  }

  print(Sys.time() - start)
}


#::::::::::::::::::::::::::::::::::::::::::::::
# Notes
#::::::::::::::::::::::::::::::::::::::::::::::

# Both specifications below in the package example lead to the same result
#

# Endogenous & Exogenous | Instruments & Exogenous

# m_iv <- ivreg(log(wage) ~ education + poly(experience, 2) + ethnicity + smsa + south |
#                 nearcollege + poly(age, 2) + ethnicity + smsa + south,
#               data = SchoolingReturns)
# 
# Exogenous | Endogenous | Instruments
# m_iv <- ivreg(log(wage) ~ ethnicity + smsa + south | education + poly(experience, 2) |
#                 nearcollege + poly(age, 2), data = SchoolingReturns)


# In the example:
# `ethnicity` `smsa` and `south` are exogenous
# `education` (treatment) and `experience` are endogenous
# `nearcollege`, `age` are the instruments


# In p. 17 they use as specification:
# "Y ~ A + X + W | A + X + Z"
# Which means:
# They consider treatment `A` exogenous!
# They consider NCO `W` endogenous
# They consider NCE `Z` an instrument
# They consider covariates `X` exogenous


# # Thus, in our case we have
# # Outcome               Exogenous              |Endogenous  | Instruments
# {outcome} ~ {outcome_covariates} + {treatment} |  {NCO}     | {NCE}
# 




# 
# Call:
#   lm(formula = formula_Y, data = dt_completecase_Y)
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -2.89814 -0.42405 -0.00911  0.43072  2.87671 
# 
# Coefficients:
#                            Estimate  Std. Error t value Pr(>|t|)    
# (Intercept)               2.350e-01  1.699e-01   1.383 0.166696    
# AInitiated                7.531e-02  2.250e-02   3.347 0.000820 ***
#   W1                      5.029e-02  9.587e-03   5.246 1.60e-07 ***
#   W2                      1.363e-01  1.036e-02  13.160  < 2e-16 ***
#   W3                      5.452e-01  1.022e-02  53.318  < 2e-16 ***


# Point estimate of around 0.075
# Consistent with the main analysis
# After maybe removing the confounding from a binary indicator of
# being worse at school. i.e. a cause of low pretest performance and a cause of
# being medicated after the posttest


# Without X

# lm(Y ~ A + W1 + W2 + W3,
#    data = dt_pred)


# No covariates (X is empty):
# Call:
#   lm(formula = Y ~ A + W1 + W2 + W3, data = dt_pred)
# 
# Coefficients:
#   (Intercept)   AInitiated      W1           W2  
# -0.18414     -0.02306      0.04793      0.13224  
# W3  
# 0.54115 
# 
# Not surprisingly, the effect is close to zero as we are not taking into account
# the observed confounding. The non-negative effects of the W1, W2 and W3
# proxies suggest moderate evidence that they capture residual confounding.

