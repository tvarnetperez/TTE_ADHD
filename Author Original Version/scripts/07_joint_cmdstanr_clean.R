# If doubts about previous version, check 11_brms_joint_long.R
# There is a commented implementation to include random intercepts for schools at
# grade 5 and 8. Was deemed unnecessary in the end given stability of results

# 0. Dependencies and configuration---------------------------------------------
set.seed(1235)
library(data.table)
library(brms)
library(posterior)
library(here)
if ("glue" %in% rownames(installed.packages()) == FALSE) {
  install.packages("glue")
}

if (Sys.info()["sysname"] == "Windows") {
  cmdstanr::cmdstan_version(error_on_NA = FALSE)
  cmdstanr::set_cmdstan_path('N:/durable/software/cmdstan-2.33.1')
  cmdstanr::cmdstan_version(error_on_NA = FALSE)
  
  # Define some utility functions
  source(file = here::here("scripts", "07b_joint_cmdstanr_utils.R"))
  
  # Alias data.table
  dt <- dt_eligible_std
}

# If working on TSD / Cluster, set the path to cmdstan in a writable directory
if (Sys.info()["sysname"] != "Windows") {
  library(cmdstanr)
  cmdstanr::cmdstan_version(error_on_NA = FALSE)
  cmdstanr::set_cmdstan_path("/tsd/p2149/data/durable/shared/software/cmdstan-2.33.1/")
  cmdstanr::cmdstan_version(error_on_NA = FALSE)
  
  # Define some utility functions
  source(file = here::here("07b_joint_cmdstanr_utils.R"))
  
  # Load data
  dt <- readRDS(here::here("dt_eligible_std.RDS")) |> as.data.table()
}

# Sampler settings
options(mc.cores = parallelly::availableCores())
n_iter              <- 2000L
num_chains          <- 4L
filename_stan_model <- "definite_last_joint.stan"


# Check time of complete script
start_time <- Sys.time()
# 2. Data prepping --------------------------------------------------------

  # Hard code missing NA indicators and standardized posttests
exempt_pretest_names  <- c("exempt.p.EN_5", "exempt.p.RE_5", "exempt.p.MA_5")
exempt_posttest_names <- c("exempt.p.EN_8", "exempt.p.RE_8", "exempt.p.MA_8")
score_vars <- c("p.EN_8_std", "p.RE_8_std", "p.MA_8_std")
standardized_scores <- TRUE

# Pending to adapt to new 01_prepare_data which is made taking into account standardized_scores
# Could just add standardized scores as a command argument
# Or make it more robust as it is now capturing missingness indicators as well
#   # Try to find all names for score variables
# score_vars <- grep("p\\.", colnames(dt), value = TRUE) 
# 
#   # Based on the range and mean, automatically determine whether we are in the standardized or
#   # raw scores case
#   # Range is within -4.5 and +4.5
# range_check <- all({sapply(dt[, .SD, .SDcols = score_vars], function(x) range(x, na.rm = TRUE)) |> abs()} < 4.5)
#   # Mean is within -1 and 1
# mean_check <-  all({sapply(dt[, .SD, .SDcols = score_vars], function(x) mean(x, na.rm = TRUE)) |> abs()} < 1L)
# 
# stopifnot("There is ambiguous information to whether the score variables are standardized or raw scores.
#           Be sure that `score_vars` only includes column names for national test scores." = range_check == mean_check)
# 
# standardized_scores <- ifelse(range_check & mean_check == TRUE, TRUE, FALSE)

# Since they won't be used in analysis, 
# we remove some variables
dt[, initiated_01 := NULL]
dt[, s.id5 := NULL]
dt[, s.id8 := NULL]

# Vectors of score variable names
pretest_names <- c("p.EN_5"
                   , "p.MA_5"
                   , "p.RE_5")

posttest_names <- c("p.EN_8"
                    , "p.MA_8"
                    , "p.RE_8")

# Change variable names in case we are dealing with standardized scores
if (standardized_scores == TRUE) {
  pretest_names  <- paste0(pretest_names, "_std")
  posttest_names <- paste0(posttest_names, "_std")
}

test_names <- c(pretest_names, posttest_names)

s_avg_names <- grep("s\\.avg",
                    colnames(dt),
                    value = TRUE)

# Vector of variables names to log-transform for numerical stability
log_vars <- c(grep("nvisits",
                   colnames(dt),
                   value = TRUE),
              grep("pre6",
                   colnames(dt),
                   value = TRUE),
              grep("days",
                   colnames(dt),
                   value = TRUE) # These are also visits but on a different naming format
) |> unique()

print(log_vars)

# Vector of numeric vars
numeric_vars <- sapply(dt, is.numeric) |> which() |>  names()
# Remove vars to log transform and others from this vector
numeric_vars <- setdiff(numeric_vars, c(log_vars
                                        , "id"
                                        , "m_age_std"
                                        # , "initiated_01"
                                        ))

# Do not re-standardize if using standardized scores, else we lose interpretability
  # (and the numerical stability gain is already there)
if (standardized_scores == TRUE) {
  numeric_vars <- setdiff(numeric_vars, test_names)
}

# Standardize all variables in `numeric_vars`
dt[, (numeric_vars) := lapply(.SD,
                              scale), 
   .SDcols = c(numeric_vars)]
   
# To get rid of means way too small due (e-16) to floating point inaccuracies
dt[, (numeric_vars) := lapply(.SD,
                              function(x) round(x, digits = 7)), 
   .SDcols = c(numeric_vars)]

# Log-transform all variables in `log_vars`
dt[, (log_vars) := lapply(.SD,
                          function(x) log(1 + x)),
   .SDcols = c(log_vars)]

# Dots are not allowed in variable names in Stan
colnames(dt) <- colnames(dt) |> strip()

# 2.1. Typing -------------------------------------------------------------


# 3. Partition predictor variables of different models --------------------

# We will partition all our predictor variables into four groups
# for different corresponding models following different
# restrictions on temporality and missingness:
# 1. Imputation of pretest with variables at grade 5 or before
# 2. Imputation of posttest with future variables (between grade 6 and 8)
# 3. Outcome model for posttest with baseline variables (at grade 6 or before)
# 4. Imputed pretests and posttests used in imputation of a posttest

# Objects starting with `eta_` are the string of linear predictor terms
# for that respective subset of variables

# :: 3.1 Pretest imputation model covariates ---------------------------------
# Pre-grade 5 predictors (other test scores from grade 5 itself should be fine)
pretest_imputation_before5_vars <-  c(
  exempt_pretest_names,
  "cs.5.m", "cs.5.f", # Civil status of mother and father (grade 5)
  "s.avg_5_RE",
  "s.avg_5_MA",
  "sex",                           # Child variables: sex,
  "Preglength", "birthweight",     # Gestational age in weeks, Weight in grams
  "byear", "bmonth",               # Birth year, birth month
  "m.edu", "m.age",
  "parity"
  #, "s.id5" # Will be included as random effect
) |> strip()


# # Add random intercept
#   # Store this in a temporal files so _vars vectors still contain variable
#   # names rather than regression terms
# tmp_before5 <- gsub(pattern = "^(.*sid.*)$", # Find element containing "s.id"
#                                         replacement = "(1 | \\1)", # Enclose as random intercept
#                                         pretest_imputation_before5_vars)

tmp_before5 <- pretest_imputation_before5_vars

# Collapse into a single string
eta_before5 <- paste(tmp_before5,
                     collapse = " + ")
# Prints: [1] "cs5m + cs5f + savg5RE + savg5MA + sex + Preglength + birthweight + byear + bmonth + agedate5 + medu + magestd + parity + (1 | sid5)"

# Remove intermediate temporal vector
rm(tmp_before5)

# :: 3.2 Posttest imputation model covariates without missing -----------------
# Future variables, grade 6 and later

# Filter all variables that include 6to8 in their name
posttest_imputation_future_vars <- c(
  exempt_posttest_names,
  "meddays8to9",
  grep("6to8",
       colnames(dt), # this was unique(imputation_variables)
       value = TRUE)
  # ,  "s.id8" # For random intercept for school at grade 8
) |> strip()

# # Add random intercept
# tmp_future  <- gsub(pattern = "^(.*sid.*)$", # Find element containing "s.id"
#                                         replacement = "(1 | \\1)", # Enclose as random intercept
#                                         posttest_imputation_future_vars)
tmp_future <- posttest_imputation_future_vars

# Collapse into single string
eta_future6to8 <- paste(tmp_future,
                        collapse = " + ")
                        
# Remove intermediate temporal vector
rm(tmp_future)

# :: 3.3 Posttest outcome model covariates -----------------------------------
# Baseline covariates that will be listwise deleted

# Those that are neither test scores nor any of the above
posttest_outcome_5to6_vars <- setdiff(colnames(dt), 
                                       c(pretest_imputation_before5_vars, 
                                         posttest_imputation_future_vars,
                                         strip(test_names),
                                         "id",
                                         "initiated") # These are the fourth group
                                       ) |> strip()

# Collapse into a single string
eta_5to6 <- paste(posttest_outcome_5to6_vars,
                      collapse = " + ")
                      
#Prints: "nchildrenm + cs6f + cs6m + pre6P81father + pre6P81mother + pre6allfather +
#pre6allmother + pre6extfather + pre6extmother + pre6intfather + pre6intmother +
#nsibswdiag + pre6nvisitsP06 + pre6nvisitsP24 + pre6nvisitsall + pre6nvisitsext +
#pre6nvisitsint + savg8EN + savg8RE + savg8MA + meddaysp6 + meddays8to9 +
#proppostexemptions + ADHDf + ADHDm + P81sany + initiated + medicatedpt0"

# Listwise deletion for variables that will not be imputed
# Confirm that score variables 'pretest_names' and 'posttest_names' are in dt
imputed_cols <- test_names |> strip()
print(all(imputed_cols %in% colnames(dt))) # Should be TRUE

# Get the columns we want to listwise delete
complete_cols <- setdiff(colnames(dt), imputed_cols) # i.e. variables that are not going to be imputed
# Delete missing rows
dt <- dt[complete.cases(dt[, ..complete_cols]), ]

# :: 3.4 Posttest imputation model missing covariates -------------------------
# Enclose pretests and other posttest scores in mi( )
posttest_imputation_missing_vars <- enclose_mi(imputed_cols) |> unname()

# This is added by hand in the end, so not needed
# # Collapse into single string
# eta_mi_outcome <- paste(posttest_imputation_missing_vars, collapse = " + ")

# 4. Programatically define bform for brms formula -----------------------------

# Linear predictor for missing pretests. This goes in outcome model as they are baseline terms
eta_mi_pretests <- pretest_names |> strip() |> enclose_mi() |> paste(collapse = " + ")
  # This outputs: [1] "mi(pEN5) + mi(pMA5) + mi(pRE5)"

# Define bform

bform <- 
  # Numeracy posttest
  bf(glue::glue("{strip(test_names) |> select_domain(domain = 'MA') |> select_number(number = 8)} | mi() ~ initiated +",
                "{eta_before5} +",
                "{eta_5to6} +",
                "{eta_mi_pretests}
              ")) +
  # English posttest
  bf(glue::glue("{strip(test_names) |> select_domain(domain = 'EN') |> select_number(number = 8)} | mi() ~ initiated +",
                "{eta_before5} +",
                "{eta_5to6} +",
                "{eta_mi_pretests}
              ")) +
  # Reading posttest
  bf(glue::glue("{strip(test_names) |> select_domain(domain = 'RE') |> select_number(number = 8)} | mi() ~ initiated +",
                "{eta_before5} +",
                "{eta_5to6} +",
                "{eta_mi_pretests}
              ")) +
  # Numeracy pretest
  bf(glue::glue("{strip(test_names) |> select_domain(domain = 'MA') |> select_number(number = 5)} | mi() ~ {eta_before5} +
              {build_eta_mi_domain(domain = 'MA', grade = 5)}")) +
  # English pretest
  bf(glue::glue("{strip(test_names) |> select_domain(domain = 'EN') |> select_number(number = 5)} | mi() ~ {eta_before5} +
              {build_eta_mi_domain(domain = 'EN', grade = 5)}")) +
  # Reading pretest
  bf(glue::glue("{strip(test_names) |> select_domain(domain = 'RE') |> select_number(number = 5)} | mi() ~ {eta_before5} +
              {build_eta_mi_domain(domain = 'RE', grade = 5)}"))

# 5. Priors for regression parameters ----------------------------------------

# Integer of how many response variables
num_responses <- bform$forms |> length()

# Vectorized one-liner alternative for setting multivariate priors
my_prior <- brms::set_prior("normal(0,2)",
                            class = rep(c("b", "Intercept", "sigma"),
                                        each = num_responses),
                            resp = bform$forms |> names())

# 5. Priors for imputed variables ---------------------------------------------
imputed_values_names <- c("Ymi_p.EN_5", "Ymi_p.MA_5",
                          "Ymi_p.RE_5", "Ymi_p.EN_8",
                          "Ymi_p.MA_8", "Ymi_p.RE_8") |> gsub("(?<!mi)[._](?!_p)",
                                                              "",
                                                              perl = TRUE,
                                                              x = _)
  # This outputs: "Ymi_pEN5" "Ymi_pMA5" "Ymi_pRE5" "Ymi_pEN8" "Ymi_pMA8" "Ymi_pRE8"

if (standardized_scores == TRUE) {
  imputed_values_names <- paste0(imputed_values_names, "std")
}

# Vectorizes automatically priors for missing value vectors
imputed_priors <- stanvar(scode = glue::glue(
  "lprior += normal_lpdf({imputed_values_names}| 0,3);"),
  block = "tparameters")

# 6. Additional imputation with future variables -------------------------------
outcome <- strip(test_names) |> select_domain(domain = 'MA') |> select_number(number = 8)

# This is only to get the length of missing_terms for parameter definition in Stan
# Extract predictors terms inside `mi()`

# Regular expression pattern to match 'mi(...)'
pattern <- "mi\\([^)]*8[^)]*\\)" # Note this is for grade 8

# The string noting the formula of predictors for the corresponding `outcome`. 
  # We assume all 3 outcomes have the same number of predictors
bform_outcome_formula_chr <- bform$forms[[outcome]]$formula[3] |> as.character()
# Store them in a character vector
  # These are terms enclosed in "mi( . . .)"
non_baseline_missing_terms <- regmatches(bform_outcome_formula_chr,
                                         gregexpr(pattern,
                                                  bform_outcome_formula_chr,
                                                  perl = TRUE))[[1]]


# Make design matrix that includes:
  #  future variables to be used in imputation plus
  #  the other covariates before grade 5 and from 5 to 6
# Make a string of a formula with only linear predictors of those variables

combined_5to8_vars <- c(posttest_imputation_future_vars,
                        pretest_imputation_before5_vars,
                        posttest_outcome_5to6_vars) |> unique()

formula_str <- paste(combined_5to8_vars,
                     collapse = " + ")

# Not needed because the random effect enclosings were stored in temporal objects
# # Remove the (1 |  ) enclosing for random effects
# formula_str <- gsub("\\(1\\| (.*?)\\)", "\\1", formula_str)

# Make a formula object excluding the intercept
formula_obj <- as.formula(paste0("~ 0 + ", formula_str))

# Make model matrix for non missing predictors to be used in special imputation of missing values
X_Yi <- model.matrix(formula_obj,
                     data = dt)

# Stan code defining parameters
parameters.impute.y = glue::glue("
  real b_pMA8i_Intercept;        // intercept
  real b_pRE8i_Intercept;        // intercept
  real b_pEN8i_Intercept;        // intercept
  vector[{length(posttest_imputation_missing_vars) - 1L}] bsp_pMA8i;    // regression coefficient for vars with missings
  vector[{length(posttest_imputation_missing_vars) - 1L}] bsp_pRE8i;    
  vector[{length(posttest_imputation_missing_vars) - 1L}] bsp_pEN8i;    
  vector[{dim(X_Yi)[2]}] bpMA8i;   // regression coefficient for vars without missings
  vector[{dim(X_Yi)[2]}] bpRE8i;   // regression coefficient for vars without missings
  vector[{dim(X_Yi)[2]}] bpEN8i;   // regression coefficient for vars without missings
  real<lower=0> sigma_pMA8i;     // error variance
  real<lower=0> sigma_pRE8i;     // error variance
  real<lower=0> sigma_pEN8i;     // error variance
")

# These are just priors for respective quantities in `parameters.impute.y`
# Note they are automatically vectorized?
priors.impute.y = "
  lprior += normal_lpdf(b_pMA8i_Intercept | 0,1);
  lprior += normal_lpdf(b_pRE8i_Intercept | 0,1);
  lprior += normal_lpdf(b_pEN8i_Intercept | 0,1);
  lprior += normal_lpdf(bpMA8i | 0,1);
  lprior += normal_lpdf(bpRE8i | 0,1);
  lprior += normal_lpdf(bpEN8i | 0,1);
  lprior += normal_lpdf(bsp_pMA8i | 0,1);
  lprior += normal_lpdf(bsp_pRE8i | 0,1);
  lprior += normal_lpdf(bsp_pEN8i | 0,1);
  lprior += normal_lpdf(sigma_pMA8i | 0,1) - 1 * normal_lccdf(0 | 0,1);
  lprior += normal_lpdf(sigma_pRE8i | 0,1) - 1 * normal_lccdf(0 | 0,1);
  lprior += normal_lpdf(sigma_pEN8i | 0,1) - 1 * normal_lccdf(0 | 0,1);
"

# finally we add the code for the imputation model
# the general idea is as follows:
# 1) We specify an intercept mu_Yi
# 2) We add the effects of variables with missing values to the intercept mu_Yi
#    In this examples these are 3 variables
# 3) we use normal_id_glm_lpdf to calculate the likelihood
#    This function is computationally efficient and takes as input
#    the intercept mu_Yi which was just generated. For vars without missing: 
#    the design matrix X_Yi and regression coefficients bYi 
#    and the error variance sigma_Yi


# Pending to define programatically?
if (standardized_scores == TRUE) {
  code.impute.y <- 
    glue::glue("
  // ADDITIONAL IMPUTATION OF OUTCOME
  // vector for mu and effects of variables with missing values
  vector[N_{{strip(test_names) |> select_domain(domain = 'MA') |> select_number(number = 8)}}] mu_pMA8i = rep_vector(0.0, N_{{strip(test_names) |> select_domain(domain = 'MA') |> select_number(number = 8)}});
  vector[N_{{strip(test_names) |> select_domain(domain = 'EN') |> select_number(number = 8)}}] mu_pEN8i = rep_vector(0.0, N_{{strip(test_names) |> select_domain(domain = 'EN') |> select_number(number = 8)}});
  vector[N_{{strip(test_names) |> select_domain(domain = 'RE') |> select_number(number = 8)}}] mu_pRE8i = rep_vector(0.0, N_{{strip(test_names) |> select_domain(domain = 'RE') |> select_number(number = 8)}});
  mu_pMA8i += b_pMA8i_Intercept;
  mu_pEN8i += b_pRE8i_Intercept;
  mu_pRE8i += b_pEN8i_Intercept;
  for (n in 1:N_{{strip(test_names) |> select_domain(domain = 'MA') |> select_number(number = 8)}}) {
   mu_pMA8i[n] += (bsp_pMA8i[1]) * Yl_pEN8std[n] + (bsp_pMA8i[2]) * Yl_pRE8std[n] +
bsp_pMA8i[3] * Yl_pEN5std[n] + bsp_pMA8i[4] * Yl_pRE5std[n] + bsp_pMA8i[5] * Yl_pEN5std[n];;
}
  for (n in 1:N_{{strip(test_names) |> select_domain(domain = 'EN') |> select_number(number = 8)}}) {
   mu_pEN8i[n] += (bsp_pEN8i[1]) * Yl_pMA8std[n] + (bsp_pEN8i[2]) * Yl_pRE8std[n] +
bsp_pEN8i[3] * Yl_pEN5std[n] + bsp_pEN8i[4] * Yl_pRE5std[n] + bsp_pEN8i[5] * Yl_pEN5std[n];;
}
  for (n in 1:N_{{strip(test_names) |> select_domain(domain = 'RE') |> select_number(number = 8)}}) {
   mu_pRE8i[n] += (bsp_pRE8i[1]) * Yl_pEN8std[n] + (bsp_pRE8i[2]) * Yl_pMA8std[n] +
bsp_pRE8i[3] * Yl_pEN5std[n] + bsp_pRE8i[4] * Yl_pRE5std[n] + bsp_pRE8i[5] * Yl_pEN5std[n];;
  }
  // likelihood
  target += normal_id_glm_lpdf(Yl_pMA8std | X_Yi, mu_pMA8i, bpMA8i, sigma_pMA8i);
  target += normal_id_glm_lpdf(Yl_pRE8std | X_Yi, mu_pRE8i, bpRE8i, sigma_pRE8i);
  target += normal_id_glm_lpdf(Yl_pEN8std | X_Yi, mu_pEN8i, bpEN8i, sigma_pEN8i);
", .open = "{{", .close = "}}" # Else it conflicts with the curly braces in the loop
    )
  
}
if (standardized_scores == FALSE) {
  code.impute.y <- 
    glue::glue("
  // ADDITIONAL IMPUTATION OF OUTCOME
  // vector for mu and effects of variables with missing values
  vector[N_{{strip(test_names) |> select_domain(domain = 'MA') |> select_number(number = 8)}}] mu_pMA8i = rep_vector(0.0, N_{{strip(test_names) |> select_domain(domain = 'MA') |> select_number(number = 8)}});
  vector[N_{{strip(test_names) |> select_domain(domain = 'EN') |> select_number(number = 8)}}] mu_pEN8i = rep_vector(0.0, N_{{strip(test_names) |> select_domain(domain = 'EN') |> select_number(number = 8)}});
  vector[N_{{strip(test_names) |> select_domain(domain = 'RE') |> select_number(number = 8)}}] mu_pRE8i = rep_vector(0.0, N_{{strip(test_names) |> select_domain(domain = 'RE') |> select_number(number = 8)}});
  mu_pMA8i += b_pMA8i_Intercept;
  mu_pEN8i += b_pRE8i_Intercept;
  mu_pRE8i += b_pEN8i_Intercept;
  for (n in 1:N_{{strip(test_names) |> select_domain(domain = 'MA') |> select_number(number = 8)}}) {
   mu_pMA8i[n] += (bsp_pMA8i[1]) * Yl_pEN8[n] + (bsp_pMA8i[2]) * Yl_pRE8[n] +
bsp_pMA8i[3] * Yl_pEN5[n] + bsp_pMA8i[4] * Yl_pRE5[n] + bsp_pMA8i[5] * Yl_pEN5[n];;
}
  for (n in 1:N_{{strip(test_names) |> select_domain(domain = 'EN') |> select_number(number = 8)}}) {
   mu_pEN8i[n] += (bsp_pEN8i[1]) * Yl_pMA8[n] + (bsp_pEN8i[2]) * Yl_pRE8[n] +
bsp_pEN8i[3] * Yl_pEN5[n] + bsp_pEN8i[4] * Yl_pRE5[n] + bsp_pEN8i[5] * Yl_pEN5[n];;
}
  for (n in 1:N_{{strip(test_names) |> select_domain(domain = 'RE') |> select_number(number = 8)}}) {
   mu_pRE8i[n] += (bsp_pRE8i[1]) * Yl_pEN8[n] + (bsp_pRE8i[2]) * Yl_pMA8[n] +
bsp_pRE8i[3] * Yl_pEN5[n] + bsp_pRE8i[4] * Yl_pRE5[n] + bsp_pRE8i[5] * Yl_pEN5[n];;
  }
  // likelihood
  target += normal_id_glm_lpdf(Yl_pMA8 | X_Yi, mu_pMA8i, bpMA8i, sigma_pMA8i);
  target += normal_id_glm_lpdf(Yl_pRE8 | X_Yi, mu_pRE8i, bpRE8i, sigma_pRE8i);
  target += normal_id_glm_lpdf(Yl_pEN8 | X_Yi, mu_pEN8i, bpEN8i, sigma_pEN8i);
", .open = "{{", .close = "}}" # Else it conflicts with the curly braces in the loop
    )
  
}



# Assign all Stan code to respective blocks
stanvars <-
  imputed_priors + 
  stanvar(x = X_Yi, name = "X_Yi", block = "data") + 
  stanvar(scode = parameters.impute.y, block = "parameters") +
  stanvar(scode = priors.impute.y, block = "tparameters") +
  stanvar(scode = code.impute.y, block = "likelihood", position = "end")

# 7. Generate Stan model ---------------------------------------------------
# For diagnostic purposes

    # data format
stan_data <- make_standata(
 bform,
 dt,
 prior = my_prior,
 stanvars = stanvars
)

# Stan code
make_stancode(
  bform,
  dt,
  prior = my_prior,
  stanvars = stanvars
) |>
  cat(file = filename_stan_model)

mod <- cmdstanr::cmdstan_model(filename_stan_model)


# 8. Fit Pathfinder -------------------------------------------------------

# Pathfinder fast fit for diagnostic purposes
fit_pf <- mod$pathfinder(data = stan_data)

saveRDS(fit_pf, file = "last_fit_pathfinder_colossus.RDS")


# # 9. Fit Stan model ---------------------------------------------------------
fit_mcmc = 
  brm(bform,
      data = dt,
      iter = n_iter,
      chains = num_chains,
      cores = getOption("mc.cores"),
      backend = "cmdstanr",
      prior = my_prior,
      stanvars = stanvars,
      silent = 0
  )


# # Store end time and print elapsed time
end_time <- Sys.time()
end_time - start_time

# # Store fit object together with elapsed time
bfit_list <- list("brm_fit" = fit_mcmc,
                  "total_time" = end_time - start_time)

# Save object including runtime
saveRDS(bfit_list,
        file = here::here(glue::glue("bfit_colossus_last_definite.RDS")))

# 9. Sample processing with posterior package -----------------------------

# bfit <- bfit_colossus$brm_fit

# Select any variable that contains one of the variables in the
# character vector `response_variables` but omit 
regex_pattern <- paste0("(?!.*mi)(", paste(c(pretest_names, posttest_names) |> strip(), collapse = "|"), ")")

draws_bfit <- brms::as_draws_df(bfit,
                                variables = response_variables,
                                inc_warmup = FALSE)

draws_bfit <- data.table::as.data.table(draws_bfit)

# Subset columns to remove samples from imputed values
# Then subset only to those that include response variables
# Lastly only to the coefficients of the exposure
colnames_subset <- colnames(draws_bfit) |>
  grep("mi", x = _, invert = TRUE, value = TRUE) |> 
  grep(paste(c(pretest_names, posttest_names) |> strip(), collapse = "|"),
       x = _, value = TRUE) |>
  grep("initiated", x = _, value = TRUE)


# Summary of posterior samples for the effect of initiation
summary_dt <- draws_bfit[, .(mean = sapply(.SD, mean),
                             sd = sapply(.SD, sd)), .SDcols = colnames_subset][
                               , domain := c("MA", "EN", "RE")][
                                 , `:=`(lower = mean - 1.96*sd,
                                        upper = mean + 1.96*sd)]
summary_dt[]



# Confirm if there were any divergent transitions
n_divergent <- function(x) {
  stopifnot(is(x, "brmsfit"))
  out <- lapply(x$fit@sim$samples, function(y) 
    sum(attr(y, "sampler_params")[["divergent__"]]))
  sum(unlist(out))
}



# bfit_colossus from 24/01/2024
# is skimmed version (i.e. wrong because of no redundancy in added model by hand)
# also was missing the before 5 variables in the outcome model, only the 5to6
# (ex- "baseline") were included