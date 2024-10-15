library(ggplot2)

# Run 00 and 01 to load and prepare data before hand

# We will only work with standardized posttests
if (standardized_scores == TRUE) {
  posttest_names <- posttest_names_std
}

# Pre-allocate data.table for plotting
effect_summaries <- list("Mean_diff" = vector(mode = "numeric"),
                         "Lower"     = vector(mode = "numeric"),
                         "Upper"     = vector(mode = "numeric"),
                         "Domain"    = factor(NA_character_,
                                              levels = c("MA", "RE", "EN")),
                         "Subset" = factor(NA_integer_,
                                           levels = c("Complete", "Imputed")),
                         "Model" = vector(mode = "character")) |>
  data.table::as.data.table()
effect_summaries <- effect_summaries[-1,] 

# Create linear predictor without random effect school ids at 5th and 8th grade
eta_X <- paste(setdiff(outcome_covariates, c("s.id5", "s.id8")), collapse = " + ")

# formula_str <- NULL
# # Partition outcome_covariates
# 
# factor_vars <- lapply(dt_completecase[, mget(setdiff(outcome_covariates,
#                                                      "initiated_01"))],
#                       function(x) is.factor(x)) |> unlist() |>
#   which() |> names()
# num_vars    <- lapply(dt_completecase[, mget(setdiff(outcome_covariates,
#                                                      "initiated_01"))],
#                       function(x) is.numeric(x)) |> unlist() |>
#   which() |> names()
# 
# # Generate design matrix for linear terms
# #   # Add the factor terms
# formula_str <- paste(factor_vars[5:9], collapse = " + ")
#   # Add linear terms for numeric variables
# formula_str <- paste(formula_str,
#                      paste(num_vars, collapse = " + "),
#                      sep = " + ")
# 
# # # OPTIONAL add polynomials
# # k = 2
# # poly_terms <- sapply(num_vars, function(x) {
# #   paste0("I(", x, "^", 2:k, ")")
# # })
# # formula_str <- paste(formula_str, paste(unlist(poly_terms),
# #                                         collapse=" + "),
# #                      sep=" + ")
# 
# 
# # Convert the formula string to formula object
# # formula_obj <- as.formula(paste("~ -1 + ", formula_str))
#   # It's easier to remove the intercept later so that k-1 dummies are made
# formula_obj <- as.formula(paste("~ ", formula_str))
# # covariate_design_mat <- model.matrix(formula_obj, data = dt_completecase_Y)
# 
# 
# # list()
# # Create the design matrix
# # covariate_design_mat <- model.matrix(formula_obj, data = dt_completecase_Y)
# 
# 


# Crude difference --------------------------------------------------------

 # Function to return a list with observed means, mean diference,
 # and t.test confidence interval and df                                       
crude_difference <- function(dt,
                                     posttest){
  treated_mean <- dt[initiated == "Initiated"][[posttest]] |> na.omit() |> mean() 
  control_mean <- dt[initiated == "Control"][[posttest]]   |> na.omit() |> mean()
  associational_difference <- treated_mean - control_mean
  tst <- t.test(dt[initiated == "Initiated"][[posttest]],
         dt[initiated == "Control"][[posttest]])
  
  return(list("treated_mean" = treated_mean,
              "control_mean" = control_mean,
              "observed_difference" = associational_difference,
              "lower" = tst$conf.int[1],
              "upper" = tst$conf.int[2],
              "df"    = tst$parameter)
         )
}

crude_difference(dt = dt_completecase, posttest = posttest_names[[1]])
# $observed_difference
# [1] 0.008219983
crude_difference(dt = dt_completecase, posttest = posttest_names[[2]])
# $observed_difference
# [1] -0.01415213
crude_difference(dt = dt_completecase, posttest = posttest_names[[3]])
# $observed_difference
# [1] 0.01437826
# We see that differences are quite smaller than the adjusted analysis already
# we presume the adjustment set estimate is more trustable
# as it confirms findings in the literature  
  

# Extract summary values for differentmodels
  # Crude differences
for (outcome in posttest_names) {
  tmp <- crude_difference(dt = dt_completecase, posttest = outcome)
  # Columns that are constant across the loop
  sublist_tmp <- list( "Subset" = "Complete Case",
                       "Model" = "Unadjusted"
  )
  
  tmp_list <- append(list("Mean_diff" = tmp$observed_difference,
                          "Lower"     = tmp$lower,
                          "Upper"     = tmp$upper,
                          "Domain"    = identify_domain(outcome)),
                sublist_tmp)
  
  effect_summaries <- rbind.data.frame(effect_summaries, as.data.frame(tmp_list)) |>  as.data.table()
}


# g-formula with BART -----------------------------------------------------


# :: For complete case (of covariates and outcome) ------------------------

if (!file.exists(here::here("output", "objects", "effect_summaries","bart_complete_caseEN.RDS")) |
    !file.exists(here::here("output", "objects", "effect_summaries","bart_complete_caseMA.RDS")) |
    !file.exists(here::here("output", "objects", "effect_summaries","bart_complete_caseRE.RDS"))) {
  
  source(here::here("scripts", "functions",
                    "fit_BART_wrapper.R"))
  
  treatment <- "initiated"
  
  outcome_covariates <- c(treatment, outcome_covariates)
  
  for (domain in c("MA", "EN", "RE")) {
    # for (domain in c("RE")) {
    outcome <- grep(pattern = domain, posttest_names, value = TRUE)
    
    fit_BART(outcome_string     = outcome,
             treatment_string   = treatment ,
             covariates_vector  = outcome_covariates,
             grouping_variables = c("s.id5", "s.id8"),
             input_data         = dt_completecase[!is.na(get(outcome))],
             estimands          = c("SATE"), 
             imputation_prefix  = "m",
             # Sampler specifics
             cores              = parallelly::availableCores(),
             total_iterations   = 4000L,
             warmup_proportion  = 0.25,
             num_chains         = parallelly::availableCores(),
             # Output and saving options
             output_samples     = TRUE,
             output_summaries   = TRUE,
             save_output        = TRUE,
             file_name          = glue::glue("/bart_complete_case{domain}.RDS"),
             file_path          = here::here("output", "objects", "effect_summaries"),
             seed               = 1234, #((100000 * batch) + i),
             random_slopes      = TRUE,
             include_U_linear   = FALSE
    )
  }
  
  
} else {
warning("Not fitting BART on complete case subset, as previously fitted output is available in the here folder")
}
# Load the summaries of each domain
samples_cc_EN <- readRDS(file = here::here("output", "objects", "effect_summaries", 
                                           "bart_complete_caseEN.RDS"))$summaries$SATE
samples_cc_MA <- readRDS(file = here::here("output", "objects", "effect_summaries", 
                                           "bart_complete_caseMA.RDS"))$summaries$SATE
samples_cc_RE <- readRDS(file = here::here("output", "objects", "effect_summaries", 
                                           "bart_complete_caseRE.RDS"))$summaries$SATE
# Store them in a list
summaries_cc_list <- list("EN" = samples_cc_EN,
                          "RE" = samples_cc_RE,
                          "MA" = samples_cc_MA)

# Compute standard error of the mean
for (domain in c("EN", "RE", "MA")) {
  summaries_cc_list[[domain]]$std_error <- summaries_cc_list[[domain]]$sd / sqrt(summaries_cc_list[[domain]]$N)
}

# Set the subset and model for these estimates
sublist_tmp <- list("Subset" = "Complete Case",
                    "Model" = "g-formula w/ BART")

# For each domain, extract and store mean estimate, interval bounds and domain
for (domain in c("MA", "EN", "RE")) {
  # Extract and store the mean and confidence interval bounds for each domain
  tmp_list <- append(list("Mean_diff" = summaries_cc_list[[domain]]$mean,
                          "Lower" = summaries_cc_list[[domain]]$mean -
                            1.96*summaries_cc_list[[domain]]$std_error,
                          "Upper" = summaries_cc_list[[domain]]$mean +
                            1.96*summaries_cc_list[[domain]]$std_error,
                          "Domain" = domain),
                     sublist_tmp)
  # Add them to our effect summary data.table
  effect_summaries <- rbind.data.frame(effect_summaries,
                                       as.data.frame(tmp_list)) |>
    as.data.table()
}

 
# :: For analytic sample (Multiple Imputation)------------------------------------------------

# Load summarized posterior samples object
tmp <- readRDS(here::here("output",
                          "objects",
                          "bart_analysis",
                          "summarized_posteriors.RDS"))

    # Specify Model and subset
sublist_tmp <- list( "Subset" = "Multiple Imputation - Analytic",
                     "Model" = "g-formula w/ BART")

    # Extract and store the mean and confidence interval bounds for each domain
for (domain in c("EN", "RE", "MA")) {
  tmp_list <- append(list("Mean_diff" = tmp$analytic[Estimand == "SATE" &
                                                              Domain == domain]$mean,
                          "Lower"     = tmp$analytic[Estimand == "SATE" &
                                                              Domain == domain]$lower,
                          "Upper"     = tmp$analytic[Estimand == "SATE" &
                                                              Domain == domain]$upper,
                          "Domain"    = domain),
                     sublist_tmp)
    # Add them to our effect summary data.table
  effect_summaries <- rbind.data.frame(effect_summaries,
                                       as.data.frame(tmp_list)) |>
    as.data.table()
}

# :: For washout sample (Multiple Imputation)------------------------------------------------

    # Specify Model and subset
sublist_tmp <- list( "Subset" = "Multiple Imputation - Washout",
                     "Model" = "g-formula w/ BART")
    # Extract and store the mean and confidence interval for each domain
for (domain in c("EN", "RE", "MA")) {
  tmp_list <- append(list("Mean_diff" = tmp$washout[Estimand == "SATE" &
                                                              Domain == domain]$mean,
                          "Lower"     = tmp$washout[Estimand == "SATE" &
                                                              Domain == domain]$lower,
                          "Upper"     = tmp$washout[Estimand == "SATE" &
                                                              Domain == domain]$upper,
                          "Domain"    = domain),
                     sublist_tmp)
    # Add them to our effect summary data.table
  effect_summaries <- rbind.data.frame(effect_summaries,
                                       as.data.frame(tmp_list)) |>
    as.data.table()
}

 

# Complete case simple linear model -----------------------------------------------------


# :: Simple linear model --------------------------------------------------

sublist_tmp <- list("Subset" = "Complete Case",
                    "Model" = "Adjusted Linear Model")


# This still adjusts only for baseline covariates

for (posttest in posttest_names) {
  # Extract domain
  domain <- identify_domain(posttest)
  
  # Exclude missing posttest observations
  dt_completecase_Y <- dt_completecase[!is.na(get(posttest))]
  
  # Create formula
  lm_formula_object <- as.formula(paste0(glue::glue("{posttest} ~ initiated + {eta_X}")))
  
  # Fit
  tmp_fit <- lm(lm_formula_object,
     data = dt_completecase_Y)
  
  # Store in list
  tmp_list <- append(list(  "Mean_diff" = tmp_fit$coefficients[["initiatedInitiated"]],
                            "Lower"     = confint(tmp_fit, "initiatedInitiated")[1],
                            "Upper"     = confint(tmp_fit, "initiatedInitiated")[2],
                            "Domain"    = domain),
                     sublist_tmp)
  # Append
  effect_summaries <- rbind.data.frame(effect_summaries,
                                       as.data.frame(tmp_list)) |>
    as.data.table()
}


# :: Negative control exposure --------------------------------------------

  sublist_tmp <- list("Subset" = "Complete Case",
                      "Model" = "NCE + Adjusted Linear Model")
  
  
  # This still adjusts only for baseline covariates
  
  for (posttest in posttest_names) {
    # Extract domain
    domain <- identify_domain(posttest)
    
    # Exclude missing posttest observations
    dt_completecase_Y <- dt_completecase[!is.na(get(posttest))]
    # Create formula
    lm_formula_object <- as.formula(paste0(glue::glue("{posttest} ~ NCE + initiated + {eta_X}")))
    # Fit
    tmp_fit <- lm(lm_formula_object,
                  data = dt_completecase_Y)
    
    # Store in list
    tmp_list <- append(list(  "Mean_diff" = tmp_fit$coefficients[["NCE"]],
                              "Lower"     = confint(tmp_fit, "NCE")[1],
                              "Upper"     = confint(tmp_fit, "NCE")[2],
                              "Domain"    = domain),
                       sublist_tmp)
    # Append list
    effect_summaries <- rbind.data.frame(effect_summaries,
                                         as.data.frame(tmp_list)) |>
      as.data.table()
  }

# :: Bayesian joint imputation -----------------------------------------------
  
  # Load output from the full joint model
  bfit_list <- readRDS(file = here::here("output",
                                         "objects",
                                         "bayesian_joint",
                                         "bfit_colossus_definite.RDS"))


bfit_coef <- bfit_list$brm_fit |> brms::fixef()

# Select only treatment coefficients for each outcome
A_indices <- rownames(bfit_coef) |> grep(pattern = "initiated")
bfit_coef <- bfit_coef[A_indices,]
rm(A_indices)

# Define the sample scope and analytic model
sublist_tmp <- list("Subset" = "BI Pretests and Outcome",
                    "Model" = "Adjusted Linear Model")


for (posttest in posttest_names) {
  # Extract domain
  domain <- identify_domain(posttest)
  # Identify corresponding row for that domain
  i <- grep(rownames(bfit_coef), pattern = domain)
  
  # Extract and store the mean and confidence interval bounds for each domain
  tmp_list <- append(list(  "Mean_diff" = bfit_coef[i,][["Estimate"]],
                            "Lower"     = bfit_coef[i,][["Q2.5"]],
                            "Upper"     = bfit_coef[i,][["Q97.5"]],
                            "Domain"    = domain),
                     sublist_tmp)
  
  # Add them to our effect_summaries data.table
  effect_summaries <- rbind.data.frame(effect_summaries,
                                       as.data.frame(tmp_list)) |>
    as.data.table()
}


# Proximal causal inference -----------------------------------------------

# Load output from the bootstrap
p2sls_fit <- readRDS(file = here::here("output",
                                       "objects",
                                       "P2SLS",
                                       "bootstraps_P2SLS.RDS"))

p2sls_ci <- readRDS(file = here::here("output",
                                      "objects",
                                      "P2SLS",
                                      "bootstraps_ci_bca.RDS"))

names(p2sls_ci) <- c("EN", "RE", "MA")

# Define the sample scope and analytic model
sublist_tmp <- list("Subset" = "Complete Case",
                    "Model" = "P2SLS")

# tmp_ci <- list("EN" = NA, "RE" = NA, "MA" = NA)

# for (posttest in posttest_names) {
#   domain <- identify_domain(posttest)
#   id <- which(posttest_names == posttest)
#   tmp_ci[[domain]] <- ps2ls_ci[[id]]
# }


for (i in 1:length(posttest_names)) {
  # Extract domain
  domain <- identify_domain(posttest_names[i])

  
  # Extract and store the mean and confidence interval bounds for each domain
  tmp_list <- append(list(  "Mean_diff" = p2sls_ci[[domain]]$t0,
                            "Lower"     = p2sls_ci[[domain]]$bca[4],
                            "Upper"     = p2sls_ci[[domain]]$bca[5],
                            "Domain"    = domain),
                     sublist_tmp)
  
  # Add them to our effect_summaries data.table
  effect_summaries <- rbind.data.frame(effect_summaries,
                                       as.data.frame(tmp_list)) |>
    as.data.table()
}

# # Remove all temporal objects
rm(list = grep(pattern = "tmp", x = ls(), value = TRUE))

saveRDS(effect_summaries, file = here::here("output",
                                            "objects",
                                            "effect_summaries",
                                            "figure3_effects.RDS"))

# Plot all previous model specifications ----------------------------------

# Load last stored effect_summaries
if (exists(quote(effect_summaries)) == FALSE) {
  effect_summaries <- readRDS(file = here::here("output",
                                                "objects",
                                                "effect_summaries",
                                                "figure3_effects.RDS"))
  
}

# Create variable for specific model-sample configuration
effect_summaries$subsetXmodel <- interaction(effect_summaries$Subset, effect_summaries$Model)

# Re-order
effect_summaries$subsetXmodel <- factor(effect_summaries$subsetXmodel,
                                        levels = rev(c(
                                          'Complete Case.Unadjusted'
                                          ,'Multiple Imputation - Analytic.g-formula w/ BART'
                                          ,'Multiple Imputation - Washout.g-formula w/ BART'
                                          ,'Complete Case.g-formula w/ BART'
                                          # ,'BI Pretests and Outcome.Adjusted Linear Model'
                                          ,'Complete Case.Adjusted Linear Model'
                                          ,'Complete Case.NCE + Adjusted Linear Model' 
                                          ,'Complete Case.P2SLS' 
                                        )
                                        )
)


# Plot
fig3 <- ggplot(data = effect_summaries,
               aes(x = Mean_diff,
                   xmin = Lower,
                   xmax = Upper,
                   y = subsetXmodel,
                   group = Domain,
                   color = Domain,
                   shape = Domain)) +
  geom_vline(xintercept = 0,
             linetype = "dashed",
             color = "gray50",
             linewidth = linewidth_plot) +
  geom_point(position = position_dodge(width = 0.4), size = 3) +
  geom_linerange(position = position_dodge(width = 0.4), linewidth = linewidth_plot) +
  coord_cartesian(xlim = c(-0.3, 0.3)) +
  labs(x = "Difference between posttest and pretest\nnational test standardized scores",
       y = "Data subset and \nmodel specification") +
  theme(axis.text.y = element_text(hjust = 0)) +
  khroma::scale_color_highcontrast()


fig3

# Save ggplot object
saveRDS(fig3, file = here::here("output", "objects", "effect_summaries", "fig3_ggplot.RDS"))

# Save file
ggsaver(fig3, filename = "figure3_effects",
        size = "custom",
        custom_plot = c(6L, 12L))

# Copy to export folder outside TSD virtual machine
file.copy(from = here::here("output", "plots", "figure3_effects.pdf"),
          to = "N:\\durable\\file-export\\new", overwrite = TRUE)

# Make LaTeX supplementary table ------------------------------------------

  # Make copy of `data.table`
effect_table <- data.table::copy(effect_summaries)

  # Round numeric columns
numeric_cols <- sapply(effect_table, is.numeric)
numeric_cols <- numeric_cols[numeric_cols] |> names()

effect_table[, (numeric_cols) := lapply(.SD, function(x) round(x, digits = 3)), .SDcols = numeric_cols ]
rm(numeric_cols)

  # Make `Mean_diff` a string variable with CI integrated
effect_table$Mean_diff <- as.character(effect_table$Mean_diff)
effect_table[, string_effect := glue::glue("{Mean_diff} [{Lower} ; {Upper}]", envir = .SD)]



  # Add sample size per subset and domain

    # For the complete case analysis
for (domain in c("EN", "RE", "MA")) {
  posttest <- grep(pattern = domain, posttest_names, value = TRUE)
  effect_table[grepl(pattern = "Complete Case", subsetXmodel) & Domain == domain, n := dt_completecase[!is.na(get(posttest))] |> nrow()]
}
  #   # For the Bayesian joint model
  # effect_table[grepl(pattern = "BI Pretests and Outcome", subsetXmodel), n := {bfit_list$brm_fit$data |> dim()}[1]]

    # For the MI Bart models
  effect_table[grepl(pattern = "Analytic", subsetXmodel), n := nrow(dt_analytic)]
  effect_table[grepl(pattern = "Washout", subsetXmodel), n := nrow(dt_analytic[medicated.pt0 == "No"])]
  
  # Combine into one single variable
  effect_table[, n_perc := glue::glue("{{n}} ({{round({n / nrow(dt_eligible)}*100,1)}}\\%)",
                                      .open = "{{",
                                      .close = "}}",
                                      .envir = .SD)]
  

  # Save raw table as it makes it easier for programmatically writing text
saveRDS(effect_table, file = here::here("output", "objects", "effect_summaries", "effect_table_raw.RDS"))  

    # Rename Domain levels
  effect_table$Domain <- forcats::fct_recode(effect_table$Domain,
                                             "English" = "EN",
                                             "Reading" = "RE",
                                             "Numeracy" = "MA"
  )

    
    # Remove unnecessary columns
  effect_table[, c("Mean_diff", "Subset", "Model", "Lower", "Upper", "n") := NULL]
  
  # Add exception for washout sample, as percentage of eligible is different
    # 49.1% of analytic eligible, 68.7% of washout eligible
  perc_elig <- round({nrow(dt_analytic[medicated.pt0 == "No"]) / nrow(dt_eligible[medicated.pt0 == "No"])} * 100, 1)
  effect_table[subsetXmodel == unique(grep("Washout", subsetXmodel, value = TRUE)) , n_perc := paste0(n_perc, "\\dag")]
  
  
  # Keep only first occurence of subsetXmodel
effect_table$subsetXmodel <- as.character(effect_table$subsetXmodel)
effect_table[, subsetXmodel := ifelse(!duplicated(subsetXmodel), subsetXmodel, "")]


  # Re-arrange columns
effect_table <- effect_table[, c("subsetXmodel", "Domain", "string_effect", "n_perc")]


# Rename columns
setnames(effect_table,
         c("subsetXmodel", "string_effect", "n_perc"),
         c("Model specification", "Mean difference [CI_{95}]", "N (\\% of eligible)"))


# Save object
saveRDS(effect_table, file = here::here("output", "objects", "effect_summaries", "effect_table.RDS"))

# Latexify
stargazer::stargazer(effect_table, summary = FALSE, model.numbers = FALSE,
                     rownames = FALSE,
                     table.placement = "!htb",
                     out = here::here("output", "objects", "effect_summaries", "latex_effects.tex"),
                     notes = glue::glue("\\dag The reported percentage is relative to the analytic eligible sample. For the washout eligible sample the percentage is {perc_elig}\\%."))

# Copy to export folder
file.copy(from = here::here("output", "objects", "effect_summaries", "latex_effects.tex"),
          to = "N:\\durable\\file-export\\new", overwrite = TRUE)


# Pending to correct how stargazer process strings. In output, replace:
#  `\textbackslash{}textbackslash ` with empty string ''
# `dag` with \dag
# `\_` with _
# `\{` and `\}` with { }

# Check for remaining \textbackslash
# Change last \midrule to \bottomrule


# identical(writeLines(tmp_latex) , tmp_latex)
# all.equal(writeLines(tmp_latex))
# 
# gsub("\textbackslash ", "", tmp_latex)

# rm(bfit_list)
