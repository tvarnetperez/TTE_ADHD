# Extract variables from job script submission----------------------------------
args <- commandArgs(trailingOnly = TRUE)

# This was necessary when running in parallel machines outside the cluster
#batch <- args[1] |> as.integer() # First argument must be the counter
#batch_size <- args[2] |> as.integer() # Second argument must be the size
# first_dataset_index <- args[1] |> as.integer()
# last_dataset_index  <- args[2] |> as.integer()
thinning <- args[1] |> as.integer() # How many imputations to skip due to autocorrelation
domain_string <- args[2] |> as.character()
analyze_washout_subset <- args[3] |> as.integer() |> as.logical()
starting_index <- args[4] |> as.integer()

# print(glue::glue("The indices go from {first_dataset_index} until {last_dataset_index}."))

# Define outcome and treatment variable names
  # Running for standardized outcome
outcome <- glue::glue("p.{domain_string}_8_std")
treatment <- "initiated"

# Read eligible IDs
eligible_IDs <- readRDS(file = "eligible_IDs.RDS")

# Sensitivity analyses
# analyze_washout_subset <- FALSE # Set to TRUE to analyze only those that never took meds
washout <- ifelse(analyze_washout_subset == TRUE, "washout_", "")
U_linear <- FALSE
U_label <- ifelse(U_linear == TRUE, "U_", "")

# Console checks
print(glue::glue("outcome is {outcome} and treatment is {treatment}"))
print(glue::glue("Args are: {args}"))
#print(glue::glue("batch is: {batch}"))
#print(glue::glue("batch_size is: {batch_size}"))
glue::glue("thinning is: {thinning}")
glue::glue("washout is: {washout}")
glue::glue("U_linear is {U_linear} and U_label is {U_label}")


# Define function to extract domain ("MA", "EN", "RE") in a string
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

# Define wrapper function
fit_BART <- function(outcome_string,
                     treatment_string,
                     covariates_vector,
                     grouping_variables,
                     input_data,
                     estimands, 
                     imputation_prefix = "m",
                     # Sampler specifics
                     cores               = parallelly::availableCores(),
                     total_iterations    = 2000,
                     warmup_proportion   = 0.50,
                     num_chains          = parallelly::availableCores(),
                     # Output and saving options
                     output_samples      = TRUE,
                     output_summaries    = TRUE,
                     save_output         = FALSE,
                     file_path           = NULL,
                     file_name           = NULL,
                     random_intercepts   = TRUE,
                     random_slopes       = TRUE,
                     include_U_linear    = FALSE,
                     interact_treatment  = TRUE,
                     seed                = 12345 # To use for multithreading
){
  library(data.table)  
  library(stan4bart)
  tmp_begin_time <- Sys.time()
  # Check inputs
  stopifnot(
    "`outcome_string` must be a character string of length 1." =
      is.character(outcome_string) & length(outcome_string) == 1,
    "`treatment_string`` must be a character string of length 1." =
      is.character(treatment_string) & length(outcome_string) == 1,
    "`estimands` must be a character vector." = 
      is.character(estimands),
    "`input_data`` must be a data.frame or data.table." = 
      is.data.frame(input_data) | data.table::is.data.table(input_data),
    "`imputation_prefix` must be a character string of length 1." =
      is.character(imputation_prefix) & length(imputation_prefix) == 1,
    "Treatment vector `input_data[[treatment_string]] should be numeric." =
      is.numeric(input_data[[treatment_string]])
  )

  # library(rlang) ; `:=` <- data.table::`:=` # As to not mask `:=` in data.table
  
  # Convert data.frame to data.table
  if (is.data.table(input_data) == FALSE) {
    input_data <- as.data.table(input_data)
  }
  
  # Prepare variables and model formula ------------------------------------------
  
  # If `input_data` has more rows than the eligible ones (as imputation is done in
  # merged sample), restrict first only to eligible
  if (nrow(input_data) > length(eligible_IDs)) {
    input_data <- input_data[id %in% eligible_IDs]
  }
  
  # Listwise deletion of NAs in covariates to feed into stan4bart
  input_complete_dt <- input_data[complete.cases(
    input_data[, .SD, .SDcols = outcome_covariates]),
  ]
  
  # Extract the appropriate U for the domain
  U_name <- paste0("U_", identify_domain(outcome_string))
  
  # Build formula
  # Decide whether unobserved confounder is included as linear term or not
  param_linear_terms <- ifelse(include_U_linear == TRUE,
                               glue::glue("{treatment_string} + {U_name} +"),
                               glue::glue("{treatment_string} +"))
  
  # Preallocate
  tmp <- vector()
  # Generate random intercept and/or slope for each grouping variable
  for (i in 1:length(grouping_variables)) {
    if (random_slopes == TRUE & random_intercepts == TRUE) {
      tmp <- paste(glue::glue("(1 + {treatment_string} | {grouping_variables[i]})"), tmp,
                   sep = " + ") 
    }
    if (random_slopes == TRUE & random_intercepts == FALSE) {
      tmp <- paste(glue::glue("({treatment_string} | {grouping_variables[i]})"), tmp,
                   sep = " + ") 
    }
    if (random_slopes == FALSE & random_intercepts == TRUE) {
      tmp <- paste(glue::glue("(1 | {grouping_variables[i]})"), tmp,
                   sep = " + ") 
    }
  }
  param_random_terms <- tmp ; rm(tmp)
  # Select all non-grouping variables into the bart non-parametric term

  if (interact_treatment == FALSE) {
    bart_covariates <- setdiff(outcome_covariates, grouping_variables)
  }
  # We include the treatment_string to allow for interactions
  if (interact_treatment == TRUE) {
    bart_covariates <- setdiff(c(outcome_covariates, treatment_string), grouping_variables)
  }
 
  nonparam_bart_term <- paste0("bart(",
                               paste(bart_covariates, collapse = " + "),
                               ")")
  # Paste all together into a formula
  formula_formula <- paste0(outcome_string, " ~ ",
                            param_linear_terms,
                            param_random_terms,
                            nonparam_bart_term) |> as.formula()
  
  # Fit model
  fit <- call("stan4bart",
              formula      = formula_formula,
              data         = input_complete_dt,
              treatment    = treatment_string,
              verbose      = TRUE,
              iter         = total_iterations,
              warmup       = floor(warmup_proportion * total_iterations),
              chains       = num_chains,
              cores        = cores,
              seed         = seed) |> eval() # So that bart() term gets parsed correctly
  
  # Total number of samples is then:
  # iterations * warm_up * num_chains * n_rows
  # We have 1000 iterations per chain for each row
  # That is, 4000 samples for each participant per fit
  
  # Extract results --------------------------------------------------------------
  
  # Export binary treatment variable
  A                   <- input_complete_dt[[treatment_string]] # Pending to clean this one
  # Create indices for treated and untreated
  treated_indices     <- which(A == 1)
  untreated_indices   <- which(A == 0)
  
  # Pre-allocate list of posterior samples 
  samples_list <- list()
  # Pre-allocate list of summary outputs of the function
  summaries_list = list()
  
  if (output_samples == TRUE) {
    #::::::::::::: CATE :::::::::::::::::
    #::::::::::::::::::::::::::::::::::::
    if ("CATE" %in% estimands) {
      # CATE
      # Matrices of size: n.observations x n.samples
      mu.obs.samples <- extract(fit, sample = "train") # Mu since it's E[Y_i|A = a, X_i]
      mu.cf.samples  <- extract(fit, sample = "test") 
      
      # The conditional counterfactual means for everyone set to A = 1 or A = 0 
      mu.1.samples <- A * mu.obs.samples + (1 - A) * mu.cf.samples # \hat{E[Y_i^1| X_i]}
      mu.0.samples <- (1 - A) * mu.obs.samples + A * mu.cf.samples # \hat{E[Y_i^0| X_i]}
      
      # Create dt with ID column and the individual CATE
      # Note each individual has T individual cate, one per posterior sample
      dt_samples <- data.table(ID = 1:nrow(input_complete_dt),
                               mu.1.samples - mu.0.samples)
      # Replace column names so they include imputation counter
      setnames(dt_samples,
               colnames(dt_samples),
               gsub("^V(\\d+)$", paste0(imputation_prefix, "_\\1"), colnames(dt_samples))
      )
      
      # Add to output list of samples for the CATE estimand
      samples_list$CATE <- copy(dt_samples)
      
      if (output_summaries == TRUE) {
        if ("CATE" %in% estimands) {
          summaries_list$CATE <- dt_samples[,
                                            `:=`(mean_i = rowMeans(dt_samples[,-"ID"]))][,
                                                                                         .(mean = mean(mean_i),
                                                                                           sd   = sd(mean_i), # Standard deviation of individual means
                                                                                           N    = .N)
                                            ]
          rm(dt_samples) # Remove to avoid name collisions in next estimands calculations
        }
      }
    }
    # :::::::::::::: SATE :::::::::::::::::
    # :::::::::::::::::::::::::::::::::::::
    if ("SATE" %in% estimands | "SATT" %in% estimands | "SATU" %in% estimands) {
      ## SATE
      # Draw from the posterior predictive distribution.
      y.obs <- input_complete_dt[[outcome_string]]
      y.cf.samples <- extract(fit, sample = "test", type = "ppd") # N.B: 2022 paper calls `type`, `value`
      y.1.samples <- A * y.obs + (1 - A) * y.cf.samples
      y.0.samples <- (1 - A) * y.obs + A * y.cf.samples
      dt_samples  <- data.table(ID = 1:nrow(input_complete_dt),
                                y.1.samples - y.0.samples)
      
      if ("SATT" %in% estimands | "SATU" %in% estimands) {
        dt_samples[treated_indices, treatment_group := "Treated"]
        dt_samples[untreated_indices, treatment_group := "Untreated"]
      }
      samples_list$SAT <- copy(dt_samples)
      # Pending to clean up
      if (output_summaries == TRUE) {
        if ("SATE" %in% estimands | "SATT" %in% estimands | "SATU" %in% estimands) {
          if("SATE" %in% estimands) {
            summaries_list$SATE <- dt_samples[,
                                              `:=`(mean_i = rowMeans(dt_samples[,-c("ID", "treatment_group")]))][,
                                                                                                                 .(mean = mean(mean_i),
                                                                                                                   se   = sd(mean_i),
                                                                                                                   N    = .N)
                                              ]
          }
          if ("SATT" %in% estimands) {
            summaries_list$SATT <- dt_samples[treatment_group == "Treated",
			.(mean = mean(mean_i),  
			sd   = sd(mean_i),    
			N    = .N)
            ]
          }
          
          if ("SATU" %in% estimands) {
            summaries_list$SATU <- dt_samples[treatment_group == "Untreated",
			.(mean = mean(mean_i),
			sd   = sd(mean_i),
			N    = .N)
            ]
          }
        }
        
      }
      
    }
    rm(dt_samples)
  }
  
  output     <- list(samples = samples_list, summaries = summaries_list)
  tmp_end_time <- Sys.time()
  
  # Remove environment from formula, else RDS file will explode 30x in size
  formula_no_env <- fit$formula
  attr(formula_no_env, ".Environment") <- NULL
  
  # Make diagnostic objects to add to log
  
  # E-BFMI calculations
  energies_by_chain <- fit$stan['energy__', , ] 
  EBFMIs <- apply(energies_by_chain, 2, function(x) { # as in rstan::get_bfmi()
    numer <- sum(diff(x)^2)/length(x)
    denom <- var(x)
    numer/denom
  })
  prop_low_EBFMIs <- sum(EBFMIs < 0.2)  / length(EBFMIs)
  mean_EBFMI      <- mean(EBFMIs)         
  
  # Divergent transitions across all iterations and chains
  total_divergent <- fit$stan['divergent__', , ] |> sum() # Number of divergent transitions
  
  diagnostics <- list("low_EBFMIs" = prop_low_EBFMIs,
                      "mean_EBFMI" = mean_EBFMI,
                      "total_divergent" = total_divergent)
  
  
  # Make list for log
  output$log <- list(
    "diagnostics" = diagnostics,
    "iterations" = total_iterations,
    "warmup_prop" = warmup_proportion,
    "seed"  = seed,
    "cores" = cores,
    "multithreaded" = ifelse(cores > 1L, "Yes", "No"),
    "elapsed_time" = tmp_end_time - tmp_begin_time,
    "nrow(input_data)" = nrow(input_data),
    "nrow(input_complete_dt)" = nrow(input_complete_dt),
    "model_formula" = formula_no_env) 
  
  if (save_output == TRUE) {
    stopifnot(
      "`file_name` must not be NULL" = !is.null(file_name),
      "`file_path` must not be NULL" = !is.null(file_path),
      "`file_name` must be a string" = is.character(file_name),
      "`file_path` must be a string" = is.character(file_path)
    )
    saveRDS(object = output,
            file   = paste0(file_path, file_name))
  }
  
  if (output_samples == TRUE | output_summaries == TRUE) {
    return(output)
  } else {
    return(fit)
  }
  
  
}

print(paste0("The working directory is: ",getwd()))
print(paste0("The available files are ls(): ",list.files()))
print(paste0("The md5sum is: ", tools::md5sum(file = "pmm_x_imputed_list.RDS")))

print(paste0("File info for permissions is: ", file.info("pmm_x_imputed_list.RDS")$mode))
# Load input data
  pmm_x_imputed_list <- readRDS(file = "pmm_x_imputed_list.RDS")


print(glue::glue("The class of input_data is : {class(pmm_x_imputed_list[[1]])}."))

#i <- 1
#tmp_object <- ifelse(analyze_washout_subset == TRUE, pmm_x_imputed_list[[i]][medicated.pt0 == "No"], pmm_x_imputed_list[[i]])
print(glue::glue("Analyze washout subset option is set to: {analyze_washout_subset}"))
#print(glue::glue("Logical test is: {analyze_washout_subset == TRUE}"))
#print(glue::glue("Are they equal: {all.equal(pmm_x_imputed_list[[i]], tmp_object)}"))
#print(glue::glue("The class of the ifelse argument is: {class(tmp_object)}"))
#print(glue::glue("The dim of the ifelse argument is: {dim(tmp_object)}"))
#print(str(tmp_object))
#rm(i); rm(tmp_object)

# Define range of datasets to analyze
# Can just define these indices as command line arguments instead?
#first_dataset_index <- (batch-1)*batch_size + 1
#last_dataset_index  <- (batch)*batch_size
#print(glue::glue("The indices go from {first_dataset_index} until {last_dataset_index}."))
#print(glue::glue("The post-thinning indices are: {seq(first_dataset_index, last_dataset_index, by = thinning)}"))


outcome_covariates <- readRDS(file = "outcome_covariates.RDS")

# String to mark imputation index in file name
imputation_prefix <- "m"

domain <- identify_domain(outcome)
print(glue::glue("The identified domain is: {domain}"))

# For every `i` imputed dataset, fit the BART model and store the output 
for (i in seq(starting_index, length(pmm_x_imputed_list), by = thinning)) {
  fit_BART(outcome_string     = outcome,
           treatment_string   = treatment,
           covariates_vector  = outcome_covariates,
           grouping_variables = c("s.id5", "s.id8"),
           input_data         = if (analyze_washout_subset == TRUE) {pmm_x_imputed_list[[i]][medicated.pt0 == "No"]} else {pmm_x_imputed_list[[i]]},
           estimands          = c("SATE", "SATT", "SATU"), 
           imputation_prefix  = "m",
           # Sampler specifics
           cores              = parallelly::availableCores(),
           total_iterations   = 4000, # 3000 samples with a burnin of 1000
           warmup_proportion  = 0.25,
           num_chains         = parallelly::availableCores(),
           # Output and saving options
           output_samples     = TRUE,
           output_summaries   = TRUE,
           save_output        = TRUE,
           file_name          = glue::glue("output_{washout}{domain}_{imputation_prefix}{i}.RDS"),
		       file_path          = "/tsd/p2149/home/p2149-tomasfv/bash/definite/bart_output/",
           seed               = 100000 + i, #((100000 * batch) + i),
		       random_slopes      = TRUE, 
		       include_U_linear   = U_linear
  )
}