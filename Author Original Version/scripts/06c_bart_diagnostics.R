# `stan4bart` does not compute ESS nor Rhat by default
# So we will compute them using the functions from `posterior`:
# rhat(), ess_bulk() and ess_tail()
# The argument is a matrix of samples * chain for a parameter of interest
# so we will cast the dt of samples of effect back into being separated by chain

library(data.table)
library(posterior)

# Compute Stan diagnostics -----------------------------------------------------

# # Load `bart_output` object
# tmp <- readRDS(choose.files())
# 
# # Assign sample data.table
# dt <- tmp$samples$SAT[,-c("ID","treatment_group")]
# 
# # Extract number of chains
# num_chains <- tmp$log$cores[[1]] 
#   # By the arguments we gave in the call, does not necessarily have to be the case in general that chains = cores
# 
# # Extract number of iterations
# total_iter <- dim(tmp$samples$SAT)[2] - 2L # Have to remove id and treatment_group variables
# chain_iter <- total_iter  / num_chains
# 
# # Extract number of subjects
# 
# # :: On averaged effect estimates -----------------------------------------
# 
# # Pre-allocate numeric vector
# avg_dx        <- vector(mode = "numeric", length = 3L)
# names(avg_dx) <- c("Rhat", "ESS_bulk", "ESS_tail") 
# 
# # Average across participants to get a 1 x samples matrix of SATE samples
# dt_avg <- colMeans(dt)
# 
# # We fill by column so that each column has the samples from the respective chain
# mat_avg <- dt_avg |> as.matrix() |> matrix(data = _, nrow = chain_iter, ncol = num_chains, byrow = FALSE)
# 
# # Compute diagnostics
# avg_dx[['Rhat']]    <- posterior::rhat(mat_avg)     # 1.128113
# avg_dx[['ESS_bulk']]<- posterior::ess_bulk(mat_avg) # 21.00174
# avg_dx[['ESS_tail']]<- posterior::ess_tail(mat_avg) # 53.48727
# 
# avg_dx
# 
# # Same result as just using
# arr_avg <- array(mat_avg, dim = c(500, 4, 1)) |> posterior::as_draws_array()
# summarize_draws(arr_avg)
# 
# 

# Extract diagnostics for each outputted bart object ----------------------------------------

library(data.table)
library(posterior)

# Pre-allocate data.table
dt_dx <- data.table(
    "divergent"       = NA_integer_ # Count of divergent transitions
  , "Rhat"            = NA_real_
  , "ESS_bulk"        = NA_real_
  , "ESS_tail"        = NA_real_
  , "mean_E-BFMI"     = NA_real_
  , "prop_low_E-BFMI" = NA_real_
  , "m_Dataset"       = NA_integer_ # Imputed dataset
  , "Domain"          = NA_character_ 
  , "Sample"          = NA_character_ # Analytic or Washout
)

dt_dx <- dt_dx[-1,]

# System specific file path
if (Sys.info()["sysname"] == "Windows") {
  file_path <-  'M:\\p2149-tomasfv\\bash\\definite\\bart_output\\'
}

if (Sys.info()["sysname"] != "Windows") {
  file_path <-  '/tsd/p2149/home/p2149-tomasfv/bash/definite/bart_output/'
}

# Sequence of imputed data set indices to cover
sequence <- seq(1, 250, 10)

if (Sys.info()["sysname"] != "Windows") {
args <- commandArgs(trailingOnly = TRUE)
print(args)
washout <- ifelse(args[1] |> as.integer() |> as.logical(), "washout_", "")
}
# Washout string, empty string means Analytic sample
if (Sys.info()["sysname"] == "Windows") {
washout <- ""
}

for (domain in c("MA", "EN", "RE")) {
  for (imputation in sequence) { # Imputation counter
    tmp <- readRDS(file = glue::glue("{file_path}output_{washout}{domain}_m{imputation}.RDS"))
    
    # Assign sample data.table
    dt <- tmp$samples$SAT[,-c("ID","treatment_group")]
    
    # Average across participants to get a 1 x samples matrix of SATE samples
    dt_avg <- colMeans(dt)
    
    # Extract number of chains
    num_chains <- tmp$log$cores[[1]] 
    
    # Extract number of iterations
    total_iter <- dim(tmp$samples$SAT)[2] - 2L # Have to remove id and treatment_group variables
    chain_iter <- total_iter  / num_chains
    
    # Create matrix
      # We fill by column so that each column has the samples from the respective chain
    mat_avg <- dt_avg |> as.matrix() |> matrix(data = _, nrow = chain_iter, ncol = num_chains, byrow = FALSE)
    
    # Assign entry into list
    tmp_list <- list(
      "divergent"         = tmp$log$diagnostics$total_divergent # Count of divergent transitions
      ,"Rhat"             = posterior::rhat(mat_avg)     
      ,"ESS_bulk"         = posterior::ess_bulk(mat_avg) 
      ,"ESS_tail"         = posterior::ess_tail(mat_avg)
      , "mean_E-BFMI"     = tmp$log$diagnostics$mean_EBFMI
      , "prop_low_E-BFMI" = tmp$log$diagnostics$low_EBFMIs
      ,"m_Dataset"        = imputation # Imputed dataset
      ,"Domain"           = domain 
      ,"Sample"           = ifelse(washout == "", "Analytic", "Washout") # Analytic or Washout
    )
    
    # Rbind into the final data.table
    dt_dx <- data.table::rbindlist(list(dt_dx, tmp_list), use.names = TRUE, fill = TRUE, idcol = FALSE)
    
    # Print message
    print(glue::glue("Domain: {domain}, dataset {which(sequence == imputation)}/{length(sequence)} completed!"))
  }
}

dt_dx <- unique(dt_dx)

# System specific path to store output
if (Sys.info()["sysname"] != "Windows") {
saveRDS(dt_dx, glue::glue("dt_dx{washout}.RDS"))
}

if (Sys.info()["sysname"] == "Windows") {
saveRDS(dt_dx, here::here("output", "objects", "bart_analysis", glue::glue("dt_dx{washout}.RDS")))
}
# 
# dt_dx
# # :: Extract indices of problematic imputed datasets ----------------------
# 
# 
# 
# 
# 
# # # Plot --------------------------------------------------------------------
# dt_dx <- readRDS(choose.files()) #m_Dataset is wrong. Sequence is wrong, then cannot identify which ones should be removed. Maybe still usable.
# library(ggplot2)
# 
# 
# # Relationship between effective sample sizes across domains
# 
# ggplot(data = dt_dx[Sample == "Analytic"],
#        aes(fill = Domain,
#            color = Domain,
#            x = ESS_bulk,
#            y = ESS_tail)) +
#   geom_point(size = 5L, alpha = 0.7) +
#   geom_segment(aes(x = 100,  y = 0, # Vertical line
#                    xend = 100, yend = Inf),
#                linewidth = linewidth_plot,
#                linetype = "dashed",
#                col = "gray45") +
#   geom_segment(aes(x = 0,  y = 100,
#                    xend = Inf, yend = 100),
#                linewidth = linewidth_plot,
#                linetype = "dashed",
#                col = "gray45") +
#   coord_cartesian(xlim = c(0, 2000),
#                   ylim = c(0, 8000), expand = FALSE) +
#   labs(caption = "Dashed lines mark rule of thumb of 100 ESS.") +
#   khroma::scale_color_highcontrast()
# 
# 
# # Relationship of dataset to divergent transitions and Rhat
# 
# dt_dx[, any_divergent := as.factor(as.integer(divergent > 0))]
# dt_dx[, count_dataset := match(m_Dataset, sequence)]
# 
# ggplot(data = dt_dx[Sample == "Analytic"],
#        aes(x = count_dataset,
#            y = Rhat,
#            color = any_divergent,
#            fill = Domain)) +
#   geom_point(size = 10L, shape = 21, stroke = 2L
#              # , alpha = 0.8 # Breaks the alpha in scale_color_manual
#              ) +
#   labs(x = "Imputed Dataset") +
#   khroma::scale_fill_highcontrast() +
#   guides(fill = guide_legend(override.aes = list(color = rgb(0,0,0, alpha = 0)))) +
#   scale_color_manual(values = c(rgb(0,0,1, alpha = 0), "black"),
#                      labels = c("", "At least 1 divergent transition")) +
#   coord_cartesian(ylim = c(1, 1.2))
# 



# Should samples be pooled across datasets?
# Remember Paul Bruckner's comment on Rhat issues when dealing with imputed datasets