# Cycle through all batches
# and imputations within each batch
# to calculate sample average treatment effects for each predictive posterior sample
# and store in list for appropriate estimand
extract_m_samples <- function(num_batches       = 1L,
                              thinning          = 10L,
                              num_imputations   = 500L,
                              num_samples       = 750L,
                              save_file_name    = "replace_this_name",
                              file_path         = "output/colossus",
                              save_file         = FALSE,
                              washout_subset    = FALSE,
                              meaneffects_list
){
  washout <- ifelse(washout_subset == TRUE, "washout_", "")
  file_path <- file_path #ifelse(washout_subset == TRUE, paste0(file_path, "/washout"), file_path)
  sequence <- seq(1, num_imputations, by = thinning)
  
  for (domain in c("MA", "EN", "RE")) {
    for(batch in 1:num_batches){
      for (imputation in 1:length(sequence)) { # Imputation counter
        tmp <- readRDS(here::here(file_path, glue::glue("output_{washout}{domain}_1_500_m{sequence[imputation]}.RDS")))
        # Calculate indices for storing of results
        tmp_first <- ((imputation-1)*num_samples)+1
        tmp_last  <- ((imputation)*num_samples)
        
        meaneffects_list[[domain]]$SATE[tmp_first:tmp_last] <- tmp$samples$SAT[,
                                                                               .(colMeans(.SD)),
                                                                               .SDcols = sample_colnames][[1]] # [[1]] to get vector as output
        meaneffects_list[[domain]]$SATT[tmp_first:tmp_last] <- tmp$samples$SAT[treatment_group == "Treated",
                                                                               .(colMeans(.SD)),
                                                                               .SDcols = sample_colnames][[1]]
        meaneffects_list[[domain]]$SATU[tmp_first:tmp_last] <- tmp$samples$SAT[treatment_group == "Untreated",
                                                                               .(colMeans(.SD)),
                                                                               .SDcols = sample_colnames][[1]]
        print(glue::glue("Domain: {domain}, Imputation: {imputation} completed."))
      }
    }
  }
  if (save_file == TRUE) {
    saveRDS(meaneffects_list, file = here::here(file_path, paste0(save_file_name, ".RDS"))) 
  }
  
  return(meaneffects_list)
}


                             
# Create long data.table with effect samples for each estimand and each domain
melt_effect_list <- function(name_long_dt,
                             meaneffects_list){
  
  # Check input
  stopifnot("`name_long_dt` must be a string that contains {domain}" =
              is.character(name_long_dt) & grepl("\\{domain\\}", name_long_dt))
  
  # Get name of long dt without the domain substring
  tmp_no_domain <- gsub("\\{domain\\}", "", name_long_dt) 
  # Melt domain-wise and add variable with respective Domain
  for(domain in c("MA", "EN", "RE")) {
    assign(glue::glue(name_long_dt), as.data.table(meaneffects_list[[domain]]) |>
             melt(measure.vars = c("SATE", "SATT", "SATU"),
                  variable.name = "Estimand",
                  value.name = "Effect"))
    get(glue::glue(name_long_dt))[, Domain := glue::glue("{domain}")]
  }
  
  # Join all long data.tables into one
  assign(tmp_no_domain, do.call(rbind, mget(ls(pattern = tmp_no_domain))))
  
  return(get(tmp_no_domain))
}