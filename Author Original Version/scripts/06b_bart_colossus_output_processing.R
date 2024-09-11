library(data.table)
library(ggplot2)
library(patchwork)

# 8 March 2024
# Have to inspect objects, as seen from the slurm output files,
# some scripts were mysteriously run with only a single chain
# no multithreaded. So their sample dimensions should also be different.
# Was this only for the washout samples?



# Colossus output processing
# tmp <- readRDS(here::here("output", "colossus", "output_MA_1_m1.RDS"))
# batch           <- 1L
# batch_size      <- 230L
# num_imputations <- 230L
# num_batches     <- num_imputations / batch_size
# num_samples     <- 750L
# thinning        <- 10L

num_imputations <- 250L
num_samples = 30000L
thinning <- 10L
folder_path <- "M:\\p2149-tomasfv\\bash\\definite\\bart_output\\"
sample_colnames <- c(paste0("V", 1:num_samples))

sequence <- seq(1, num_imputations, by = thinning)

# PENDING, can remove num_imputations from argument of function and
# automatically define it outside the loop
# num_samples <- setdiff(colnames(tmp$samples$SAT),
#                        c("ID", "treatment_group")) |>
#   length() # Number of posterior predictive samples per imputation

# sample_colnames <- setdiff(colnames(tmp$samples$SAT),
#         c("ID", "treatment_group"))

  # Pre-allocate vectors and put them in a list
SATE_pred <- vector(mode = "numeric", length = num_samples*(num_imputations / thinning))
SATT_pred <- vector(mode = "numeric", length = num_samples*(num_imputations / thinning))
SATU_pred <- vector(mode = "numeric", length = num_samples*(num_imputations / thinning))
  # Higher level, by estimand
meaneffects_list <- list("SATE" = SATE_pred,
                         "SATT" = SATT_pred,
                         "SATU" = SATU_pred)
  # Higher level, by domain
meaneffects_list <- list("MA" = meaneffects_list,
                         "EN" = meaneffects_list,
                         "RE" = meaneffects_list)

std_meaneffects_list <- meaneffects_list
washout_meaneffects_list <- meaneffects_list

# tmp <- readRDS(file = "M:\\p2149-tomasfv\\bash\\definite\\bart_output\\output_EN_m1.RDS")

# Cycle through all batches
# and imputations within each batch
# to calculate sample average treatment effects for each predictive posterior sample
# and store in list for appropriate estimand
extract_m_samples <- function(thinning,
                              num_batches,
                              num_imputations,
                              num_samples,
                              save_file_name    = "replace_this_name",
                              file_path         = 'M:\\p2149-tomasfv\\bash\\definite\\bart_output\\',
                              save_file         = FALSE,
                              washout_subset    = FALSE,
                              meaneffects_list
                              ){
  washout <- ifelse(washout_subset == TRUE, "washout_", "")
  file_path <- file_path #ifelse(washout_subset == TRUE, paste0(file_path, "/washout"), file_path)
  sequence <- seq(1, num_imputations, by = thinning)
  
  

   
  
  for (domain in c("MA", "EN", "RE")) {
    # for(batch in 1:num_batches){
    for (imputation in 1:length(sequence)) { # Imputation counter
      tmp <- readRDS(file = glue::glue("{file_path}output_{washout}{domain}_m{sequence[imputation]}.RDS"))
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
      print(glue::glue("Domain: {domain}, Imputation: {imputation}/{length(sequence)} completed."))
    }
    # }
  }
  if (save_file == TRUE) {
    saveRDS(meaneffects_list, file = here::here(file_path, paste0(save_file_name, ".RDS"))) 
  }
  
  return(meaneffects_list)
}

# Extract samples for both datasets
meaneffects_list <- extract_m_samples(thinning = thinning,
                                      num_imputations = num_imputations,
                                      num_samples = num_samples,
                                      washout_subset = FALSE,
                                      meaneffects_list = meaneffects_list)

washout_meaneffects_list <- extract_m_samples(thinning = thinning,
                                              num_imputations = num_imputations,
                                              num_samples = num_samples,
                                              washout_subset = TRUE,
                                              meaneffects_list = washout_meaneffects_list)


# Store the extracted samples
saveRDS(meaneffects_list,
        file = here::here("output",
                          "objects",
                          "bart_analysis",
                          "definite_meaneffects_list.RDS"
                          ))

saveRDS(washout_meaneffects_list,
        file = here::here("output",
                          "objects",
                          "bart_analysis",
                          "definite_washout_meaneffects_list.RDS"
                          ))


# Prepare plotting ----------------------------------------------------------------
  # Function to create long data.table with effect samples for each estimand and each domain
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

  # Create long format data.tables
long_dt <- melt_effect_list(name_long_dt =  "long_dt_{domain}",
                            meaneffects_list)

washout_long_dt <- melt_effect_list(name_long_dt = "washout_long_dt_{domain}",
                 washout_meaneffects_list)

  # Save data.table objects
saveRDS(long_dt, here::here("output",
                            "objects",
                            "bart_analysis",
                            "long_dt.RDS"))

saveRDS(washout_long_dt, here::here("output",
                                    "objects",
                                    "bart_analysis",
                                    "washout_long_dt.RDS"))


  # Pre-allocate list
summarized_posteriors <- list("analytic" = NA, "washout" = NA)

  # Compute analytic sample posterior means
tmp_means <- long_dt[,
        .(mean = mean(Effect),
          sd   = sd(Effect)),
        by = .(Estimand, Domain)]

tmp_means[, `:=`(lower = mean - 1.96*sd,
                       upper = mean + 1.96*sd),
                by = .(Estimand, Domain)]
  # Store
summarized_posteriors$analytic <- tmp_means
  # Delete temp file
rm(tmp_means)

# Compute washout sample posterior means
tmp_means <- washout_long_dt[,
                       .(mean = mean(Effect),
                         sd   = sd(Effect)),
                       by = .(Estimand, Domain)]

tmp_means[, `:=`(lower = mean - 1.96*sd,
                       upper = mean + 1.96*sd),
                by = .(Estimand, Domain)]

  # Store
summarized_posteriors$washout <- tmp_means
  # Delete temp file
rm(tmp_means)


# Save list object with both
saveRDS(summarized_posteriors, here::here("output",
                                          "objects",
                                          "bart_analysis",
                                          "summarized_posteriors.RDS"))

linewidth_plot <-  1L
alpha_plot <- 0.6



# Manuscript plot ---------------------------------------------------------

# Read objects if not available in environment (i.e. computed in this script call)
if (exists(quote(summarized_posteriors)) == FALSE) {
  summarized_posteriors <- readRDS(here::here("output",
                                              "objects",
                                              "bart_analysis",
                                              "summarized_posteriors.RDS"))
}

if (exists(quote(long_dt)) == FALSE &
    exists(quote(washout_long_dt)) == FALSE) {
# Read data.table objects
long_dt <- readRDS(here::here("output",
                            "objects",
                            "bart_analysis",
                            "long_dt.RDS"))

washout_long_dt <- readRDS(here::here("output",
                                    "objects",
                                    "bart_analysis",
                                    "washout_long_dt.RDS"))
}


# Plot hyperparameters
num_bins <- 100L
cartesian_xlim <- c(-0.2, 0.2)
cartesian_ylim <- c(0L, 1L)


# Top panel: Analytic
pA <- ggplot2::ggplot(long_dt) +
  geom_histogram(aes(x = Effect,
                     y =  after_stat(ndensity),
                     fill = Estimand),
                 alpha = 0.4,
                 bins = num_bins,
                 position = "identity") +
  labs(y = "") +
  geom_vline(data = summarized_posteriors$analytic,
             aes(xintercept = mean, color = Estimand),
             linetype = "twodash",
             linewidth = linewidth_plot) +
  geom_vline(data = summarized_posteriors$analytic,
             aes(xintercept = lower, color = Estimand),
             linetype = "dotted",
             linewidth = linewidth_plot,
             alpha = alpha_plot) +
  geom_vline(data = summarized_posteriors$analytic,
             aes(xintercept = upper, color = Estimand),
             linetype = "dotted",
             linewidth = linewidth_plot,
             alpha = alpha_plot) +
  facet_wrap(~ Domain) +
  theme(strip.text = element_text(color = "white"),
        strip.background     = element_rect(fill = "gray85",
                                            color = "gray85"
        ),
        axis.title.x = element_blank()) + ggtitle(label = "a") +
  coord_cartesian(xlim = cartesian_xlim
                  # ,ylim = cartesian_ylim
                  ) +
  scale_y_continuous(breaks = seq(0, 1, by = 0.5)) +
  khroma::scale_fill_highcontrast(guide = "none") +
  khroma::scale_color_highcontrast(guide = "none")

# Bottom panel: Washout
pB <- ggplot2::ggplot(washout_long_dt) +
  geom_histogram(aes(x = Effect,
                     y =  after_stat(ndensity),
                     fill = Estimand),
                 alpha = 0.4,
                 bins = num_bins,
                 position = "identity") +
  labs(y = "") +
  geom_vline(data = summarized_posteriors$washout,
             aes(xintercept = mean, color = Estimand),
             linetype = "twodash",
             linewidth = linewidth_plot) +
  geom_vline(data = summarized_posteriors$washout,
             aes(xintercept = lower, color = Estimand),
             linetype = "dotted",
             linewidth = linewidth_plot,
             alpha = alpha_plot) +
  geom_vline(data = summarized_posteriors$washout,
             aes(xintercept = upper, color = Estimand),
             linetype = "dotted",
             linewidth = linewidth_plot,
             alpha = alpha_plot) +
  facet_wrap(~ Domain) +
  theme(strip.text       = element_blank(),
        strip.background = element_blank()) +
  labs(title = "b",
       x = "Average effect of initiating medication on grade 8 national test standardized score") +
  coord_cartesian(xlim = cartesian_xlim
                  # ,ylim = cartesian_ylim
                  ) +
  scale_y_continuous(breaks = seq(0, 1, by = 0.5)) +
  khroma::scale_fill_highcontrast() +
  khroma::scale_color_highcontrast()

# pApB <- pA / pB + plot_layout(axes = "collect") # patchwork version too old to do this
pApB <- {pA / pB} + labs(y = "Scaled density") + theme(axis.title.y = element_text(hjust=3))
pApB

# 12 inches x 8 inches
ggsave(pApB,
       filename = "main_analysis_plot.pdf",
       path = here::here("output", "plots"),
       device = "pdf",
       height = 8L, width = 12L,
       units = "in")

file.copy(from = here::here("output", "plots", "main_analysis_plot.pdf"),
          to = "N:\\durable\\file-export\\new", overwrite = TRUE)

# Variability of effects explained by group membership --------------------
if (exists(quote(meaneffects_list)) == FALSE &
    exists(quote(washout_meaneffects_list)) == FALSE) {
# Store the extracted samples
meaneffects_list <- readRDS(file = here::here("output",
                                              "objects",
                                              "bart_analysis",
                                              "definite_meaneffects_list.RDS"
                            ))

washout_meaneffects_list <- readRDS(file = here::here("output",
                                                      "objects",
                                                      "bart_analysis",
                                                      "definite_washout_meaneffects_list.RDS"
                                    ))
}


num_post_samples <- length(meaneffects_list$MA$SATE)

anova_dt <- data.table("Effect" = NA_real_,
                       "Domain" = NA_character_)[-1] # Remove row with NA's
anova_dt <- rbind.data.frame(anova_dt,
                             list("Effect" = meaneffects_list$MA$SATE,
                               "Domain" = rep("MA", num_post_samples)),
                             list("Effect" = meaneffects_list$EN$SATE,
                                  "Domain" = rep("EN", num_post_samples)),
                             list("Effect" = meaneffects_list$RE$SATE,
                                  "Domain" = rep("RE", num_post_samples)))

lm(Effect ~ Domain, data = anova_dt) |> summary() 
# Outcome Imputation only: R^2 = 0.281
# Outcome and covariate baseline imputation: r^2 = 0.3046
# 10 chains and 3000 samples per chain: R2 = 31.4%
  # Sanity check
DescTools::EtaSq(aov(Effect ~ Domain, data = anova_dt)) # Same


# Save ANOVA R squared:
saveRDS(DescTools::EtaSq(aov(Effect ~ Domain, data = anova_dt))[1],
        here::here("output", "objects", "bart_analysis", "R2.RDS"))
