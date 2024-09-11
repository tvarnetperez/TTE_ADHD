# Programmatic text body sections -----------------------------------------

  # Alternatively, just load the effect_table_raw object
library(data.table)
dt <- readRDS(file = here::here("output", "objects", "effect_summaries",
                                "effect_table_raw.RDS"))

# Shorthand for CI latex string
ci <- "$\\operatorname{CI}_{95}$"

setnames(dt, old = "Mean_diff", new = "mean")



# :: Abstract -------------------------------------------------------------

# Write text for abstract
A_abstract_text <- glue::glue("
The resulting analytic sample size consisted of \\
{dt[grepl('Analytic', Subset)]$n[1] |> format(big.mark = ',')} children diagnosed with ADHD, \\
of which about 9\\% had a missing grade 8 national test score. \\
The main result is that initiating ADHD medication had a very small \\
positive average effect on national test scores for all three domains: \\
English, numeracy and reading (standardized mean diferences: \\
{dt[grepl('Analytic', Subset) & Domain == 'EN',]$mean} \\
({ci} = [{dt[grepl('Analytic', Subset) & Domain == 'EN',]$Lower} ; \\
{dt[grepl('Analytic', Subset) & Domain == 'EN',]$Upper}]), \\
{dt[grepl('Analytic', Subset) & Domain == 'MA',]$mean} \\
({ci} = [{dt[grepl('Analytic', Subset) & Domain == 'MA',]$Lower} ; \\
{dt[grepl('Analytic', Subset) & Domain == 'MA',]$Upper}]), \\
{dt[grepl('Analytic', Subset) & Domain == 'RE',]$mean} \\
({ci} = [{dt[grepl('Analytic', Subset) & Domain == 'RE',]$Lower} ; \\
{dt[grepl('Analytic', Subset) & Domain == 'RE',]$Upper}]) \\
respectively).
"
)


# :: Flow diagram ---------------------------------------------------------

tmp <- readRDS(file = here::here("output", "descriptives", "flow_counts.RDS"))

B_flow_text <- glue::glue("
The analysis plan consists of multiple phases: a design phase, an imputation phase, \\
an analysis phase and a sensitivity analysis phase. There are four main \\
datasets used in different steps at different phases. 

For the design phase, the most general sample was used to determine \\
the time-zero cutoff, containing 38,215 observations. \\
This included dates of ADHD diagnosis for all children at any time. 

Having set a time-zero, the followed-up sample includes \\
{tmp$included[['merged']] |> format(big.mark = ',')} observations, \\
of which {tmp$excluded[['Missing all pretests']]  |> format(big.mark = ',')} \\
have missing pretests for all three domains and thus should not \\
be part of the analytic sample. %However, they include valuable information \\
about the relationship among covariates, so the imputation model is fit to \\
this dataset. 

Then there is an eligible sample that removes these ineligible \\
observations, resulting in {tmp$included[['eligible']] |> format(big.mark = ',')} \\
observations. This is the dataset that was used for determining \\
the grace-period cutoff. %Standardization of mother's age \\
was done relative to this sample.

This eligible sample is further reduced into the analytic sample via \\
listwise deletion for covariates that were not able to be included in the \\
imputation model, namely civil status at grades 5 and 6 for both parents \\
(due to low cell count) and school ID at grade 8. \\
It results in {tmp$included[['analytic']] |> format(big.mark = ',')} observations. \\
The main analysis results and the descriptive tables are relative \\
to this dataset. %Since the same model covariates are used for \\
all three domain outcomes (including the pre-tests of every domain), the same \\
main analytic sample is used for the analysis and sensitivity analysis phase \\
of each domain. 

The analytic sample is further reduced for the different sensitivity \\
analyses. For only considering children with a complete washout period---i.e, no \\
ADHD medication use before time-zero, it results in \\
{tmp$included[['washout']] |> format(big.mark = ',')} observations. \\
Supplementary descriptive tables are relative to this dataset. While for the \\
simple linear model analysis, the NCE and proximal causal inference procedures \\
it is reduced to a complete case sample of \\
{tmp$included[['complete case']] |> format(big.mark = ',')} observations.
")

rm(tmp)

# :: Participants ---------------------------------------------------------

tmp <- readRDS(here::here("output", "descriptives", "freq_init_raw.RDS"))
tmp2 <- readRDS(here::here("output", "descriptives", "SMDs_cats_balance.RDS"))

  
# Write text for participants description
C_participants_text1 <- glue::glue("
 Table \\ref{tab:freq_numeracy} shows frequencies across emulated treatment arm \\
 for different categorical variables in the main analytic sample. We see that \\
 in general, the emulated arms are relatively similar with regards to this set \\
 of variables. The largest differences are for the proportion of males \\
 ({{tmp$sex[group == 'm']$Initiated}} \\
 in the `initiated' versus \\
 {{tmp$sex[group == 'm']$Control}} \\
 in the `control') and number of siblings with \\
 any mental health diagnosis \\
 ({{tmp$n.sibs.w.diag[group == '0']$Initiated}} \\
 with zero diagnosed siblings \\
 for the `initiated', versus 
 {{tmp$n.sibs.w.diag[group == '0']$Control}} in the `control'), with \\
 standardized mean differences (SMD) of \\
 {{tmp2[Variable == 'sexm']$`Std. Mean Diff.` |> round(2)}} and \\
 {{tmp2[Variable == 'n.sibs.w.diag1']$`Std. Mean Diff.` |> round(2)}}, respectively. \\
 All remaining variables have SMDs with an absolute value less than 0.1. \\ 
", .open = "{{", .close = "}}")

 # Remove temporary files
rm(tmp); rm(tmp2)

tmp  <- readRDS(here::here("output", "descriptives", "med_perc_dt.RDS"))
tmp2 <- readRDS(here::here("output", "descriptives", "extra_meddays_dt.RDS"))
tmp3 <- readRDS(here::here("output", "descriptives", "desc_init_raw.RDS"))

# Since table is already formatted with mean (sd) format, we will modify it 
# for ease of access with two functions:
extract_mean_before_parentheses <- function(input_string) {
  # Regular expression to match an optional negative sign, followed by digits,
  # optionally followed by a decimal point and more digits
  pattern <- "-?\\d+\\.?\\d*"
  
  # Find the match
  match_positions <- regexpr(pattern, input_string)
  
  # Extract the matched pattern
  matched <- regmatches(input_string, match_positions)
  
  # Convert the matched pattern to numeric
  as.numeric(matched)
}

extract_sd_within_parentheses <- function(input_string) {
  # Regular expression to match a number inside parenthesis
  pattern <- "\\((-?\\d+\\.?\\d*)\\)"
  
  # Find the match
  match_positions <- regexpr(pattern, input_string)
  
  # Extract the matched pattern
  matched <- regmatches(input_string, match_positions)
  
  # Remove the parenthesis
  matched_number <- gsub("[()]", "", matched)
  
  # Convert the matched pattern to numeric
  as.numeric(matched_number)
}


D_participants_text2 <- glue::glue(
 "
 Table \\ref{tab:desc_numeracy_1} shows summary statistics for continuous \\
 variables in the main analytic sample. We note that there are few if \\
 any differences in the selected variables across the emulated arms, \\
 including the national test scores or GPA. On the other hand, we can \\
 see that the cutoffs chosen for time-zero and grace period generated \\
 a clear distinction of groups. The average estimated number of medication \\ 
 blocks---periods of treatment with no interruption longer than \\
 30 days---was of \\
 {{tmp3[original_name == 'n.blocks.6to8' & x_axis == 'Initiated']$mean_sd |>
 extract_mean_before_parentheses()}} \\
  (SD = {{tmp3[original_name == 'n.blocks.6to8' & x_axis == 'Initiated']$mean_sd |>
 extract_sd_within_parentheses()}}) for the `initiated' condition and \\
  {{tmp3[original_name == 'n.blocks.6to8' & x_axis == 'Control']$mean_sd |>
 extract_mean_before_parentheses()}} \\
 (SD = {{tmp3[original_name == 'n.blocks.6to8' & x_axis == 'Control']$mean_sd |>
 extract_sd_within_parentheses()}}) for the `control' condition. This can also be seen \\
 in Figure \\ref{fig:hist_meddays_by_arm}, which shows the estimated percentage \\
 of medicated days between time-zero and the grade 8 national test. \\
 We see there is a clear differentiation of groups by the emulated assignment: \\ 
 the pupils in the `initiated' group were medicated for about 
 {{tmp[sample == 'Analytic' & initiated == 'Initiated']$mean |> round(0)}}\\% of \\
 the days between their assignment and the posttest, whereas those \\
 in the `control group' were medicated for about \\
 {{tmp[sample == 'Analytic' & initiated == 'Control']$mean |> round(0)}}\\% of the days, \\
 in both the analytic sample and the not-previously medicated subsample. \\
 In the `control' group, {{tmp2[Sample == 'Analytic' & Type == '0 untreated']$Perc}}\\% \\
 of pupils had zero estimated medicated days \\
 ({{tmp2[Sample == 'Washout' & Type == '0 untreated']$Perc}}\\% in washout subsample), \\
 while in the `initiated' group, \\
 {{tmp2[Sample == 'Analytic' & Type == '100 treated']$Perc}}\\% of pupils had 100\\% estimated medicated days \\
 ({{tmp2[Sample == 'Washout' & Type == '100 treated']$Perc}}\\% in washout subsample). \\
", .open = "{{", .close = "}}")

  # Remove temporary files
rm(tmp); rm(tmp2); rm(tmp3)


# :: Results --------------------------------------------------------------

  # Write text for result section with the effect_table 
    # `\\` avoids glue generating a new line
E_analytic_text <- glue::glue("
Regarding results for the analytic sample, the sample average \\
treatment effect (SATE) had a posterior mean (and credible interval) of \\
{dt[grepl('Analytic', Subset) & Domain == 'EN',]$mean} \\
({ci} = [{dt[grepl('Analytic', Subset) & Domain == 'EN',]$Lower} ; \\
{dt[grepl('Analytic', Subset) & Domain == 'EN',]$Upper}]), \\
{dt[grepl('Analytic', Subset) & Domain == 'MA',]$mean} \\
({ci} = [{dt[grepl('Analytic', Subset) & Domain == 'MA',]$Lower} ; \\
{dt[grepl('Analytic', Subset) & Domain == 'MA',]$Upper}]),
{dt[grepl('Analytic', Subset) & Domain == 'RE',]$mean} \\
({ci} = [{dt[grepl('Analytic', Subset) & Domain == 'RE',]$Lower} ; \\
{dt[grepl('Analytic', Subset) & Domain == 'RE',]$Upper}]) \\
standardized score points for the domains of English, Numeracy and Reading, respectively.
")

F_washout_text <- glue::glue("
For the sensitivity analyses involving only the subset of pupils that have never \\
taken before ADHD pharmacological treatment, the corresponding SATE and intervals are \\
{dt[grepl('Washout', Subset) & Domain == 'EN',]$mean} \\
({ci} = [{dt[grepl('Washout', Subset) & Domain == 'EN',]$Lower} ; \\
{dt[grepl('Washout', Subset) & Domain == 'EN',]$Upper}], \\
{dt[grepl('Washout', Subset) & Domain == 'MA',]$mean} \\
({ci} = [{dt[grepl('Washout', Subset) & Domain == 'MA',]$Lower} ; \\
{dt[grepl('Washout', Subset) & Domain == 'MA',]$Upper}], \\
{dt[grepl('Washout', Subset) & Domain == 'RE',]$mean} \\
({ci} = [{dt[grepl('Washout', Subset) & Domain == 'RE',]$Lower} ; \\
{dt[grepl('Washout', Subset) & Domain == 'RE',]$Upper}] \\
standardized score points for the domains of English, Numeracy and Reading, respectively.
")

  # Heterogeneity text objects
tmp <- readRDS(here::here("output",
                          "objects",
                          "bart_analysis",
                          "long_dt.RDS"))

tmp2 <- readRDS(here::here("output",
                           "objects",
                           "bart_analysis",
                           "washout_long_dt.RDS"))

R2 <- readRDS(here::here("output", "objects", "bart_analysis", "R2.RDS"))

# Percentages of negative effects
tmp  <- tmp[,  .(neg_perc = sum(Effect < 0) / .N), by = c("Domain", "Estimand")][
  order(-neg_perc)][, neg_perc := round({neg_perc*100}, digits = 1)]
tmp2 <- tmp2[, .(neg_perc = sum(Effect < 0) / .N), by = c("Domain", "Estimand")][
  order(-neg_perc)][, neg_perc := round({neg_perc*100}, digits = 1)]

G_heterogeneity_text <- glue::glue("
The estimated sample average effect on the treated (SATT) posterior distributions \\
show a shift towards the negative values relative to the total sample. This is most noticeable \\
for the numeracy domain. \\
For the case of English, the model and data are compatible with a very \\
small probability that pupils who took treatment could have done marginally \\
better by not initiating medication instead \\
({tmp[Domain == 'EN' & Estimand == 'SATT']$neg_perc}\\% and \\
{tmp2[Domain == 'EN' & Estimand == 'SATT']$neg_perc}\\%, for the analytic and \\
washout samples, respectively). The percent of posterior \\
samples with a negative value is less than 1.5\\% for all other posterior \\
distributions outside the English domain. An ANOVA indicates \\
that about {round(R2*100, 1)}\\% of the variability in effect estimates is accounted for \\
by distinguishing the domain of the national test. \\
")

rm(tmp); rm(tmp2); rm(R2)


# :: Sensitivity analyses ----------------------------------------------------

  # Negative control exposure text
H_nce_text <- glue::glue("
For the negative control exposure, we expect to see no effect \\
whatsoever as this is the effect for initiation after the grade 8 national test. \\
This is the case for both Numeracy and Reading \\
({{ci}} = [\\
{{dt[grepl('NCE', Model) & Domain == 'MA',]$Lower}} ; \\
{{dt[grepl('NCE', Model) & Domain == 'MA',]$Upper}}\\
] and \\
[\\
{{dt[grepl('NCE', Model) & Domain == 'RE',]$Lower}} ; \\
{{dt[grepl('NCE', Model) & Domain == 'RE',]$Upper}}\\
], respectively). 
For English we do find evidence of residual confounding. \\
However, its magnitude and direction cannot be interpreted without further \\
strong assumptions \\cite{lipsitch2010negative}.
", .open = "{{", .close = "}}")
                       
  # Proximal causal inference text
I_p2sls_text <- glue::glue("
The point estimates coming from the proximal causal inference analysis via P2SLS \\
that attempt to correct for a binary unmeasured confounder seem to change only \\
the estimate for the English domain, in line with the evidence coming from the \\
NCE estimate that some residual confounding would be present. Further interpretation \\
is restricted by highly variable estimates stemming from the two-stage procedure.
")

                         
#   # Joint imputation text
# G_joint_text <- glue::glue("
#                          
# ")
#     #
# G_joint_text2 <- glue::glue("
# Whether due to a regression dilusion mechanism or due to pupils with \\
# missing covariate values being less likely to benefit from pharmaceutical \\
# treatment, the sensitivity analysis that imputes pretests as well only \\
# suggests to further attenuate our already small magnitude of effect of initiation.
# ")

  # Load small summaries from the Keilow replication script
tmp <- readRDS(here::here("output", "objects", "keilow_prev.RDS"))
tmp2 <- readRDS(here::here("output", "objects", "keilow_effects.RDS"))
tmp2[, c("mean", "se") := lapply(.SD, function(x) round(x, digits = 3)), .SDcols = c("mean", "se")]
  # Replication text
J_keilow_text <- glue::glue("
Given the stark difference between our effect estimate and the one reported in \\
\\cite{keilow2018medical}, we performed an informal replication analysis to \\
see whether the changes in magnitude could be explained by the differing \\
populations.  We first note that there is a considerable difference in prevalence \\
of individuals with less than 90 days medicated ({{tmp[Pop == 'Danish', DPT]}}\\% and \\
{{tmp[Pop == 'Norwegian', DPT]}}\\%, for Keilow et al.'s population and ours, respectively) \\
or without more than 30 days between dispensations ({{tmp[Pop == 'Danish', CPT]}}\\% \\
and {{tmp[Pop == 'Norwegian', CPT]}}\\%). \\
Despite this difference, we manage to estimate a similar effect only after adjustment:
{{tmp2[Pop == 'Danish' & adj == 'Adjusted', mean]}} (S.E. = {{tmp2[Pop == 'Danish' & adj == 'Adjusted', se]}}) and \\
{{tmp2[Pop == 'Norwegian' & adj == 'Adjusted', mean]}} (S.E. = {{tmp2[Pop == 'Norwegian' & adj == 'Adjusted', se]}}), \\
for Keilow et al.'s population and ours, respectively. Further details are available in the shared code.
", .open = "{{", .close = "}}")

    # For discussion
K_keilow_text2 <- glue::glue("
Regarding differences in magnitude with the previous literature, we mostly \\
attribute them to differences in estimand \\cite{lundberg2021your}. Our flexible modeling \\
via BART together with the results from the NCE and P2SLS sensitivity analyses make us believe \\
that residual confounding is not the culprit for this difference. \\
Under a comparable analytic and identifying strategy to \\cite{keilow2018medical} \\
we are also able to estimate a larger order of magnitude effect in the Norwegian population. \\
However, this effect is not comparable to a long-term intention-to-treat effect, which is our \\
estimand of interest. Rather, this is the effect of discontinuation among those who initiated medication \\
(assuming non-responsiveness to medication is random and exchangeable over time).
", .open = "{{", .close = "}}")

  # Remove temporal data.tables
rm(tmp); rm(tmp2)


# :: Discussion --------------------------------------------------------------

# Extract minimum lower bound and highest upper bound by domain for main analysis
extremes <- dt[subsetXmodel %in% grep(subsetXmodel, pattern = "Analytic", value = TRUE),
   .(minimum_ci = min(Lower),
     maximum_ci = max(Upper)), by = Domain]

# Load average standardized posttest scores
tmp2 <- readRDS(here::here("output", "descriptives", "desc_init_raw.RDS"))

means <- c()

means <- tmp2[original_name %in% grep(pattern = "8_std", original_name, value = TRUE) &
       x_axis == "Total",
      extract_mean_before_parentheses(mean_sd)]

names(means) <- c("MA", "EN", "RE")
  # MA    EN    RE 
# -0.59 -0.42 -0.62 

extremes[, `:=`(min_perc_mean = abs(round({minimum_ci / means[[Domain]] * 100}, 2)),
             max_perc_mean    = abs(round({maximum_ci / means[[Domain]] * 100}, 2))),
         by = Domain]

#> extremes
# Domain minimum_ci maximum_ci min_perc_mean max_perc_mean
# 1:     EN      0.015      0.085          3.57         20.24
# 2:     RE      0.035      0.108          5.65         17.42
# 3:     MA      0.042      0.117          7.12         19.83

# This should go in results

L_effect_relative_text <- glue::glue("
To communicate the magnitude of estimated effects in a perhaps more relevant \\
scale for stakeholders, we can informally state that the estimated compatibility intervals \\
suggest that medication initiation reduces between \\
{extremes[,min(min_perc_mean)]} and {extremes[,max(max_perc_mean)]}\\% of the average gap \\
between the grade 8 scores of the ADHD sample relative to the general sample. Even then, this is \\
a slight overestimation as the achievement gap is already taking into account the scores of \\
medicated children.
"
)
# Could be made more proper by comparing the untreated counterfactual mean


# :: Supplements ----------------------------------------------------------

# Captions for ecdf figures

ecdf_t0dx_text <- readRDS(here::here("output", "descriptives",
                                   "ecdf_t0dx_text.RDS"))


ecdf_graceperiod_text <- readRDS(here::here("output", "descriptives",
                                            "ecdf_graceperiod_text.RDS"))

# Save text
  # Capture all text objects
grep(x = ls(), pattern = "text", value = TRUE)

mget(grep(x = ls(), pattern = "text", value = TRUE)) |> unlist() |> paste(collapse = "\n\n") |>
  writeLines(here::here("output", "programmatic_text.txt"))

file.copy(from = here::here("output", "programmatic_text.txt"),
          to = "N:\\durable\\file-export\\new", overwrite = TRUE)



# TeX Flow Chart Programmatic Code ----------------------------------------



# † Cemetery † ------------------------------------------------------------

# melt_effect_list <- function(name_long_dt,
#                              meaneffects_list){
#   
#   # Check input
#   stopifnot("`name_long_dt` must be a string that contains {domain}" =
#               is.character(name_long_dt) & grepl("\\{domain\\}", name_long_dt))
#   
#   # Get name of long dt without the domain substring
#   tmp_no_domain <- gsub("\\{domain\\}", "", name_long_dt) 
#   # Melt domain-wise and add variable with respective Domain
#   for(domain in c("MA", "EN", "RE")) {
#     assign(glue::glue(name_long_dt), as.data.table(meaneffects_list[[domain]]) |>
#              melt(measure.vars = c("SATE", "SATT", "SATU"),
#                   variable.name = "Estimand",
#                   value.name = "Effect"))
#     get(glue::glue(name_long_dt))[, Domain := glue::glue("{domain}")]
#   }
#   
#   # Join all long data.tables into one
#   assign(tmp_no_domain, do.call(rbind, mget(ls(pattern = tmp_no_domain))))
#   
#   return(get(tmp_no_domain))
# }
# 
# summarize_melted <- function(melted_dt){
#   # Summarize by estimand and domain
#   posterior_means <- melted_dt[,
#                              .(mean = mean(Effect),
#                                sd   = sd(Effect)),
#                              by = .(Estimand, Domain)]
#   
#   # Compute 95% credible interval
#   posterior_means[, `:=`(lower = mean - 1.96*sd,
#                          upper = mean + 1.96*sd),
#                   by = .(Estimand, Domain)]
#   
#   return(posterior_means)
# }
#   
# # :: Main analysis -----------------------------------------------------------
# 
# results_path <- here::here("output",
#                            "colossus",
#                            "processed_samples",
#                            "interx_ran_slope")
# 
# # Load RDS files containing the list of results
#   # Domain > Estimand > Samples of mean effects
#   # 750 posterior sample averages x 50 multiple imputations = 37,500 samples
# A_no_washout_raw      <- readRDS(here::here(results_path,
#                                         "interx_ranslope_meaneffects_list.RDS"))   
# B_washout_raw <- readRDS(here::here(results_path,
#                                     "washout_interx_ranslope_meaneffects_list.RDS")) 
# 
# 
# 
# 
# # How much posterior density is on negative values
#   # i.e. how likely is it that the treatment could be detrimental
#   # (although magnitude would also be negligibly so)
# 
#   # Melt to get long data.table with effect by estimand and domain
# A_no_washout_melted <- melt_effect_list(name_long_dt = "long_dt_{domain}",
#                                         meaneffects_list = A_no_washout_raw)
# 
# B_washout_melted <- melt_effect_list(name_long_dt = "long_dt_{domain}",
#                  meaneffects_list = B_washout_raw)
# 
#   # For analytic sample
# A_no_washout_melted[
#   , list(n_negative       = sum(Effect < 0),
#          percent_negative = {{sum(Effect < 0)/.N}*100} |> round(3))
#   , by = c("Estimand", "Domain")]  
#   # For washout subsample
# B_washout_melted[
#   , list(n_negative       = sum(Effect < 0),
#          percent_negative = {{sum(Effect < 0)/.N}*100} |> round(3))
#   , by = c("Estimand", "Domain")]  
#   
#   
# # Summary measures (mean and CI)
# A_no_washout <- A_no_washout_melted |> summarize_melted()
# B_washout <- B_washout_melted |>  summarize_melted()
# 
# 
#   # Round numeric values for readability
# rounding <- 2L
# numeric_columns <- names(A_no_washout)[sapply(A_no_washout, is.numeric)] 
#  
#     # (.SD) is not accepted, thus the need for numeric_columns
# A_no_washout[, (numeric_columns) := lapply(.SD, round, 2),
#              .SDcols = is.numeric]
# B_washout[, (numeric_columns) := lapply(.SD, round, 2),
#           .SDcols = is.numeric]


#   # Write text for result section reporting main findings.
# result_text <- glue::glue("Regarding results for the analytic sample, the sample average \\
#                      treatment effect (SATE) had a posterior mean (and credible interval) of ",
#                      "{{A_no_washout[Estimand == 'SATE' & Domain == 'EN',]$mean}} \\
#                       ($\\operatorname{CI}_{95}$ = \\
#                      [{{A_no_washout[Estimand == 'SATE' & Domain == 'EN',]$lower}}; \\
#                      {{A_no_washout[Estimand == 'SATE' & Domain == 'EN',]$upper}}]), \\
#                      {{A_no_washout[Estimand == 'SATE' & Domain == 'MA',]$mean}}\\
#                       ($\\operatorname{CI}_{95}$ = \\
#                      [{{A_no_washout[Estimand == 'SATE' & Domain == 'MA',]$lower}}; \\
#                      {{A_no_washout[Estimand == 'SATE' & Domain == 'MA',]$upper}}]) and \\
#                      {{A_no_washout[Estimand == 'SATE' & Domain == 'RE',]$mean}}\\
#                       ($\\operatorname{CI}_{95}$ = \\
#                      [{{A_no_washout[Estimand == 'SATE' & Domain == 'RE',]$lower}}; \\
#                      {{A_no_washout[Estimand == 'SATE' & Domain == 'RE',]$upper}}])",
#                      " raw score points for the domains of English, Numeracy and Reading, respectively. \\
#                      For the sensitivity analyses involving only the subset of pupils that have never \\
#                      taken before ADHD pharmacological treatment, the corresponding SATE and intervals \\
#                      are ",
#                      "{{B_washout[Estimand == 'SATE' & Domain == 'EN',]$mean}} \\
#                       ($\\operatorname{CI}_{95}$ = \\
#                      [{{B_washout[Estimand == 'SATE' & Domain == 'EN',]$lower}}; \\
#                      {{B_washout[Estimand == 'SATE' & Domain == 'EN',]$upper}}]), \\
#                      {{B_washout[Estimand == 'SATE' & Domain == 'MA',]$mean}}\\
#                       ($\\operatorname{CI}_{95}$ = \\
#                      [{{B_washout[Estimand == 'SATE' & Domain == 'MA',]$lower}}; \\
#                      {{B_washout[Estimand == 'SATE' & Domain == 'MA',]$upper}}]) and \\
#                      {{B_washout[Estimand == 'SATE' & Domain == 'RE',]$mean}}\\
#                       ($\\operatorname{CI}_{95}$ = \\
#                      [{{B_washout[Estimand == 'SATE' & Domain == 'RE',]$lower}}; \\
#                      {{B_washout[Estimand == 'SATE' & Domain == 'RE',]$upper}}])"
#                      , " for the domains of English, Numeracy and Reading, respectively."
#                      , .open = "{{", .close = "}}")
#   