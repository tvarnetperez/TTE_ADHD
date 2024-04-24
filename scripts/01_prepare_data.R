library(data.table)

# :: dt_dates ----------------------------------------------------------------

# Assign NA to  first dispensions after end of follow-up
end_days_margin <- 30 # Margin of error for cut-off of the end of follow-up
end_of_followup <- "date.8"
dt_dates$date.1st.dispension <- data.table::fifelse(dt_dates$date.1st.dispension > #base::ifelse coerces dates to numeric
                                                      (dt_dates[, get(end_of_followup)] + end_days_margin),
                                                    as.Date(NA_real_), dt_dates$date.1st.dispension)

# The margin of error allows 26 cases where the date of dispension is later than
# grade 8 national test, but it makes sense as the test date is uncertain
testids <- which(dt_dates$date.1st.dispension > dt_dates$date.8)
dt_dates[testids, list(date.1st.dispension, date.8)]

# Create age at first dispension for analytical sample
dt_dates[, age.1st.dispension := as.numeric(date.1st.dispension-DOB)/365.25] # in years

# Create time since pretest until first dispension 
dt_dates[, time_after5_1st_dispension := as.numeric(date.1st.dispension - date.5)/365.25] # in years

# :: dt_merged ---------------------------------------------------------------

# Make a list with vector of names for pretest and posttest variables
pretest_names  <-  grep("^p.[E.R.M.G]", names(dt_merged),
                        value = TRUE) |> 
  grep(pattern = "[8,9,10]",
       value  = TRUE,
       invert = TRUE)
posttest_names <- grep("^p.[E.R.M.G]", names(dt_merged), # Only posttest at grade 8
                       value = TRUE) |> 
  grep(pattern = "[5, 9, 10]", 
       value  = TRUE,
       invert = TRUE)

# Add _std suffix
pretest_names_std  <- paste0(pretest_names, "_std")
posttest_names_std <- paste0(posttest_names, "_std")


# Make a nested list that subdivides by time and then domain
outcome_time_list <- list(pretest = pretest_names,posttest = posttest_names)

# Make a nested list will all names, first domain then time
time_list_tmp     <- list(pretest = NA_character_, posttest = NA_character_)
outcome_meta_list <- list(numeracy = time_list_tmp, reading = time_list_tmp, english = time_list_tmp)

outcome_meta_list$numeracy <- Map(replace, outcome_meta_list$numeracy,
                                  values = sapply(outcome_time_list,
                                                  function(x) grep(pattern = "[MA]", x, value = TRUE)))
outcome_meta_list$english  <- Map(replace, outcome_meta_list$english,
                                  values = sapply(outcome_time_list,
                                                  function(x) grep(pattern = "[N]" , x, value = TRUE)))
outcome_meta_list$reading  <- Map(replace, outcome_meta_list$reading,
                                  values = sapply(outcome_time_list,
                                                  function(x) grep(pattern = "[R]" , x, value = TRUE)))

# More vectors of names for brevity
all_outcome_names_vector <- grep("^p.[E.R.M]", names(dt_merged), # Scores at grade 5, 8 and 9
                                 value = TRUE)
avg_school_scores_names <- grep(pattern = "^s.avg", colnames(dt_merged), value = TRUE) # All school average variables

exempt_pretest_names  <- paste0("exempt.", outcome_time_list$pretest) # Will be used to create exemption indicators
exempt_posttest_names <- paste0("exempt.", outcome_time_list$posttest)

# 0 scores as NA for pupils
dt_merged[, (all_outcome_names_vector) := lapply(.SD, function(x) ifelse(x == 0, NA, x)),
          .SDcols = all_outcome_names_vector] # NB: .SDcols makes it so that our character vector is now in scope


# Create exemption indicators
  # Exemptions from grade 5 are domain-specific, e.g.:
  # table(is.na(dt_merged$p.EN_5), is.na(dt_merged$p.MA_5),  is.na(dt_merged$p.RE_5)) 

# There must be a more data.table and nicer way to do this, PENDING
# Indicate if pupil has a missing pretest or posttest
for (i in 1:length(posttest_names)) {
  dt_merged[, (exempt_pretest_names[i])  := is.na(get(outcome_time_list$pretest[i]))]
  dt_merged[, (exempt_posttest_names[i])  := is.na(get(outcome_time_list$posttest[i]))]
}

# Create number and proportion of valid missing posttests variable
# How many missing posttest did the pupil have (this will count NA's as 0, thus the next step is needed)
dt_merged[, num_post_exemptions  := sum(.SD, na.rm = TRUE),
          by = 1:nrow(dt_merged), .SDcols = exempt_posttest_names] # 1:nrow(dt_merged) works as indexing per row
# How many valid (i.e. with non-missing pretests) posttests did the pupil have
dt_merged[, num_valid_posttests := sum(!is.na(.SD)),
          by = 1:nrow(dt_merged), .SDcols = exempt_posttest_names] 
# Compute the proportion
dt_merged[, prop_post_exemptions := num_post_exemptions/num_valid_posttests]
# These NA are people outside the analytic sample
dt_merged$prop_post_exemptions <- replace(dt_merged$prop_post_exemptions,
                                          is.nan(dt_merged$prop_post_exemptions),
                                          NA_real_)
# Pending to change 'exemptions' to 'missing'.

# Create birth month variable
  # We know there is relative age effect involved (Aune et al., 2018)
dt_merged[, bmonth := as.factor(lubridate::month(DOB))]

# Create ADHD diagnosis registration for parents indicator
  # Note this variables are already restricted to only before time-zero
dt_merged[, ADHD.f := pre6.P81.father > 0]
dt_merged[, ADHD.m := pre6.P81.mother > 0]
dt_merged$ADHD.f <- as.factor(dt_merged$ADHD.f)
dt_merged$ADHD.m <- as.factor(dt_merged$ADHD.m)

# Typing, variables into factors
  # Sex
dt_merged[, sex := factor(sex)]
  # Birth year
dt_merged[, byear  := factor(byear)]
  # School IDs
dt_merged[, s.id5 := factor(s.id5)]
dt_merged[, s.id8 := factor(s.id8)]

  # Parity
# We will collapse parity of 5 and higher as one factor level due to low cell count
# > table(dt_merged$parity)
# 0     1    2     3    4    5    6   7    8    9 
# 6983 5193 2337  675  161   58   18  2    1    1 


dt_merged$parity <- factor(dt_merged$parity,
                           levels = 0:9,
                           labels = c(0:4, rep("5 or higher", 5)))
dt_merged$parity <- dt_merged$parity |> as.ordered() 

  # n.children.m should have 5+ collapsed
  # 1    2    3    4    5    6    8 
# 4978 7074 2234  340   36   12    1 
dt_merged$n.children.m <- factor(dt_merged$n.children.m,
                                 levels = 1:7,
                                labels = c(1:4, rep("5 or higher",
                                                    length(unique(dt_merged$n.children.m[!is.na(dt_merged$n.children.m)])) - 4L)) 
                                )

  # n.sibs.w.diag should also be collapsed on 5+
dt_merged$n.sibs.w.diag <- factor(dt_merged$n.sibs.w.diag,
                                  levels = 0:4,
                                  labels = c(0:1, rep("2 or more", 3L)))

  # Sibling ADHD diagnosis
# We will also collapse into a Yes/No factor the diagnoses for siblings
# > dt_merged$P81.s |> table()
#     0     1     2     3 
# 13039  1545   111     1 
dt_merged[, P81.s.any := ifelse(P81.s > 0, 1L, 0L)]  
dt_merged$P81.s.any <- as.factor(dt_merged$P81.s.any)


  # Civil status variables
dt_merged[, grep("^cs", names(dt_merged),
                 value = TRUE) := lapply(.SD, as.factor),
          .SDcols = grep("^cs", names(dt_merged),
                         value = TRUE)]

# # To increase unique values for civil status, interact them
# dt_merged$cs.5.mf <- interaction(dt_merged$cs.5.m, dt_merged$cs.5.f)
# dt_merged$cs.5.mf <- droplevels(dt_merged$cs.5.mf)
# dt_merged$cs.6.mf <- interaction(dt_merged$cs.6.m, dt_merged$cs.6.f)
# dt_merged$cs.6.mf <- droplevels(dt_merged$cs.6.mf)
# 
# # Try by collapsing widowed states as they are few:
# 
# maternal_widow   <- grep(".*widdow\\..*",attributes(dt_merged$cs.5.mf)$levels, value = TRUE)
# paternal_widower <- grep(".*\\.widdow.*",attributes(dt_merged$cs.5.mf)$levels, value = TRUE)
# 
# levels(dt_merged$cs.5.mf)[attributes(dt_merged$cs.5.mf)$levels %in% maternal_widow]   <- "widow.x" 
# levels(dt_merged$cs.5.mf)[attributes(dt_merged$cs.5.mf)$levels %in% paternal_widower] <- "x.widower"
# 
# levels(dt_merged$cs.6.mf)[attributes(dt_merged$cs.6.mf)$levels %in% maternal_widow]   <- "widow.x" 
# levels(dt_merged$cs.6.mf)[attributes(dt_merged$cs.6.mf)$levels %in% paternal_widower] <- "x.widower"
# # Still too few values unique for imputation


# Exposure definition
dt_merged[, initiated := factor(med.days.6to6.25 > 0,
                                labels = c("Control", "Initiated"))] 
dt_merged[, initiated_01 := (as.numeric(initiated)-1)] # Numeric 0-1 dummy version for some packages

# Create variable for medication before time-zero indicator
dt_merged[, medicated.pt0 := factor(med.days.p6 > 0, labels = c("Yes", "No"))]

# Create medicated.pct variable for percent of possible days medicated
  # The denominator is set at 3 years instead of max from the data, as some
  # very few cases repeated a year.
  # This has an approximation error of max. 30 days due to grace period after time-zero
dt_merged[, med.days.pct.6to8 := (med.days.6to8 / as.numeric((date.8 - (date.5 + 365))))*100]

# Absence of visits are equivalent to 0 visits in all relevant variables
n.visits_variables <- grep(".*n\\.visits.*", names(dt_merged),
                           value = TRUE)
lapply(n.visits_variables, function(var) {
  set(dt_merged, which(is.na(dt_merged[[var]])), var, 0)
}) |> invisible()


# Create age at tests variables
dt_merged[, `:=`(
  age_date.5  = as.numeric(date.5  - DOB)/365.25,
  age_date.8  = as.numeric(date.8  - DOB)/365.25,
  age_date.9  = as.numeric(date.9  - DOB)/365.25,
  age_date.10 = as.numeric(date.10 - DOB)/365.25)]

# There is considerable variability for age at national tests, which
# widens in consequent years 
# > dt_merged[, .(min = min(na.omit(age_date.5)), max = max(na.omit(age_date.5)),
# mean = mean(na.omit(age_date.5)))]
# min      max
# 1: 8.91718 11.66598
# > dt_merged[, .(min = min(na.omit(age_date.8)), max = max(na.omit(age_date.8)),
# mean = mean(na.omit(age_date.8)))]
# min      max
# 1: 11.00068 14.66667
# > dt_merged[, .(min = min(na.omit(age_date.9)), max = max(na.omit(age_date.9)),
# mean = mean(na.omit(age_date.9)))]
# min      max
# 1:  12 15.66598


# Create negative control exposure
  # Grace period in days
grace_period <- 90
  # How many days after grade 8 national test
    # To guarantee that it happens after and thus
    # that it is a negative control
margin_of_error <- 30

# Those that initiated medication in the grace period after
# grade 8 national test, will have the negative control exposure set to 1
dt_merged[med.start.8to9 >
            {date.8 + margin_of_error} &
            med.start.8to9 <
            {date.8 + {margin_of_error + grace_period}},
          NCE := 1]
# Else, they have a value of 0
dt_merged[is.na(NCE), NCE := 0]

# :::: Scaling of scores --------------------------------------------------

# Until 2014, scores were relative to national average = 50 and std = 10
# We know that since 2014, English and Numeracy scores are standardized 
# with respect to scores in year 2014.
# Since 2016, Reading has been fixed to scores on year 2016
# However further inspection in the original data shows that
# this is not the case for the data we received, raw scores will be used
# and standardized only relative to a given domain ADHD subpopulation.
# At 2022, scaled points are being re-set to 50 and so are no longer comparable
# with previous years


# We will standardize relative to the domain and year of each national test

# Load data.table with mean and standard deviation of scores per year and domain
load(here::here("data", "semiraw", "test_averages.Rdata"))
# Change variable name for consistency
colnames(test_averages)[(colnames(test_averages) == "topic")] <- "domain"
# Change factor levels from Norwegian to English two-letters shorthands
  # LE(sing) to RE(ading)
  # RE(gning) to MA(thematical reasoning)
test_averages$domain <- factor(test_averages$domain,
                               levels = c("EN", "LE", "RE"),
                               labels = c("EN", "RE", "MA")
)

# Loop through each row of test_averages to standardize respective test
for(i in 1:nrow(test_averages)) {
    # Temporarily store domain, grade and year for each row
  domain_i <- test_averages$domain[i]
  grade_i  <- test_averages$grade[i]
  year_i   <- test_averages$year[i]
    # Construct variable name in the target data.table
  score_colname  <- glue::glue("p.{domain_i}_{grade_i}")
  year_colname   <- glue::glue("year.{grade_i}")
  
  # Get the respective mean and standard deviation for that row
  mean_i <- test_averages$m[i]
  sd_i   <- test_averages$sd[i]

  # Update dt_merged by standardizing the scores on-the-fly during join
  dt_merged[get(year_colname) == year_i, 
            (paste0(score_colname, "_std")) := (get(score_colname) - mean_i) / sd_i]
}
  # Remove loop environment variables
rm(domain_i); rm(grade_i); rm(year_i);rm(score_colname);rm(year_colname);rm(mean_i);rm(sd_i); 

# Check the result
dt_merged[, lapply(.SD, function(x) mean(na.omit(x))),
          .SDcols = c(pretest_names_std, posttest_names_std)]

# As we'd expect, we see that the ADHD-diagnosed subsample has below average scores
# (all between averages between -0.70 and between -0.39)
# when standardized relative to the overall population of test takers in a given
# year, grade and domain.

# Discard test_averages data.table
rm(test_averages)

# Create eligible datatables: Remove pupils that are not eligible for any domain
NA_pretests_ids <- which(is.na(dt_merged$p.MA_5) & is.na(dt_merged$p.RE_5) & is.na(dt_merged$p.EN_5))
dt_eligible <- dt_merged[-NA_pretests_ids] # 2,648 removed
dt_dates_eligible <- dt_dates[-NA_pretests_ids]

# Standardize mother's age relative to general eligible dataset
dt_eligible[, m.age_std := scale(m.age)] 


# # Create dt_eligible_DOMAIN
#   # And standardize relative to each subset the test scores
# 
#     # Numeracy
#       # Store ids in a vector
# eligible_MA_ids <- dt_eligible[which(!is.na(dt_eligible$p.MA_5))][["id"]]
#       # Subset
# dt_eligible_MA <- dt_eligible[dt_eligible$id %in% eligible_MA_ids] # 725  less
#       # Standardize
# for(score in unlist(outcome_meta_list$numeracy)){
#   dt_eligible_MA[, (paste0(score, "_std")) := scale(get(score)), 
#               by = byear]
# }
# 
#     # Reading
#       # Store ids in a vector
# eligible_RE_ids <- dt_eligible[which(!is.na(dt_eligible$p.RE_5))][["id"]]
#       # Subset
# dt_eligible_RE <- dt_eligible[dt_eligible$id %in% eligible_RE_ids] # 1204 less
#       # Standardize
# for(score in unlist(outcome_meta_list$reading)){
#   dt_eligible_RE[, (paste0(score, "_std")) := scale(get(score)), 
#               by = byear]
# }
# 
#     # English
#       # Store ids in a vector
# eligible_EN_ids <- dt_eligible[which(!is.na(dt_eligible$p.EN_5))][["id"]]
#       # Subset
# dt_eligible_EN <- dt_eligible[dt_eligible$id %in% eligible_EN_ids] # 2625 less
# for(score in unlist(outcome_meta_list$english)){
#   dt_eligible_EN[, (paste0(score, "_std")) := scale(get(score)), 
#               by = byear]
# }


# :::: Column reduction --------------------------------------------------------
# Define vector with names of covariates to use in imputation model
imputation_covariates <- c(
  "initiated", # We include since consensus is to transform and then impute (von Hippel, 2009). (Even if this is just a transformation of medication variables already present)
  if(standardized_scores == TRUE) {posttest_names_std} else {posttest_names},
  if(standardized_scores == TRUE) {pretest_names_std} else {pretest_names},
  # "s.id8", "s.id5",              # School IDs for random effects. Could not use due to few unique values. Uncongeniality risk
  # avg_school_scores_names,         # School averages for the 3 domains and 3 grades + GPA
  #                                             # We subset the school averages used since
  # "s.avg_5_EN",                # This has 11% missing due to the cohort where there was no grade 5 EN test
  exempt_posttest_names,
  exempt_pretest_names,
  "s.avg_5_RE",   "s.avg_5_MA",  #46% of the eligible sample goes missing with s.avg_10 GPA
  "s.avg_8_EN",   "s.avg_8_RE",   "s.avg_8_MA",  #16% of the eligible sample goes missing with grade 9 school averages
  "Preglength", "birthweight",     # Gestational age in weeks, Weight in grams
  "parity",  "n.children.m",       # Family variables. `n.children.m` uses the future.
  "pre6.P81.father",               # Number of visits for an ADHD diagnosis pre-timezero
  "pre6.P81.mother",               # Number of visits for an ADHD diagnosis pre-timezero
  "ADHD.m", "ADHD.f",              # Indicator for having at least one registration for ADHD
  "pre6.all.mother", "pre6.all.father", # Number of visits pre time-zero for any disorder
  "pre6.int.mother", "pre6.int.father", # Number of visits pre time-zero for internalizing disorders
  "pre6.ext.mother", "pre6.ext.father", # Number of visits pre time-zero for externalizing disorders
  "m.edu", "m.age",                # Mother variables: education and age at birth. `m.edu` uses the future.
  "P81.s.any",                     # Sibling variables: Any sibling with ADHD diagnosis?
  "n.sibs.w.diag",                 # Number of siblings with any diagnosis
  "sex",                           # Child variables: sex,
  "byear", "bmonth", #"age_date.5",# birth year, birth month, age at grade 5 test
  "medicated.pt0", "med.days.p6",  # Medication variables until baseline
  "med.days.6to8", "med.days.8to9",# Medication variables after baseline
  "pre6.n.visits_all",             # Visits for any diagnosis
  "pre6.n.visits_P24",             # Specific learning problem
  "pre6.n.visits_int",             # Internalizing disorders      
  "pre6.n.visits_ext",             # Externalizing disorders  
  "pre6.n.visits_P06",             # Sleep disturbances 
  # Same as above but between baseline and posttest
  "post6to8.n.visits_all",         # Idem
  "post6to8.n.visits_P24",         # Idem
  "post6to8.n.visits_int",         # Idem     
  "post6to8.n.visits_ext"          # Idem  
  )

# Define vector of names for outcome model covariates 
outcome_covariates <- c(
  if(standardized_scores == TRUE) {pretest_names_std} else {pretest_names},
  exempt_pretest_names,
  "parity",                        # Family variables, number of children is omitted (future)
  "pre6.P81.father",               # Father's number of registered visits for an ADHD diagnosis pre-timezero
  "pre6.P81.mother",               # Mother's number of registered visits for an ADHD diagnosis pre-timezero
  "ADHD.m", "ADHD.f",              # Indicator for having at least one registration for ADHD
  "pre6.all.mother", "pre6.all.father", # Number of visits pre time-zero for any disorder
  "pre6.int.mother", "pre6.int.father", # Number of visits pre time-zero for internalizing disorders
  "pre6.ext.mother", "pre6.ext.father", # Number of visits pre time-zero for externalizing disorders
  "m.edu", "m.age",                # Mother variables: education and age at birth
  "P81.s.any",                     # Sibling variables: Any sibling with ADHD diagnosis?
  "n.sibs.w.diag",                 # Number of siblings with any diagnosis
  "sex",                           # Child variables: sex,
  "Preglength", "birthweight",     # Gestational age in weeks, Weight in grams
  "byear", "bmonth", #"age_date.5",# Birth year, birth month, age at grade 5 test
  "pre6.n.visits_all",             # Visits for any diagnosis
  "pre6.n.visits_P24",             # Specific learning problem
  "pre6.n.visits_int",             # Internalizing disorders      
  "pre6.n.visits_ext",             # Externalizing disorders  
  # "pre6.n.visits_P99a",          # Pervasive and specific mental disorders
  "pre6.n.visits_P06",             # Sleep disturbances
  "medicated.pt0", "med.days.p6",  # Medication indicator and number of days until baseline
  # "s.avg_5_EN",                    # Grade 5 school average scores for different domains
  "s.avg_5_RE",
  "s.avg_5_MA",
  # Uncongenial subset:
  "cs.5.m", "cs.5.f",              # Civil status of mother and father (grade 5)
  "cs.6.m", "cs.6.f",              # Civil status of mother and father (grade 6)
  "s.id8", "s.id5"                 # School IDs for random effects
)

# Define congenial subset of variables, common to imputation and outcome vectors
congenial_covariates <- intersect(imputation_covariates, outcome_covariates)
uncongenial_covariates <- setdiff(outcome_covariates, congenial_covariates)

# Create sample that will haveimputed congenial covariates and outcomes
  #  from the eligible sample
  # but also complete case on the uncongenial variables
dt_analytic <- dt_eligible[complete.cases(
  dt_eligible[, .SD, .SDcols = c(uncongenial_covariates)]
)] # n = 11,835


# Create complete case of covariates analytic dataset
dt_completecase <- dt_eligible[complete.cases(
  dt_eligible[, .SD, .SDcols = outcome_covariates])] 

# Create complete case of *a* posttest
dt_completecase_Y <- dt_completecase[complete.cases(dt_completecase[,
                                                                    .SD,
                                                                    .SDcols = c(posttest_names_std[1])])]

# Create complete case of posttests
dt_completecase_Ys <- dt_completecase[complete.cases(dt_completecase[,
                                                                     .SD,
                                                                     .SDcols = c(posttest_names_std)])]

# Vector with only the variable names we will use
variables_of_interest <- union(c("id", "initiated_01",
                                 "med.days.pct.6to8",
                                 "prop_post_exemptions", # For descriptive tables
                                 "n.sibs.w.diag",        # For descriptive tables
                                  outcome_covariates),
                               imputation_covariates)

# Add variable names that we want to keep for special purposes
if (exists(quote(reduction_exclusions))) {
  variables_of_interest <- c(variables_of_interest, reduction_exclusions)
} 


if (descriptive_dataset == TRUE) {
  table1_vars_num <- c('p.MA_5',  'p.EN_5', 'p.RE_5',
                       'p.MA_8', 'p.EN_8',  'p.RE_8', # Future
                       'p.MA_5_std',  'p.EN_5_std', 'p.RE_5_std',
                       'p.MA_8_std', 'p.EN_8_std',  'p.RE_8_std', # Future
                       'p.MA_9',  'p.RE_9', 'p.GPA_10', # Future
                       "n.visits",
                       "age.ADHD.diag",
                       "age_date.5",
                       "m.age",
                       "pre6.n.visits_P24",
                       # "pre6.n.visits_P99a",
                       "pre6.n.visits_P06",
                       "med.days.6to8", # Future
                       "n.blocks.6to8" # Future
  )
  
  dt_desc2 <- dt_merged[
    complete.cases(dt_merged[, .SD, .SDcols = outcome_covariates])
    ,.SD
    , .SDcols = c(table1_vars_num, "initiated", "medicated.pt0")]
  
  dt_desc2_washout <- dt_desc2[medicated.pt0 == "No"]
}



# Comment this block if you want to keep all objects
  # Remove all original data
rm(list = grep("_original",names(.GlobalEnv), value = TRUE))
rm(list = grep("tmp", names(.GlobalEnv), value = TRUE))
rm(dt_dates)

  # Reduce columns
if (reduce_columns == TRUE) {
  dt_merged[, setdiff(colnames(dt_merged),
                      variables_of_interest)  := NULL]
  dt_eligible[, setdiff(colnames(dt_eligible),
                        c(variables_of_interest)) := NULL]
  dt_completecase[, setdiff(colnames(dt_completecase),
                            c(variables_of_interest)) := NULL]
  dt_completecase_Ys[, setdiff(colnames(dt_completecase_Ys),
                            c(variables_of_interest)) := NULL]
}

# Get eligible_IDs for subsetting analytic sample that imputes covariates
eligible_IDs <- dt_eligible$id

  # For joint multivariate analysis, quick solution
dt_eligible_std <- data.table::copy(dt_eligible)
dt_eligible_std[, setdiff(colnames(dt_eligible),
                                         setdiff(variables_of_interest, posttest_names)) := NULL]


  # Collect garbage to free memory
gc()