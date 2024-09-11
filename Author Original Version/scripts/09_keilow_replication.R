# The present script is an attempt to partially replicate
# the effect magnitude found in Keilow et al. (2019) paper:
# "Medical treatment of ADHD and children's academic performance"
# We use similar variables and analysis plan as in their article
# to see if we can replicate the one order of magnitude larger effect.

# If we still find a difference, there are two possible reasons for a stark difference in results.
# One would imply strong weaknesses in our analysis, namely:
# Confounding bias: Our baseline confounding adjustment was severely insufficient
# Or, rather:
# Selection bias: There are severe differences between Danish cohorts of earlier years and our Norwegian cohorts.

# 0. Dependencies ---------------------------------------------------------
library(here)
library(data.table)


# Load additional data files required

# Population means and sds for GPA
load(here::here("data", "semiraw", "test_and_gpa_pop_stats.Rdata"))
dt_pop_GPA <- school.perf.pop.mean_and_sd[variable == "p.GPA_10"]
rm(school.perf.pop.mean_and_sd)

# 1. Preparation ----------------------------------------------------------

  # Make a copy of our largest dataset
dt_keilow <- data.table::copy(dt_merged)

  # Apply exclusion and inclusion criteria
    # Missing GPA are excluded
sum(is.na(dt_keilow$p.GPA_10)) # 7,433/15,189. This excludes 48.9% of the sample
dt_keilow <- dt_keilow[!is.na(p.GPA_10),]
    # Exclude children who initiated 3 months before GPA or less
dt_keilow <- dt_keilow[med.start.6to10 < {dt_keilow$date.10 - 90},] # n = 30 excluded, n = 7,726 left
    # Include children only children who were medicated between grade 6 and 10
dt_keilow <- dt_keilow[med.days.6to10 > 0, ] # n = 1,660 excluded, n = 6,066 left

  # Standardization of scores relative to full population sample
    # Create year for GPA score
dt_keilow[, year_GPA := data.table::year(date.10)]
# > table(dt_keilow$year_GPA)
# 2014 2015 2016 2017 2018 2019 2020 
# 3    2    1206 1180 1192 1254 1229 

    # Will exclude rows for years 2014 (n=3) and 2015 (n=4)
dt_keilow <- dt_keilow[year_GPA != 2014 & year_GPA != 2015] # 6,066 to 6,061
  # For each year for the GPA, standardize the score relative
  # to the full population that got their gpa that year
for (i in unique(dt_pop_GPA$year)) {
  dt_keilow[year_GPA == i,
            p.GPA_10_std := (p.GPA_10 - dt_pop_GPA[year == i,
                                                   m])/
              dt_pop_GPA[year == i, sd]]
}

  # We note that on average the eligible sample has 1 SD
  # lower score than the general population for that year, as expected
dt_keilow[, .(m = mean(p.GPA_10_std),
              sd = sd(p.GPA_10_std)),
          by = year_GPA]
          
#     year_GPA         m        sd
# 1:     2017 -1.051826 0.8660863
# 2:     2018 -1.065998 0.8949887
# 3:     2019 -1.023514 0.9361927
# 4:     2016 -1.069576 0.8971777
# 5:     2020 -1.008740 0.9488654


  # Create exposure variable PT_group (Pharmacological treatment)
    # "Having purchased medication for maximum three months within the data window"
    # are set to Discontinuous Pharmacological Treatment
    # We will ignore for now that some dispensations are
    # enough stock for three months. So we will just
    # stay close to the Keilow operationalization
dt_keilow[med.days.6to10 < 90,
          PT_group := "DPT"] # n = 158

    # For those that had "regular and stable use of medication" (p.5)
    # (since categories are mutually exclusive, we assume this to mean >90 days medicated) 
    # with maximum number of days between dispensions less than 30,
    # their PT group is set to Continuous Pharmacological Treatment

# dt_keilow$med.delta.max.6to10 |> hist()
dt_keilow[med.delta.max.6to10 < 30 & med.days.6to10 > 90,
          PT_group := "CPT"]  # n = 22, much less than Keilow original paper,
# indicating different dispension patterns
          


    # Those that are neither, will be set to
    # Ambiguous Pharmacological Treatment
dt_keilow[is.na(PT_group),
          PT_group := "APT"] # n = 5,881
    # Typing
dt_keilow$PT_group <- as.factor(dt_keilow$PT_group)
dt_keilow$PT_group <- relevel(dt_keilow$PT_group, ref = "CPT")


# We note the drastically different prevalence of people under each ADHD treatment pattern.
# 
# Keilow paper has out of an n = 2,659 for the treatment sample
# 11% for DPT (SD = 0.31) 
# 70% for APT (SD = 0.46)
# 19% for CPT (SD = 0.39)

# We have:

# > {dt_keilow$PT_group |> table() |>  prop.table()} * 100
# 
# CPT        APT        DPT 
# 0.3629764 97.0301930  2.6068306 
  # Store temporal table
tmp <- {{dt_keilow$PT_group |> table() |>  prop.table()} * 100} |> round(digits = 2)


keilow_prev <- data.table("CPT" = c(19, tmp[["CPT"]]),
                          "APT" = c(70, tmp[["APT"]]),
                          "DPT" = c(11, tmp[["DPT"]]),
                          "Pop" = c("Danish", "Norwegian"))
                          
saveRDS(keilow_prev, file = here::here("output", "objects", "keilow_prev.RDS"))
# > keilow_prev
#     CPT   APT   DPT       Pop
# 1: 19.00 70.00 11.00    Danish
# 2:  0.36 97.03  2.61 Norwegian

  # Remove temporal table
rm(tmp)

# ::1.1. Define vectors of variables ------------------------------------------

keilow_controls <- c(
  "sex",
  "ADHD.m", # ADHD diagnosis of mother
  "ADHD.f", # ADHD diagnosis of father
  "m.edu", # Mother's education
  "m.age", # Mother's age (at childbirth??, ask)
  "birthweight", # Birth weight
  "Preglength"  # Gestational age
  )

# "Models with controls also include
# parent age at childbirth,
# unemployment rate, income,
# negative income, as well as
# individual level information on birth weight,
# gestational age,
# age at treatment start,
# and being graded with the new grading scale"


# 2. OLS fit --------------------------------------------------------------

# We will compare to the following results (p.11)
# 
# OLS without controls for teacher evaluation GPA
# 
# -0.12 (0.07)
# -0.09* (0.05) 
# 
# OLS with controls for teacher evaluation GPA
# 
# -0.18** (0.07)
# -0.11* (0.05)


# :: 2.1 Without covariates -------------------------------------------------

  # Use CPT as reference level
fit_lm_unadjusted <- lm(p.GPA_10_std ~ PT_group,
   data = dt_keilow)


summary(fit_lm_unadjusted)
# We could not remotely replicate the magnitude of
# point estimate for the effect
# although we maintain the correct direction


# Their estimated effects for OLS without controls were respectively
# APT = -0.12 (0.05)
# DPT = -0.13 (0.07)

# We have about three times as much a larger standard error
# understandable from the lower counts for DPT and CPT

# Residuals:
#   Min      1Q  Median      3Q     Max 
# -3.5584 -0.5925 -0.0361  0.5730  2.9514 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept) -1.01694    0.19399  -5.242 1.64e-07 ***
# PT_groupAPT -0.02668    0.19436  -0.137    0.891    
# PT_groupDPT -0.02775    0.20706  -0.134    0.893    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.9099 on 6058 degrees of freedom
# Multiple R-squared:  3.153e-06,	Adjusted R-squared:  -0.000327 
# F-statistic: 0.009549 on 2 and 6058 DF,  p-value: 0.9905

# Could difference in cohort birthyears and population be
# the only reasons for such a massive difference in effect?

# In their paper they had that the discontinuation effect
# was of a similar magnitude as the sex gap

lm(p.GPA_10_std ~ sex,
   data = dt_keilow)
# Coefficients:
# (Intercept)         sexm  
# -0.7738          -0.3635  

# > fit_lm_unadjusted$coefficients[["PT_groupDPT"]] / -0.3635
# [1] 0.0763445

# Our crude estimate is nowhere close. 
# Our estimate for DPT (discontinuation) is
#  7.6% of the sex gap


# :: 2.2 With covariates ----------------------------------------------------

  # Make formula
formula_lm <- as.formula(paste("p.GPA_10_std",
                            " ~ PT_group + ",
                            paste(keilow_controls,
                                  collapse = " + ")))

  # Fit OLS
fit_lm_adjusted <- lm(formula_lm, data = dt_keilow)
summary(fit_lm_adjusted)


# Surprisingly, we manage to replicate the magnitude of effect
# once we adjust for covariates. This is different
# from the original article where the unadjusted model
# was comparable to the adjusted one. Furthermore we see
# the increase both in APT and DPT estimates, whereas the original
# Keilow paper only noticed an almost doubling for DPT but no changes for
# the APT point estimate.
# Either adjustment for m.edu or sex are sufficient to 
# increase the effect by one order of magnitude.


# Residuals:
#   Min      1Q  Median      3Q     Max 
# -3.9055 -0.5487 -0.0192  0.5447  3.1934 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept) -4.895e-01  2.940e-01  -1.665   0.0959 .  
# PT_groupAPT -1.609e-01  1.907e-01  -0.844   0.3989    
# PT_groupDPT -1.757e-01  2.025e-01  -0.867   0.3857    
# sexm        -3.658e-01  2.551e-02 -14.338  < 2e-16 ***
# ADHD.mTRUE   7.999e-02  3.786e-02   2.113   0.0347 *  
# ADHD.fTRUE  -8.282e-03  4.383e-02  -0.189   0.8501    
# m.edu.L      6.927e-01  4.055e-02  17.081  < 2e-16 ***
# m.edu.Q      1.374e-01  3.519e-02   3.905 9.54e-05 ***
# m.edu.C      4.623e-02  4.263e-02   1.084   0.2783    
# m.edu^4     -1.333e-01  3.276e-02  -4.070 4.76e-05 ***
# m.age        1.168e-02  2.178e-03   5.365 8.43e-08 ***
# birthweight  1.777e-05  2.312e-05   0.769   0.4421    
# Preglength  -1.133e-02  6.591e-03  -1.718   0.0858 .  
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.8508 on 5774 degrees of freedom
# (274 observations deleted due to missingness)
# Multiple R-squared:  0.1274,	Adjusted R-squared:  0.1256 
# F-statistic: 70.28 on 12 and 5774 DF,  p-value: < 2.2e-16

#  Residuals:
#   Min      1Q  Median      3Q     Max 
# -3.9056 -0.5489 -0.0190  0.5448  3.1936 


# What to conclude then?
# Treatment sample is 2,659 pupils for Keilow
# Our treatment sample is more than double (n = 6,061)
# yet we have much more variable estimates since
# there are muuuuch fewer pupils that do not match into the APT treatment pattern
# We get a similar point estimate for the adjusted case, suggesting it is just a matter
# of difference in estimands. Not too concerned with standard errors since
# we are working with population-level data.

  # Create data.table with effect summaries
keilow_effects <- data.table(
  "mean"  = c(-0.12, -0.18,
              fit_lm_unadjusted$coefficients[["PT_groupDPT"]], fit_lm_adjusted$coefficients[["PT_groupDPT"]])
  ,"se"   = c( 0.07,  0.07,
               summary(fit_lm_unadjusted)$coefficients["PT_groupDPT",]["Std. Error"], summary(fit_lm_adjusted)$coefficients["PT_groupDPT",]["Std. Error"])
  ,"adj"  = c("Unadjusted", "Adjusted",
              "Unadjusted", "Adjusted")
  ,"Pop"  = c("Danish", "Danish",
              "Norwegian", "Norwegian")
)


  # Save it 
saveRDS(keilow_effects, file = here::here("output", "objects", "keilow_effects.RDS"))

# > keilow_effects
#         mean        se        adj       Pop
# 1: -0.12000000 0.0700000 Unadjusted    Danish
# 2: -0.18000000 0.0700000   Adjusted    Danish
# 3: -0.02775123 0.2070587 Unadjusted Norwegian
# 4: -0.17568815 0.2025247   Adjusted Norwegian
