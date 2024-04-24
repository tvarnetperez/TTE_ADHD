# Objective

# To decide on values for hyperparameters:
# Post-exam diagnosis window
# Time-zero
# Grace period

# To examine the trajectories of adherence
library(data.table)

# Dependencies ------------------------------------------------------------

# Get quantile value for vector 'x'
empirical_quantile <- function(x, prob) { # Give cumulative probability, get value: e.g. 0.90 -> 11.25
  if (is.unsorted(x)) x <- sort(x)
  n <- length(x)
  approx(seq(0, 1, length = n), x, prob)$y
}

# Get cumulative prob for vector 'x'
empirical_cdf <- function(x, value) { # Give value, get cumulative probability: e.g. 11.25 -> 0.90
  if (is.unsorted(x)) x <- sort(x)
  n <- length(x)
  {1:n/n}[which(x > value)[1]]
}

# Get mode as in most frequent value for vector 'x'
Mode <- function(x) {
  a <- table(x)
  as.numeric(names(a)[a == max(a)])
}


# First glance -----------------------------------------------------------------

# # School variables
# desc_tmp <- Hmisc::describe(dt_eligible[, .SD, .SDcols = avg_school_scores])
# plot_tmp <- plot(desc_tmp)
# plot_tmp
# # Why the big number of missing GPA scores?
# # https://www.ssb.no/utdanning/artikler-og-publikasjoner/stadig-flere-mangler-grunnskolepoeng
# #Pupils will not receive a final grade if they get special training or tuition,
# #if they are exempted from several subjects or if they have health issues and get
# #exempted from receiving a final grade in some subjects.

# ADHD diagnosis through time ----------------------------------------------
# ADHD diagnosis is ICPC-2 code P81 "Hyperkinetic disorder" ( ICD-10 code F90)

# Note here we are using X[Y] merges or joins.
# " chaining creates a subsetted dataset which isn't anymore linked by reference to the original dataset"
dt_adhd_diags <- dt_diags[icpc == "P81"][
  , c("inclusion.date", "med.start") := NULL][
    dt_eligible, .(age.ADHD.diag  = as.numeric(first.visit - DOB)/365.25,
                   time.ADHD.diag = as.numeric(first.visit - date.5)/365.25,
                   id = id), on = "id"]

# Updated data, using all diagnosis until end of follow-up (date.8):
# Set ADHD diagnosis date to NA if it is after end of follow-up (date.8 + margin_of_error)
dt_diags_all$date.ADHD <- replace(dt_diags_all$date.ADHD,
                                  dt_diags_all$date.ADHD > (dt_diags_all$date.8 + end_days_margin),
                                  NA_real_)
# Create time since pretest until ADHD diagnosis variable
dt_diags_all[, time.5.ADHD.diag := as.numeric(date.ADHD - date.5)/365.25]


#  and latest first.visit is "2021-09-13" (which depends on the date.5 of the subject)
# Should it even be possible to get an ADHD diagnosis before age 4?
# 0.86% (133 out of 15430 non-NA) have an ADHD diagnosis before age 4, earliest is age 2.
# # Pending ask Kristin

# Define either the cumulative percentage you want (scalar) or the value cutoff
# t0_cutoff     <- 0.90
t0_cutoff             <- empirical_cdf(x = na.omit(dt_diags_all$time.5.ADHD.diag), value = 1)
time_after5_t0_cutoff <- empirical_quantile(x = na.omit(dt_diags_all$time.5.ADHD.diag), prob = t0_cutoff)
time_after5_t0_cutoff
# 90% at 1.75 years after grade 5 national test
# 85% at 1.33 years after grade 5 national test
# 80% at 0.91 years after grade 5 national test
# 70% at 0.19 years after grade 5 national test 

# Histogram and eCDF for first visit by time after grade 5 in subgroup
# of children ever diagnosed with ADHD during study follow-up
temp_plot <- ggplot(data = dt_diags_all, aes(x = time.5.ADHD.diag)) +
  geom_histogram(bins = 100, alpha = alpha_plot)
# Get max count from histogram geom, which is the 1st since it's the only one
max_count <- ggplot_build(temp_plot)$data[[1]]$count |> max()
hist_n    <- format(ggplot_build(temp_plot)$data[[1]]$count |> sum(), big.mark = ",")
# Adjust scale
temp_plot <- temp_plot + stat_ecdf(aes(y = after_stat(y)*max_count),
                                   linewidth = linewidth_plot) +
  scale_x_continuous(breaks = seq(-6, 3, 2)) +
  scale_y_continuous(sec.axis = sec_axis(trans  =  ~./max_count,
                                         name   = "Cumulative Percentage",
                                         labels = scales::percent, breaks = seq(0, 1, 0.20))) +
  geom_segment(aes(x = time_after5_t0_cutoff,  y = t0_cutoff*max_count,
                   xend = Inf, yend = t0_cutoff*max_count),
               linewidth = linewidth_plot,
               linetype = "dashed",
               col = "gray45") +
  geom_segment(aes(x = time_after5_t0_cutoff,  y = t0_cutoff*max_count,
                   xend = time_after5_t0_cutoff, yend = -Inf),
               linewidth = linewidth_plot,
               linetype = "dashed",
               col = "gray45") +  
  # geom_hline(yintercept = t0_cutoff * max_count,
  #            linetype = "dashed", linewidth = linewidth_plot, col = "gray45") +
  # geom_vline(xintercept = time_after5_t0_cutoff,
  #            linetype = "dashed", linewidth = linewidth_plot, col = "gray45") +
  labs(title    = stringr::str_wrap("Empirical cumulative distribution function
                                    and histogram for time at first diagnosis", wrapping_length),
       subtitle = stringr::str_wrap(
         glue::glue("Dashed lines indicate that 
                    {round(time_after5_t0_cutoff, 2)} year(s) after the grade 5 national test,
                    {round(t0_cutoff*100, 2)}% of ADHD-diagnosed children
                    would have already had their first ADHD visit.
                    Non-missing sample size for histogram is
                    {hist_n}."), wrapping_length),
       y        = "Count",
       x        = "Time (in years) from grade 5 national test until first diagnosis appointment",
       caption  = "Note: Diagnoses after end of follow-up period were excluded.") #+ khroma::scale_color_highcontrast()
if (print_all == TRUE) {print(temp_plot)}
if (save_all == TRUE){
  ggsaver(plot = temp_plot,
          path = here::here("output", "descriptives"),
          filename = "t0_cutoff_ecdfhist",
          manuscript_mode = TRUE)}


# Copy to export folder outside TSD virtual machine
file.copy(from = here::here("output", "descriptives", "t0_cutoff_ecdfhist_mm.pdf"),
          to =  "N:\\durable\\file-export\\new", overwrite = TRUE)

# Store text for programmatic text script
ecdf_t0dx_text <- glue::glue("
Dashed lines indicate that \\
{round(time_after5_t0_cutoff, 2)} year(s) after the grade 5 national test, \\
{round(t0_cutoff*100, 2)}\\% of ADHD-diagnosed children \\
would have already had their first ADHD visit. \\
Non-missing sample size for histogram is \\
{hist_n}.
"
)

  # Save
saveRDS(ecdf_t0dx_text, here::here("output", "descriptives",
                                    "ecdf_t0dx_text.RDS"))


# Medication initiation through time ------------------------------

# :: Full sample (no wash-out period) ---------------------------------

# Full sample (including those that never received medication in the denominator)
tmp_1st_dispension <- dt_dates_eligible$time_after5_1st_dispension
tmp_1st_dispension <- ifelse(is.na(tmp_1st_dispension), 99, tmp_1st_dispension )
empirical_cdf(x = tmp_1st_dispension, value = 1) # 72.6%
rm(tmp_1st_dispension)

# Subset of those ever to receive medication (hence the na.omit)
# Choose value based on cdf or quantile
t0_cutoff     <- empirical_cdf(x = na.omit(dt_dates_eligible$time_after5_1st_dispension), value = 1) # At what age have XX% of dispensions occurred?
time_t0_cutoff <- empirical_quantile(x = na.omit(dt_dates_eligible$time_after5_1st_dispension), prob = t0_cutoff)
time_t0_cutoff
# 0.95  = 1.497
# 0.925 = 1.256
# 0.90  = 1.065
# 0.85  = 0.665


temp_plot <- ggplot(data = dt_dates_eligible, aes(x = time_after5_1st_dispension)) +
  geom_histogram(bins = 100, alpha = alpha_plot)
# Get max count from histogram geom, which is the 1st since it's the only one
max_count <- ggplot_build(temp_plot)$data[[1]]$count |> max()
hist_n    <- format(ggplot_build(temp_plot)$data[[1]]$count |> sum(), big.mark = ",")
# Adjust scale
temp_plot <- temp_plot + stat_ecdf(aes(y = after_stat(y)*max_count),
                                   linewidth = linewidth_plot) +
  scale_x_continuous(breaks = seq(-6, 3, 2)) +
  scale_y_continuous(sec.axis = sec_axis(trans  =  ~./max_count,
                                         name   = "Cumulative Percentage",
                                         labels = scales::percent, breaks = seq(0, 1, 0.20))) +
  geom_segment(aes(x = time_t0_cutoff,  y = t0_cutoff*max_count,
                   xend = Inf, yend = t0_cutoff*max_count),
               linewidth = linewidth_plot,
               linetype = "dashed",
               col = "gray45") +
  geom_segment(aes(x = time_t0_cutoff,  y = t0_cutoff*max_count,
                   xend = time_t0_cutoff, yend = -Inf),
               linewidth = linewidth_plot,
               linetype = "dashed",
               col = "gray45") +
  # geom_hline(yintercept = t0_cutoff * max_count,
  #            linetype = "dashed", linewidth = linewidth_plot, col = "gray45") +
  # geom_vline(xintercept = time_t0_cutoff,
  #            linetype = "dashed", linewidth = linewidth_plot, col = "gray45") +
  labs(title    = stringr::str_wrap("Empirical cumulative distribution function
                                    and histogram for time at first dispension", wrapping_length),
       subtitle = stringr::str_wrap(glue::glue("Dashed lines indicate that at
                                               {round(time_t0_cutoff, 2)} years after the grade 5 national test,
                                               {round(t0_cutoff,2)*100}% of medicated children
                                               would have already received their
                                               first dispensation.
                                               Non-missing sample size for histogram is
                                               {hist_n}."), wrapping_length),
       y        = "Count",
       x        = "Time (in years) from grade 5 national test until first dispension",
       caption  = "Note: Dispensions after end of follow-up period were excluded.") #+ khroma::scale_color_highcontrast()

if (print_all == TRUE) {print(temp_plot)}

if (save_all == TRUE) {
  ggsaver(plot = temp_plot,
          path = here::here("output", "descriptives"),
          filename = "graceperiod_ecdfhist", manuscript_mode = manuscript_mode)
}


# Copy to export folder outside TSD virtual machine
file.copy(from = here::here("output", "descriptives", "graceperiod_ecdfhist_mm.pdf"),
          to =  "N:\\durable\\file-export\\new", overwrite = TRUE)

# Copy to export folder outside TSD virtual machine

# Store text for programmatic text script
ecdf_graceperiod_text <- glue::glue(
"Dashed lines indicate that at \\
{round(time_t0_cutoff, 2)} years after the grade 5 national test, \\
{round(t0_cutoff,2)*100}\\% of medicated children \\
would have already received their \\
first dispensation. \\
Non-missing sample size for histogram is \\
{hist_n}.
"
)


# Save
saveRDS(ecdf_graceperiod_text, here::here("output", "descriptives",
                                   "ecdf_graceperiod_text.RDS"))                                               

# :: Not previously medicated  -------------------------------

# For each pupil, they cannot have been medicated before 'personal_cut_off_prevmed',
# i.e. one year after their grade 5 exam.
dt_dates_eligible[, personal_cutoff_prevmed := date.5 + lubridate::years(1)]

t0_cutoff     <- 0.50 # 90% of dispensions should be at this age
time_t0_cutoff <- empirical_quantile(x = na.omit(dt_dates_eligible[date.1st.dispension > personal_cutoff_prevmed, time_after5_1st_dispension]),
                                     prob = t0_cutoff)
time_t0_cutoff
# 0.95 = 12.95
# 0.90 = 12.66 
# 0.85 = 12.46 
# 0.80 = 12.29
# 0.50 = 11.69

# Only 20% of the never-medicated at the same cutoff of the 90% at the full sample
empirical_cdf(na.omit(dt_dates_eligible[date.1st.dispension > personal_cutoff_prevmed, time_after5_1st_dispension]),
              value = 11.25)

bla <- dt_dates_eligible[date.1st.dispension > personal_cutoff_prevmed, time_after5_1st_dispension]

temp_plot <- ggplot(data = dt_dates_eligible[date.1st.dispension > personal_cutoff_prevmed], aes(x = time_after5_1st_dispension)) +
  geom_histogram(bins = 100, alpha = alpha_plot)
# Get max count from histogram geom, which is the 1st since it's the only one
max_count <- ggplot_build(temp_plot)$data[[1]]$count |> max()
hist_n    <- format(ggplot_build(temp_plot)$data[[1]]$count |> sum(), big.mark = ",")

# Adjust scale
temp_plot <- temp_plot + stat_ecdf(aes(y = after_stat(y)*max_count),
                                   linewidth = linewidth_plot) +
  scale_x_continuous(breaks = seq(10, 20, 1)) +
  scale_y_continuous(sec.axis = sec_axis(trans  =  ~./max_count,
                                         name   = "Cumulative Percentage",
                                         labels = scales::percent, breaks = seq(0, 1, 0.20))) +
  geom_hline(yintercept = t0_cutoff * max_count,
             linetype = "dashed", linewidth = linewidth_plot, col = "gray45") +
  geom_vline(xintercept = time_t0_cutoff,
             linetype = "dashed", linewidth = linewidth_plot, col = "gray45") +
  labs(title    = stringr::str_wrap("Empirical cumulative distribution function
                                    and histogram for time at first dispension for previously unmedicated subgroup", wrapping_length),
       subtitle = stringr::str_wrap(glue::glue("Dashed lines indicate that at
                                               {round(time_t0_cutoff, 2)} years after the grade 5 national test,
                                               {t0_cutoff*100}% of medicated children
                                               would have already received their
                                               first dispensation.
                                               Non-missing sample size for histogram is
                                               {hist_n}."), wrapping_length),
       y        = "Count",
       x        = "Time (in years) from grade 5 national test until first dispension",
       caption  = "Note: Dispensions after end of follow-up period were excluded.") #+ khroma::scale_color_highcontrast()

if (print_all == TRUE) {print(temp_plot)}

if (save_all == TRUE) {
  ggsave(plot     = temp_plot,
         path     = here::here("output"),
         filename = glue::glue("washout_firstdispension_histecdf.{output_format}"),
         device   = output_format,
         height   = large_plot[1],
         width    = large_plot[2],
         units    = "in"
  )
  
}




# Adherence trajectory heterogeneity --------------------------------------------
# 
# tmp = 
#   dt_copy %>% 
#   .[med.pre6 == "No" & med.perc.6to8 > 0] %>% 
#   .[, days.treat2test.6to8 := as.numeric(date.8 - med.start.6to8)] %>% 
#   .[, med.perc.6to8.since.start := med.days.6to8/days.treat2test.6to8] %>% 
#   .[days.treat2test.6to8 < 750]
# 
# tmp %>%
#   ggplot(aes(x = med.perc.6to8)) + 
#   geom_histogram(bins = 25) +
#   xlab("proportion of days treated from grade 6 t0 8")
# 
# tmp[, `:=`(possible_to_max_ratio = (days.treat2test.6to8 / max(days.treat2test.6to8)) )] 
# # This is the ratio of how many days of treatment were possible compared to the
# # maximum of days possible if they had started immediately at the beginning
# tmp[, med_to_possible_ratio := med.perc.6to8 / possible_to_max_ratio ]
# tmp[med_to_possible_ratio > 1, med_to_possible_ratio := 1]
# # This is the ratio of the medicated days to possible days.
# # Because we are getting rid of the max denominator: med/max / pos/max
# # We cap at 1, although there are 40 values that go over 1, and multiple
# # that are close to 2.
# 
# # Empirical CDF
# ggplot(data = tmp, aes(med_to_possible_ratio)) +
#   stat_ecdf(geom = "step") +
#   labs(x = "Proportion of medicated days out of available days after initiation"
#        , y = "P(X < x)") 
# 
# # We show the percentage of medicated days to total, against the possible days to total ratio
# # the slope marks the 0.85
# 
# max_percentile = 0.90
# min_percentile = 0.75
# num_of_lines = 4
# 
# lines_df <- data.frame(slopes     = seq(from = max_percentile, to = min_percentile, length.out = num_of_lines)
#                        , intercepts = rep(0, num_of_lines)
#                        , id         = letters[1:num_of_lines]
# )
# 
# lines_df$perc_below <- {sapply(lines_df$slopes, function(x) sum(tmp$med_to_possible_ratio < x) / nrow(tmp) )*100} |> round(1)
# 
# 
# 
# ggplot(data = tmp, aes(x = possible_to_max_ratio, y = med.perc.6to8)) +
#   geom_point(alpha = 0.2) +
#   geom_segment(data = lines_df, aes(x = 0, xend = 1.015
#                                     , y = intercepts + slopes*0
#                                     , yend = intercepts + slopes*1.025
#                                     , color = id), size = 1.5, alpha = 0.8, linetype = "dotdash") +
#   geom_text(data = lines_df, aes(1.02, slopes + 0.02, color = id,
#                                  label = paste0(perc_below, "% of the sample below"))
#             , hjust = 0) +
#   labs(y = stringr::str_wrap("Proportion of medicated days  out of total days between grade 6 and 8", 50)
#        , x = stringr::str_wrap("Proportion of time to treat versus total period time", 50)) + 
#   khroma::scale_color_discreterainbow() +
#   scale_x_continuous(minor_breaks = seq(0, 1, length.out = 9), breaks = seq(0, 1, length.out = 5), limits = c(0, 1.2)) +
#   theme(legend.position = "none")


# Table 1 Descriptives by Exposure Status ---------------------------------

stratifying_variable <- "initiated"
# stratifying_variable <- "medicated.pt0"


# Define sample on which descriptives will be calculated

# Rename to not alter original code
dt_desc <- data.table::copy(dt_analytic)

# For categorical variables
# :::::::::::::::::::::::::

# Make function to compute frequencies by strata and overall
make_freq_table = function(dt, freq_variable, x_axis) {
  
  # Copy to avoid changing the original by reference
  temp_dt <- data.table::copy(dt)
  # Store the original variable name
  temp_dt[, original_name := freq_variable]
  # The variable we want to get frequencies from will now be called "group"
  data.table::setnames(temp_dt, freq_variable, "group")[, group := as.character(group)]
  # The variable we want to have the columns by will now be called "x_axis"
  data.table::setnames(temp_dt, x_axis, "x_axis")
  
  # Generate the values of the freq_variable for each x_axis group
  tmp = temp_dt[, .(N = .N), by = .(group, x_axis, original_name)]
  # Add the '99' group for the overall sample, i.e. collapsing the x_axis groups
  tmp = rbind(tmp,
              tmp[, .(N = sum(N)), by = .(group, original_name)][, x_axis := 99]) 
  
  # Create the data.table with percent and number of observations
  tmp <- tmp[
    , percent := N/sum(N), by = .(x_axis, original_name)][
      , stat_char := paste0(round(percent*100, 1), "% (n = ", N, ")")
    ] |> 
    data.table::dcast(group + original_name ~ x_axis, value.var = "stat_char")
  
  # Check if 'group' was numeric in the original dt and sort accordingly
  # Use base::order() rather than setorder() to push <NA> as last row
  if(is.numeric(dt[[freq_variable]])) {
    tmp <- tmp[order(as.numeric(group), original_name)]
  } else {
    tmp <- tmp[order(group, original_name)]
  }
  
  # Retain only one column of original_name
  tmp <- tmp[, !duplicated(colnames(tmp)), with = FALSE]
  
  # Replace all but the first occurrence of each original_name with an empty string
  # This is done across the entire dataset
  tmp[, original_name := ifelse(!duplicated(original_name), original_name, "")]
  
  # Move original_name to be the first column
  setcolorder(tmp, c("original_name", setdiff(names(tmp), "original_name")))
  
  return(tmp)
}

# Vector of categorical variable names
table1_vars_cats <- c("sex", 
                      "bmonth", "byear",
                      "parity", "n.sibs.w.diag",
                      "cs.6.m", "cs.6.f",
                      "prop_post_exemptions",
                      exempt_pretest_names,
                      exempt_posttest_names
)                 
# # Unnecessary step with original objects, as they are data.tables from the beginning
# dt_desc <- as.data.table(dt_desc)

dt_desc$bmonth <- dt_desc$bmonth |> as.numeric()

# First generate a list with the frequency for each variable in `table1_vars_cats`
combined_tables <- lapply(table1_vars_cats, function(x) {
  # Create frequency table for each variable
  freq_table <- make_freq_table(dt_desc, x, stratifying_variable)
  
  # Add an empty row after each variable for separation
  rbind(freq_table, setNames(replicate(ncol(freq_table),
                                       ""
                                       , simplify = FALSE # So it returns a list, where each elements is its own column, so dimension match
  ), 
  names(freq_table)))
})


# Create raw combined_tables object to facilitate programmatic text
  # Add names to facilitate access by variable name
names(combined_tables) <- table1_vars_cats
saveRDS(combined_tables, file = here::here("output", "descriptives", glue::glue("freq_{substr(stratifying_variable, 1, 4)}_raw.RDS")))

# Combine all tables
final_table <- do.call(rbind, combined_tables)

# Remove last empty row
final_table <- final_table[-dim(final_table)[1],]
final_table

# Recode factors to expand abbreviations into final format
final_table$original_name <- forcats::fct_recode(final_table$original_name,
                                                 "Sex" = "sex",
                                                 "Birth month" = "bmonth",
                                                 "Birth year" = "byear",
                                                 "Parity" = "parity",
                                                 "\\makerow{3}{Num. siblings with \\\\ any health diag.}" = "n.sibs.w.diag",
                                                 "\\makerow{3}{Mother's civil \\\\ status at grade 6}" = "cs.6.m",
                                                 "\\makerow{3}{Father's civil \\\\ status at grade 6}" = "cs.6.f",
                                                 "\\makerow{3}{Proportion of \\\\ missing posttests}" = "prop_post_exemptions",
                                                 "\\multirow{3}{0pt}{\\makecell[l]{Missing English \\\\ (Grade 5) score}}" = "exempt.p.EN_5",
                                                 "\\multirow{3}{0pt}{\\makecell[l]{Missing Reading \\\\ (Grade 5) score}}" = "exempt.p.RE_5",
                                                 "\\multirow{3}{0pt}{\\makecell[l]{Missing Numeracy \\\\ (Grade 5) score}}" = "exempt.p.MA_5",
                                                 "\\multirow{3}{0pt}{\\makecell[l]{Missing English  \\\\ (Grade 8) score}}" = "exempt.p.EN_8",
                                                 "\\multirow{3}{0pt}{\\makecell[l]{Missing Reading  \\\\ (Grade 8) score}}" = "exempt.p.RE_8",
                                                 "\\multirow{3}{0pt}{\\makecell[l]{Missing Numeracy \\\\ (Grade 8) score}}" = "exempt.p.MA_8"
                                                 )
final_table$group <-  forcats::fct_recode(final_table$group,
                                          "Female" = "f",
                                          "Male" = "m",
                                          "Married or in partnership" = "mar_par",
                                          "Divorced or separated" = "sep_div",
                                          "Unmarried" = "unmarried",
                                          "Widow or Widower" = "widdow",
                                          "$\\frac{1}{3}$" = "0.333333333333333",
                                          "$\\frac{2}{3}$" = "0.666666666666667",
                                          "Missing" = "TRUE",
                                          "Non-missing" = "FALSE")

# Rename column names to proper table format
setnames(final_table,
         c("99", "original_name", "group"),
         c("Total", "Variable", "Value"))

# LaTeXify
latex_freq <- stargazer::stargazer(final_table,
                                   summary = FALSE,
                                   digits = 2,
                                   rownames = FALSE)

# Store LaTeX text object
writeLines(latex_freq,
            con = here::here("output", "descriptives", glue::glue("{substr(stratifying_variable, 1, 4)}_freqtable.txt"))
)

# Copy into export folder outside virtual machine
file.copy(from = here::here("output", "descriptives", glue::glue("{substr(stratifying_variable, 1, 4)}_freqtable.txt")),
          to =  "N:\\durable\\file-export\\new", overwrite = TRUE)


# Remove table
rm(final_table)

# For numerical variables
# :::::::::::::::::::::::

# Make function to compute descriptives by strata and overall
make_desc_table <- function(dt, continuous_variable, x_axis, num_digits = 2) {
  # Get mode as in most frequent value for vector 'x'
  Mode <- function(x) {
    a <- table(x)
    as.numeric(names(a)[a == max(a)])
  }
  
  
  # Copy to avoid changing the original by reference
  temp_dt <- data.table::copy(dt)
  # Store the original variable name
  temp_dt[, original_name := continuous_variable]
  # The variable we want to get descriptives from will now be called "group"
  data.table::setnames(temp_dt, continuous_variable, "var")
  # The variable we want to have the columns by will now be called "x_axis"
  data.table::setnames(temp_dt, x_axis, "x_axis")
  
  # Generate for the values of the continuous_variable for each x_axis group
  tmp = temp_dt[, .(na_perc   = {length(var[is.na(var)])/.N} * 100,
                    mean      = mean(var, na.rm = TRUE),
                    sd        = sd(var, na.rm = TRUE),
                    median    = round(median(var, na.rm = TRUE), num_digits),
                    mode      = round(Mode(var), num_digits),
                    iqr       = round(IQR(var, na.rm = TRUE), num_digits),
                    range_low = (round(range(var, na.rm = TRUE), num_digits))[1],
                    range_hi  = (round(range(var, na.rm = TRUE), num_digits))[2]),
                by = .(x_axis, original_name)]
  # Compute for the total sample, i.e. collapsing the x_axis groups
  tmp = rbind(tmp,
              temp_dt[, .(na_perc   = 100*{length(var[is.na(var)])/.N},
                          mean      = mean(var, na.rm = TRUE),
                          sd        = sd(var, na.rm = TRUE),
                          median    = round(median(var, na.rm = TRUE), num_digits),
                          mode      = round(Mode(var), num_digits),
                          iqr       = round(IQR(var, na.rm = TRUE), num_digits),
                          range_low = (round(range(var, na.rm = TRUE), num_digits))[1],
                          range_hi  = (round(range(var, na.rm = TRUE), num_digits))[2]
              ), by = original_name][, x_axis := "Total"]) 
  
  if(length(tmp$mode) > length(levels(tmp$x_axis))) 
    warning("There is more than one mode in  `continuous_variable`, duplicated rows will be created.")
  
  # Replace all but the first occurrence of each original_name with NA
  # This is done across the entire dataset
  tmp[, original_name := ifelse(!duplicated(original_name), original_name, "")]
  
  # Create the formatted data.table with percent and number of observations
  tmp <- tmp[
    , .(original_name,
        x_axis = x_axis,
        na_perc = round(na_perc, 2),
        mean_sd = paste0(round(mean, num_digits), " (", round(sd, num_digits) ,")"),
        median_iqr = paste0(median, " (", iqr, ")"),
        mode_range = paste0(mode, " [", range_low," ; ", range_hi, "]"))]
  return(
    tmp
  )
}

# Vector of numerical variable names
table1_vars_num <- c('p.MA_5',  'p.EN_5', 'p.RE_5',
                     'p.MA_8', 'p.EN_8',  'p.RE_8', 
                     'p.MA_5_std',  'p.EN_5_std', 'p.RE_5_std',
                     'p.MA_8_std', 'p.EN_8_std',  'p.RE_8_std', 
                     # Future
                     # 'p.MA_9',  'p.RE_9', 'p.GPA_10', # Future
                     # "n.visits",
                     # "age.ADHD.diag",
                     "age_date.5",
                     "m.age",
                     "pre6.n.visits_P24",
                     # "pre6.n.visits_P99a",
                     "pre6.n.visits_P06",
                     "med.days.6to8", # Future
                     "n.blocks.6to8" # Future
)

  # Rbind the descriptive tables for every variablein `table1_vars_num`
desc_table <- do.call(
  rbind,
  lapply(table1_vars_num, function(x) make_desc_table(dt_desc,
                                                      x,
                                                      stratifying_variable))
)

# First generate a list with the frequency for each variable in `table1_vars_cats`
combined_tables <- lapply(table1_vars_num, function(x) {
  # Create frequency table for each variable
  desc_table <- make_desc_table(dt_desc, x, stratifying_variable)
  
  # Add an empty row after each variable for separation
  rbind(desc_table, setNames(replicate(ncol(desc_table),
                                       ""
                                       , simplify = FALSE # So it returns a list, where each elements is its own column, so dimension match
  ), 
  names(desc_table)))
})

# Combine all tables
final_table <- do.call(rbind, combined_tables)


# Replace 0.00 missing percentage as an em-dash for the table
final_table$na_perc <- ifelse(final_table$na_perc == 0, "---", final_table$na_perc)


# Repeat original variable name for ease of indexing in programmatic text script 
tmp_table <- data.table::copy(final_table)

  # Function that applies only to our own specific case, as we repeat the
  # variable name twice and leave one row empty for readability
repeater <- function(vec){
# Identify non-empty strings
non_empty <- vec != ""

# Shift the identification vector to find positions to fill
to_fill_1 <- shift(non_empty, n = -1, type = "lead", fill = FALSE)
to_fill_2 <- shift(non_empty, n = -2, type = "lead", fill = FALSE)

# Combine conditions where either the current position is non-empty
# or one of the next two positions should be filled
should_fill <- non_empty | to_fill_1 | to_fill_2

# Now, create a new vector where these conditions are met
# Initialize with empty strings
new_vec <- rep("", length(vec))

# Fill in the new vector based on conditions
new_vec[should_fill] <- vec[non_empty][cumsum(non_empty)[should_fill]]

# Output the new vector
new_vec
}
tmp_table$original_name <- repeater(tmp_table$original_name)

# Save and remove temporal object
saveRDS(tmp_table, file = here::here("output", "descriptives", glue::glue("desc_{substr(stratifying_variable, 1, 4)}_raw.RDS")))
rm(tmp_table)

# Recode factor level names
final_table$original_name <- forcats::fct_recode(
  final_table$original_name,
  'Numeracy (Grade 5) raw'                 = "p.MA_5"
  ,'English  (Grade 5) raw'                = "p.EN_5"
  ,'Reading  (Grade 5) raw'                = "p.RE_5"
  ,'Numeracy (Grade 8) raw'                = "p.MA_8"
  ,'English  (Grade 8) raw'                = "p.EN_8"
  ,'Reading  (Grade 8) raw'                = "p.RE_8"
  ,'Numeracy (Grade 5) std.'       = "p.MA_5_std"
  ,'English  (Grade 5) std.'       = "p.EN_5_std"
  ,'Reading  (Grade 5) std.'       = "p.RE_5_std"
  ,'Numeracy (Grade 8) std.'       = "p.MA_8_std"
  ,'English  (Grade 8) std.'       = "p.EN_8_std"
  ,'Reading  (Grade 8) std.'       = "p.RE_8_std"
  # ,'Numeracy (Grade 9)'                = "p.MA_9"
  # ,'English  (Grade 9)'                = "p.EN_9"
  # ,'Reading  (Grade 9)'                = "p.RE_9"
  # ,'GPA (Grade 10)'                    = "p.GPA_10"
  ,'Visits ADHD'                       = "n.visits"
  ,'Age first ADHD diagnosis'          = "age.ADHD.diag"
  ,'Age at grade 5 national test'      = "age_date.5" 
  ,"Mother's age at birth"             = "m.age"  
  ,'Visits learning problems'          = "pre6.n.visits_P24" 
  ,'Visits sleeping disorders'         = "pre6.n.visits_P06" 
  ,'Est. medicated days'               = "med.days.6to8" 
  # ,'Visits specifics mental disorders' =
  ,'Est. number of blocks'             = "n.blocks.6to8"
)

# Rename column names to proper table format
setnames(final_table,
         old = c("na_perc", "mean_sd", "median_iqr", "mode_range"),
         new = c("Missing \\%","Mean (SD)" ,"Median (IQR)" ,"Mode [Min ; Max]")
)

# The descriptives for the initiated group shows that there is not much overlap
# as a priori suspected.
# The mean number of days medicated for the initiated is 616.11, with a median of 698
# Whereas the mean number of days medicated for control is 85, with a median of 0

# Get LaTeX code for table
latex_desc <- stargazer::stargazer(final_table,
                                   summary = FALSE,
                                   digits = NA,
                                   rownames = FALSE)

# clean_latex <- function(latex_text){}
latex_desc <- gsub(pattern = "\\\\textbackslash ", "", x = latex_desc) |> 
gsub(pattern = "\\\\_", "_", x = _)

writeLines(latex_desc,
           con = here::here("output", "descriptives", glue::glue("{substr(stratifying_variable, 1, 4)}_desctable.txt")))

# Copy into export folder outside virtual machine
file.copy(from = here::here("output", "descriptives", glue::glue("{substr(stratifying_variable, 1, 4)}_desctable.txt")),
          to =  "N:\\durable\\file-export\\new", overwrite = TRUE)

# Return any characters after 2 in the string
# stringr::str_extract(deparse(quote(dt_desc2_washout)), "(?<=2).*")

# Medicated days by treatment arm histogram -------------------------------

# Alias to the actual dataset we will use
dt <- dt_analytic

#::: Same but for percent of days

y_intercept_text <- 2700L
x_offset_text <- 5L
num_bins <- 15L 

# Create data.tables for plots
summary_dt <- dt[, .(mean = mean(med.days.pct.6to8)),
                 by = c("initiated")]
summary_dt$sample <- c("Analytic")
summary_dt_washout <- dt[medicated.pt0 == "No",
                         .(mean = mean(med.days.pct.6to8)),
                         by = c("initiated")]
summary_dt_washout$sample <- c("Washout")

# Join in a single dt object
med_perc_dt <- rbind(summary_dt, summary_dt_washout)
# Save RDS object
saveRDS(med_perc_dt, here::here("output", "descriptives", "med_perc_dt.RDS"))

pA <- ggplot(data = dt,
             aes(x = med.days.pct.6to8,
                 # color = initiated
                 fill = initiated)
) +
  geom_histogram(alpha = 0.6, position = "identity", bins = num_bins, aes(color = NULL)) +
  geom_vline(data = summary_dt,
             aes(xintercept = mean, color = initiated),
             linetype = "dashed",
             linewidth = linewidth_plot) +
  geom_text(aes(x     = summary_dt[initiated == "Control"]$mean - x_offset_text ,
                y     = y_intercept_text,
                label = glue::glue("Mean: \n{summary_dt[initiated == 'Control']$mean |> round(1)}%")),
            colour= rgb(0.000, 0.266, 0.531), alpha = 1) +
  geom_text(aes(x     = summary_dt[initiated == "Initiated"]$mean - x_offset_text ,
                y     = y_intercept_text,
                label = glue::glue("Mean: \n{summary_dt[initiated == 'Initiated']$mean |> round(1)}%")),
            colour = rgb(0.863, 0.664, 0.199), alpha = 1) +
  labs(y = "Count") +
  coord_cartesian(ylim = c(0L, 3000L),
                  xlim = c(0L, 100L)) +
  theme(axis.title.x = element_blank()) + ggtitle(label = "a") +
  scale_y_continuous(breaks = seq(0, 3000, by = 1000)) +
  khroma::scale_color_highcontrast(guide = "none") +
  khroma::scale_fill_highcontrast(guide = "none")


pB <- ggplot(data = dt[medicated.pt0 == "No"],
             aes(x = med.days.pct.6to8,
                 # color = initiated,
                 fill = initiated)) +
  geom_histogram(alpha = 0.6, position = "identity", bins = num_bins) +
  labs(x = stringr::str_wrap("Estimated percent of medicated days betwen time
                             zero and grade 8 national test",
                             wrapping_length),
       y = "Count") +
  geom_vline(data = summary_dt_washout,
             aes(xintercept = mean, color = initiated),
             linetype = "dashed",
             linewidth = linewidth_plot) +
  geom_text(aes(x     = summary_dt_washout[initiated == "Control"]$mean - x_offset_text ,
                y     = y_intercept_text,
                label = glue::glue("Mean: \n{summary_dt_washout[initiated == 'Control']$mean |> round(1)}%")),
            colour= rgb(0.000, 0.266, 0.531), alpha = 1) +
  geom_text(aes(x     = summary_dt_washout[initiated == "Initiated"]$mean - x_offset_text ,
                y     = y_intercept_text,
                label = glue::glue("Mean: \n{summary_dt_washout[initiated == 'Initiated']$mean |> round(1)}%")),
            colour = rgb(0.863, 0.664, 0.199), alpha = 1) +
  coord_cartesian(ylim = c(0L, 3000L),
                  xlim = c(0L, 100L)) +
  ggtitle(label = "b") +
  scale_y_continuous(breaks = seq(0, 3000, by = 1000)) +
  khroma::scale_color_highcontrast() +
  khroma::scale_fill_highcontrast()

# Arrange both plots
pD <- pA / pB
pD

# Save
ggsaver(plot = pD,
        filename = "meddays_by_arm_pct_new_gridless",
        size = "custom",
        custom_plot = c(8L, 8L))
# Copy to export folder  
file.copy(from = here::here("output", "plots", "meddays_by_arm_pct_new_gridless.pdf"),
          to = "N:\\durable\\file-export\\new", overwrite = TRUE)

# How many of the initiated have 100% medicated days?
dt[initiated == "Initiated", sum(med.days.pct.6to8 == 100)/.N] * 100
# 5.7539% for analytic
dt[initiated == "Initiated" & medicated.pt0 == "No", sum(med.days.pct.6to8 == 100)/.N] * 100
# 6.076% for washout

# How many in control arm have about 0 estimated medicated days?
dt[initiated == "Control",.(below = sum(med.days.6to8 < 1)/.N)] * 100
# 72.1%
dt[medicated.pt0 == "No" & initiated == "Control",.(below = sum(med.days.6to8 < 1)/.N)] * 100
# 67.90%

extra_meddays_dt <- data.table(
  "Perc" = c(dt[initiated == "Initiated", sum(med.days.pct.6to8 == 100)/.N] * 100,
             dt[initiated == "Control",.(below = sum(med.days.6to8 < 1)/.N)] * 100,
             dt[initiated == "Initiated" & medicated.pt0 == "No", sum(med.days.pct.6to8 == 100)/.N] * 100,
             dt[medicated.pt0 == "No" & initiated == "Control",.(below = sum(med.days.6to8 < 1)/.N)] * 100) |> unlist(),
  "Type" = c("100 treated",
             "0 untreated",
             "100 treated",
             "0 untreated"),
  "Sample" = c("Analytic",
               "Analytic",
               "Washout",
               "Washout")
)

extra_meddays_dt[, Perc := round(as.vector(Perc), digits = 1)]

# Store in an object
saveRDS(extra_meddays_dt, file = here::here("output", "descriptives", "extra_meddays_dt.RDS"))

# Balance evaluation ------------------------------------------------------

library(MatchIt)
table1_vars_cats

bal_formula <- paste0("initiated ~ ", paste0(table1_vars_cats, collapse = " + ")) |>
  as.formula()

# For Freq. Table
m.out <- matchit(bal_formula,
                 data = dt_analytic,
                 replace = TRUE)

# Make temporary data.frame out of balance diagnostics of unmatched sample
dt_balance <- summary(m.out)$sum.all |>
  as.data.frame() 
# Create character column that has variable names
dt_balance$Variable <- rownames(dt_balance)
# Relocate to first position and convert to data.table
dt_balance <- dt_balance[, c(ncol(dt_balance),
                             1:{ncol(dt_balance)-1})] |> data.table()
# Order by descending order for SMD
dt_balance[order(-`Std. Mean Diff.`)]

#                  Variable Means Treated Means Control Std. Mean Diff. Var. Ratio    eCDF Mean     eCDF Max Std. Pair Dist.
# 1:               distance  0.6422966435  0.6325701462    0.2050191790  1.0207404 5.340685e-02 7.993595e-02              NA
# 2:                   sexm  0.7427248677  0.6886549708    0.1236922969         NA 5.406990e-02 5.406990e-02              NA
# 3:         n.sibs.w.diag1  0.1992063492  0.1567251462    0.1063616113         NA 4.248120e-02 4.248120e-02              NA
# 4:          cs.6.fmar_par  0.4891534392  0.4596491228    0.0590225221         NA 2.950432e-02 2.950432e-02              NA


# Store data.table
saveRDS(dt_balance[order(-`Std. Mean Diff.`)][, c("Variable",
                                                  "Means Treated",
                                                  "Means Control",
                                                  "Std. Mean Diff.")], 
        here::here("output", "descriptives", "SMDs_cat_balance.RDS"))

  # Remove
rm(dt_balance)

# For continuous variables
formula_tmp <- as.formula(paste0("initiated ~ ",
                                 paste0(table1_vars_num, collapse = " + ")))

# For Freq. Table
m.out <- matchit(formula_tmp,
                 data = dt_analytic[complete.cases(dt_analytic[,
                                                               .SD,
                                                               .SDcols = c(table1_vars_num)])],
                 replace = TRUE)


# Make data.frame out of balance diagnostics of unmatched sample
dt_balance <- summary(m.out)$sum.all |>
  as.data.frame() 
# Create character column that has variable names
dt_balance$Variable <- rownames(dt_balance)
# Relocate to first position and convert to data.table
dt_balance <- dt_balance[, c(ncol(dt_balance),
                             1:{ncol(dt_balance)-1})] |> as.data.table()
# Order by descending order for SMD
dt_balance[order(-`Std. Mean Diff.`)]

#             Variable Means Treated Means Control Std. Mean Diff. Var. Ratio   eCDF Mean    eCDF Max Std. Pair Dist.
# 1:          distance     0.9148396    0.15788809    4.4724802225  0.3834074 0.477035524 0.843418753              NA
# 2:     med.days.6to8   630.7683939   88.46413263    3.5212380868  0.7745027 0.724715735 0.800948515              NA
# 3:     n.blocks.6to8     1.3039576    0.32299546    1.5782039431  1.0973685 0.140137444 0.734493192              NA
# 4:          n.visits    89.8421053   50.21936460    0.5194636063  1.4755158 0.115845789 0.357087879              NA
# 5: pre6.n.visits_P24     1.5144839    0.95007564    0.0986394549  1.9458668 0.011348185 0.034700709              NA
# 6:            p.RE_5    17.1991024   16.62481089    0.0886552463  0.9834971 0.017414856 0.048181267              NA

# Conveniently, we get only large differences in the variables we intend to differentiate the groups for
# Specific learning problems (P24) is the closest after those

# Store data.table
saveRDS(dt_balance[order(-`Std. Mean Diff.`)][, c("Variable",
                                                  "Means Treated",
                                                  "Means Control",
                                                  "Std. Mean Diff.")], 
        here::here("output", "descriptives", "SMDs_num_balance.RDS"))

# Flow diagram ------------------------------------------------------------
# library(PRISMAstatement)

# flow_exclusions(
#   incl_counts = c(
#     # These first values had to be hard-coded as
#     #  they come from the confidential linkage population
#     548999L,
#     460275L,
#     17738L,
#     nrow(dt_merged), 
#     nrow(dt_eligible),
#     nrow(dt_analytic),
#     nrow(dt_analytic[medicated.pt0 == "No"]),
#     nrow(dt_completecase))
# ,
# ,
#   total_label = "Children living in Norway\nwith birth years 2000-2007",
#   incl_labels = c("Born in Norway",
#                   "With ADHD",
#                   "Followed-up sample",
#                   "Eligible sample",
#                   "Main analytic sample",
#                   "Washout sample",
#                   "Complete case sample"),
#   excl_labels = c("Born outside Norway",
#                   "Without ADHD",
#                   "With serious\ncomorbid disorder \n(+1 missing data on both parents)",
#                   "Missing score for all grade 5 national tests",
#                   "Had been previously medicated",
# 
#   )
#   , format_args = list(big.mark = ",")
# )
# 


included_counts <- c(548999L,
                     460275L,
                     17738L,
                     nrow(dt_merged), 
                     nrow(dt_eligible),
                     nrow(dt_analytic),
                     nrow(dt_analytic[medicated.pt0 == "No"]),
                     nrow(dt_completecase_Y))

excluded_counts <- diff(included_counts) |> abs()
excluded_counts[length(excluded_counts)] <- nrow(dt_analytic) - nrow(dt_completecase_Y)

# Add name attribute for specific counts
included_counts
# 548999 460275  17738  15189  12541  11835   8458   8068
names(included_counts) <- c("Living in Norway",
                            "Born in Norway",
                            "ADHD-diagnosed",
                            "merged",
                            "eligible",
                            "analytic",
                            "washout",
                            "complete case"
)

excluded_counts
# 88724 442537   2549   2648    706   3377   3767
names(excluded_counts) <- c("Born outside Norway",
                            "Without ADHD",
                            "With serious comorbid disorder",
                            "Missing all pretests",
                            "Missing unimputed covariates", # Civil status of both parents at grade 5 and 6 and s.id8
                            "Previously medicated",
                            "Missing any covariate")

# Store in a single list object
saveRDS(list("included" = included_counts,
                    "excluded" = excluded_counts),
        here::here("output", "descriptives", "flow_counts.RDS"))



# Standardized score distributions ----------------------------------------


# Melt into long format
dt_scores <- data.table::melt(dt_analytic,
                              id.vars = c("id"),
                              measure.vars = c(posttest_names_std, pretest_names_std),
                              variable.name = "grade_domain",
                              value.name = "Score")



# Create the desired variables from grade_domain and discard it
dt_scores[, Grade := gsub(pattern = "[^0-9]", "", grade_domain)]
dt_scores[, Domain := substr(grade_domain, start = 3, stop = 4)]
dt_scores[, grade_domain := NULL]


# Create summarized data.table with means

dt_scores_means <- dt_scores[,
                             .(mean = mean(Score, na.rm = TRUE)), 
                             by = c("Domain", "Grade")]

# Get the effect estimates

effect_tmp <- readRDS(file = here::here("output", "objects", "effect_summaries",
                                       "effect_table_raw.RDS"))

# Add to dt_scores the effect for each domain with lower and upper domain for grade 8
dt_scores_means <- rbind(
dt_scores_means[Grade == '8'][effect_tmp[Subset %in% grep("Analytic", Subset, value = TRUE),
           .SD,
           .SDcols = c("Mean_diff", "Lower", "Upper", "Domain")], on = .(Domain = Domain)] ,
dt_scores_means[Grade == '5'][,`:=`(Mean_diff = NA_real_, Lower = NA_real_, Upper = NA_real_)] 
)



# Plot 
  # For pretests
pA <- ggplot(data = dt_scores[Grade == '5']) +
  geom_histogram(aes(fill = Domain, x = Score),
                 alpha = 0.6,
                 position = "identity",
                 bins = 30L) +
  labs(x = "Grade 5 standardized score",
       y = "Count") +
  geom_vline(data = dt_scores_means[Grade == '5'],
             aes(color = Domain,
                 xintercept = mean),
             linewidth = linewidth_plot,
             linetype = "dotted") + 
  geom_vline(xintercept = 0L,
             linetype = "dashed",
             linewidth = linewidth_plot
             ) +
  coord_cartesian(ylim = c(0, 1000L),
                  xlim = c(-3, 3)) +
  khroma::scale_colour_highcontrast() +
  khroma::scale_fill_highcontrast()

  # For posttests
pB <- ggplot(data = dt_scores[Grade == '8']) +
  geom_histogram(aes(fill = Domain, x = Score),
                 alpha = 0.6,
                 position = "identity",
                 bins = 30L) +
  labs(x = "Grade 8 standardized score",
       y = "Count") +
  geom_vline(data = dt_scores_means[Grade == '8'],
             aes(color = Domain,
                 xintercept = mean),
             linewidth = linewidth_plot,
             linetype = "dotted") + 
  geom_linerange(data = dt_scores_means[Grade == '8'],
                 aes(xmin = mean + Lower,
                     xmax = mean + Upper,
                     color = Domain,
                     y = 1000L),
                 linewidth = 2L,
                 alpha = 0.6) +
  annotate(geom = "text",
           x = dt_scores_means[Grade == '8', min(mean)],
           y = 1000,
           hjust = 1,
           label = "Estimated effect interval ") +
  geom_vline(xintercept = 0L,
             linetype = "dashed",
             linewidth = linewidth_plot
  ) +
  coord_cartesian(ylim = c(0, 1000L),
                  xlim = c(-3, 3)) +
  theme( axis.title.y = element_blank(),     
         axis.text.y  = element_blank(), 
         axis.ticks.y = element_blank(),
         axis.line.y  = element_blank()) +
  guides(fill = "none", color = "none") +
  khroma::scale_colour_highcontrast() +
  khroma::scale_fill_highcontrast()

pC <- pA + pB
pC


ggsaver(pC,
        path = here::here("output", "descriptives"),
        filename = "std_scores_hist",
        output_format = "pdf",
        size = "custom",
        custom_plot = c(7L, 13L))
# 13 x 7 inches sounds good

file.copy(from = here::here("output", "descriptives", "std_scores_hist.pdf"),
          to =  "N:\\durable\\file-export\\new", overwrite = TRUE)
