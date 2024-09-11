# Objective

  # To evaluate how informative our covariates are of both
    # (a) treatment allocation
    # (b) loss-to-follow up
  # Check overlap and predictability of imputation model
    # Check available scores in other tests for people exempted in a given test

# 0. Dependencies ---------------------------------------------------------

library(WeightIt)
library(patchwork)
library(Hmisc)

# 1. Data preparation -----------------------------------------------------

# GGally::ggpairs(dt_merged[, .SD, .SDcols = avg_school_scores_names],
#         lower = list(continuous = GGally::wrap("points", alpha = 0.15)))



# 2. Imputation model -----------------------------------------------------

# We will use samples due to the number of points
# Also subset only to outcomes
plot_samples <- nrow(dt_merged)*0.50
dt_samp      <- dt_merged[sample(1:nrow(dt_merged), plot_samples),
                        .SD,
                        .SDcols =  c(all_outcome_names_vector,
                                     # "prop_post_exemptions",
                                     "p.GPA_10")] 


cor(dt_merged[, c(all_outcome_names_vector, "p.GPA_10")])



ggplot(data = heatmap_df,
       aes(x    = x_names,
           y    = y_names,
           fill = value)) + 
  geom_tile(alpha = plot_alpha) +
  coord_fixed(ratio = y_over_x_ratio) +
  geom_text(aes(label = value), size = rel(8)) +
  labs() + 
  khroma::scale_fill_iridescent()

GGally::ggpairs(dt_samp[, ],
                lower = list(continuous = wrap("points", alpha = 0.05)))

  # Scatter plots for those with no exemptions versus
  # different number of exemptions out of valid tests
    # We can see broadly that the relationship between scores
    # for grades 8 and 9 does not change for those
    # that were exempted from other tests
  # PENDING, axis breaks are not equal for both plots
  # add correlation coefficients and/or slopes by subgroup


  # Numeracy
p1 <- ggplot(data = dt_merged[prop_post_exemptions > 0 & prop_post_exemptions < 1],
             aes(x = p.MA_8, y = p.MA_9,
                 color = as.factor(prop_post_exemptions))) +
  geom_point(position = "jitter", alpha = alpha_plot, size = 4) +
  scale_y_continuous(breaks = seq(0, 60, 20), minor_breaks = seq(10, 50, 20)) +
  scale_x_continuous(breaks = seq(0, 60, 20), minor_breaks = seq(10, 50, 20)) +
  geom_smooth(method = "lm", fill = NA) +
  # khroma::scale_color_bright() 
  khroma::scale_color_mediumcontrast()
p2 <- ggplot(data = dt_merged[prop_post_exemptions == 0]) +
  geom_point(aes(x = p.MA_8, y = p.MA_9, color = as.factor(prop_post_exemptions)),
             position = "jitter", alpha = alpha_plot/2, size = 4) +
  scale_y_continuous(breaks = seq(0, 60, 20), minor_breaks = seq(10, 50, 20)) +
  scale_x_continuous(breaks = seq(0, 60, 20), minor_breaks = seq(10, 50, 20)) +
  theme(axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y  = element_blank()) +
  khroma::scale_color_vibrant()
  # khroma::scale_color_vibrant()

nu_cor_plot <- p1+p2
nu_cor_plot <- nu_cor_plot 
if (print_all == TRUE) {print(nu_cor_plot)}

  # Reading
p1 <- ggplot(data = dt_merged[prop_post_exemptions > 0 & prop_post_exemptions < 1],
             aes(x = p.RE_8, y = p.RE_9,
                 color = as.factor(prop_post_exemptions))) +
  geom_point(position = "jitter", alpha = alpha_plot, size = 4) +
  scale_y_continuous(breaks = seq(0, 60, 20), minor_breaks = seq(10, 50, 20)) +
  scale_x_continuous(breaks = seq(0, 60, 20), minor_breaks = seq(10, 50, 20)) +
  geom_smooth(method = "lm", fill = NA) +
  # khroma::scale_color_bright() 
  khroma::scale_color_mediumcontrast()
p2 <- ggplot(data = dt_merged[prop_post_exemptions == 0]) +
  geom_point(aes(x = p.RE_8, y = p.RE_9, color = as.factor(prop_post_exemptions)),
             position = "jitter", alpha = alpha_plot/2, size = 4) +
  theme(axis.title.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y  = element_blank()) +
  # khroma::scale_color_bright()
  khroma::scale_color_vibrant()

re_cor_plot <- p1+p2
if (print_all == TRUE) {print(re_cor_plot)}


# :: 2.1 Bar chart for available scores ------------------------------------------



  # Wide to long for school performance scores
melted_tmp <- data.table::melt(dt_merged, id.vars = "id",
     measure = patterns("^p\\."), # Have to escape the escape character due to R,
     #alternatively can use "^p[:.:]"
     variable.name = "test",
     value.name = "score")

  # Create domain variable
melted_tmp[, domain := fcase(
  grepl(pattern = "^p.[M]", test), "Numeracy",
  grepl(pattern = "^p.[R]", test), "Reading",
  grepl(pattern = "^p.[E]", test), "English",
  default = "GPA" # Default return value, NA by default, for when all of the 
  #logical conditions when1, when2, ..., whenN are FALSE or missing for some entries.
)]

  # Create year variable
melted_tmp[, year := fcase(
  grepl(pattern = "[5]", test), "5",
  grepl(pattern = "[8]", test), "8",
  grepl(pattern = "[9]", test), "9",
  default = "10" # Default return value, NA by default, for when all of the 
  #logical conditions when1, when2, ..., whenN are FALSE or missing for some entries.
)]

  # Indicate if posttest is missing
melted_tmp[year == 8,
           exempted_8_domain := ifelse(is.na(score), TRUE, FALSE),
           by = domain]

  # Indicate if they will be part of analytical sample
melted_tmp[year == 5,
           exempted_5_domain := ifelse(is.na(score), TRUE, FALSE),
           by = domain]

  # Within every domain and eligible participants,
  # for those that were exempted from the posttest
  # count how many of them have scores for grade 9,for the GPA, and both
melted_tmp[exempted_5_domain == FALSE & exempted_8_domain == TRUE,]

# Exempted_5_domain marker has to be for each id within each domain?




# Available scores within same domain
# However, we should use cross-domain scores to impute
dt_merged[!is.na(p.MA_5) & is.na(p.MA_8),
        available_scores_MA := fcase(
  is.na(p.MA_9)  & !is.na(p.GPA_10), "Only GPA",
  !is.na(p.MA_9)  &  is.na(p.GPA_10),  "Only Grade 9",
  !is.na(p.MA_9)  & !is.na(p.GPA_10), "Both GPA and Grade 9",
  default = "None"),
  by = .(id)]

dt_merged[!is.na(p.RE_5) & is.na(p.RE_8),
        available_scores_RE := fcase(
  is.na(p.RE_9)  & !is.na(p.GPA_10), "Only GPA",
  !is.na(p.RE_9)  &  is.na(p.GPA_10),  "Only Grade 9",
  !is.na(p.RE_9)  & !is.na(p.GPA_10), "Both GPA and Grade 9",
  default = "None"),
  by = .(id)]

dt_merged[!is.na(p.EN_5) & is.na(p.EN_8),
        available_scores_EN := fcase(
  !is.na(p.GPA_10), "Only GPA",
  default = "None"),
  by = .(id)]




melted_tmp <- melt(dt_merged[,.(id,
                              available_scores_MA,
                              available_scores_RE,
                              available_scores_EN)],
                   id.var = "id",
                   variable.name = "domain",
                   value.name = "available_scores")
                   
melted_tmp <- na.omit(melted_tmp)

levels(melted_tmp$domain) <- c("Numeracy", "Reading", "English")
# Change order
melted_tmp$available_scores <- factor(melted_tmp$available_scores,
                                      levels = c(
                                        "None",
                                        "Only Grade 9",
                                        "Only GPA",
                                        "Both GPA and Grade 9"
                                      ))



# 
ggplot(data = melted_tmp, aes(y = domain, fill = available_scores)) +
  geom_bar() +
  geom_text(aes(label = after_stat(count)),
            # vjust = 1.5,
            stat = "count",
            position = position_stack(vjust = 0.5),
            angle = 90,
            color = "white",
            size = rel(4)) +
  labs(y = "",
       x = "Count",
       title = stringr::str_wrap("Available scores for pupils exempted
                                 from their respective grade 8 test",
                                 wrapping_length)) +
  khroma::scale_fill_bright()

# We see that it is a very small percentage of exempted at grade 8 cases
# for which we have available data in the future.
# For numeracy and reading, 90% of pupils have no other score data
# For English, 95% do not have other score data.


# # Does not work
# # There should be a better way
# percentages_df <-  melted_tmp[, .(onlyGPA       = {sum(available_scores == "Only GPA") / .N}*100
#                |> round(3),
#                onlygrade9    = {sum(available_scores == "Only Grade 9") / .N}*100
#                |> round(3),
#                bothGPAgrade9 = {sum(available_scores == "Both GPA and Grade 9") / .N}*100
#                |> round(3),
#                none          = {sum(available_scores == "None") / .N}*100
#                |> round(3)),
#            by = domain]
#            
# melted_percentages_df <- data.table::melt(percentages_df,
#                                           id.vars = "domain",
#                                           variable.name = "available_score",
#                                           value.name ="percentage")
#            
# ggplot(data = melted_tmp, aes(y = domain, fill = available_scores)) +
#   geom_bar() + geom_text(data = melted_percentages_df,
#                          aes(label = percentage),
#                          # vjust = 1.5,
#                          stat = "count",
#                          position = position_stack(vjust = 0.5),
#                          angle = 90,
#                          color = "white",
#                          size = rel(4)) +
#   khroma::scale_fill_bright()

# For numeracy
# All subjects with exempted_5_domain == FALSE
# All subjects with exempted_8_domain == TRUE
# How many of these subjects have NA scores only for same domain and year 9
# How many of these subjects have NA scores only for GPA at grade 10
# How many of these subjects have NA scores for both same domain year 9 and GPA at grade 10?


# ggplot(data = melted_tmp, aes(x = domain)) +
#   geom_bar()

# PENDING
# Bar chart showing for how many of exempted at grade 8 we have observed values
# for grade 9 tests and gpa at grade 10
# Add heatmap

# We see that observed correlations for tests of the same domain is moderately
#  high (i.e. all >= 0.560). As expected we see the highest correlations between
#  tests at years 8 and 9, especially for the numeracy test (cor(MA_8, MA_9) = 0.813;
#  cor(RE_8, RE_9) = 0.738). These correlations account for over half the variability.
#  At the same time, all tests are moderately correlated with GPA at grade 10, with the correlation
#  increasing the closer to grade 10 the test was.
#  We consider an imputation model that also uses information from the future in order
#  to impute the exempted tests at grade 8.
#  


# 3. Loss-to-follow up ----------------------------------------------------
# 
# # Guido code
# grep("5",
#      grep("^p.[E,R,L,G]", # Most likely a mistake, wanted to include M for MA rather than L
#           names(dt_merged), value = TRUE),
#      invert = TRUE, value = TRUE)

names(dt_merged)
grep("^p.[E,R.M.G]", names(dt_merged), value = TRUE)
grep("5", grep("^p.[E,R.M.G]", names(dt_merged), value = TRUE), value = TRUE, invert = TRUE)











# NOTES

# Regular expressions

# Caret outside square brackets is just to indicate beginning of line
# e.g. here we look for all names that start with p
grep("^p", names(dt_merged),
     value = TRUE)
   
# Starts with p. and then has either an E, R, M or G
grep("^p.[E,R,M,G]", names(dt_merged),
     value = TRUE)
     
# Invert is true, so it will show the names that do not have 5 in it
grep("5",names(dt_merged),
     value = TRUE, invert = TRUE)
     
# Note that this is different from the following
# which will only exclude a complete string equal to "5"
grep("[^5]",names(dt_merged),
     value = TRUE)

