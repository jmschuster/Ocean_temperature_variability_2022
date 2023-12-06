rm(list = ls())

setwd("C:/Users/Owner/Documents/HOTS Database/Final Paper")

library(brms)
library(tidyverse)
library(tidybayes)
library(gghalves)
library(ggbeeswarm)
library(emmeans)

model_data <- read.csv("metadata_with_variables - br diff e063 - 25th Sept_with_spatial_blocks.csv", header = TRUE)

### daily_median

## explore data
ggplot(model_data, aes(x = climate_classification, y = daily_median))+
  geom_half_point(aes(color = climate_classification),
                  transformation = position_quasirandom(width = 0.1),
                  side = "l", size = 1, alpha = 0.5)+
  geom_half_boxplot(aes(fill = climate_classification), side = 'r')

ggplot(model_data, aes(x = duration_in_years, y = daily_median)) +
  geom_point(aes(color = climate_classification)) +
  geom_smooth(method = "lm")

ggplot(model_data, aes(x = depth_in_m, y = daily_median)) +
  geom_point(aes(color = climate_classification)) +
  geom_smooth(method = "lm")


# model brms with simple no priors and nested random effects

mod_daily_median <- brm(daily_median ~ climate_classification + duration_in_years + depth_in_m + (1|spatial_blocks),
                        data = model_data, 
                        family = "gamma",
                        chains = 4, iter = 2000, warmup = 1000,
                        control = list(adapt_delta = 0.99))

pp_check(mod_daily_median, ndraws = 100) # check fit with gamma distribution

summary(mod_daily_median)

# cant interpret coefficients because of gamma distribution
# use emmeans to show marginal effects at average values of other explanatory variables
# 0.9 confidence interval

marginal_effect <- emmeans(mod_daily_median, ~ climate_classification,
                           at = list(duration_in_years = mean(model_data$duration_in_years),
                                     depth_in_m = mean(model_data$depth_in_m)),
                           epred = TRUE,
                           level = 0.9)

# contrast marginal effects to see what climate classification levels are sig dif from each other,
# i.e estimate and CI do not include zero

contrast_marginal_effect <- contrast(marginal_effect, method = "revpairwise")

# to visualize gather draws and plot, set .width levels to view confidence intervals

marginal_effect_draws_daily_median <- gather_emmeans_draws(contrast_marginal_effect)

ggplot(marginal_effect_draws_daily_median, aes(x = .value, y = contrast, fill = contrast)) +
  stat_halfeye(.width = c(0.8,0.9)) +
  geom_vline(xintercept = 0)


### weekly_median

## explore data
ggplot(model_data, aes(x = climate_classification, y = weekly_median))+
  geom_half_point(aes(color = climate_classification),
                  transformation = position_quasirandom(width = 0.1),
                  side = "l", size = 1, alpha = 0.5)+
  geom_half_boxplot(aes(fill = climate_classification), side = 'r')

ggplot(model_data, aes(x = duration_in_years, y = weekly_median)) +
  geom_point(aes(color = climate_classification)) +
  geom_smooth(method = "lm")

ggplot(model_data, aes(x = depth_in_m, y = weekly_median)) +
  geom_point(aes(color = climate_classification)) +
  geom_smooth(method = "lm")


# model brms with simple no priors and nested random effects

mod_weekly_median <- brm(weekly_median ~ climate_classification + duration_in_years + depth_in_m + (1|spatial_blocks),
             data = model_data, 
             family = "gamma",
             chains = 4, iter = 2000, warmup = 1000,
             control = list(adapt_delta = 0.99))

pp_check(mod_weekly_median, ndraws = 100) # check fit with gamma distribution

summary(mod_weekly_median)

# cant interpret coefficients because of gamma distribution
# use emmeans to show marginal effects at average values of other explanatory variables
# 0.9 confidence interval

marginal_effect <- emmeans(mod_weekly_median, ~ climate_classification,
                           at = list(duration_in_years = mean(model_data$duration_in_years),
                                     depth_in_m = mean(model_data$depth_in_m)),
                           epred = TRUE,
                           level = 0.9)

# contrast marginal effects to see what climate classification levels are sig dif from each other,
# i.e estimate and CI do not include zero

contrast_marginal_effect <- contrast(marginal_effect, method = "revpairwise")

# to visualize gather draws and plot, set .width levels to view confidence intervals

marginal_effect_draws_weekly_median <- gather_emmeans_draws(contrast_marginal_effect)

ggplot(marginal_effect_draws_weekly_median, aes(x = .value, y = contrast, fill = contrast)) +
  stat_halfeye(.width = c(0.8,0.9)) +
  geom_vline(xintercept = 0)


### biweekly_median

## explore data
ggplot(model_data, aes(x = climate_classification, y = biweekly_median))+
  geom_half_point(aes(color = climate_classification),
                  transformation = position_quasirandom(width = 0.1),
                  side = "l", size = 1, alpha = 0.5)+
  geom_half_boxplot(aes(fill = climate_classification), side = 'r')

ggplot(model_data, aes(x = duration_in_years, y = biweekly_median)) +
  geom_point(aes(color = climate_classification)) +
  geom_smooth(method = "lm")

ggplot(model_data, aes(x = depth_in_m, y = biweekly_median)) +
  geom_point(aes(color = climate_classification)) +
  geom_smooth(method = "lm")


# model brms with simple no priors and nested random effects

mod_biweekly_median <- brm(biweekly_median ~ climate_classification + duration_in_years + depth_in_m + (1|spatial_blocks),
                           data = model_data, 
                           family = "gamma",
                           chains = 4, iter = 2000, warmup = 1000,
                           control = list(adapt_delta = 0.99))

pp_check(mod_biweekly_median, ndraws = 100) # check fit with gamma distribution

summary(mod_biweekly_median)

# cant interpret coefficients because of gamma distribution
# use emmeans to show marginal effects at average values of other explanatory variables
# 0.9 confidence interval

marginal_effect <- emmeans(mod_biweekly_median, ~ climate_classification,
                           at = list(duration_in_years = mean(model_data$duration_in_years),
                                     depth_in_m = mean(model_data$depth_in_m)),
                           epred = TRUE,
                           level = 0.9)

# contrast marginal effects to see what climate classification levels are sig dif from each other,
# i.e estimate and CI do not include zero

contrast_marginal_effect <- contrast(marginal_effect, method = "revpairwise")

# to visualize gather draws and plot, set .width levels to view confidence intervals

marginal_effect_draws_biweekly_median <- gather_emmeans_draws(contrast_marginal_effect)

ggplot(marginal_effect_draws_biweekly_median, aes(x = .value, y = contrast, fill = contrast)) +
  stat_halfeye(.width = c(0.8,0.9)) +
  geom_vline(xintercept = 0)


### monthly_median

## explore data
ggplot(model_data, aes(x = climate_classification, y = monthly_median))+
  geom_half_point(aes(color = climate_classification),
                  transformation = position_quasirandom(width = 0.1),
                  side = "l", size = 1, alpha = 0.5)+
  geom_half_boxplot(aes(fill = climate_classification), side = 'r')

ggplot(model_data, aes(x = duration_in_years, y = monthly_median)) +
  geom_point(aes(color = climate_classification)) +
  geom_smooth(method = "lm")

ggplot(model_data, aes(x = depth_in_m, y = monthly_median)) +
  geom_point(aes(color = climate_classification)) +
  geom_smooth(method = "lm")


# model brms with simple no priors and nested random effects

mod_monthly_median <- brm(monthly_median ~ climate_classification + duration_in_years + depth_in_m + (1|spatial_blocks),
                          data = model_data, 
                          family = "gamma",
                          chains = 4, iter = 2000, warmup = 1000,
                          control = list(adapt_delta = 0.99))

pp_check(mod_monthly_median, ndraws = 100) # check fit with gamma distribution

summary(mod_monthly_median)

# cant interpret coefficients because of gamma distribution
# use emmeans to show marginal effects at average values of other explanatory variables
# 0.9 confidence interval

marginal_effect <- emmeans(mod_monthly_median, ~ climate_classification,
                           at = list(duration_in_years = mean(model_data$duration_in_years),
                                     depth_in_m = mean(model_data$depth_in_m)),
                           epred = TRUE,
                           level = 0.9)

# contrast marginal effects to see what climate classification levels are sig dif from each other,
# i.e estimate and CI do not include zero

contrast_marginal_effect <- contrast(marginal_effect, method = "revpairwise")

# to visualize gather draws and plot, set .width levels to view confidence intervals

marginal_effect_draws_monthly_median <- gather_emmeans_draws(contrast_marginal_effect)

ggplot(marginal_effect_draws_monthly_median, aes(x = .value, y = contrast, fill = contrast)) +
  stat_halfeye(.width = c(0.8,0.9)) +
  geom_vline(xintercept = 0)


### annual_median

## explore data
ggplot(model_data, aes(x = climate_classification, y = annual_median))+
  geom_half_point(aes(color = climate_classification),
                  transformation = position_quasirandom(width = 0.1),
                  side = "l", size = 1, alpha = 0.5)+
  geom_half_boxplot(aes(fill = climate_classification), side = 'r')

ggplot(model_data, aes(x = duration_in_years, y = annual_median)) +
  geom_point(aes(color = climate_classification)) +
  geom_smooth(method = "lm")

ggplot(model_data, aes(x = depth_in_m, y = annual_median)) +
  geom_point(aes(color = climate_classification)) +
  geom_smooth(method = "lm")

ggplot(model_data, aes(x = climate_classification, y = annual_median)) +
  geom_point(aes(color = spatial_blocks)) +
  geom_smooth(method = "lm")

# model brms with simple no priors and nested random effects

mod_annual_median <- brm(annual_median ~ climate_classification + duration_in_years + depth_in_m + (1|spatial_blocks),
                         data = model_data, 
                         family = "gamma",
                         chains = 4, iter = 2000, warmup = 1000,
                         control = list(adapt_delta = 0.99))

pp_check(mod_annual_median, ndraws = 100) # check fit with gamma distribution

summary(mod_annual_median)

# cant interpret coefficients because of gamma distribution
# use emmeans to show marginal effects at average values of other explanatory variables
# 0.9 confidence interval

marginal_effect <- emmeans(mod_annual_median, ~ climate_classification,
                           at = list(duration_in_years = mean(model_data$duration_in_years),
                                     depth_in_m = mean(model_data$depth_in_m)),
                           epred = TRUE,
                           level = 0.9)

# contrast marginal effects to see what climate classification levels are sig dif from each other,
# i.e estimate and CI do not include zero

contrast_marginal_effect <- contrast(marginal_effect, method = "revpairwise")

# to visualize gather draws and plot, set .width levels to view confidence intervals

marginal_effect_draws_annual_median <- gather_emmeans_draws(contrast_marginal_effect)

ggplot(marginal_effect_draws_annual_median, aes(x = .value, y = contrast, fill = contrast)) +
  stat_halfeye(.width = c(0.8,0.9)) +
  geom_vline(xintercept = 0)

### daily_ninetieth_perc

## explore data
ggplot(model_data, aes(x = climate_classification, y = daily_ninetieth_perc))+
  geom_half_point(aes(color = climate_classification),
                  transformation = position_quasirandom(width = 0.1),
                  side = "l", size = 1, alpha = 0.5)+
  geom_half_boxplot(aes(fill = climate_classification), side = 'r')

ggplot(model_data, aes(x = duration_in_years, y = daily_ninetieth_perc)) +
  geom_point(aes(color = climate_classification)) +
  geom_smooth(method = "lm")

ggplot(model_data, aes(x = depth_in_m, y = daily_ninetieth_perc)) +
  geom_point(aes(color = climate_classification)) +
  geom_smooth(method = "lm")


# model brms with simple no priors and nested random effects

mod_daily_ninetieth_perc <- brm(daily_ninetieth_perc ~ climate_classification + duration_in_years + depth_in_m + (1|spatial_blocks),
                                data = model_data, 
                                family = "gamma",
                                chains = 4, iter = 2000, warmup = 1000,
                                control = list(adapt_delta = 0.99))

pp_check(mod_daily_ninetieth_perc, ndraws = 100) # check fit with gamma distribution

summary(mod_daily_ninetieth_perc)

# cant interpret coefficients because of gamma distribution
# use emmeans to show marginal effects at average values of other explanatory variables
# 0.9 confidence interval

marginal_effect <- emmeans(mod_daily_ninetieth_perc, ~ climate_classification,
                           at = list(duration_in_years = mean(model_data$duration_in_years),
                                     depth_in_m = mean(model_data$depth_in_m)),
                           epred = TRUE,
                           level = 0.9)

# contrast marginal effects to see what climate classification levels are sig dif from each other,
# i.e estimate and CI do not include zero

contrast_marginal_effect <- contrast(marginal_effect, method = "revpairwise")

# to visualize gather draws and plot, set .width levels to view confidence intervals

marginal_effect_draws_daily_ninetieth_perc <- gather_emmeans_draws(contrast_marginal_effect)

ggplot(marginal_effect_draws_daily_ninetieth_perc, aes(x = .value, y = contrast, fill = contrast)) +
  stat_halfeye(.width = c(0.8,0.9)) +
  geom_vline(xintercept = 0)


### weekly_ninetieth_perc

## explore data
ggplot(model_data, aes(x = climate_classification, y = weekly_ninetieth_perc))+
  geom_half_point(aes(color = climate_classification),
                  transformation = position_quasirandom(width = 0.1),
                  side = "l", size = 1, alpha = 0.5)+
  geom_half_boxplot(aes(fill = climate_classification), side = 'r')

ggplot(model_data, aes(x = duration_in_years, y = weekly_ninetieth_perc)) +
  geom_point(aes(color = climate_classification)) +
  geom_smooth(method = "lm")

ggplot(model_data, aes(x = depth_in_m, y = weekly_ninetieth_perc)) +
  geom_point(aes(color = climate_classification)) +
  geom_smooth(method = "lm")


# model brms with simple no priors and nested random effects

mod_weekly_ninetieth_perc <- brm(weekly_ninetieth_perc ~ climate_classification + duration_in_years + depth_in_m + (1|spatial_blocks),
                                 data = model_data, 
                                 family = "gamma",
                                 chains = 4, iter = 2000, warmup = 1000,
                                 control = list(adapt_delta = 0.99))

pp_check(mod_weekly_ninetieth_perc, ndraws = 100) # check fit with gamma distribution

summary(mod_weekly_ninetieth_perc)

# cant interpret coefficients because of gamma distribution
# use emmeans to show marginal effects at average values of other explanatory variables
# 0.9 confidence interval

marginal_effect <- emmeans(mod_weekly_ninetieth_perc, ~ climate_classification,
                           at = list(duration_in_years = mean(model_data$duration_in_years),
                                     depth_in_m = mean(model_data$depth_in_m)),
                           epred = TRUE,
                           level = 0.9)

# contrast marginal effects to see what climate classification levels are sig dif from each other,
# i.e estimate and CI do not include zero

contrast_marginal_effect <- contrast(marginal_effect, method = "revpairwise")

# to visualize gather draws and plot, set .width levels to view confidence intervals

marginal_effect_draws_weekly_ninetieth_perc <- gather_emmeans_draws(contrast_marginal_effect)

ggplot(marginal_effect_draws_weekly_ninetieth_perc, aes(x = .value, y = contrast, fill = contrast)) +
  stat_halfeye(.width = c(0.8,0.9)) +
  geom_vline(xintercept = 0)


### biweekly_ninetieth_perc

## explore data
ggplot(model_data, aes(x = climate_classification, y = biweekly_ninetieth_perc))+
  geom_half_point(aes(color = climate_classification),
                  transformation = position_quasirandom(width = 0.1),
                  side = "l", size = 1, alpha = 0.5)+
  geom_half_boxplot(aes(fill = climate_classification), side = 'r')

ggplot(model_data, aes(x = duration_in_years, y = biweekly_ninetieth_perc)) +
  geom_point(aes(color = climate_classification)) +
  geom_smooth(method = "lm")

ggplot(model_data, aes(x = depth_in_m, y = biweekly_ninetieth_perc)) +
  geom_point(aes(color = climate_classification)) +
  geom_smooth(method = "lm")


# model brms with simple no priors and nested random effects

mod_biweekly_ninetieth_perc <- brm(biweekly_ninetieth_perc ~ climate_classification + duration_in_years + depth_in_m + (1|spatial_blocks),
                                   data = model_data, 
                                   family = "gamma",
                                   chains = 4, iter = 2000, warmup = 1000,
                                   control = list(adapt_delta = 0.99))

pp_check(mod_biweekly_ninetieth_perc, ndraws = 100) # check fit with gamma distribution

summary(mod_biweekly_ninetieth_perc)

# cant interpret coefficients because of gamma distribution
# use emmeans to show marginal effects at average values of other explanatory variables
# 0.9 confidence interval

marginal_effect <- emmeans(mod_biweekly_ninetieth_perc, ~ climate_classification,
                           at = list(duration_in_years = mean(model_data$duration_in_years),
                                     depth_in_m = mean(model_data$depth_in_m)),
                           epred = TRUE,
                           level = 0.9)

# contrast marginal effects to see what climate classification levels are sig dif from each other,
# i.e estimate and CI do not include zero

contrast_marginal_effect <- contrast(marginal_effect, method = "revpairwise")

# to visualize gather draws and plot, set .width levels to view confidence intervals

marginal_effect_draws_biweekly_ninetieth_perc <- gather_emmeans_draws(contrast_marginal_effect)

ggplot(marginal_effect_draws_biweekly_ninetieth_perc, aes(x = .value, y = contrast, fill = contrast)) +
  stat_halfeye(.width = c(0.8,0.9)) +
  geom_vline(xintercept = 0)


### monthly_ninetieth_perc

## explore data
ggplot(model_data, aes(x = climate_classification, y = monthly_ninetieth_perc))+
  geom_half_point(aes(color = climate_classification),
                  transformation = position_quasirandom(width = 0.1),
                  side = "l", size = 1, alpha = 0.5)+
  geom_half_boxplot(aes(fill = climate_classification), side = 'r')

ggplot(model_data, aes(x = duration_in_years, y = monthly_ninetieth_perc)) +
  geom_point(aes(color = climate_classification)) +
  geom_smooth(method = "lm")

ggplot(model_data, aes(x = depth_in_m, y = monthly_ninetieth_perc)) +
  geom_point(aes(color = climate_classification)) +
  geom_smooth(method = "lm")


# model brms with simple no priors and nested random effects

mod_monthly_ninetieth_perc <- brm(monthly_ninetieth_perc ~ climate_classification + duration_in_years + depth_in_m + (1|spatial_blocks),
                                  data = model_data, 
                                  family = "gamma",
                                  chains = 4, iter = 2000, warmup = 1000,
                                  control = list(adapt_delta = 0.99))

pp_check(mod_monthly_ninetieth_perc, ndraws = 100) # check fit with gamma distribution

summary(mod_monthly_ninetieth_perc)

# cant interpret coefficients because of gamma distribution
# use emmeans to show marginal effects at average values of other explanatory variables
# 0.9 confidence interval

marginal_effect <- emmeans(mod_monthly_ninetieth_perc, ~ climate_classification,
                           at = list(duration_in_years = mean(model_data$duration_in_years),
                                     depth_in_m = mean(model_data$depth_in_m)),
                           epred = TRUE,
                           level = 0.9)

# contrast marginal effects to see what climate classification levels are sig dif from each other,
# i.e estimate and CI do not include zero

contrast_marginal_effect <- contrast(marginal_effect, method = "revpairwise")

# to visualize gather draws and plot, set .width levels to view confidence intervals

marginal_effect_draws_monthly_ninetieth_perc <- gather_emmeans_draws(contrast_marginal_effect)

ggplot(marginal_effect_draws_monthly_ninetieth_perc, aes(x = .value, y = contrast, fill = contrast)) +
  stat_halfeye(.width = c(0.8,0.9)) +
  geom_vline(xintercept = 0)


### annual_ninetieth_perc

## explore data
ggplot(model_data, aes(x = climate_classification, y = annual_ninetieth_perc))+
  geom_half_point(aes(color = climate_classification),
                  transformation = position_quasirandom(width = 0.1),
                  side = "l", size = 1, alpha = 0.5)+
  geom_half_boxplot(aes(fill = climate_classification), side = 'r')

ggplot(model_data, aes(x = duration_in_years, y = annual_ninetieth_perc)) +
  geom_point(aes(color = climate_classification)) +
  geom_smooth(method = "lm")

ggplot(model_data, aes(x = depth_in_m, y = annual_ninetieth_perc)) +
  geom_point(aes(color = climate_classification)) +
  geom_smooth(method = "lm")


# model brms with simple no priors and nested random effects

mod_annual_ninetieth_perc <- brm(annual_ninetieth_perc ~ climate_classification + duration_in_years + depth_in_m + (1|spatial_blocks),
                                 data = model_data, 
                                 family = "gamma",
                                 chains = 4, iter = 2000, warmup = 1000,
                                 control = list(adapt_delta = 0.99))

pp_check(mod_annual_ninetieth_perc, ndraws = 100) # check fit with gamma distribution

summary(mod_annual_ninetieth_perc)

# cant interpret coefficients because of gamma distribution
# use emmeans to show marginal effects at average values of other explanatory variables
# 0.9 confidence interval

marginal_effect <- emmeans(mod_annual_ninetieth_perc, ~ climate_classification,
                           at = list(duration_in_years = mean(model_data$duration_in_years),
                                     depth_in_m = mean(model_data$depth_in_m)),
                           epred = TRUE,
                           level = 0.9)

# contrast marginal effects to see what climate classification levels are sig dif from each other,
# i.e estimate and CI do not include zero

contrast_marginal_effect <- contrast(marginal_effect, method = "revpairwise")

# to visualize gather draws and plot, set .width levels to view confidence intervals

marginal_effect_draws_annual_ninetieth_perc <- gather_emmeans_draws(contrast_marginal_effect)

ggplot(marginal_effect_draws_annual_ninetieth_perc, aes(x = .value, y = contrast, fill = contrast)) +
  stat_halfeye(.width = c(0.8,0.9)) +
  geom_vline(xintercept = 0)
