rm(list = ls())

# setwd("/home/581/dh3280/ocean_weather_paper/Main/")
setwd("C:/Users/dlcyli/OneDrive/2024/Ocean Weather Paper - Sensitivity tests Bayesian models output")

library(brms)
library(tidyverse)
library(tidybayes)
library(gghalves)
library(ggbeeswarm)
library(emmeans)
library(grid)
library(gridExtra)
library(ggpubr)

load("all_temp_models.RData")

temp_annual = read.csv("temp range annual - 19 Mar 2023_with_spatial_blocks.csv")
temp_short_term = read.csv("temp range - 19 Mar 2023_with_spatial_blocks.csv")

temp_short_term$depth_in_m[which(temp_short_term$depth_in_m == -99)] = 1.5
temp_annual$depth_in_m[which(temp_annual$depth_in_m == -99)] = 1.5

t1 = Sys.time()

# N.B: In all models, values equal to 0 were removed to allow the gamma distribution to be used. 
# This is reasonable to do because there were negligible 0s in the data 

### q_diurnal_median

## explore data
ggplot(temp_short_term, aes(x = climate_classification, y = q_diurnal_median))+
  geom_half_point(aes(color = climate_classification),
                  transformation = position_quasirandom(width = 0.1),
                  side = "l", size = 1, alpha = 0.5)+
  geom_half_boxplot(aes(fill = climate_classification), side = 'r')

ggplot(temp_short_term, aes(x = depth_in_m, y = q_diurnal_median)) +
  geom_point(aes(color = climate_classification)) +
  geom_smooth(method = "lm")


# model brms with simple flat priors (default) and nested random effects
which(temp_short_term$q_diurnal_median == 0) # values equal to 0 - reject
temp_short_term$q_diurnal_median[which(temp_short_term$q_diurnal_median == 0)] = NA
mod_q_diurnal_median <- brm(q_diurnal_median ~ climate_classification + depth_in_m + 
                              (1|spatial_blocks/plot_id),
                            data = temp_short_term, 
                            family = Gamma(link = "log"),
                            chains = 4, iter = 2000, warmup = 1000,
                            control = list(adapt_delta = 0.99,
                                           max_treedepth = 15))

check_fit_q_diurnal_median = pp_check(mod_q_diurnal_median, ndraws = 100) + 
  annotation_custom(grobTree(textGrob(expression(bold("a")), x=0.95,  y=0.92,
                                      gp=gpar(fontsize=15)))) + 
  theme(legend.position = "None") # check fit with gamma distribution

summary_q_diurnal_median = summary(mod_q_diurnal_median)

# cant interpret coefficients because of gamma distribution
# use emmeans to show marginal effects at average values of other explanatory variables
# 0.9 credible interval

marginal_effect <- emmeans(mod_q_diurnal_median, ~ climate_classification,
                           at = list(depth_in_m = mean(temp_short_term$depth_in_m)),
                           epred = TRUE,
                           level = 0.9)

# contrast marginal effects to see what climate classification levels are sig dif from each other,
# i.e estimate and CI do not include zero

contrast_marginal_effect <- contrast(marginal_effect, method = "revpairwise")

# to visualize gather draws and plot, set .width levels to view credible intervals

marginal_effect_draws_q_diurnal_median <- gather_emmeans_draws(contrast_marginal_effect)

ggplot_q_diurnal_median = ggplot(marginal_effect_draws_q_diurnal_median, 
                                 aes(x = .value, y = contrast, fill = contrast)) +
  stat_halfeye(.width = c(0.8,0.9)) +
  geom_vline(xintercept = 0) + 
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "None") + 
  annotation_custom(grobTree(textGrob(expression(bold("a")), x=0.05,  y=0.92,
                                      gp=gpar(fontsize=15))))

### s_diurnal_median

## explore data
ggplot(temp_short_term, aes(x = climate_classification, y = s_diurnal_median))+
  geom_half_point(aes(color = climate_classification),
                  transformation = position_quasirandom(width = 0.1),
                  side = "l", size = 1, alpha = 0.5)+
  geom_half_boxplot(aes(fill = climate_classification), side = 'r')

ggplot(temp_short_term, aes(x = depth_in_m, y = s_diurnal_median)) +
  geom_point(aes(color = climate_classification)) +
  geom_smooth(method = "lm")


# model brms with simple flat priors (default) and nested random effects
which(temp_short_term$s_diurnal_median == 0) # values equal to 0 - reject
temp_short_term$s_diurnal_median[which(temp_short_term$s_diurnal_median == 0)] = NA
mod_s_diurnal_median <- brm(s_diurnal_median ~ climate_classification + depth_in_m + 
                              (1|spatial_blocks/plot_id),
                            data = temp_short_term, 
                            family = Gamma(link = "log"),
                            chains = 4, iter = 2000, warmup = 1000,
                            control = list(adapt_delta = 0.99,
                                           max_treedepth = 15))

check_fit_s_diurnal_median = pp_check(mod_s_diurnal_median, ndraws = 100) + 
  annotation_custom(grobTree(textGrob(expression(bold("b")), x=0.95,  y=0.92,
                                      gp=gpar(fontsize=15)))) + 
  theme(legend.position = "None") # check fit with gamma distribution

summary_s_diurnal_median = summary(mod_s_diurnal_median)

# cant interpret coefficients because of gamma distribution
# use emmeans to show marginal effects at average values of other explanatory variables
# 0.9 credible interval

marginal_effect <- emmeans(mod_s_diurnal_median, ~ climate_classification,
                           at = list(depth_in_m = mean(temp_short_term$depth_in_m)),
                           epred = TRUE,
                           level = 0.9)

# contrast marginal effects to see what climate classification levels are sig dif from each other,
# i.e estimate and CI do not include zero

contrast_marginal_effect <- contrast(marginal_effect, method = "revpairwise")

# to visualize gather draws and plot, set .width levels to view credible intervals

marginal_effect_draws_s_diurnal_median <- gather_emmeans_draws(contrast_marginal_effect)

ggplot_s_diurnal_median = ggplot(marginal_effect_draws_s_diurnal_median, 
                                 aes(x = .value, y = contrast, fill = contrast)) +
  stat_halfeye(.width = c(0.8,0.9)) +
  geom_vline(xintercept = 0) + 
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "None") + 
  annotation_custom(grobTree(textGrob(expression(bold("b")), x=0.05,  y=0.92,
                                      gp=gpar(fontsize=15))))

### daily_median

## explore data
ggplot(temp_short_term, aes(x = climate_classification, y = daily_median))+
  geom_half_point(aes(color = climate_classification),
                  transformation = position_quasirandom(width = 0.1),
                  side = "l", size = 1, alpha = 0.5)+
  geom_half_boxplot(aes(fill = climate_classification), side = 'r')

ggplot(temp_short_term, aes(x = depth_in_m, y = daily_median)) +
  geom_point(aes(color = climate_classification)) +
  geom_smooth(method = "lm")


# model brms with simple flat priors (default) and nested random effects
which(temp_short_term$daily_median == 0) # values equal to 0 - reject
temp_short_term$daily_median[which(temp_short_term$daily_median == 0)] = NA
mod_daily_median <- brm(daily_median ~ climate_classification + depth_in_m + 
                          (1|spatial_blocks/plot_id),
                        data = temp_short_term, 
                        family = Gamma(link = "log"),
                        chains = 4, iter = 2000, warmup = 1000,
                        control = list(adapt_delta = 0.99,
                                       max_treedepth = 15))

check_fit_daily_median = pp_check(mod_daily_median, ndraws = 100) + 
  annotation_custom(grobTree(textGrob(expression(bold("c")), x=0.95,  y=0.92,
                                      gp=gpar(fontsize=15)))) + 
  theme(legend.position = "None") # check fit with gamma distribution

summary_daily_median = summary(mod_daily_median)

# cant interpret coefficients because of gamma distribution
# use emmeans to show marginal effects at average values of other explanatory variables
# 0.9 credible interval

marginal_effect <- emmeans(mod_daily_median, ~ climate_classification,
                           at = list(depth_in_m = mean(temp_short_term$depth_in_m)),
                           epred = TRUE,
                           level = 0.9)

# contrast marginal effects to see what climate classification levels are sig dif from each other,
# i.e estimate and CI do not include zero

contrast_marginal_effect <- contrast(marginal_effect, method = "revpairwise")

# to visualize gather draws and plot, set .width levels to view credible intervals

marginal_effect_draws_daily_median <- gather_emmeans_draws(contrast_marginal_effect)

ggplot_daily_median = ggplot(marginal_effect_draws_daily_median, 
                             aes(x = .value, y = contrast, fill = contrast)) +
  stat_halfeye(.width = c(0.8,0.9)) +
  geom_vline(xintercept = 0) + 
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "None") + 
  annotation_custom(grobTree(textGrob(expression(bold("c")), x=0.05,  y=0.92,
                                      gp=gpar(fontsize=15))))


### weekly_median

## explore data
ggplot(temp_short_term, aes(x = climate_classification, y = weekly_median))+
  geom_half_point(aes(color = climate_classification),
                  transformation = position_quasirandom(width = 0.1),
                  side = "l", size = 1, alpha = 0.5)+
  geom_half_boxplot(aes(fill = climate_classification), side = 'r')

ggplot(temp_short_term, aes(x = depth_in_m, y = weekly_median)) +
  geom_point(aes(color = climate_classification)) +
  geom_smooth(method = "lm")


# model brms with simple flat priors (default) and nested random effects
which(temp_short_term$weekly_median == 0) # values equal to 0 - reject
temp_short_term$weekly_median[which(temp_short_term$weekly_median == 0)] = NA
mod_weekly_median <- brm(weekly_median ~ climate_classification + depth_in_m + 
                           (1|spatial_blocks/plot_id),
                         data = temp_short_term, 
                         family = Gamma(link = "log"),
                         chains = 4, iter = 2000, warmup = 1000,
                         control = list(adapt_delta = 0.99,
                                        max_treedepth = 15))

check_fit_weekly_median = pp_check(mod_weekly_median, ndraws = 100) + 
  annotation_custom(grobTree(textGrob(expression(bold("d")), x=0.95,  y=0.92,
                                      gp=gpar(fontsize=15)))) + 
  theme(legend.position = "None") # check fit with gamma distribution

summary_weekly_median = summary(mod_weekly_median)

# cant interpret coefficients because of gamma distribution
# use emmeans to show marginal effects at average values of other explanatory variables
# 0.9 credible interval

marginal_effect <- emmeans(mod_weekly_median, ~ climate_classification,
                           at = list(depth_in_m = mean(temp_short_term$depth_in_m)),
                           epred = TRUE,
                           level = 0.9)

# contrast marginal effects to see what climate classification levels are sig dif from each other,
# i.e estimate and CI do not include zero

contrast_marginal_effect <- contrast(marginal_effect, method = "revpairwise")

# to visualize gather draws and plot, set .width levels to view credible intervals

marginal_effect_draws_weekly_median <- gather_emmeans_draws(contrast_marginal_effect)

ggplot_weekly_median = ggplot(marginal_effect_draws_weekly_median, 
                              aes(x = .value, y = contrast, fill = contrast)) +
  stat_halfeye(.width = c(0.8,0.9)) +
  geom_vline(xintercept = 0) + 
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "None") + 
  annotation_custom(grobTree(textGrob(expression(bold("d")), x=0.05,  y=0.92,
                                      gp=gpar(fontsize=15))))


### biweekly_median

## explore data
ggplot(temp_short_term, aes(x = climate_classification, y = biweekly_median))+
  geom_half_point(aes(color = climate_classification),
                  transformation = position_quasirandom(width = 0.1),
                  side = "l", size = 1, alpha = 0.5)+
  geom_half_boxplot(aes(fill = climate_classification), side = 'r')

ggplot(temp_short_term, aes(x = depth_in_m, y = biweekly_median)) +
  geom_point(aes(color = climate_classification)) +
  geom_smooth(method = "lm")


# model brms with simple flat priors (default) and nested random effects
which(temp_short_term$biweekly_median == 0) # values equal to 0 - reject
temp_short_term$biweekly_median[which(temp_short_term$biweekly_median == 0)] = NA
mod_biweekly_median <- brm(biweekly_median ~ climate_classification + depth_in_m + 
                             (1|spatial_blocks/plot_id),
                           data = temp_short_term, 
                           family = Gamma(link = "log"),
                           chains = 4, iter = 2000, warmup = 1000,
                           control = list(adapt_delta = 0.99,
                                          max_treedepth = 15))

check_fit_biweekly_median = pp_check(mod_biweekly_median, ndraws = 100) + 
  annotation_custom(grobTree(textGrob(expression(bold("e")), x=0.95,  y=0.92,
                                      gp=gpar(fontsize=15)))) + 
  theme(legend.position = "None") # check fit with gamma distribution

summary_biweekly_median = summary(mod_biweekly_median)

# cant interpret coefficients because of gamma distribution
# use emmeans to show marginal effects at average values of other explanatory variables
# 0.9 credible interval

marginal_effect <- emmeans(mod_biweekly_median, ~ climate_classification,
                           at = list(depth_in_m = mean(temp_short_term$depth_in_m)),
                           epred = TRUE,
                           level = 0.9)

# contrast marginal effects to see what climate classification levels are sig dif from each other,
# i.e estimate and CI do not include zero

contrast_marginal_effect <- contrast(marginal_effect, method = "revpairwise")

# to visualize gather draws and plot, set .width levels to view credible intervals

marginal_effect_draws_biweekly_median <- gather_emmeans_draws(contrast_marginal_effect)

ggplot_biweekly_median = ggplot(marginal_effect_draws_biweekly_median, 
                                aes(x = .value, y = contrast, fill = contrast)) +
  stat_halfeye(.width = c(0.8,0.9)) +
  geom_vline(xintercept = 0) + 
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "None") + 
  annotation_custom(grobTree(textGrob(expression(bold("e")), x=0.05,  y=0.92,
                                      gp=gpar(fontsize=15))))


### monthly_median

## explore data
ggplot(temp_short_term, aes(x = climate_classification, y = monthly_median))+
  geom_half_point(aes(color = climate_classification),
                  transformation = position_quasirandom(width = 0.1),
                  side = "l", size = 1, alpha = 0.5)+
  geom_half_boxplot(aes(fill = climate_classification), side = 'r')

ggplot(temp_short_term, aes(x = depth_in_m, y = monthly_median)) +
  geom_point(aes(color = climate_classification)) +
  geom_smooth(method = "lm")


# model brms with simple flat priors (default) and nested random effects
which(temp_short_term$monthly_median == 0) # values equal to 0 - reject
temp_short_term$monthly_median[which(temp_short_term$monthly_median == 0)] = NA
mod_monthly_median <- brm(monthly_median ~ climate_classification + depth_in_m + 
                            (1|spatial_blocks/plot_id),
                          data = temp_short_term, 
                          family = Gamma(link = "log"),
                          chains = 4, iter = 2000, warmup = 1000,
                          control = list(adapt_delta = 0.99,
                                         max_treedepth = 15))

check_fit_monthly_median = pp_check(mod_monthly_median, ndraws = 100) + 
  annotation_custom(grobTree(textGrob(expression(bold("f")), x=0.95,  y=0.92,
                                      gp=gpar(fontsize=15)))) + 
  theme(legend.position = "None") # check fit with gamma distribution

summary_monthly_median = summary(mod_monthly_median)

# cant interpret coefficients because of gamma distribution
# use emmeans to show marginal effects at average values of other explanatory variables
# 0.9 credible interval

marginal_effect <- emmeans(mod_monthly_median, ~ climate_classification,
                           at = list(depth_in_m = mean(temp_short_term$depth_in_m)),
                           epred = TRUE,
                           level = 0.9)

# contrast marginal effects to see what climate classification levels are sig dif from each other,
# i.e estimate and CI do not include zero

contrast_marginal_effect <- contrast(marginal_effect, method = "revpairwise")

# to visualize gather draws and plot, set .width levels to view credible intervals

marginal_effect_draws_monthly_median <- gather_emmeans_draws(contrast_marginal_effect)

ggplot_monthly_median = ggplot(marginal_effect_draws_monthly_median, 
                               aes(x = .value, y = contrast, fill = contrast)) +
  stat_halfeye(.width = c(0.8,0.9)) +
  geom_vline(xintercept = 0) + 
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "None") + 
  annotation_custom(grobTree(textGrob(expression(bold("f")), x=0.05,  y=0.92,
                                      gp=gpar(fontsize=15))))


### annual_median

## explore data
ggplot(temp_annual, aes(x = climate_classification, y = annual_median))+
  geom_half_point(aes(color = climate_classification),
                  transformation = position_quasirandom(width = 0.1),
                  side = "l", size = 1, alpha = 0.5)+
  geom_half_boxplot(aes(fill = climate_classification), side = 'r')

ggplot(temp_annual, aes(x = depth_in_m, y = annual_median)) +
  geom_point(aes(color = climate_classification)) +
  geom_smooth(method = "lm")


# model brms with simple flat priors (default) and nested random effects
which(temp_annual$annual_median == 0) # values equal to 0 - reject
temp_annual$annual_median[which(temp_annual$annual_median == 0)] = NA
mod_annual_median <- brm(annual_median ~ climate_classification + depth_in_m +
                           (1|spatial_blocks),
                         data = temp_annual,
                         family = Gamma(link = "log"),
                         chains = 4, iter = 2000, warmup = 1000,
                         control = list(adapt_delta = 0.99,
                                        max_treedepth = 15))

check_fit_annual_median = pp_check(mod_annual_median, ndraws = 100) + 
  annotation_custom(grobTree(textGrob(expression(bold("g")), x=0.95,  y=0.92,
                                      gp=gpar(fontsize=15)))) + 
  theme(legend.position = "None") # check fit with gamma distribution

summary_annual_median = summary(mod_annual_median)

# cant interpret coefficients because of gamma distribution
# use emmeans to show marginal effects at average values of other explanatory variables
# 0.9 credible interval

marginal_effect <- emmeans(mod_annual_median, ~ climate_classification,
                           at = list(depth_in_m = mean(temp_annual$depth_in_m)),
                           epred = TRUE,
                           level = 0.9)

# contrast marginal effects to see what climate classification levels are sig dif from each other,
# i.e estimate and CI do not include zero

contrast_marginal_effect <- contrast(marginal_effect, method = "revpairwise")

# to visualize gather draws and plot, set .width levels to view credible intervals

marginal_effect_draws_annual_median <- gather_emmeans_draws(contrast_marginal_effect)

ggplot_annual_median = ggplot(marginal_effect_draws_annual_median, 
                              aes(x = .value, y = contrast, fill = contrast)) +
  stat_halfeye(.width = c(0.8,0.9)) +
  geom_vline(xintercept = 0) + 
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "None") + 
  annotation_custom(grobTree(textGrob(expression(bold("g")), x=0.05,  y=0.92,
                                      gp=gpar(fontsize=15))))

### q_diurnal_ninetieth_perc

## explore data
ggplot(temp_short_term, aes(x = climate_classification, y = q_diurnal_ninetieth_perc))+
  geom_half_point(aes(color = climate_classification),
                  transformation = position_quasirandom(width = 0.1),
                  side = "l", size = 1, alpha = 0.5)+
  geom_half_boxplot(aes(fill = climate_classification), side = 'r')

ggplot(temp_short_term, aes(x = depth_in_m, y = q_diurnal_ninetieth_perc)) +
  geom_point(aes(color = climate_classification)) +
  geom_smooth(method = "lm")


# model brms with simple flat priors (default) and nested random effects
which(temp_short_term$q_diurnal_ninetieth_perc == 0) # values equal to 0 - reject
temp_short_term$q_diurnal_ninetieth_perc[which(temp_short_term$q_diurnal_ninetieth_perc == 0)] = NA
mod_q_diurnal_ninetieth_perc <- brm(q_diurnal_ninetieth_perc ~ climate_classification + depth_in_m + 
                                      (1|spatial_blocks/plot_id),
                                    data = temp_short_term, 
                                    family = Gamma(link = "log"),
                                    chains = 4, iter = 2000, warmup = 1000,
                                    control = list(adapt_delta = 0.99,
                                                   max_treedepth = 15))

check_fit_q_diurnal_ninetieth_perc = pp_check(mod_q_diurnal_ninetieth_perc, ndraws = 100) + 
  annotation_custom(grobTree(textGrob(expression(bold("a")), x=0.95,  y=0.92,
                                      gp=gpar(fontsize=15)))) + 
  theme(legend.position = "None") # check fit with gamma distribution

summary_q_diurnal_ninetieth_perc = summary(mod_q_diurnal_ninetieth_perc)

# cant interpret coefficients because of gamma distribution
# use emmeans to show marginal effects at average values of other explanatory variables
# 0.9 credible interval

marginal_effect <- emmeans(mod_q_diurnal_ninetieth_perc, ~ climate_classification,
                           at = list(depth_in_m = mean(temp_short_term$depth_in_m)),
                           epred = TRUE,
                           level = 0.9)

# contrast marginal effects to see what climate classification levels are sig dif from each other,
# i.e estimate and CI do not include zero

contrast_marginal_effect <- contrast(marginal_effect, method = "revpairwise")

# to visualize gather draws and plot, set .width levels to view credible intervals

marginal_effect_draws_q_diurnal_ninetieth_perc <- gather_emmeans_draws(contrast_marginal_effect)

ggplot_q_diurnal_ninetieth_perc = ggplot(marginal_effect_draws_q_diurnal_ninetieth_perc, 
                                         aes(x = .value, y = contrast, fill = contrast)) +
  stat_halfeye(.width = c(0.8,0.9)) +
  geom_vline(xintercept = 0) + 
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "None") + 
  annotation_custom(grobTree(textGrob(expression(bold("a")), x=0.05,  y=0.92,
                                      gp=gpar(fontsize=15))))

### s_diurnal_ninetieth_perc

## explore data
ggplot(temp_short_term, aes(x = climate_classification, y = s_diurnal_ninetieth_perc))+
  geom_half_point(aes(color = climate_classification),
                  transformation = position_quasirandom(width = 0.1),
                  side = "l", size = 1, alpha = 0.5)+
  geom_half_boxplot(aes(fill = climate_classification), side = 'r')

ggplot(temp_short_term, aes(x = depth_in_m, y = s_diurnal_ninetieth_perc)) +
  geom_point(aes(color = climate_classification)) +
  geom_smooth(method = "lm")


# model brms with simple flat priors (default) and nested random effects
which(temp_short_term$s_diurnal_ninetieth_perc == 0) # values equal to 0 - reject
temp_short_term$s_diurnal_ninetieth_perc[which(temp_short_term$s_diurnal_ninetieth_perc == 0)] = NA
mod_s_diurnal_ninetieth_perc <- brm(s_diurnal_ninetieth_perc ~ climate_classification + depth_in_m + 
                                      (1|spatial_blocks/plot_id),
                                    data = temp_short_term, 
                                    family = Gamma(link = "log"),
                                    chains = 4, iter = 2000, warmup = 1000,
                                    control = list(adapt_delta = 0.99,
                                                   max_treedepth = 15))

check_fit_s_diurnal_ninetieth_perc = pp_check(mod_s_diurnal_ninetieth_perc, ndraws = 100) + 
  annotation_custom(grobTree(textGrob(expression(bold("b")), x=0.95,  y=0.92,
                                      gp=gpar(fontsize=15)))) + 
  theme(legend.position = "None") # check fit with gamma distribution

summary_s_diurnal_ninetieth_perc = summary(mod_s_diurnal_ninetieth_perc)

# cant interpret coefficients because of gamma distribution
# use emmeans to show marginal effects at average values of other explanatory variables
# 0.9 credible interval

marginal_effect <- emmeans(mod_s_diurnal_ninetieth_perc, ~ climate_classification,
                           at = list(depth_in_m = mean(temp_short_term$depth_in_m)),
                           epred = TRUE,
                           level = 0.9)

# contrast marginal effects to see what climate classification levels are sig dif from each other,
# i.e estimate and CI do not include zero

contrast_marginal_effect <- contrast(marginal_effect, method = "revpairwise")

# to visualize gather draws and plot, set .width levels to view credible intervals

marginal_effect_draws_s_diurnal_ninetieth_perc <- gather_emmeans_draws(contrast_marginal_effect)

ggplot_s_diurnal_ninetieth_perc = ggplot(marginal_effect_draws_s_diurnal_ninetieth_perc, 
                                         aes(x = .value, y = contrast, fill = contrast)) +
  stat_halfeye(.width = c(0.8,0.9)) +
  geom_vline(xintercept = 0) + 
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "None") + 
  annotation_custom(grobTree(textGrob(expression(bold("b")), x=0.05,  y=0.92,
                                      gp=gpar(fontsize=15))))

### daily_ninetieth_perc

## explore data
ggplot(temp_short_term, aes(x = climate_classification, y = daily_ninetieth_perc))+
  geom_half_point(aes(color = climate_classification),
                  transformation = position_quasirandom(width = 0.1),
                  side = "l", size = 1, alpha = 0.5)+
  geom_half_boxplot(aes(fill = climate_classification), side = 'r')

ggplot(temp_short_term, aes(x = depth_in_m, y = daily_ninetieth_perc)) +
  geom_point(aes(color = climate_classification)) +
  geom_smooth(method = "lm")


# model brms with simple flat priors (default) and nested random effects
which(temp_short_term$daily_ninetieth_perc == 0) # values equal to 0 - reject
temp_short_term$daily_ninetieth_perc[which(temp_short_term$daily_ninetieth_perc == 0)] = NA
mod_daily_ninetieth_perc <- brm(daily_ninetieth_perc ~ climate_classification + depth_in_m + 
                                  (1|spatial_blocks/plot_id),
                                data = temp_short_term, 
                                family = Gamma(link = "log"),
                                chains = 4, iter = 2000, warmup = 1000,
                                control = list(adapt_delta = 0.99,
                                               max_treedepth = 15))

check_fit_daily_ninetieth_perc = pp_check(mod_daily_ninetieth_perc, ndraws = 100) + 
  annotation_custom(grobTree(textGrob(expression(bold("c")), x=0.95,  y=0.92,
                                      gp=gpar(fontsize=15)))) + 
  theme(legend.position = "None") # check fit with gamma distribution

summary_daily_ninetieth_perc = summary(mod_daily_ninetieth_perc)

# cant interpret coefficients because of gamma distribution
# use emmeans to show marginal effects at average values of other explanatory variables
# 0.9 credible interval

marginal_effect <- emmeans(mod_daily_ninetieth_perc, ~ climate_classification,
                           at = list(depth_in_m = mean(temp_short_term$depth_in_m)),
                           epred = TRUE,
                           level = 0.9)

# contrast marginal effects to see what climate classification levels are sig dif from each other,
# i.e estimate and CI do not include zero

contrast_marginal_effect <- contrast(marginal_effect, method = "revpairwise")

# to visualize gather draws and plot, set .width levels to view credible intervals

marginal_effect_draws_daily_ninetieth_perc <- gather_emmeans_draws(contrast_marginal_effect)

ggplot_daily_ninetieth_perc = ggplot(marginal_effect_draws_daily_ninetieth_perc, 
                                     aes(x = .value, y = contrast, fill = contrast)) +
  stat_halfeye(.width = c(0.8,0.9)) +
  geom_vline(xintercept = 0) + 
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "None") + 
  annotation_custom(grobTree(textGrob(expression(bold("c")), x=0.05,  y=0.92,
                                      gp=gpar(fontsize=15))))


### weekly_ninetieth_perc

## explore data
ggplot(temp_short_term, aes(x = climate_classification, y = weekly_ninetieth_perc))+
  geom_half_point(aes(color = climate_classification),
                  transformation = position_quasirandom(width = 0.1),
                  side = "l", size = 1, alpha = 0.5)+
  geom_half_boxplot(aes(fill = climate_classification), side = 'r')

ggplot(temp_short_term, aes(x = depth_in_m, y = weekly_ninetieth_perc)) +
  geom_point(aes(color = climate_classification)) +
  geom_smooth(method = "lm")


# model brms with simple flat priors (default) and nested random effects
which(temp_short_term$weekly_ninetieth_perc == 0) # values equal to 0 - reject
temp_short_term$weekly_ninetieth_perc[which(temp_short_term$weekly_ninetieth_perc == 0)] = NA
mod_weekly_ninetieth_perc <- brm(weekly_ninetieth_perc ~ climate_classification + depth_in_m + 
                                   (1|spatial_blocks/plot_id),
                                 data = temp_short_term, 
                                 family = Gamma(link = "log"),
                                 chains = 4, iter = 2000, warmup = 1000,
                                 control = list(adapt_delta = 0.99,
                                                max_treedepth = 15))

check_fit_weekly_ninetieth_perc = pp_check(mod_weekly_ninetieth_perc, ndraws = 100) + 
  annotation_custom(grobTree(textGrob(expression(bold("d")), x=0.95,  y=0.92,
                                      gp=gpar(fontsize=15)))) + 
  theme(legend.position = "None") # check fit with gamma distribution

summary_weekly_ninetieth_perc = summary(mod_weekly_ninetieth_perc)

# cant interpret coefficients because of gamma distribution
# use emmeans to show marginal effects at average values of other explanatory variables
# 0.9 credible interval

marginal_effect <- emmeans(mod_weekly_ninetieth_perc, ~ climate_classification,
                           at = list(depth_in_m = mean(temp_short_term$depth_in_m)),
                           epred = TRUE,
                           level = 0.9)

# contrast marginal effects to see what climate classification levels are sig dif from each other,
# i.e estimate and CI do not include zero

contrast_marginal_effect <- contrast(marginal_effect, method = "revpairwise")

# to visualize gather draws and plot, set .width levels to view credible intervals

marginal_effect_draws_weekly_ninetieth_perc <- gather_emmeans_draws(contrast_marginal_effect)

ggplot_weekly_ninetieth_perc = ggplot(marginal_effect_draws_weekly_ninetieth_perc, 
                                      aes(x = .value, y = contrast, fill = contrast)) +
  stat_halfeye(.width = c(0.8,0.9)) +
  geom_vline(xintercept = 0) + 
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "None") + 
  annotation_custom(grobTree(textGrob(expression(bold("d")), x=0.05,  y=0.92,
                                      gp=gpar(fontsize=15))))


### biweekly_ninetieth_perc

## explore data
ggplot(temp_short_term, aes(x = climate_classification, y = biweekly_ninetieth_perc))+
  geom_half_point(aes(color = climate_classification),
                  transformation = position_quasirandom(width = 0.1),
                  side = "l", size = 1, alpha = 0.5)+
  geom_half_boxplot(aes(fill = climate_classification), side = 'r')

ggplot(temp_short_term, aes(x = depth_in_m, y = biweekly_ninetieth_perc)) +
  geom_point(aes(color = climate_classification)) +
  geom_smooth(method = "lm")


# model brms with simple flat priors (default) and nested random effects
which(temp_short_term$biweekly_ninetieth_perc == 0) # values equal to 0 - reject
temp_short_term$biweekly_ninetieth_perc[which(temp_short_term$biweekly_ninetieth_perc == 0)] = NA
mod_biweekly_ninetieth_perc <- brm(biweekly_ninetieth_perc ~ climate_classification + depth_in_m + 
                                     (1|spatial_blocks/plot_id),
                                   data = temp_short_term, 
                                   family = Gamma(link = "log"),
                                   chains = 4, iter = 2000, warmup = 1000,
                                   control = list(adapt_delta = 0.99,
                                                  max_treedepth = 15))

check_fit_biweekly_ninetieth_perc = pp_check(mod_biweekly_ninetieth_perc, ndraws = 100) + 
  annotation_custom(grobTree(textGrob(expression(bold("e")), x=0.95,  y=0.92,
                                      gp=gpar(fontsize=15)))) + 
  theme(legend.position = "None") # check fit with gamma distribution

summary_biweekly_ninetieth_perc = summary(mod_biweekly_ninetieth_perc)

# cant interpret coefficients because of gamma distribution
# use emmeans to show marginal effects at average values of other explanatory variables
# 0.9 credible interval

marginal_effect <- emmeans(mod_biweekly_ninetieth_perc, ~ climate_classification,
                           at = list(depth_in_m = mean(temp_short_term$depth_in_m)),
                           epred = TRUE,
                           level = 0.9)

# contrast marginal effects to see what climate classification levels are sig dif from each other,
# i.e estimate and CI do not include zero

contrast_marginal_effect <- contrast(marginal_effect, method = "revpairwise")

# to visualize gather draws and plot, set .width levels to view credible intervals

marginal_effect_draws_biweekly_ninetieth_perc <- gather_emmeans_draws(contrast_marginal_effect)

ggplot_biweekly_ninetieth_perc = ggplot(marginal_effect_draws_biweekly_ninetieth_perc, 
                                        aes(x = .value, y = contrast, fill = contrast)) +
  stat_halfeye(.width = c(0.8,0.9)) +
  geom_vline(xintercept = 0) + 
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "None") + 
  annotation_custom(grobTree(textGrob(expression(bold("e")), x=0.05,  y=0.92,
                                      gp=gpar(fontsize=15))))


### monthly_ninetieth_perc

## explore data
ggplot(temp_short_term, aes(x = climate_classification, y = monthly_ninetieth_perc))+
  geom_half_point(aes(color = climate_classification),
                  transformation = position_quasirandom(width = 0.1),
                  side = "l", size = 1, alpha = 0.5)+
  geom_half_boxplot(aes(fill = climate_classification), side = 'r')

ggplot(temp_short_term, aes(x = depth_in_m, y = monthly_ninetieth_perc)) +
  geom_point(aes(color = climate_classification)) +
  geom_smooth(method = "lm")


# model brms with simple flat priors (default) and nested random effects
which(temp_short_term$monthly_ninetieth_perc == 0) # values equal to 0 - reject
temp_short_term$monthly_ninetieth_perc[which(temp_short_term$monthly_ninetieth_perc == 0)] = NA
mod_monthly_ninetieth_perc <- brm(monthly_ninetieth_perc ~ climate_classification + depth_in_m + 
                                    (1|spatial_blocks/plot_id),
                                  data = temp_short_term, 
                                  family = Gamma(link = "log"),
                                  chains = 4, iter = 2000, warmup = 1000,
                                  control = list(adapt_delta = 0.99,
                                                 max_treedepth = 15))

check_fit_monthly_ninetieth_perc = pp_check(mod_monthly_ninetieth_perc, ndraws = 100) + 
  annotation_custom(grobTree(textGrob(expression(bold("f")), x=0.95,  y=0.92,
                                      gp=gpar(fontsize=15)))) + 
  theme(legend.position = "None") # check fit with gamma distribution

summary_monthly_ninetieth_perc = summary(mod_monthly_ninetieth_perc)

# cant interpret coefficients because of gamma distribution
# use emmeans to show marginal effects at average values of other explanatory variables
# 0.9 credible interval

marginal_effect <- emmeans(mod_monthly_ninetieth_perc, ~ climate_classification,
                           at = list(depth_in_m = mean(temp_short_term$depth_in_m)),
                           epred = TRUE,
                           level = 0.9)

# contrast marginal effects to see what climate classification levels are sig dif from each other,
# i.e estimate and CI do not include zero

contrast_marginal_effect <- contrast(marginal_effect, method = "revpairwise")

# to visualize gather draws and plot, set .width levels to view credible intervals

marginal_effect_draws_monthly_ninetieth_perc <- gather_emmeans_draws(contrast_marginal_effect)

ggplot_monthly_ninetieth_perc = ggplot(marginal_effect_draws_monthly_ninetieth_perc, 
                                       aes(x = .value, y = contrast, fill = contrast)) +
  stat_halfeye(.width = c(0.8,0.9)) +
  geom_vline(xintercept = 0) + 
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "None") + 
  annotation_custom(grobTree(textGrob(expression(bold("f")), x=0.05,  y=0.92,
                                      gp=gpar(fontsize=15))))


### annual_ninetieth_perc

## explore data
ggplot(temp_annual, aes(x = climate_classification, y = annual_ninetieth_perc))+
  geom_half_point(aes(color = climate_classification),
                  transformation = position_quasirandom(width = 0.1),
                  side = "l", size = 1, alpha = 0.5)+
  geom_half_boxplot(aes(fill = climate_classification), side = 'r')

ggplot(temp_annual, aes(x = depth_in_m, y = annual_ninetieth_perc)) +
  geom_point(aes(color = climate_classification)) +
  geom_smooth(method = "lm")


# model brms with simple flat priors (default) and nested random effects
which(temp_annual$annual_ninetieth_perc == 0) # values equal to 0 - reject
temp_annual$annual_ninetieth_perc[which(temp_annual$annual_ninetieth_perc == 0)] = NA
mod_annual_ninetieth_perc <- brm(annual_ninetieth_perc ~ climate_classification + depth_in_m +
                                   (1|spatial_blocks),
                                 data = temp_annual,
                                 family = Gamma(link = "log"),
                                 chains = 4, iter = 2000, warmup = 1000,
                                 control = list(adapt_delta = 0.99,
                                                max_treedepth = 15))

check_fit_annual_ninetieth_perc = pp_check(mod_annual_ninetieth_perc, ndraws = 100) +
  annotation_custom(grobTree(textGrob(expression(bold("g")), x=0.95,  y=0.92,
                                      gp=gpar(fontsize=15)))) +
  theme(legend.position = "None") # check fit with gamma distribution
summary_annual_ninetieth_perc = summary(mod_annual_ninetieth_perc)

# cant interpret coefficients because of gamma distribution
# use emmeans to show marginal effects at average values of other explanatory variables
# 0.9 credible interval

marginal_effect <- emmeans(mod_annual_ninetieth_perc, ~ climate_classification,
                           at = list(depth_in_m = mean(temp_annual$depth_in_m)),
                           epred = TRUE,
                           level = 0.9)

# contrast marginal effects to see what climate classification levels are sig dif from each other,
# i.e estimate and CI do not include zero

contrast_marginal_effect <- contrast(marginal_effect, method = "revpairwise")

# to visualize gather draws and plot, set .width levels to view credible intervals

marginal_effect_draws_annual_ninetieth_perc <- gather_emmeans_draws(contrast_marginal_effect)

ggplot_annual_ninetieth_perc = ggplot(marginal_effect_draws_annual_ninetieth_perc, 
                                      aes(x = .value, y = contrast, fill = contrast)) +
  stat_halfeye(.width = c(0.8,0.9)) +
  geom_vline(xintercept = 0) + 
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "None") + 
  annotation_custom(grobTree(textGrob(expression(bold("g")), x=0.05,  y=0.92,
                                      gp=gpar(fontsize=15))))

t2 = Sys.time()
t2-t1

# Save all objects 
# save.image(file = "all_temp_models.RData")

setwd("G:/Other computers/My Laptop/Documents/HOTS Database/Final Paper/RMarkDown/Sensitivity Test/Bayesian modeling for sensitivity tests/Main")

# plots
ggplot_legend = ggplot(marginal_effect_draws_annual_ninetieth_perc, 
                       aes(x = .value, y = contrast, fill = contrast)) +
  labs(fill="Contrast") +
  stat_halfeye(.width = c(0.8,0.9)) +
  geom_vline(xintercept = 0) 

legend1= get_legend(ggplot_legend)
as_ggplot(legend1)

legend2= get_legend(pp_check(mod_annual_ninetieth_perc, ndraws = 100))
as_ggplot(legend2)

ggsave(filename="temp_median.png",height=5, width=11, units="in", 
       plot=grid.arrange(ggplot_q_diurnal_median, ggplot_s_diurnal_median, 
                         ggplot_daily_median, ggplot_weekly_median, 
                         ggplot_biweekly_median, ggplot_monthly_median, 
                         ggplot_annual_median, as_ggplot(legend1),
                         ncol = 4), device="png")

ggsave(filename="temp_ninetieth_perc.png",height=5, width=11, units="in", 
       plot=grid.arrange(ggplot_q_diurnal_ninetieth_perc, ggplot_s_diurnal_ninetieth_perc, 
                         ggplot_daily_ninetieth_perc, ggplot_weekly_ninetieth_perc, 
                         ggplot_biweekly_ninetieth_perc, ggplot_monthly_ninetieth_perc, 
                         ggplot_annual_ninetieth_perc, as_ggplot(legend1),
                         ncol = 4), device="png")
