library(tidyverse)
library(ggplot2)

library(magrittr)

library(stargazer)
library(lfe)

theme_set(theme_minimal())

## load data 
dt <- read_csv("https://mids-w203.s3-us-west-1.amazonaws.com/pppub19.csv") %>%
  rename_all(tolower) %>%
  filter(prwkstat == 2)

## build features
dt <- dt %>%
  mutate(
    a_sex = as.factor(a_sex), 
    sex_two_category = factor(a_sex, levels = 1:2, labels = c('Male', 'Female')),
    wsal_val = ifelse(wsal_val < 0, NA, wsal_val),
    occup_f = factor(occup)
  )

## First summary: outcome variable
wage_salary_histogram <- dt %>%
  ggplot(aes(x=wsal_val)) +
  geom_histogram() +
  labs(title = 'Histogram of Wage Salaries')

## there are some clear outliers on the salary scale. 
wage_salary_histogram

## a log transform makes this log-normal(ish) distributed 
wage_salary_histogram +
  scale_x_continuous(trans = 'log')

## whare are the mean wages in the two-category groups?
dt %>%
  group_by(a_sex) %>%
  summarize(mean_wages = mean(wsal_val))

## is there a difference in the (non-logged) wages? 
dt %$%
  t.test(wsal_val ~ a_sex)

## is there a difference in the (logged) wages?
dt %>%
  mutate(log_wages = log(wsal_val + 1)) %$%
  t.test(log_wages ~ a_sex)


histogram_one <- dt %>%
  ggplot(aes(x = wsal_val / 1000, fill = a_sex)) +
  geom_density(alpha = 0.4) +
  scale_fill_manual(
        name = NULL,
        values = c("#003262", "#FDB515"),
        labels = c("Men","Women")) +
    xlim(0, 250) +
    labs(
        x = 'Total Wage and Salary (Thousands of $)',
        y = 'Density',
        title = 'Women Earn Less than Men') + 
    theme(
        legend.position = c(.95, .95),
        legend.justification = c("right", "top"),
        legend.box.just = "right",
        legend.margin = margin(6, 6, 6, 6),
      axis.text.y = element_blank())
histogram_one
## this feels like it could communicate more clearly... perhaps a log-transform?

histogram_two <- dt %>%
  ggplot(aes(x =log(wsal_val + 1), fill = a_sex)) +
  geom_density(alpha = 0.4) +
  scale_fill_manual(
        name = NULL,
        values = c("#003262", "#FDB515"),
        labels = c("Men","Women")) +
    xlim(0, 250) +
    labs(
        x = 'Total Wage and Salary (Thousands of $)',
        y = 'Density',
        title = 'Women Earn Less than Men') +
    theme(
        legend.position = c(.95, .95),
        legend.justification = c("right", "top"),
        legend.box.just = "right",
        legend.margin = margin(6, 6, 6, 6),
      axis.text.y = element_blank())
histogram_two 
## nope! 

histogram_three <- dt %>%
  ggplot(aes(x = wsal_val / 1000, fill = a_sex)) +
  geom_density(alpha = 0.4) +
  scale_fill_manual(
        name = NULL,
        values = c("#003262", "#FDB515"),
        labels = c("Men","Women")) +
    xlim(0, 150) +
    labs(
        x = 'Total Wage and Salary (Thousands of $)',
        y = 'Density',
        title = 'Women Earn Less than Men') +
    theme(
        legend.position = c(.95, .95),
        legend.justification = c("right", "top"),
        legend.box.just = "right",
        legend.margin = margin(6, 6, 6, 6),
      axis.text.y = element_blank())
histogram_three

ggsave(
  filename = "wage_hist_250.pdf",
  plot = histogram_one,
  device = 'pdf', bg = 'transparent', 
  units = 'mm',
  width = 128, height = 96
  )
ggsave(
  filename = 'wage_hist_150.pdf',
  plot = histogram_three,
  device = 'pdf', bg = 'transparent',
  units = 'mm',
  width = 128, height = 96
)


## Fit models for wages 

model_one <- lm(wsal_val / 1000 ~ sex_two_category, data = dt)

dt %>%
    sample_n(1000) %>%
    ggplot(aes(x = sex_two_category, y = wsal_val / 1000, color = sex_two_category)) +
    geom_jitter(width = .2, height = 0, alpha = 1) +
    geom_abline(
        slope     = coef(model_one)['sex_two_categoryFemale'],
        intercept = coef(model_one)['(Intercept)'],
        color     = 'darkred') +
    scale_color_manual(
        values = c("#003262", "#FDB515")) +     
    ylim(0,250) +
    labs(
        x        = NULL,
        y        = 'Total Wage and Salary (Thousands of $)',
        title    = 'Regression View of Wage Gap',
        subtitle = 'Random Subset of 1,000 Data Points') +
    theme(legend.position = 'none')
ggsave("wage_slope.pdf",
       device = 'pdf',
       units = 'mm', width = 96, height = 96,
       bg = 'transparent')

stargazer(
    m1,
    type = 'latex', out = './model_female.tex', float = FALSE,
    omit.stat = c('rsq', 'f', 'ser', 'adj.rsq'),
    digits = 0, 
    dep.var.caption = 'Dependent Variable: Pay',
    dep.var.labels.include = FALSE,
    covariate.labels = c('Female', 'Intercept'),
    star.cutoffs = c(0.05, 0.01, 0.001),
    align = FALSE)


model_two   <- lm(wsal_val ~ a_sex + occup, data = dt) # this model doesn't make sense -- occup should not be continuous
# model_three <- lm(wsal-al ~ a_sex + factor(occup), data = dt)
model_three <- lfe::felm(wsal_val ~ a_sex | occup, data = dt) #  cheating with some advanced stuff... 

stargazer(
    model_one, model_two, model_three,
    type = 'latex', out = './model_female_occupation.tex', float = FALSE,
    ## type = 'text',
    omit = "occup",
    add.lines = list(
        c('Occupation FE', 'No', 'No', 'Yes')
        ), 
    omit.stat = c('rsq', 'f', 'ser', 'adj.rsq'),
    digits = 0, 
    dep.var.caption = 'Dependent Variable: Pay',
    dep.var.labels.include = FALSE,
    covariate.labels = c('Female', 'Intercept'),
    model.names = FALSE,
    star.cutoffs = c(0.05, 0.01, 0.001),
    align = FALSE)


## stargazer(
##     m1, m2,
##     type = 'latex', out = './model_female_occupation_alt.tex', float = FALSE,
##     ## type = 'text',
##     omit.stat = c('rsq', 'f', 'ser', 'adj.rsq')o
##     digits = 0,
##     dep.var.caption = 'Dependent Variable: Pay',
##     dep.var.labels.include = FALSE,
##     covariate.labels = c('Female', 'Intercept'),
##     model.names = FALSE,
##     star.cutoffs = c(0.05, 0.01, 0.001),
##     align = FALSE)



## Estimate a model for age, and age squared.
## again, using felm to pull off occupation fixed effects 
model_four <- felm(wsal_val ~ a_sex + a_age + I(a_age^2) | occup, data = dt)


stargazer(
    model_one, model_two, model_four,
    type = 'latex', out = './model_female_occupation_age.tex', float = FALSE,
    ## type = 'text',
    omit = "occup",
    omit.stat = c('rsq', 'f', 'ser', 'adj.rsq'),
    digits = 0, 
    dep.var.caption = 'Dependent Variable: Pay',
    dep.var.labels.include = FALSE,
    covariate.labels = c('Female', 'Age', 'Age^2', 'Intercept'),
    add.lines = list(
        c('Occupation FE', 'No', 'Yes', 'Yes')
    ), 
    model.names = FALSE,
    star.cutoffs = c(0.05, 0.01, 0.001),
    align = FALSE
)
   
model_five = felm(wsal_val ~ a_sex * a_age | occup, data = dt)

stargazer(
    m5,
    type = 'latex', out = './model_female_occupation_interaction.tex', float = FALSE,
    ## type = 'text',
    omit = "occup",
    omit.stat = c('rsq', 'f', 'ser', 'adj.rsq'),
    digits = 0, 
    dep.var.caption = 'Dependent Variable: Pay',
    dep.var.labels.include = FALSE,
    covariate.labels = c('Female', 'Age', 'Female:Age', 'Intercept'),
    add.lines = list(
        c('Occupation FE', 'No', 'Yes', 'Yes')
    ), 
    model.names = FALSE,
    star.cutoffs = c(0.05, 0.01, 0.001),
    align = FALSE
)
