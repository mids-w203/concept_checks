library(tidyverse)
library(magrittr)
library(lubridate)
library(patchwork)

theme_set(theme_minimal())

d <- read_csv('~/MIDS/w203/concept_checks/unit_09/kenya_testing/trends_data.csv')

glimpse(d)

d %<>%
  mutate(
    ever_tested = case_when(
      ever == 'No'  ~ 0,
      ever == 'Yes' ~ 1),
    visit_date = as_date(vdate),
    visit_year = year(as_date(vdate)),
    visit_month = month(as_date(vdate)),
    marital_status = factor(mstatus2, levels = c('Single', 'Separated/ Divorced/ Widowed', 'Married')),
    age = age2)

d_visits <- d %>%
  group_by(visit_year, visit_month) %>%
  filter(visit_year >= 2010, visit_year < 2020) %>%
  summarise(
    number_new_visits = sum(ever_tested),
    total_visits = n())

new_visits_plot <- d_visits %>%
  ggplot() +
  aes(x = visit_month, y = number_new_visits, color = as.factor(visit_year)) +
  geom_smooth(se=FALSE)

total_visits_plot <- d_visits  %>%
  ggplot() +
  aes(x = visit_month, y = total_visits, color = as.factor(visit_year)) +
  geom_smooth(se=FALSE)

new_visits_plot | total_visits_plot


d %>%
  group_by(
    year = year(dmy(vdate)))  %>%
  filter(
    year >= 2010, year <= 2020)  %>%
  summarise(
    number_new_visits = sum(ever),
    total_visits = n()) %>%
  ggplot() +
  aes(x = year, y = total_visits) +
  geom_line()

model <- d %>%
  select(
    site, age2, gender, mstatus2, ever) %>%
  lm(ever ~ site + age2 + gender + mstatus2, data = .)
summary(model)
