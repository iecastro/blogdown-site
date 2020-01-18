library(readr)
library(dplyr)
library(lubridate)

# source: https://github.com/jamisoncrawford/index 
syr_data <- read_csv("https://raw.githubusercontent.com/jamisoncrawford/index/master/Tables/index_0.14.2_redact.csv")

distinct(syr_data, 
         lubridate::year(Month))

syr_data %>% 
  filter(year(Month) >= 2018) %>% 
  select(Month, `Child Lead Poison Rate`) %>% 
  arrange(desc(Month)) # no 2019-20 bll data

# codebook --
# https://github.com/jamisoncrawford/index/blob/master/CodeBook.md

# Child Lead Poison Rate or annual percentage of children with lead poisoning 
# is pre-calculated by OCHD upstream as the proportion of total children 
# (under 6 years of age) in each tract with reportedly toxic levels of lead (EBLL).

# `G3 ELA Failure Rate` or annual percentage of children failing third-grade 
# ELA exam is pre-calculated by SCSD upstream as the proportion of total 
# third-grade students within each tract who performed below grade level in 
# the Grade 3 ELA (English Language Arts) exam.
# For comparative purposes, this score is reduced to 10% of its value, 
# representing 1 in 10 students.
# This is preprocessed similarly for variable ELA Failure Rate, Impoverished

# `ELA Failure Rate, Impoverished` is the **annual percentage of children failing 
# third-grade ELA exam** within a census tract per Syracuse City School District, 
# or SCSD (2016-2017)

syr_data %>% 
  mutate(year = lubridate::year(Month)) %>% 
  filter(year %in% c(2015:2018)) %>% 
  select(year, GEOID, 
         ebll_pct = `Child Lead Poison Rate`,
         ela_fail_impov = `ELA Failure Rate, Impoverished`,
         ela_fail_rate = `G3 ELA Failure Rate`) %>% 
  distinct(year, GEOID, .keep_all = TRUE) %>% 
  write_csv("content/post/env-pb-syr/data/syr-children-data.csv")
  
