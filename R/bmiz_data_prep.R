library(readxl)
library(dplyr)
library(ggplot2)
library(zscorer)

bmi_data <- readxl::read_excel('test_data/test-data.xlsx')

dfs <- split.data.frame(bmi_data, bmi_data$participant)

bmi_data$age_days = bmi_data$age*365.25
bmi_data$wt = bmi_data$wt*.4535
bmi_data$ht = bmi_data$ht*2.54 
bmi_data$sex <- 2

bmi_data <- addWGSR(data = bmi_data, sex = 'sex', firstPart = 'wt', secondPart = 'ht', thirdPart = 'age_days', index = 'bfa', output = 'bmiAgeZ', digits = 2) %>% 
  rename('bmiz' = bmiAgeZ)
