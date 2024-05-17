
# # makes a tsibble object with existing data. This object has ONLY Age in months, BMIz scores (at months assessed) and participant number
# ts_data <- bmi_data |>
#   select(-c(age, adult_ht_in)) |>
#   as_tsibble(index = agemos, key = participant) |>
#   tsibble::fill_gaps()
#

# #creates fit data based on a few models
# bmiz_fit <- ts_data |>
#   model(Mean = MEAN(bmiz), #Mean of all measurements
#         Linear = TSLM(my_scaled_logit(bmiz, lower = -2, upper = 3) ~ agemos), # A linear model which uses a scaled logit model with upper and lower bounds
#         arima = ARIMA(bmiz ~ pdq(0,1,0))) %>%  # A 'Naive' Model which uses the last observation and includes a random walk from the last obs
#   mutate(C1 = (Mean  + arima + Linear)/3) |> # Note to self - CIs on the linear model don't play well with CIs on the ARIMA and mean models given the scaled logit model. Can't figure out how to combine them with good SEs, but the trajectories themselves look plausible for this model
#   mutate(C2 = (arima + Mean)/2) |> #Combines the RW model with the Mean model, such that predictions are pulled between the mean and the
#   forecast(h = 220)  %>% #forecasts across 220 months - e.g. from three years old
#   filter(agemos <252) #removes ages > 20, assumes 20.9 is terminal age
