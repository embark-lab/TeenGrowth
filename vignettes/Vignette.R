## ----setup, include = FALSE, warning=FALSE, message=FALSE---------------------

knitr::opts_chunk$set(
  warning = FALSE,
  message = FALSE,
  collapse = TRUE,
  comment = "#>",
  fig.width = 10,  # Set global figure width
  fig.height = 6  # Set global figure height
)

## ----echo = FALSE, message=FALSE, warning=FALSE-------------------------------
library(TeenGrowth)
library(tidyr)
library(dplyr)

## ----warning=FALSE, message=FALSE---------------------------------------------
clean_data = clean_data(demo,
                        id_col_name = 'participant', 
                        age_col_name = 'age',
                        sex_col_name = 'sex',
                        ht_col_name = 'height',
                        wt_col_name = 'weight',
                        adult_ht_col_name = 'adult_height_in', 
                        ed_aoo_col_name = 'ed_aoo', 
                        ht_unit = 'in', 
                        wt_unit = 'lb')

## ----warning=FALSE, message=FALSE---------------------------------------------
BMI_forecast <- forecast_bmi(
             data = clean_data, 
             central_value = "mean",
             ci = 95)



## ----warning=FALSE, message=FALSE---------------------------------------------
plot_eBMI(clean_data = clean_data, 
          forecast_data = BMI_forecast, 
          px = 2)

## ----warning=FALSE, message=FALSE---------------------------------------------
plot_weight(clean_data = clean_data, 
          forecast_data = BMI_forecast, 
          px = 2)


## ----warning=FALSE, message=FALSE---------------------------------------------
forecast_data <- clean_forecast_data(BMI_forecast, 
                    px = 2, 
                    model = 'mean')
knitr::kable(forecast_data)


## ----warning=FALSE, message=FALSE---------------------------------------------

wt_restore <- demo |> filter(participant == 2)
wt_restore_clean <-     tx_plot_clean(wt_restore,
                        age_col_name = 'age',
                        age_unit = 'years',
                        ht_col_name = 'height',
                        wt_col_name = 'weight',
                        adult_ht = wt_restore$adult_height_in[1],
                        ht_unit = 'in', 
                        wt_unit = 'lb', 
                        dob = '2009-01-01', 
                        tx_start_date = '2024-01-01')

  
wt_restore_forecast <- BMI_forecast |> filter (id == 2)
  

Wt_Restore_Plot(wt_restore_clean, 
                wt_restore_forecast, 
                slope_per_week = 0.5)


