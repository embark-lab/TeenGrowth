#logistical scaling fucntion

scaled_logit <- function(x, lower = 0, upper = 1) {
  log((x - lower) / (upper - x))
}
inv_scaled_logit <- function(x, lower = 0, upper = 1) {
  (upper - lower) * exp(x) / (1 + exp(x)) + lower
}
my_scaled_logit <- new_transformation(
  scaled_logit, inv_scaled_logit)


## Month to Year Conversion fuction for GGplot
m2y <- function(age_m){
  year <- (age_m/12)
  return(year)
}