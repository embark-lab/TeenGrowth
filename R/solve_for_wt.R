solve_for_weight <- function(bmi, height){
  w = (bmi*(height^2))/703
}

wt_lookup_by_age <- function (id, age_1, df) {
  df <- df |> 
    filter(participant == id) 
  wt <- approx(df$agemos, df$weight, xout = age_1, ties = "ordered")$y
  lower <- approx(df$agemos, df$weight_lower, xout = age_1, ties = "ordered")$y
  upper <- approx(df$agemos, df$weight_upper, xout = age_1, ties = 'ordered')$y
  x = c(wt, lower, upper)
  names(x) = c('projected wt', 'lower CI', 'upper CI')
  return(x)
}

tibble_func_1 <- function(x) {
  y = as_tibble(x)
  return (y) }

tibble_func_2 <- function (dfs) {
  empty_vec <- vector(mode = 'list', length = (length(dfs)))
  tibbles <- purrr::map(empty_vec, tibble_func_1)
  return(tibbles) }