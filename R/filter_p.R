filter_p <- function(df, id) {
  df %>% 
    filter(participant == id)
}