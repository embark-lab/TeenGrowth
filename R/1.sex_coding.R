#' Align Sex Coding
#'
#' This function detects how the sex variable is coded ('Male/Female', 'M/F', or '0(Male)/1(Female)') and converts them to 1/2.
#'
#' @param sex A vector of sex values coded as 'Male/Female', 'M/F', '0(Male)/1(Female)', or '1(Male)/2(Female)'.
#'
#' @return A list containing the detected coding system and the aligned sex vector with values 1 for male and 2 for female.
#'
#' @examples
#' sex_vector <- c("Male", "Female", "Male")
#' align_sex_coding(sex_vector)
#'
#' @export
align_sex_coding <- function(sex) {
  if (all(sex %in% c("Male", "Female", NA))) {
    coding <- "Male/Female"
    sex[sex == "Male"] <- 1
    sex[sex == "Female"] <- 2
  } else if (all(sex %in% c("M", "F", NA))) {
    coding <- "M/F"
    sex[sex == "M"] <- 1
    sex[sex == "F"] <- 2
  } else if (all(sex %in% c(1, 2, NA))) {
    coding <- "1(Male)/2(Female)"
  } else if (all(sex %in% c(0, 1, NA))) {
    coding <- "0(Male)/1(Female)"
    sex[sex == 0] <- 1
    sex[sex == 1] <- 2
   } else {
    stop("Invalid coding. Please use 'Male/Female', 'M/F', '0/1', or '1/2'.")
  }
  return(list(coding = coding, sex = sex))
}

#' Align Sex Coding in Data Frame
#'
#' This function aligns the sex coding in a data frame by detecting how the sex variable is coded ('Male/Female', 'M/F', or '0(Male)/1(Female)') and converting them to 1/2.
#'
#' @param data A data frame containing a column with sex values.
#' @param sex_column The name of the column containing the sex values. Defaults to 'sex'.
#'
#' @return The data frame with the sex column values aligned to 1 for male and 2 for female.
#'
#' @examples
#' data <- data.frame(sex = c("Male", "Female", "Male"), age = c(25, 30, 35))
#' align_sex_coding_in_df(data)
#'
#' @export
align_sex_coding_in_df <- function(data, sex_column = 'sex') {
  data <- data %>%
    mutate("{sex_column}" := align_sex_coding(.data[[sex_column]])$sex)
  return(data)
}
