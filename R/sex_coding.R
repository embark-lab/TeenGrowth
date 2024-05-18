
# Function that detects how sex variable is coded ('Male/Female; M/F, or 0(Male)/1(Female)') and converts them to 1/2.

align_sex_coding <- function(sex) {
  if (all(sex %in% c("Male", "Female"))) {
    coding <- "Male/Female"
    sex[sex == "Male"] <- 1
    sex[sex == "Female"] <- 2
  } else if (all(sex %in% c("M", "F"))) {
    coding <- "M/F"
    sex[sex == "M"] <- 1
    sex[sex == "F"] <- 2
  } else if (all(sex %in% c(0, 1))) {
    coding <- "0(Male)/1(Female)"
    sex[sex == 0] <- 1
    sex[sex == 1] <- 2
  } else if (all(sex %in% c(1, 2))) {
    coding <- "1(Male)/2(Female)"
  } else {
    stop("Invalid coding. Please use 'Male/Female', 'M/F', '0/1', or '1/2'.")
  }
  return(list(coding = coding, sex = sex))
}

# Define a function to align sex coding in a data frame
align_sex_coding_in_df <- function(data, sex_column = 'sex') {
  data <- data |> mutate("{sex_column}" := align_sex_coding(.data[[sex_column]])$sex)
  return(data)
}
