#convert percentile to z-score

pct_to_bmiz <- function(pct){
  bmiz <- qnorm(pct/100)
  return(bmiz)
}

#' Converts bmiz scores to bmi based on either cdc or WGSR lookup data
#'
#' @param l matching l parameter from CDC or WGSR data
#' @param m from CDC or WGSR data
#' @param s from CDC or WGSR data
#' @param bmiz bmiz score from original data
#'
#' @return a bmi
#'

bmiz_to_bmi <- function(l, m, s, bmiz) {
 x = m * (exp ((log(bmiz*l*s + 1))/l))
 return(x)
}

#' Converts bmi to bmiz scores based on either cdc or WGSR lookup data
#' @param l matching l parameter from CDC or WGSR data
#' @param m from CDC or WGSR data
#' @param s from CDC or WGSR data
#' @param bmi bmi score from original data
#' @return a bmiz
#' @export

bmi_to_bmiz <- function(l, m, s, bmi) {
  x = (exp(l * log(bmi/m)) - 1) / (l * s)
  return(x)
}


#'
#' BMI Lookup
#' @param data_point 'bmiz score or bmi percentile'
#' @param data_source Data source ('cdc' or 'wgsr')
#' @param sex '1 = Male, 2 = Female'
#' @param age 'age'
#' @param age_unit 'unit of age - optional'
#' @import dplyr
#' @return bmi scores based on bmiz and age
#' @export
#'
#'
#'
#'

bmi_lookup <- function(data_point, type = 'bmiz', data_source = 'cdc', sex = 2, age, age_unit = NULL, dob = NULL, date_assessed = NULL) {

  if (type == 'pct') {
    bmiz <- pct_to_bmiz(data_point)  # Convert percentile to Z-score
  } else if (type == 'bmiz') {
    bmiz <- data_point
  } else {
    stop("Invalid type. Use 'bmiz' or 'pct'.")
  }


  if (data_source == 'cdc') {
    age_mos <- age_in_months(age_in_days(age = as.numeric(age), age_unit = age_unit, dob = dob, date_assessed = date_assessed))

    age_mos <- ifelse(age_mos >= 0 & age_mos < 0.5, 0, as.integer(age_mos + 0.5) - 0.5)

    # Check if agemos exceeds the maximum age in wgsrData
    max_age_cdc <- max(cdcData$Agemos)
    if (age_mos > max_age_cdc) {
      age_mos <- max_age_cdc
    }

    lkpIndexSex <- cdcData[cdcData$Sex == sex , ]
    L <- approx(lkpIndexSex$Agemos, lkpIndexSex$L, xout = age_mos, ties = "ordered")$y
    M <- approx(lkpIndexSex$Agemos, lkpIndexSex$M, xout = age_mos, ties = "ordered")$y
    S <- approx(lkpIndexSex$Agemos, lkpIndexSex$S, xout = age_mos, ties = "ordered")$y

    return(bmiz_to_bmi(l = L, m = M, s = S, bmiz = bmiz))
  } else if (data_source == 'wgsr') {

    age_days <- round(age_in_days(age = age, age_unit = age_unit, dob = dob, date_assessed = date_assessed),0)

    # Check if agemos exceeds the maximum age in wgsrData
    max_age_wgsr <- max(wgsrData$given)
    if (age_days > max_age_wgsr) {
      age_days <- max_age_wgsr
    }

    lkpIndexSex <- wgsrData[wgsrData$sex == sex & wgsrData$index == 'bfa', ]
    L <- approx(lkpIndexSex$given, lkpIndexSex$l, xout = age_days, ties = "ordered")$y
    M <- approx(lkpIndexSex$given, lkpIndexSex$m, xout = age_days, ties = "ordered")$y
    S <- approx(lkpIndexSex$given, lkpIndexSex$s, xout = age_days, ties = "ordered")$y

    return(bmiz_to_bmi(l = L, m = M, s = S, bmiz = bmiz))
  } else {
    stop("Invalid data_source. Use 'cdc' or 'wgsr'.")
  }
}

#'
#' BMIz Lookup
#' @param bmi 'bmi score'
#' @param data_source Data source ('cdc' or 'wgsr')
#' @param sex '1 = Male, 2 = Female'
#' @param age 'age'
#' @param age_unit 'unit of age - optional'
#' @import dplyr
#' @return bmiz scores based on bmi and age
#' @export

bmiz_lookup <- function(bmi, sex = 2, age, data_source = 'cdc', age_unit = NULL, dob = NULL, date_assessed = NULL) {
  if (data_source == 'cdc') {
    age_mos <- age_in_months(age_in_days(age = as.numeric(age), age_unit = age_unit, dob = dob, date_assessed = date_assessed))
    age_mos <- ifelse(age_mos >= 0 & age_mos < 0.5, 0, as.integer(age_mos + 0.5) - 0.5)

    # Check if agemos exceeds the maximum age in wgsrData
    max_age_cdc <- max(cdcData$Agemos)
    if (age_mos > max_age_cdc) {
      age_mos <- max_age_cdc
    }

    lkpIndexSex <- cdcData[cdcData$Sex == sex , ]
    L <- approx(lkpIndexSex$Agemos, lkpIndexSex$L, xout = age_mos, ties = "ordered")$y
    M <- approx(lkpIndexSex$Agemos, lkpIndexSex$M, xout = age_mos, ties = "ordered")$y
    S <- approx(lkpIndexSex$Agemos, lkpIndexSex$S, xout = age_mos, ties = "ordered")$y

    return(bmi_to_bmiz(l = L, m = M, s = S, bmi = bmi))
  }
  else if (data_source == 'wgsr') {

    age_days <- round(age_in_days(age = age, age_unit = age_unit, dob = dob, date_assessed = date_assessed),0)

    # Check if agemos exceeds the maximum age in wgsrData
    max_age_wgsr <- max(wgsrData$given)
    if (age_days > max_age_wgsr) {
      age_days <- max_age_wgsr
    }

    lkpIndexSex <- wgsrData[wgsrData$sex == sex & wgsrData$index == 'bfa', ]
    L <- approx(lkpIndexSex$given, lkpIndexSex$l, xout = age_days, ties = "ordered")$y
    M <- approx(lkpIndexSex$given, lkpIndexSex$m, xout = age_days, ties = "ordered")$y
    S <- approx(lkpIndexSex$given, lkpIndexSex$s, xout = age_days, ties = "ordered")$y

    return(bmi_to_bmiz(l = L, m = M, s = S, bmi = bmi))
  } else {
    stop("Invalid data_source. Use 'cdc' or 'wgsr'.")
  }
}
