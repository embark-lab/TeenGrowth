#' Convert Percentile to Z-Score
#'
#' @param pct Percentile value (0-100).
#' @return Z-score corresponding to the given percentile.
#' @export
pct_to_bmiz <- function(pct) {
  bmiz <- qnorm(pct / 100)
  return(bmiz)
}

#' Convert Z-Score to BMI
#'
#' @param l L parameter from the LMS table.
#' @param m M parameter from the LMS table.
#' @param s S parameter from the LMS table.
#' @param bmiz BMI Z-score.
#' @param sigma Standard deviation (optional).
#' @param p95 95th percentile BMI value (optional).
#' @return BMI corresponding to the given Z-score.
#' @export
bmiz_to_bmi <- function(l, m, s, bmiz, sigma = NULL, p95 = NULL) {
  if (is.na(bmiz)) {
    return(NA)
  }

  if (!is.null(p95) && bmiz > 1.645) {
    if (is.null(sigma) || is.na(sigma) || is.null(p95) || is.na(p95)) {
      return(NA)
    }
    bmi_percentile <- pnorm(bmiz) * 100
    bmi_zscore <- qnorm((bmi_percentile - 90) / 10)
    bmi <- bmi_zscore * sigma + p95
  } else {
    bmi <- m * (1 + bmiz * l * s)^(1 / l)
  }
  return(bmi)
}

#' Convert BMI to Z-Score
#'
#' @param l L parameter from the LMS table.
#' @param m M parameter from the LMS table.
#' @param s S parameter from the LMS table.
#' @param bmi BMI value.
#' @param sigma Standard deviation (optional).
#' @param p95 95th percentile BMI value (optional).
#' @return Z-score corresponding to the given BMI.
#' @export
bmi_to_bmiz <- function(l, m, s, bmi, sigma = NULL, p95 = NULL) {
  if (is.na(bmi)) {
    return(NA)
  }

  if (!is.null(p95) && bmi > p95) {
    if (is.null(sigma) || is.na(sigma) || is.null(p95) || is.na(p95)) {
      return(NA)
    }
    bmi_percentile <- 90 + 10 * pnorm((bmi - p95) / sigma)
    bmiz <- qnorm(bmi_percentile / 100)
  } else {
    bmiz <- ((bmi / m)^l - 1) / (l * s)
  }
  return(bmiz)
}

#' Lookup BMI
#'
#' @param data_point Data point (BMI Z-score or percentile).
#' @param type Type of data point ('bmiz' or 'pct').
#' @param data_source Data source ('cdc' or 'wgsr').
#' @param sex Sex of the individual (1 for male, 2 for female).
#' @param age Age of the individual.
#' @param age_unit Unit of age ('months', 'years', etc.).
#' @param dob Date of birth (optional).
#' @param date_assessed Date of assessment (optional).
#' @return BMI value corresponding to the given data point.
#' @export
bmi_lookup <- function(data_point, type = 'bmiz', data_source = 'cdc', sex = 2, age, age_unit = NULL, dob = NULL, date_assessed = NULL) {
  if (is.na(data_point) || is.na(sex) || is.na(age)) {
    return(NA)
  }

  if (type == 'pct') {
    bmiz <- pct_to_bmiz(data_point)
  } else if (type == 'bmiz') {
    bmiz <- data_point
  } else {
    stop("Invalid type. Use 'bmiz' or 'pct'.")
  }

  if (data_source == 'cdc') {
    age_mos <- age_in_months(age_in_days(age = as.numeric(age), age_unit = age_unit, dob = dob, date_assessed = date_assessed))
    age_mos <- ifelse(age_mos >= 0 & age_mos < 0.5, 0, as.integer(age_mos + 0.5) - 0.5)
    max_age_cdc <- max(cdcData$agemos)
    if (age_mos > max_age_cdc) {
      age_mos <- max_age_cdc
    }
    lkpIndexSex <- cdcData[cdcData$sex == sex , ]
    L <- approx(lkpIndexSex$agemos, lkpIndexSex$L, xout = age_mos, ties = "ordered")$y
    M <- approx(lkpIndexSex$agemos, lkpIndexSex$M, xout = age_mos, ties = "ordered")$y
    S <- approx(lkpIndexSex$agemos, lkpIndexSex$S, xout = age_mos, ties = "ordered")$y
    P95 <- approx(lkpIndexSex$agemos, lkpIndexSex$P95, xout = age_mos, ties = "ordered")$y
    sigma <- approx(lkpIndexSex$agemos, lkpIndexSex$sigma, xout = age_mos, ties = "ordered")$y

    bmi <- bmiz_to_bmi(l = L, m = M, s = S, bmiz = bmiz, sigma = sigma, p95 = P95)
    return(bmi)
  } else if (data_source == 'wgsr') {
    age_days <- round(age_in_days(age = age, age_unit = age_unit, dob = dob, date_assessed = date_assessed), 0)
    max_age_wgsr <- max(wgsrData$given)
    if (age_days > max_age_wgsr) {
      age_days <- max_age_wgsr
    }
    lkpIndexSex <- wgsrData[wgsrData$sex == sex & wgsrData$index == 'bfa', ]
    L <- approx(lkpIndexSex$given, lkpIndexSex$l, xout = age_days, ties = "ordered")$y
    M <- approx(lkpIndexSex$given, lkpIndexSex$m, xout = age_days, ties = "ordered")$y
    S <- approx(lkpIndexSex$given, lkpIndexSex$s, xout = age_days, ties = "ordered")$y

    if (is.null(L) || is.na(L) || is.null(M) || is.na(M) || is.null(S) || is.na(S)) {
      return(NA)
    }

    bmi <- bmiz_to_bmi(l = L, m = M, s = S, bmiz = bmiz)
    return(bmi)
  } else {
    stop("Invalid data_source. Use 'cdc' or 'wgsr'.")
  }
}

#' Lookup BMI Z-Score
#'
#' @param bmi BMI value.
#' @param sex Sex of the individual (1 for male, 2 for female).
#' @param age Age of the individual.
#' @param data_source Data source ('cdc' or 'wgsr').
#' @param age_unit Unit of age ('months', 'years', etc.).
#' @param dob Date of birth (optional).
#' @param date_assessed Date of assessment (optional).
#' @return Z-score corresponding to the given BMI.
#' @export


# Function to lookup BMIz
bmiz_lookup <- function(bmi, sex = 2, age, data_source = 'cdc', age_unit = NULL, dob = NULL, date_assessed = NULL) {
  # Skip if bmi, sex, or age are missing
  if (is.na(bmi) || is.na(sex) || is.na(age)) {
    return(NA)
  }

  if (data_source == 'cdc') {
    age_mos <- age_in_months(age_in_days(age = as.numeric(age), age_unit = age_unit, dob = dob, date_assessed = date_assessed))
    age_mos <- ifelse(age_mos >= 0 & age_mos < 0.5, 0, as.integer(age_mos + 0.5) - 0.5)
    max_age_cdc <- max(cdcData$agemos)
    if (age_mos > max_age_cdc) {
      age_mos <- max_age_cdc
    }
    lkpIndexSex <- cdcData[cdcData$sex == sex , ]
    L <- approx(lkpIndexSex$agemos, lkpIndexSex$L, xout = age_mos, ties = "ordered")$y
    M <- approx(lkpIndexSex$agemos, lkpIndexSex$M, xout = age_mos, ties = "ordered")$y
    S <- approx(lkpIndexSex$agemos, lkpIndexSex$S, xout = age_mos, ties = "ordered")$y
    P95 <- approx(lkpIndexSex$agemos, lkpIndexSex$P95, xout = age_mos, ties = "ordered")$y
    sigma <- approx(lkpIndexSex$agemos, lkpIndexSex$sigma, xout = age_mos, ties = "ordered")$y

    bmiz <- bmi_to_bmiz(l = L, m = M, s = S, bmi = bmi, sigma = sigma, p95 = P95)
    return(bmiz)
  } else if (data_source == 'wgsr') {
    age_days <- round(age_in_days(age = age, age_unit = age_unit, dob = dob, date_assessed = date_assessed), 0)
    max_age_wgsr <- max(wgsrData$given)
    if (age_days > max_age_wgsr) {
      age_days <- max_age_wgsr
    }
    lkpIndexSex <- wgsrData[wgsrData$sex == sex & wgsrData$index == 'bfa', ]
    L <- approx(lkpIndexSex$given, lkpIndexSex$l, xout = age_days, ties = "ordered")$y
    M <- approx(lkpIndexSex$given, lkpIndexSex$m, xout = age_days, ties = "ordered")$y
    S <- approx(lkpIndexSex$given, lkpIndexSex$s, xout = age_days, ties = "ordered")$y

    if (is.null(L) || is.na(L) || is.null(M) || is.na(M) || is.null(S) || is.na(S)) {
      return(NA)
    }

    bmiz <- bmi_to_bmiz(l = L, m = M, s = S, bmi = bmi)
    return(bmiz)
  } else {
    stop("Invalid data_source. Use 'cdc' or 'wgsr'.")
  }
}


