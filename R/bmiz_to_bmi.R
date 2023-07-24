

cdcData <-read.csv('data/bmi_cdc.csv')
wgsrData <-read.csv('data/wgsrData.csv')


#' Converts bmiz scores to bmi based on either cdc or WGSR lookup data
#'
#' @param l matching l parameter from CDC or WGSR data
#' @param m from CDC or WGSR data
#' @param s from CDC or WGSR data
#' @param bmiz bmiz score from original data
#'
#' @return a bmii
#' @export
#'

bmiz_to_bmi <- function(l, m, s, bmiz) {
 x = m * (exp ((log(bmiz*l*s + 1))/l))
 return(x)
}

#' Title
#'
#' @param sex '1 = Male, 2 = Female'
#' @param agemos 'age in months'
#' @param bmiz 'bmiz score'
#'
#' @return bmi scores based on bmiz and age
#' @export
#'

#'
bmi_lookup_cdc <- function(sex, agemos, bmiz) {

## Adjust age in months based on CDC recommendation for age-based secondPart

age <- ifelse(agemos >= 0 & agemos < 0.5, 0, as.integer(agemos + 0.5) - 0.5)

## Lookup reference values and calculate z-score

lkpIndexSex <- cdcData[cdcData$Sex == sex , ] #creates mini lookup database for bmi indices based on sex
L <- approx(lkpIndexSex$Agemos, lkpIndexSex$L,
            xout = age, ties = "ordered")$y
M <- approx(lkpIndexSex$Agemos, lkpIndexSex$M,
            xout = age, ties = "ordered")$y
S <- approx(lkpIndexSex$Agemos, lkpIndexSex$S,
            xout = age, ties = "ordered")$y

bmiz_to_bmi(l = L, m = M, s = S, bmiz = bmiz)

}


#' Looks up bmis associated with bmi z scores using wgsr data
#'
#' @param sex 1 = male, 2 = female
#' @param agemos age in months
#' @param bmiz bmiz score
#'
#' @return bmi score based on bmiz and age
#' @export
#'
bmi_lookup_wgsr <- function(sex, agemos, bmiz) {

  age <- round(agemos*30.437, 0) #changes age in months to age in days

  ## Lookup reference values and calculate z-score

  lkpIndexSex <- wgsrData[wgsrData$sex == sex & wgsrData$index == 'bfa', ]
  L <- approx(lkpIndexSex$given, lkpIndexSex$l,
              xout = age, ties = "ordered")$y
  M <- approx(lkpIndexSex$given, lkpIndexSex$m,
              xout = age, ties = "ordered")$y
  S <- approx(lkpIndexSex$given, lkpIndexSex$s,
              xout = age, ties = "ordered")$y

  bmiz_to_bmi(l = L, m = M, s = S, bmiz = bmiz)

}

