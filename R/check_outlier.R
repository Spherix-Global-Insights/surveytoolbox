
check_outlier <- function(dat, var) {

  check_var_validity(dat, var, type = "num")

  if(length(unlist(na.omit(dat[var]))) > 1) { #only check variables that have multiple respondents answering

    if(var(na.omit(dat[var])) > 5) { #only include outliers for questions that have a variation of greater than 5

      up_lim <- quantile(dat[,var], 0.75, na.rm = TRUE)+1.5*IQR(dat[,var], na.rm = TRUE) # iqr outlier criterion
      low_lim <- quantile(dat[,var], 0.25, na.rm = TRUE)-1.5*IQR(dat[,var], na.rm = TRUE)

      up_lim <- determine_var(dat, up_lim)[1] # format for checking
      low_lim <- determine_var(dat, low_lim)[1]

      var_metric <- array(1, nrow(dat))

      bools <- dat[var] > up_lim | dat[var] < low_lim

      if(!is.na(match(TRUE, bools))) {

        return(bools)
      }
    }
  }
}

