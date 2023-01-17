
check_range <- function(dat, var_list, min = 0, max = 100, fix = FALSE) {

  check_var_validity(dat, var_list, type = "num")

  for(i in 1:length(var_list)) {

    errors_min <- dat[var_list[i]] < min
    errors_max <- dat[var_list[i]] > max

    errors <- errors_min | errors_max

    if(!is.na(match(TRUE, errors))) {

      if(fix) {

        dat[errors_min, var_list[i]] <- max(dat[errors_min, var_list[i]], min)
        dat[errors_max, var_list[i]] <- min(dat[errors_max, var_list[i]], max)

        cat("Cases where values are outside of range recoded in", var_list[i], "\n")

      } else {

        minmax <- data.frame(array(paste("Min ", min, ", Max ", max, sep = ""), nrow(dat)))
        colnames(minmax) <- "RANGE"

        error_report(dat, cbind(dat[var_list[i]], minmax), errors)
      }

    } else {

      cat("Test on", var_list[i], "complete, all values within range. \n")
    }
  }
  return(dat)
}
