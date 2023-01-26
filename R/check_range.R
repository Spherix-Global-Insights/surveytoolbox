
check_range <- function(dat, var_list, min = 0, max = 100, fix = FALSE, fix_type = "flat") {

  check_var_validity(dat, var_list, type = "num")

  min_val <- determine_var(dat, min)[1]
  max_val <- determine_var(dat, max)[1]

  for(i in 1:length(var_list)) {

    errors_min <- dat[var_list[i]] < min_val
    errors_max <- dat[var_list[i]] > max_val

    errors_min[is.na(errors_min)] <- FALSE # we do not flag NAs here since we don't have an option for a condition
    errors_max[is.na(errors_max)] <- FALSE

    errors <- errors_min | errors_max

    if(!is.na(match(TRUE, errors))) {

      if(fix) {
        
        if(fix_type == "flat") {
          
          if(!is.na(match(TRUE, errors_min))) dat[errors_min, var_list[i]] <- max(dat[errors_min, var_list[i]], min_val[errors_min,])
          if(!is.na(match(TRUE, errors_max))) dat[errors_max, var_list[i]] <- min(dat[errors_max, var_list[i]], max_val[errors_max,])
          
          cat("Cases where values are outside of range recoded in", var_list[i], "to the respective min or max. \n")
        
        } else if(fix_type == "mean") {
          
          dat[errors, var_list[i]] <- mean(dat[!errors, var_list[i]])
          
          cat("Cases where values are outside of range recoded in", var_list[i], "to the mean. \n")
          
        } else {
          
          warning("Did not receive a valid argument for fix_type. No recodes were made.")
        }


        return(dat)

      } else {

        minmax <- data.frame(array(paste("Min ", min, ", Max ", max, sep = ""), nrow(dat)))
        colnames(minmax) <- "RANGE"

        error_report(dat, cbind(dat[var_list[i]], minmax), errors)
      }

    } else {

      cat("Test on", var_list[i], "complete, all values within range. \n")

      if(fix) {

        return(dat)
      }
    }
  }
}
