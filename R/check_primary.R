
# Checks that primary isn't selected for an option that wasn't selected
# The codes must match up to the options exactly

check_primary <- function(dat, var_list, primary, codes = NULL) {

  if(length(primary) > 1) {

    if(length(var_list) != length(primary))
    {
      stop("The number of primary variables does not match up with the number of variables.", call. = FALSE)

    } else {

      check_value(dat, var_list, 0, ">", write_cond_statement(primary, "0", ">"))
    }

  } else {

    check_var_validity(dat, var_list, type = "num")
    check_var_validity(dat, primary, type = "num")

    errors <- array(FALSE, nrow(dat))

    if(length(codes) != length(var_list)) {

      stop("The number of codes does not match up with the number of variables.", call. = FALSE)

    } else {

      for(i in 1:nrow(dat)) {

        case <- dat[i, primary]

        if(!is.na(case)) {

          errors[i] <- !dat[i, var_list[match(case, codes)]] > 0 | is.na(dat[i, var_list[match(case, codes)]])
        }

      }
      error_report(dat, cbind(dat[var_list], dat[primary]), errors)
    }
  }
}
