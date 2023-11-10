
# Checks that the listed variables were seen based on the condition
# This is a generic check that works for any variable
# It will throw an error if the condition isn't met under all circumstances
# Special cases should use other functions to avoid errors

check_seen <- function(dat, var_list, condition = "TRUE") {

  check_var_validity(dat, var_list)

  cond_list <- determine_list(var_list, condition)

  for(i in 1:length(var_list)) {
    
    bools <- eval_expr(dat, cond_list[i])

    if(is.numeric(dat[var_list[i]][,1])) { #handles numeric variables (most common)

      errors1 <- bools & is.na(dat[var_list[i]]) #should have seen, but didn't
      errors2 <- !bools & !is.na(dat[var_list[i]]) #should not have seen, but did

      errors_bools <- errors1 | errors2 #compile errors for error report

      error_report(dat, cbind(dat[var_list[i]], bools), errors_bools)

    } else if(is.character(dat[var_list[i]][,1])) { #handles open-ended variables

      errors1 <- bools & dat[var_list[i]] == '' #should have seen, but didn't
      errors2 <- !bools & dat[var_list[i]] != '' #should not have seen, but did

      errors_bools <- errors1 | errors2 #compile errors for error report

      error_report(dat, cbind(dat[var_list[i]], bools), errors_bools)

    } else {

      message(paste("The variable", var_list[i], "is not numeric or open-ended."))
    }
  }
}
