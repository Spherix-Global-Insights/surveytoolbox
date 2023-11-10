
# Special case of the check_seen function built specifically for multi-selects
# or other question types that have a tendency to not be backfilled with 0s

check_seen_multi <- function(dat, var_list, condition = "TRUE", sl_flag = TRUE) {

  check_var_validity(dat, var_list, type = "num") #this function does not work with open-ended variables

  if(sl_flag) { #flag the question for straight-line checking

    if(max(na.omit(dat[var_list])) > 1) { #do not flag multi-select questions

      if(!exists("sl_questions")) {

        sl_questions <<- c(var_list, "/n")

      } else {

        sl_questions <<- c(sl_questions, var_list, "/n")
      }
    }
  }
  
  cond_list <- determine_list(var_list, condition)

  for(i in 1:length(var_list)) {
    
    bools <- eval_expr(dat, cond_list[i])

    soft_errors1 <- bools & is.na(dat[var_list[i]]) #should have seen, has na
    soft_errors2 <- !bools & dat[var_list[i]] == 0 #should not have seen, has 0
    soft_errors3 <- !bools & is.na(dat[var_list[i]]) #should not have seen, has na

    errors <- !bools & dat[var_list[i]] > 0  #should not have seen, has an answer >0 (always an error)

    inconsistent1 <- !is.na(match(TRUE, soft_errors1)) & !is.na(match(TRUE, soft_errors2)) #did some cases have na when they should have seen, and 0 when they should not have seen?
    inconsistent2 <- !is.na(match(TRUE, soft_errors2)) & !is.na(match(TRUE, soft_errors3)) #when they should not have seen, was there a mix of some having 0 while others had na?

    if(!is.na(match(TRUE, errors))) {

      error_report(dat, cbind(dat[var_list[i]], bools), errors)
    }
    if(inconsistent1 | inconsistent2) {

      warning(paste("NAs and 0s are inconsistent. Programming error likely in", var_list[i]))

    } else {

      if(!is.na(match(TRUE, soft_errors1))) {

        dat[var_list[i]] <- replace(dat[var_list[i]], soft_errors1, 0)
        cat(paste("NAs recoded to 0 in", var_list[i], "where", condition, "is met.\n"))

      } else if(!is.na(match(TRUE, soft_errors2))) {

        dat[var_list[i]] <- replace(dat[var_list[i]], soft_errors2, NA)
        cat(paste("0s recoded to NA in", var_list[i], "where", condition, "is not met.\n"))

      } else {

        cat("Test on", var_list[i], "complete, no recodes needed. \n")
      }
    }

  }
  return(dat)
}
