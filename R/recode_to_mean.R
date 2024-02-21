
# Recode cases to the mean where the condition is met

recode_to_mean <- function(dat, var_list, condition) {

  check_var_validity(dat, var_list, type = "num")

  cond_list <- determine_list(var_list, condition)

  for(i in 1:length(var_list)) {

    bools <- eval_expr(dat, cond_list[i])

    l <- nrow(dat[bools==TRUE, var_list[i]])

    dat[bools==TRUE, var_list[i]] <- array(mean(dat[bools==FALSE, var_list[i]]), l)
  }

  cat("All cases in", var_list, "recoded to the mean where", condition, "is true. \n")

  return(dat)
}
