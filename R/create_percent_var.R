
# Creates percentage versions of numeric variables and adds them to the end of the data
# If no total is listed, the percentage will be calculated out of the sum of variables

create_percent_var <- function(dat, var_list, total = NULL) {

  check_var_validity(dat, var_list, type = "num")

  if(!is.null(total)) {

    val <- determine_var(dat, total)[1]

  } else {

    val <- row_sum(dat, var_list)
  }

  for(i in 1:length(var_list)) {

    dat <- cbind(dat, dat[var_list[i]]/val * 100)
    dat[length(dat)] <- set_label(dat[length(dat)], label = get_label(dat[var_list[i]]))
    names(dat)[length(dat)] <- paste("pct_", var_list[i], sep='')
  }
  cat("Percentages of", var_list, "successfully created. \n")
  return(dat)
}
