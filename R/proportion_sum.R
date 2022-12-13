
# Proportionally adjust variables so that they sum up to the target

proportion_sum <- function(dat, var_list, target) {

  var_val <- determine_var(dat, var_list)[1]
  targ_val <- determine_var(dat, target)[1]

  adj <- var_val/targ_val #find the adjustment ratio
  adj <- replace(adj, is.na(adj), 1) #fix any divisions by 0

  for(i in 1:length(var_list)) {

    dat[var_list[i]] <- dat[var_list[i]]/adj #divide every number by the adjustment
  }
  cat("All cases in", var_list, "proportionally recoded to sum to the target. \n")
  return(dat)
}
