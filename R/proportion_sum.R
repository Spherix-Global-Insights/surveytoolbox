
# Proportionally adjust variables so that they sum up to the target

proportion_sum <- function(dat, var_list, target, condition = "TRUE") {

  var_val <- determine_var(dat, var_list)[1]
  targ_val <- determine_var(dat, target)[1]

  bools <- eval_expr(dat, condition)

  adj <- var_val/targ_val #find the adjustment ratio
  adj <- replace(adj, is.na(adj), 1) #fix any divisions by 0

  for(i in 1:length(var_list)) {

    dat[bools==TRUE, var_list[i]] <- dat[bools==TRUE, var_list[i]]/adj[bools==TRUE,] #divide every number by the adjustment
  }

  if(condition == "TRUE") {

    cat("All cases in", var_list, "proportionally recoded to sum to the target. \n")

  } else {

    cat("All cases in", var_list, "proportionally recoded to sum to the target where", condition, "is met. \n")
  }

  return(dat)
}
