
# Sort of a hybrid between check_seen and compare_var that allows for comparing a list of variables against lists of targets with a condition

check_value <- function(dat, var_list, target, test = "==", condition = "TRUE") {
  
  check_var_validity(dat, var_list)
  
  targ_list <- determine_list(var_list, target)
  test_list <- determine_list(var_list, test)
  cond_list <- determine_list(var_list, condition)
  
  for(i in 1:length(var_list)) {
    
    bools <- eval_expr(dat, cond_list[i])
    
    var <- dat[var_list[i]]
    targ <- eval_expr(dat, targ_list[i])
    
    compares <- eval(parse(text=paste("var", test_list[i], "targ")))
    compares[is.na(compares)] <- FALSE # we cannot compare NA values

    errors <- bools & !compares #should have value, but didn't
    
    error_report(dat, cbind(dat[var_list[i]], bools), errors)
  }
}
