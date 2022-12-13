
# Checks ranking questions to make sure there are no duplicate answers
# If there are, a programming error is likely

check_rank <- function(dat, var_list) {

  check_var_validity(dat, var_list, type = "num")

  for(i in 1:(length(var_list)-1)) {

    for(j in (i+1):length(var_list)) {

      compare_var(dat, var_list[i], var_list[j], test = "!=")
    }
  }
}
