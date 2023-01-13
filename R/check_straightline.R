
check_straightline <- function(dat, var_list) {

  check_var_validity(dat, var_list, type = "num")

  row_len <- length(dat[,1])

  uniform <- data.frame(array(FALSE, row_len))

  for(i in 1:row_len) { # checks for all the same answer

    d <- sum(duplicated(array(dat[i, var_list][!is.na(dat[i, var_list])])))

    uniform[i,] <- eval(d == sum(!is.na(dat[i, var_list])) - 1) # if answer is duplicated n-1 times, all are same answer (excludes NAs)
  }

  bools <- uniform
  return(uniform)

  if(!is.na(match(TRUE, bools))) {

    return(bools)
  }
}

