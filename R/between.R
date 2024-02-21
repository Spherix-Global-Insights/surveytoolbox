
between <- function(dat, first_var, last_var) {

  check_var_validity(dat, first_var)

  check_var_validity(dat, last_var)

  first_num <- which(colnames(dat) == first_var)[1]

  last_num <- which(colnames(dat) == last_var)[1]

  return(colnames(dat[first_num:last_num]))
}
