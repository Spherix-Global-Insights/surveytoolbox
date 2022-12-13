
# Backfills all NAs with a value (default 0)
# This is practically a specific-use case of the recode function, designed to save time

backfill <- function(dat, var_list, replace = 0) {

  check_var_validity(dat, var_list, type = "num")

  for(i in 1:length(var_list)) {

    dat[var_list[i]] <- replace(dat[var_list[i]], is.na(dat[var_list[i]]), replace)
    cat(paste("NAs recoded to", replace,  "in", var_list[i], "\n"))
  }
  return(dat)
}

