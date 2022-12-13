
# Moves the specified variables to another location within a data set

move_var <- function(dat, var_list, after) {

  check_var_validity(dat, var_list)
  check_var_validity(dat, after)

  newdat <- remove_var(dat, var_list)

  for(i in 1:length(var_list)) {

    newdat <- add_column(newdat, dat[var_list[(length(var_list)+1)-i]], .after = after)
  }

  return(newdat)
}
