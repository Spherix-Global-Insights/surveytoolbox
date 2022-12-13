
# Returns a string containing a conditional statement chaining every variable with the target
# The variable list should input with multiple variables, as a statement will be written for each one
# The target must be a string or number. It cannot be a list of variables

write_cond_statement <- function(dat, var_list, target, test = '==', operator = "|") {

  statement <- ""

  for(i in 1:length(var_list)) {

    statement <- paste(statement, var_list[i], test, target, operator, sep = " ")
  }

  statement <- substr(statement, 2, nchar(statement) - (nchar(operator)+1))

  return(statement)
}
