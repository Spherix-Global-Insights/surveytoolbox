
# Returns a string containing a conditional statement chaining every variable with the target
# The variable list should input with multiple variables, as a statement will be written for each one
# The target must be a string or number. It cannot be a list of variables

write_cond_statement <- function(var_list, target, test = '==', operator = "|") {

  statement <- ""

  if(length(target) > 1 & length(target) != length(var_list)) {

    warning("The number of target values does not equal the number of variables.")
  }

  j <- 1

  for(i in 1:length(var_list)) {

    statement <- paste(statement, var_list[i], test, target[j], operator, sep = " ")

    if(length(target) > 1) {

      j <- j + 1
    }
  }

  statement <- substr(statement, 2, nchar(statement) - (nchar(operator)+1)) # removes excess operator

  return(statement)
}
