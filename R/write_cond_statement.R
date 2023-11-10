
# Returns a string containing a conditional statement chaining every variable with the target
# The variable list should input with multiple variables, as a statement will be written for each one

write_cond_statement <- function(var_list, target, test = '==', operator = NULL) {

  targ_list <- determine_list(var_list, target)
  test_list <- determine_list(var_list, test)
  
  if(is.null(operator)) { # list of strings
    
    statement <- array()
    
    for(i in 1:length(var_list)) {
      
      statement[i] <- paste(var_list[i], test_list[i], targ_list[i], sep = " ")
      
    }
    
    return(statement)
    
  } else { # one long string
    
    statement <- ""
  
    for(i in 1:length(var_list)) {
  
      statement <- paste(statement, var_list[i], test_list[i], targ_list[i], operator, sep = " ")
  
    }

    statement <- substr(statement, 2, nchar(statement) - (nchar(operator)+1)) # removes excess operator
    
    return(statement)
  }
}
