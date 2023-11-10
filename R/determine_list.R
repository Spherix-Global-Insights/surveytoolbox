
# Determines how to handle input in relation to the variable list

determine_list <- function(var_list, input) {
  
  l <- length(var_list)
  
  list <- array()
  
  if(length(input) == 1) {
    
    list <- array(input, l)
    
  } else if(l != length(input)) {
    
    stop("The length of the input lists must be 1 or equal to the length of the variable list.", call. = FALSE)
    
  } else {
    
    list <- input
  }
  
  return(list)
}
