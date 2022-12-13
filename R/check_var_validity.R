
# Used to check whether the function can be performed on the given variables
# This is important for throwing readable error messages
# This will stop your program from running as unusable variables are detrimental

check_var_validity <- function(dat, var_list, type = "any") {

  if(length(var_list) == 0) {

    stop("Received a blank variable input, no test performed.", call. = FALSE)

  } else {

    for(i in 1:length(var_list)) {

      if(!exists(var_list[i], dat)) {

        stop(paste("The variable", var_list[i], "does not exist in this dataset."), call. = FALSE)

      } else {

        if(type == "num") {

          if(!is.numeric(dat[var_list[i]][,1])) {

            stop(paste("The variable", var_list[i], "is not numeric. This function requires numeric variables."), call. =  FALSE)
          }
        }
        if(type == "str") {

          if(!is.character(dat[var_list[i]][,1])) {

            stop(paste("The variable", var_list[i], "is not a string. This function requires string variables."), call. = FALSE)
          }
        }
      }
    }
  }
}
