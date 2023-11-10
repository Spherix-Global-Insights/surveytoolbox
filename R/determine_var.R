
# Determines what should be done with the input for calculation purposes
# This enables functions to take on multiple types of inputs for variables, instead of just a single variable

determine_var <- function(dat, var) {

  if(length(var) > 1) {       # Vector of variables is summed together

    check_var_validity(dat, var, type = "num")
    val <- row_sum(dat, var)

    errors <- dat[var]

  } else {

    if(is.character(var)) {

      if(exists(var, dat)) {  # Single variable is left alone

        check_var_validity(dat, var, type = "num")
        val <- dat[var]

        errors <- dat[var]

      } else {                # Expression is evaluated

        val <- eval_expr(dat, var, fix_na = FALSE)

        errors <- val

        colnames(val) <- var
      }

    } else {                  # Straight numeric value is created into an array of that value

      val <- data.frame(array(var, nrow(dat)))

      errors <- val
      colnames(errors) <- "TARGET"  #readability for error report
    }

  }
  return(cbind(val, errors))
}
