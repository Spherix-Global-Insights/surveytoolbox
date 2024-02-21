
error_report <- function(dat, variables, bools) {

  if(!exists("id_var")) { # sets preference variables if they do not already exist

    id_var <<- NULL
  }
  if(!exists("error_export")) {

    error_export <<- TRUE
  }

  if(is.na(match(TRUE, bools))) {

    cat("Test on", names(variables)[-length(names(variables))], "complete, no errors found. \n")

  } else {

    message("Error(s) found")

    errors <- dat[id_var]
    errors <- cbind(errors, variables)

    if(exists(names(errors)[length(names(errors))], dat)) {

      names(errors) <- paste(names(errors), get_label(dat[names(errors)]), sep = " - ") # attach the question label to the name of each column for readability

    } else {

      names(errors)[-length(names(errors))] <- paste(names(errors)[-length(names(errors))], get_label(dat[names(errors)[-length(names(errors))]]), sep = " - ") # attach the question label to the name of each column for readability
    }

    if(error_export) {

      # Error identification column
      err_col <- as.data.frame(matrix(NA, ncol = 1, nrow = nrow(dat)))
      err_col <- replace(err_col, bools, "ERROR")

      colnames(err_col) <- sys.calls()[1] # gets the function that called this (along with its arguments)

      errors <- cbind(err_col, errors)


      # Create report if it doesn't exist
      # Syntax note: <<- assigns the result to the global environment, instead of the function environment.
      if(!exists("error_report_export")) {

        error_report_export <<- list(errors)

      } else {

        error_report_export <<- c(error_report_export, list(errors))

      }

      # Handles naming
      sheet_name <- paste(names(variables)[-length(variables)], collapse = ".") # name the sheet for readability

      if(nchar(sheet_name) > 26) { # sheet names can't be too long

        sheet_name <- substr(sheet_name, 1, 26)
      }

      names(error_report_export)[length(error_report_export)] <<- sheet_name

      names(error_report_export) <<- make.names(names(error_report_export), unique = TRUE) # fixes any duplicate names (necessary for excel exporting)


    } else { # export preference is off

      errors <- errors[bools==TRUE,] # only print errors

      print(errors)
    }

  }
}
