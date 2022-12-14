
# Checks to make sure percent variables are not greater than 100
# If they are and fix is on, then they will be adjusted according to common entry typos

check_percent <- function(dat, var_list, fix = TRUE) {

  check_var_validity(dat, var_list, type = "num")

  for(i in 1:length(var_list)) {

    errors <- dat[var_list[i]] > 100

    if(!is.na(match(TRUE, errors))) {

      if(fix) {

        dat[errors, var_list[i]] <- ((dat[errors, var_list[i]]<200)*100)+(dat[errors, var_list[i]]>199)*round(dat[errors, var_list[i]]/(10^(nchar(round(dat[errors, var_list[i]]))-2)))
        cat("Cases over 100 recoded in", var_list[i], "\n")

      } else {

        error_report(dat, dat[var_list[i]], errors)
      }

    } else {

      cat("Test on", var_list[i], "complete, no recodes needed. \n")
    }
  }
  return(dat)
}
