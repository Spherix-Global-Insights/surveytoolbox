
# Evaluates the expression given as text, and returns the list of results

eval_expr <- function(dat, expression, fix_na = TRUE) {

  if(expression == "TRUE") {

    bools <- as.data.frame(array(TRUE, nrow(dat)))
    colnames(bools) <- "ALWAYS"

  } else {

    bools <- eval(parse(text=expression), dat)

    if(fix_na) bools[is.na(bools)] <- FALSE                #fix missing data

    if(length(bools) == 1) {                    #turns single value into full dataframe

      bools <- data.frame(array(bools, nrow(dat)))
    }

    bools <- data.frame(bools)
    colnames(bools) <- paste(expression, "MET?") #preparing readability for error report



  }

  return(bools)
}
