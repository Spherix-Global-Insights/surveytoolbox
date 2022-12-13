
# Evaluates the expression given as text, and returns the list of results

eval_expr <- function(dat, expression) {

  if(expression == "TRUE") {

    bools <- as.data.frame(array(TRUE, nrow(dat)))
    colnames(bools) <- "ALWAYS"

  } else {

    bools <- eval(parse(text=expression), dat)

    bools[is.na(bools)] <- FALSE                #fix missing data

    bools <- data.frame(bools)
    colnames(bools) <- paste(expression, "MET?") #preparing readability for error report

  }

  return(bools)
}
