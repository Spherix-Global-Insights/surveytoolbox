
# Compares variables with a value and returns a boolean
# both values may be either a single numeric value or one to many variables
# if multiple variables are compared in values, they will be summed up

compare_var <- function(dat, value1, value2, test = '==', condition = "TRUE") {

  det_list1 <- determine_var(dat, value1)
  val1 <- det_list1[1]
  errors1 <- det_list1[-1]

  det_list2 <- determine_var(dat, value2)
  val2 <- det_list2[1]
  errors2 <- det_list2[-1]

  bools <- eval_expr(dat, condition)

  compares <- !eval(parse(text=paste("val1", test, "val2")))
  compares[is.na(compares)] <- FALSE # we cannot compare NA values

  errors <- bools & compares
  error_report(dat, cbind(errors1, errors2), errors)
}
