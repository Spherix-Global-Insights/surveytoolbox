
outlier_analysis <- function(dat, id_var) {

  outlier_sheet <- data.frame(dat[id_var])

  for(i in 1:length(dat)) {

    if(is.numeric(dat[i][,1])) {

      new <- check_outlier(dat, names(dat[i]))

      if(length(new) > 0) {

        outlier_sheet <- cbind(outlier_sheet, new)
      }
    }
  }

  return(outlier_sheet)
}

