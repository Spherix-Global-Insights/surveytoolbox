
validity_analysis <- function(dat, id_var) {

  id_l <- length(id_var)

  validity_sheet <- data.frame(dat[id_var])



  validity_sheet <- cbind(validity_sheet, straightline_analysis(dat, id_var)[-1:-id_l]) #straightline testing
  cat("Straight-Line Analysis complete. \n")



  outlier_sheet <- outlier_analysis(dat, id_var)[-1:-id_l] #outlier testing
  cat("Outlier Analysis complete. \n")

  for(i in 1:ncol(outlier_sheet)) { #parse through every variable to decide what to show

    if(!is.na(match(TRUE, unlist(outlier_sheet[i])))) { #only include questions that have at least one outlier

      new <- dat[colnames(outlier_sheet[i])] #grab the data

      colnames(new) <- paste(colnames(outlier_sheet[i]), " (Mean: ", round(mean(unlist(na.omit(new))), 2), ", Median: ", round(median(unlist(na.omit(new))), 2), ") - ", get_label(dat[colnames(outlier_sheet[i])]), sep = "") #add some metrics to the name

      for(j in 1:nrow(new)) {

        if(!outlier_sheet[j, i] | is.na(outlier_sheet[j, i])) {

          new[j, 1] <- NA #hide answers that are not outliers
        }
      }

      validity_sheet <- cbind(validity_sheet, new)
    }
  }

  cat("Validity Analysis complete. \n")
  return(validity_sheet)
}

