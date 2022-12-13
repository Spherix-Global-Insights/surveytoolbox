
export_error_report <- function(filepath) {

  if(exists("error_report_export")) {

    write.xlsx(error_report_export, here(ld.path.ag_ind[i], str_glue(ld.prefix.ag_ind[i], "Error Report.xlsx")), overwrite = TRUE)

  } else {

    warning("There is no error report to export.")
  }

}
