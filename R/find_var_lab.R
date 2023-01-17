
# Searches through the variables labels of the data to return variables that match the given criteria
# There can be multiple keywords and omissions
# By default, only variable names that contain all keywords and no omissions are selected

find_var_lab <- function(dat, keyword, omit = NULL, all_keyword = TRUE, all_omit = FALSE) {

  if(all_keyword) {

    matches <- TRUE

  } else {

    matches <- FALSE
  }

  for(i in 1:length(keyword)) {

    new_match <- str_detect(get_label(dat), keyword[i])

    if(all_keyword) {

      matches <- matches & new_match        #only include variables that have ALL keywords

    } else {

      matches <- matches | new_match        #include variables that have ANY keywords
    }

  }

  if(is.null(omit)) {

    return(colnames(dat[matches]))

  } else {

    if(all_omit) {

      omissions <- TRUE

    } else {

      omissions <- FALSE
    }


    for(i in 1:length(omit)) {

      new_omit <- str_detect(get_label(dat), omit[i])

      if(all_omit) {

        omissions <- omissions & new_omit     #exclude variables that have ANY omissions

      } else {

        omissions <- omissions | new_omit     #exclude variables that have ALL omissions
      }
    }

    return(colnames(dat[matches & !omissions]))
  }
}
