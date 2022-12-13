
# Add a new variable to an existing dataset
# Returns the new dataset

create_new_var <- function(dat, var_nam, var_lab = "New variable", val_lab = c("No" = 0, "Yes" = 1), def_val = NA) {

  if(exists(var_nam, dat)) { # checks to make sure the variable isn't already in the dataset

    stop(paste("A variable with the name", var_nam, "already exists in this dataset."))

  } else {

    val <- determine_var(dat, def_val)[1]
    colnames(val) <- NULL

    dat$z <- unlist(val) # adds the new column to the end of the data frame with a default value

    dat$z <- set_label(dat$z, label = var_lab)
    dat$z <- set_labels(dat$z, labels = val_lab, force.labels = TRUE)

    names(dat)[names(dat) == "z"] <- var_nam

    return(dat)
  }
}

