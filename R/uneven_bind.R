
uneven_bind <- function(dat, newcol, fill = NA) {

  dat_rows <- nrow(dat)
  newcol_rows <- nrow(newcol)

  if(newcol_rows > dat_rows) {

    for(i in 1:(newcol_rows - dat_rows)) {

      dat <- rbind(dat, fill)

    }

  } else if(newcol_rows < dat_rows) {

    for(j in 1:(dat_rows - newcol_rows)) {

      newcol <- rbind(newcol, fill)

    }

  }

  return(cbind(dat, newcol))
}
