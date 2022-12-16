
uneven_bind <- function(dat1, dat2, fill = NA) {

  dat1_rows <- nrow(dat1)
  dat2_rows <- nrow(dat2)

  if(dat2_rows > dat1_rows) {

    for(i in 1:(dat2_rows - dat1_rows)) {

      dat1 <- rbind(dat1, fill)

    }

  } else if(dat2_rows < dat1_rows) {

    for(j in 1:(dat1_rows - dat2_rows)) {

      dat2 <- rbind(dat2, fill)

    }

  }

  return(cbind(dat1, dat2))
}
