
# Stacks loops of repeat questions among the same respondents on top of each other to create a longer dataset

stack_loops <- function(dat, first_var, last_var, loop_num) {

  check_var_validity(dat, first_var)
  check_var_validity(dat, last_var)

  stack_start <- which(colnames(dat) == first_var)[1] - 1
  loop_end <- which(colnames(dat) == last_var)[1]
  loop_len <- loop_end - stack_start

  stack_end <- stack_start + (loop_len * loop_num)
  data_end <- ncol(dat)

  if(data_end-stack_end == 0) {

    new_dat <- subset(dat, select= c(1:loop_end))

  } else if(data_end - stack_end < 0) {

    stop("The size of the looped question sets is greater than the size of the data.", call. = FALSE)

  } else {

    new_dat <- subset(dat, select= c(1:loop_end, stack_end:data_end)) # tacks on the rest of the data to the end
  }

  for(i in 2:loop_num-1) {

    loop_start <- (stack_start + (loop_len * i)) + 1
    loop_end <- loop_start + loop_len - 1

    cat("Stacking variables", colnames(dat[loop_start]), "to", colnames(dat[loop_end]), "\n")

    if(data_end-stack_end == 0) {

      add_dat <- subset(dat, select= c(1:stack_start, loop_start:loop_end))

    } else {

      add_dat <- subset(dat, select= c(1:stack_start, loop_start:loop_end, stack_end:data_end))
    }

    colnames(add_dat) <- colnames(new_dat)

    new_dat <- rbind(new_dat, add_dat)
  }

  # Re-applies metadata
  for(k in 1:ncol(new_dat))
  {
    attr(new_dat[,k], "label") <- attr(dat[,colnames(new_dat)[k]], "label")
    attr(new_dat[,k], "labels") <- attr(dat[,colnames(new_dat)[k]], "labels")
  }

  return(new_dat)
}

