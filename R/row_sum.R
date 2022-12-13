
# Add all elements among the variables row-wise (cases)
# var_list should be a vector c() of strings

row_sum <- function(dat, var_list)
{
  sum <- array(0, nrow(dat))

  for(i in 1:length(var_list)) {

    sum <- sum + dat[var_list[i]]
  }
  return(sum)
}
