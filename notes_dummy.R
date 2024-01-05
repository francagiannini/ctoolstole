## faster ----

#iterateFunc <- function(nsteps, initial_value) {

result_pools <-
  matrix(ncol = length(initial_value), nrow = length(nsteps))

colnames(result_pools) <- colnames(initial_value)

result_pools[1, ] <- initial_value

system.time(
  for (i in 2:length(nsteps)) {
    result_pools[i, ] <- turnover(i)
  }
  

)

# return(result_pools)
# }
#tired <- iterateFunc(nsteps, initial_value)


