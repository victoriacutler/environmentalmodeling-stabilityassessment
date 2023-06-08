stability_measure <- function(res) {
  # get the initial population of the predator
  initial_pred_pop <- res$pop[res$animal == 'pred' & res$time == min(res$time)]
  
  # get the latest population of the predator
  latest_pred_pop <- res$pop[res$animal == 'pred' & res$time == max(res$time)]
  
  # check if the predator population is at least twice the initial population
  return(latest_pred_pop >= 2 * initial_pred_pop)
}