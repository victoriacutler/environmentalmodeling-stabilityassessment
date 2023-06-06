# define a function that measures stability
stability_measure <- function(res, threshold=0.75, year_check=500) {
  # check if the populations at year_check are at least threshold percent of initial populations
  initial_prey_pop <- res$pop[res$animal == 'prey' & res$time == min(res$time)]
  initial_pred_pop <- res$pop[res$animal == 'pred' & res$time == min(res$time)]
  
  prey_pop_at_year_check <- res$pop[res$animal == 'prey' & floor(res$time) == year_check]
  pred_pop_at_year_check <- res$pop[res$animal == 'pred' & floor(res$time) == year_check]
  
  prey_stability <- all(prey_pop_at_year_check >= threshold * initial_prey_pop)
  pred_stability <- all(pred_pop_at_year_check >= threshold * initial_pred_pop)
  
  # return TRUE only if both populations are stable
  return(prey_stability & pred_stability)
}
