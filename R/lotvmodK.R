#' Lot. Voltera Model
#'
#' function computes the rate of change of populations in a predictor prey interaction
#' @param t  time (days)
#' @param pop datatype list initial conditions; list with two values prey = number of prey, pred = number of predators
#' @param pars datatype list  coefficient in Lotka-Voltera pars$rprey, pars$alpha, pars$eff, par$pmort
#'  \emph{rprey} is growth rate of prey population;
#'  \emph{eff} is the rate of ingestion of prey by predators
#'  \emph{alpha} is a interaction coefficient (higher values greater interaction)
#'  \emph{hunt_level} is the proportion of the prey that is hunted 
#'  \emph{min_prey_pop} is the minimum prey population to permit hunting 
#’  \emph{pmort}  mortality rate of predictor population
#' @examples
#' lotvod(t=1, pop=list(1,2), pop=list(0.5,0.3,0.2,0.2))
#'
#' pars = c(rprey=0.5, alpha=0.3, eff=0.2, pmort=0.2)
#' currpop  = c(prey = 1, pred=1)
#  days = seq(from=1,to=20)
#' res = ode(func=lotvmod, y=currpop, times=days, parms=pars)
#'
#' @return  lotvmod returns a list containing the following components
#' \describe{
#' \item{dprey}{rate of change of prey populutation}
#' \item{dpred}{rate of change of preditor populutation}
#'}

lotvmodK = function(t, pop, pars) {
with(as.list(c(pars, pop)), {

  if (prey < min_prey_pop) {
    dprey <- rprey * (1 - prey / K) * prey - alpha * prey * pred
  } else {
    dprey <- rprey * (1 - prey / K) * prey - alpha * prey * pred - hunt_level * prey
  }
dpred = eff * alpha * prey * pred - pmort * pred
return(list(c(prey_growth = dprey,pred_growth = dpred)))})
}




