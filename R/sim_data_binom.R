#' Simulate data at a population level
#'
#' @param N The population size
#' @param `p(V)`
#' @param `p(E|!V)`
#' @param RR
#' @param `p(E|V)`
#' @param `p(V*|V)`
#' @param `p(E*|E)`
#' @param `p(L|V*E*)`
#' @param n_sims
#'
#' @return
#' @export
#'
#' @importFrom dplyr tibble mutate
#' @importFrom magrittr "%>%" "%<>%"

sim_data_binom = function(
  N = 11 * 10^6,
  `p(V)` = .4,
  `p(E|!V)` = .0014,
  RR = .25,
  `p(E|V)` = `p(E|!V)` * RR,
  `p(V*|V)` = 0.75,
  `p(E*|E)` = 0.75,
  `p(L|V*E*)` = 0.75,
  n_sims = 1000

)
{

  d1 = dplyr::tibble(
    `V`     = rbinom(n = n_sims, size = N,        p = `p(V)`),
    `V*`    = rbinom(n = n_sims, size = V,        p = `p(V*|V)`),
    `V*E*`  = rbinom(n = n_sims, size = `V*`,     p = `p(E|V)`  * `p(E*|E)`),
    `L`     = rbinom(n = n_sims, size = `V*E*`,   p = `p(L|V*E*)`),
    `!V*E*` = rbinom(n = n_sims, size = V - `V*`, p = `p(E|V)`  * `p(E*|E)`),
    `!VE*`  = rbinom(n = n_sims, size = N - V,    p = `p(E|!V)` * `p(E*|E)`),

    `E*` = `V*E*` + `!V*E*` + `!VE*`,
    `p(E*|V*)` = L/`V*`,
    `p(E*|!V*)` = (`E*` - L) / (N - `V*`),
    `RR*` = `p(E*|V*)` / `p(E*|!V*)`
  )

  return(d1)

}