#' Simulate data at a population level
#'
#' @param N The population size
#' @param `p(V)`
#' @param `p(E|!V)`
#' @param R
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
  # data = tibble(`p(V)`,`p(E|!V)`,`p(E|V)`,`p(V*|V)`,`p(V*)`,`p(E*|E)`,`p(L|V*E*)`, R),
  N = 11 * 10^6,
  `N*` = N,
  `p(V)` = .4,
  `p(E|!V)` = .0014,
  R = .2,
  `p(E|V)` = `p(E|!V)` * R,
  `p(V*|V)` = 0.75,
  `p(E*|E)` = 0.75,
  `p(L|V*E*)` = 0.75,
  n_sims = 1000,
  `est f` = (`N*` - N) / N,
  `est p_L` = `p(L|V*E*)`,
  `est r_V` = `p(V*|V)`



)
{

  d1 = dplyr::tibble(

    `V`     = rbinom(n = n_sims, size = N,        p = `p(V)`),
    `V*`    = rbinom(n = n_sims, size = V,        p = `p(V*|V)`),
    `V*E`   = rbinom(n = n_sims, size = `V*`,     p = `p(E|V)`),
    `V*E*`  = rbinom(n = n_sims, size = `V*E`,    p = `p(E*|E)`),
    `L`     = rbinom(n = n_sims, size = `V*E*`,   p = `p(L|V*E*)`),

    `!V*E`  = rbinom(n = n_sims, size = V - `V*`, p = `p(E|V)`),
    `!V*E*` = rbinom(n = n_sims, size = `!V*E`,   p = `p(E*|E)`),

    `!VE`   = rbinom(n = n_sims, size = N - V,    p = `p(E|!V)`),
    `!VE*`  = rbinom(n = n_sims, size = `!VE`,    p = `p(E*|E)`),

    `VE` = `V*E` + `!V*E`,
    `E*` = `V*E*` + `!V*E*` + `!VE*`,
    `!V` = N - V,

    `N*` = `N*`,

    `p(E*|V*)` = L/`V*`,
    `p(E*|!V*)` = (`E*` - L) / (`N*` - `V*`),
    `R*hat` = `p(E*|V*)` / `p(E*|!V*)`,

    Rhat = (`VE`/`V`) / (`!VE` / `!V`),
    K = `R*hat`/R,

    adj_num = `R*hat` * ((1 + `est f`) * `V*` - (`N*` * `est r_V`)),

    adj_denom = (1 + `est f` ) * (`R*hat` * `V*` * (1 - `est p_L` * `est r_V`) - `est p_L` * `est r_V` * (`N*` - `V*`)),

    Rhat_adj = adj_num / adj_denom

  ) %>%
  select(-c(adj_num, adj_denom))

  return(d1)

}
