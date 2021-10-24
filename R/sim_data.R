#' Simulate data at an individual level
#'
#' @param N
#' @param `p(V)`
#' @param `p(E|!V)`
#' @param RR
#' @param `p(E|V)`
#' @param `p(V*|V)`
#' @param `p(V*|!V)`
#' @param `p(E*|E)`
#' @param `p(E*|!E)`
#' @param `p(L|V*,E*)`
#'
#' @importFrom dplyr tibble mutate if_else as_tibble
#' @importFrom purrr rbernoulli
#' @importFrom magrittr "%>%" "%<>%"

sim_data = function(
  N = 11 * 10^6,
  `p(V)` = .4,
  `p(E|!V)` = .0014,
  RR = .25,
  `p(E|V)` = `p(E|!V)` * RR,
  `p(V*|V)` = 0.75,
  `p(V*|!V)` = 0,
  `p(E*|E)` = 0.75,
  `p(E*|!E)` = 0,
  `p(L|V*,E*)` = 0.75

)
{

  d1 = dplyr::tibble(
    V = purrr::rbernoulli(n = N, p = `p(V)`),
    E = purrr::rbernoulli(n = N, p = if_else(V==1, `p(E|V)`, `p(E|!V)`)),
    `V*` = purrr::rbernoulli(n = N, p = if_else(V==1, `p(V*|V)`, `p(V*|!V)`)),
    `E*` = purrr::rbernoulli(n = N, p = if_else(E==1, `p(E*|E)`, `p(E*|!E)`)),
    `V*E*` = `V*` & `E*`,
    L = purrr::rbernoulli(n = N, p = `V*E*` * `p(L|V*,E*)`)
  )

  d2 = d1 %>%
    colSums() %>%
    t() %>%
    as_tibble() %>%
    dplyr::mutate(
      `p(E*|V*)` = L/`V*`,
      `p(E*|!V*)` = (`E*` - L) / (N - `V*`),
      `RR*` = `p(E*|V*)` / `p(E*|!V*)`
    )

  return(d2)

}
