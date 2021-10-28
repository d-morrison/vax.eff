#' Title
#'
#' @param `p(V)`
#' @param `p(E|!V)`
#' @param R
#' @param `p(E|V)`
#' @param `p(V*|V)`
#' @param `p(V*)`
#' @param `p(E*|E)`
#' @param `p(L|V*E*)`
#' @param data
#'
#' @return
#' @export
#'
reporting_multiplier = function(
  `p(V)` = .4,
  `p(E|!V)` = .0014,
  R = .25,
  `p(E|V)` = `p(E|!V)` * R,
  `p(V*|V)` = 0.75,
  `p(V*)` = `p(V*|V)` * `p(V)`,
  `p(E*|E)` = 0.75,
  `p(L|V*E*)` = 0.75,
  `mu(N*)` = NULL,
  `sd(N*)` = NULL,
  `bias(N*)` = NULL
)
{
  temp =
    tibble(`p(V)`,`p(E|!V)`,`p(E|V)`,`p(V*|V)`,`p(V*)`,`p(E*|E)`,`p(L|V*E*)`, R) %>%
    mutate(
      num = `p(L|V*E*)` * (1 - `p(V*)`),
      denom =  1 - (`p(V*)`/ `p(V*|V)`) * (1 - R * (1 - `p(V*|V)` * `p(L|V*E*)`)),
      `R*/R` = num/denom,
      `R*` = R * `R*/R`) %>%
    select(-c(num, denom))

  return(temp)
}
