#' Title
#'
#' @param `p(V)`
#' @param R
#' @param `p(V*|V)`
#' @param N
#' @param `p(L|V*,E*)`
#' @param `N*`
#' @param ... does nothing
#'
#' @return
#' @export
#'
reporting_multiplier = function(
  `p(V)` = .4,
  R = .25,
  `p(V*|V)` = 0.75,
  `p(L|V*,E*)` = 0.75,
  N = 11 * 10^6,
  `N*` = N,
  f = (`N*` - N) / N,
  ...

)
{


  temp =
    tibble(`p(V)`, `p(V*|V)`, `p(L|V*,E*)`, R, f) %>%
    mutate(
      num = `p(L|V*,E*)` * (1 +  f - `p(V*|V)` * `p(V)`),
      denom =  1 - `p(V)` + R * `p(V)` * (1 -  `p(V*|V)` * `p(L|V*,E*)`),
      `R*/R` = num/denom,
      `R*` = R * `R*/R`) %>%
    select(-c(num, denom, f))

  return(temp)
}
