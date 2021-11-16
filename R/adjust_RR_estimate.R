#' Adjustment formula
#'
#' @param R
#' @param pL
#' @param rV
#' @param f
#' @param nV
#' @param N
#'
#' @return
#' @export
#'
adjust_RR_estimate = function(
  R,
  pL,
  rV,
  f,
  nV,
  N
)
{
  adj_num = R * ((1 + f) * nV - (N * rV))

  adj_denom = (1 + f) * (R * nV * (1 - pL * rV) - pL * rV * (N - nV))

  Rhat_adj = adj_num / adj_denom

  return(Rhat_adj)
}
