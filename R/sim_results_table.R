#' Title
#'
#' @param results
#'
#' @return
#' @export
#'
#' @importFrom dplyr summarize pull mutate
#' @importFrom shiny withMathJax
#' @importFrom xtable xtable
sim_results_table = function(
  results,
  theoretical_results)
{
  true_R = theoretical_results %>% dplyr::pull(R)
  tab1 = results %>%
    dplyr::summarize(
      # K = theoretical_results %>% dplyr::pull(`R/R_{\\text{true}}`),
      `R` = theoretical_results %>% dplyr::pull(`R*`),
      # `\\bar{\\hat{K}}` = mean(`R*hat`/true_R),
      # `SD(\\hat{K})` = sd(`R*hat`/true_R),
      `\\bar{\\hat{R}}` = mean(`R*hat`),
      `SD(\\hat{R})` = sd(`R*hat`),
      `\\% \\{\\hat{R} < R_{\\text{true}}\\}` = mean(`R*hat` < true_R) * 100,
      `\\bar{\\hat{R}_{\\text{adj}}}` = mean(Rhat_adj),
      `SD(\\hat{R}_{\\text{adj}})` = sd(Rhat_adj),

      # `\\% \\{\\hat{R} < \\hat{R}\\}` = mean(`R*hat` < `Rhat`) * 100
    )


}
