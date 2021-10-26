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
sim_results_table = function(results, theoretical_results)
{
  true_RR = theoretical_results %>% dplyr::pull(R)
  tab1 = results %>%
    dplyr::summarize(
      K = theoretical_results %>% dplyr::pull(`R*/R`),
      `R^*` = theoretical_results %>% dplyr::pull(`R*`),
      `\\bar{\\hat{K}}` = mean(`RR*`/true_RR),
      `SD(\\hat{K})` = sd(`RR*`/true_RR),
      `\\bar{\\hat{R^*}}` = mean(`RR*`),
      `SD(\\hat{R^*})` = sd(`RR*`),
      `\\% \\{\\hat{R^*} < R\\}` = mean(`RR*` < true_RR) * 100,
      `\\% \\{\\hat{R^*} < \\hat{R}\\}` = mean(`RR*` < `RRhat`) * 100
    )

  # https://coderedirect.com/questions/386355/math-mode-in-shiny-table:

  tab2 = xtable::xtable(tab1,
                        align = rep("c", ncol(tab1)+1)) %>%
    print(
      include.rownames=FALSE,
      floating=FALSE, tabular.environment="array", comment=FALSE,
      print.results=FALSE,
      sanitize.colnames.function = function(x) x,
      sanitize.rownames.function = function(x) x

    )

  tagList(
    shiny::withMathJax(),
    HTML(paste0("$$", tab2, "$$"))
  )
}
