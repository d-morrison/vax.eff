#' Title
#'
#' @param tab1
#'
#' @return
#' @export
#'

format_sim_results_table = function(tab1)
{

  # https://coderedirect.com/questions/386355/math-mode-in-shiny-table:

  scale = apply(tab1, F = min, M = 2)

  tab2 = xtable::xtable(tab1,
                        display = rep("f", ncol(tab1) + 1),
                        digits = c(1, rep(3, 6), rep(1, 2)),
                        align = rep("c", ncol(tab1)+1)) %>%
    print(
      include.rownames=FALSE,
      floating=FALSE, tabular.environment="array", comment=FALSE,
      print.results=FALSE,
      sanitize.colnames.function = function(x) x,
      sanitize.rownames.function = function(x) x

    )

  return(tab2)

}
