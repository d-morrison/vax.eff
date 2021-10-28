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

  tab2 = xtable::xtable(tab1,
                        digits = 3,
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
