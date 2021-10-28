#' Title
#'
#' @param data
#'
#' @return
#' @export
#'
#' @importFrom dplyr select
#' @importFrom plotly plot_ly TeX add_histogram layout config
RR_scatterplots = function(data)
{

  # range1 = c(0,max(data %>% dplyr::select(`Rhat`, `R*hat`)) * 1.1)
  range1 = range(data %>% dplyr::select(`Rhat`, `R*hat`)) * c(.9, 1.1)

  data %>%
    plotly::plot_ly(
      alpha = 0.6,
      type = 'scatter',
      mode = 'markers',
      x = ~`Rhat`,
      y = ~`R*hat`,
      name = "sim. results"
      ) %>%
    plotly::add_lines(
      name = "x = y",
      # color = "gray",
      x = range1, y = range1) %>%

    plotly::layout(
      showlegend = FALSE,
      title = plotly::TeX("\\text{Scatterplot of }\\hat{R^*}\\text{ versus }\\hat{R}"),
      xaxis = list(
        # range = range1,
        title = plotly::TeX("\\text{Est. Relative Risk with True Counts }(\\hat{R})")

      ),
      yaxis = list(
        # range = range1,
        title = plotly::TeX("\\text{Est. RR with Available Data }(\\hat{R^*})")

      )
    ) %>%
      plotly::config(mathjax = "cdn")

  # breaks = 100,
  # col = 'darkgray',
  # border = 'white',

}
