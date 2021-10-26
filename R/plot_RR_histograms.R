#' Title
#'
#' @param data
#'
#' @return
#' @export
#'

#' @importFrom plotly plot_ly TeX add_histogram layout config
#' @importFrom dplyr select %>%
plot_RR_histograms = function(data)
{

  # range1 = range(data %>% dplyr::select(`RRhat`, `RR*`)) * c(.9, 1.1)
  data %>%
  plotly::plot_ly(
    alpha = 0.6,
    type = 'histogram',
    histnorm = "probability",
    name = plotly::TeX("\\hat{R^*}"),
    x = ~`RR*`,
    hoverinfo = 'x+y',
    nbinsx = 100
  ) %>%
    plotly::add_histogram(
      x = ~`RRhat`,
      alpha = 0.6,
      type = 'histogram',
      histnorm = "probability",
      name = plotly::TeX("\\hat{R}")
    ) %>%

    plotly::layout(
      barmode = "overlay",
      title = plotly::TeX("\\text{Histogram of Estimated Relative Risks}"),
      legend = list(x = 0.8, y = 0.9),
      xaxis = list(
        # range = range1,
        title = plotly::TeX("\\text{Estimated Relative Risk (Vaccinated vs. Not)}")

      ),
      yaxis = list(
        title = plotly::TeX("\\text{Pr}(\\hat{R}\\text{ in binned range})")
      )
    ) %>%
    plotly::config(mathjax = "cdn")

  # breaks = 100,
  # col = 'darkgray',
  # border = 'white',

}
