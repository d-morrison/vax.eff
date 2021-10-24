
#' server
#'
#' @param input
#' @param output
#' @param session
#'
#' @return
#' @export
#'
#' @importFrom plotly plot_ly layout renderPlotly
#' @importFrom shiny reactive
server = function(input, output, session)
{

  results = shiny::reactive(
    {
      sim_data_binom(
        N = as.integer(input$N),
        `p(V)` = input$pV,
        `p(V*|V)` = input$rV,
        `p(E*|E)` = input$rE,
        `p(L|V*E*)` = input$pL,
        `p(E|!V)` = input$pE,
        RR = input$RR,
        n_sims = input$nsims)

    }
  )

  output$distPlot <- plotly::renderPlotly({


    plotly::plot_ly(
      data = results(),
      alpha = 0.6,
      type = 'histogram',
      histnorm = "probability",
      x = ~`RR*`
    ) %>%
      plotly::layout(
        title = "Histogram of Estimated Relative Risks",
        xaxis = list(
          title = "Estimated Relative Risk\n(Vaccinated vs. Not)",
          range = c(0, max(1, max(results()[["RR*"]]) * 1.1))
        )
      )

    # breaks = 100,
    # col = 'darkgray',
    # border = 'white',


  })

}
