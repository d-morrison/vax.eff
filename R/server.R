
#' server
#'
#' @param input
#' @param output
#' @param session
#'
#' @return
#' @export
#'
#' @importFrom plotly plot_ly layout renderPlotly TeX config add_histogram
#' @importFrom shiny reactive withMathJax
#' @importFrom DT datatable renderDataTable formatRound
#' @importFrom dplyr summarize
#' @importFrom xtable xtable

server = function(input, output, session)
{

  results = shiny::reactive(
    {
      sim_data_binom(
        N = as.integer(input$N),
        `bias(N*)` = input$`bias(N*)`,
        `sd(N*)` = input$`sd(N*)`,
        `p(V)` = input$pV,
        `p(V*|V)` = input$rV,
        `p(E*|E)` = input$rE,
        `p(L|V*E*)` = input$pL,
        `p(E|!V)` = input$pE,
        RR = input$RR,
        n_sims = input$nsims)

    }
  )

  theoretical_results =
    shiny::reactive(
      {
        reporting_multiplier(
          `p(V)` = input$pV,
          `p(V*|V)` = input$rV,
          `p(E*|E)` = input$rE,
          `p(L|V*E*)` = input$pL,
          `p(E|!V)` = input$pE,
          R = input$RR
        )
      }
    )

  output$summary_stats = shiny::renderUI(
    sim_results_table(
      results = results(),
      theoretical_results = theoretical_results()
    ))

  output$distPlot <- plotly::renderPlotly(
    plot_RR_histograms(data = results()))

  output$scatter = plotly::renderPlotly(
    RR_scatterplots(data = results())
  )

}
