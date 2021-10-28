
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
        `bias(N*)` = input$`N*` - input$N,
        `sd(N*)` = 0,
        `p(V)` = input$pV,
        `p(V*|V)` = input$rV,
        `p(E*|E)` = input$rE,
        `p(L|V*E*)` = input$pL,
        `p(E|!V)` = input$pE,
        R = input$RR,
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
    {
      tab1 = sim_results_table(
        results = results(),
        theoretical_results = theoretical_results()
      ) %>% format_sim_results_table()

      tagList(
        shiny::withMathJax(),
        shiny::HTML(paste0("$$", tab1, "$$"))
      )


    })

  output$distPlot <- plotly::renderPlotly(
    plot_RR_histograms(data = results()))

  output$scatter = plotly::renderPlotly(
    RR_scatterplots(data = results())
  )

}
