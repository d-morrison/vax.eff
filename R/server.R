
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
        `N*` = input$`N*`,
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
          `N` = input$N,
          `N*` = input$`N*`,
          `p(V)` = input$pV,
          `p(V*|V)` = input$rV,
          `p(L|V*E*)` = input$pL,
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
