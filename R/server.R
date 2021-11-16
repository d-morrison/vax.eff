
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
        n_sims = input$nsims,
        `est f` = (input$`N*` - input$N) / input$N,
        `est r_V` = input$rV,
        `est p_L` =  input$pL)

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

  output$theorygraph = shiny::renderPlot(
    {
      fig1(
        # n_points = 300
        lwd = .5,
        pV = input$eqpV,
        R = input$eqR,
        f = input$eqf
      )
    }
  )

  output$adjustmentgraph = shiny::renderPlot(
    {
      adjustment_graph(
        R = input$est_R_unadjusted,
        f = input$fguess1,
        nV = input$calc_nV,
        N = input$calc_N,
        lwd = .5
      )
    }
  )

  adjusted_est = shiny::reactive(
    adjust_RR_estimate(
      R = input$est_R_unadjusted,
      pL = input$pL_guess1,
      rV = input$rV_guess1,
      f = input$fguess1,
      nV = input$calc_nV,
      N = input$calc_N
    )
  )

  output$calculator_out =
    renderUI(
      withMathJax(
        paste0("$$\\hat{R}_{\\text{adj}} =", format(adjusted_est(), digits = 3), "$$"))
    )


}
