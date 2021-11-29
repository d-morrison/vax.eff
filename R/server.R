
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

  url <- a("https://github.com/d-morrison/vax.eff",
           href="https://github.com/d-morrison/vax.eff")

  # url2 = a("dmorrison01@ucla.edu", href = "dmorrison01@ucla.edu")


  message1 =  'This Shiny app is a supplement to the article "Estimating Vaccine Effectiveness from Linking Population-Based Health Registries: Some Sources of Bias", by Ron Brookmeyer and Doug Morrison (currently undergoing peer review).'
  message4 = "It provides interactive versions of Figure 1 (theoretical results), Table 2 (simulation results), and Equation 3 (adjustment formula) from that paper."
  message5 = "The code implementing this app and the analyses in the paper is available at "

  message2 = "Questions about the analysis design can be sent to rbrookmeyer at ucla.edu and dmorrison01 at ucla.edu."
  message6 = "Questions about the implementation can be sent to dmorrison01 at ucla.edu."
  # message3 = "Download our article preprint from medRxiv:"
  # message_full = paste(message1, message2, message3, sep = "\n")
  output$summary =
    renderUI(column(width = 12,
      h1("About this app:"),
      message1,
      message4,
      br(),
      br(),
      message5,
      url,
      br(),
      br(),
      message2))

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
        paste0("$$\\hat{R}_{adj} =", format(adjusted_est(), digits = 3), "$$"))
    )


}
