
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

  output$summary_stats = shiny::renderUI(
    {
      tab1 = results() %>%
        dplyr::summarize(
          `\\bar{\\hat{K}}` = mean(`RR*`/input$RR),
          `SD(\\hat{K})` = sd(`RR*`/input$RR),
          `\\bar{\\hat{R^*}}` = mean(`RR*`),
          `SD(\\hat{R^*})` = sd(`RR*`),
          `\\% \\{\\hat{R^*} < R\\}` = mean(`RR*` < input$RR) * 100,
          `\\% \\{\\hat{R^*} < \\hat{R}\\}` = mean(`RR*` < `RRhat`) * 100
        )

      # https://coderedirect.com/questions/386355/math-mode-in-shiny-table:

      tab2 = xtable::xtable(tab1,
          align = rep("c", ncol(tab1)+1)) %>%
        print(
          include.rownames=FALSE,
          floating=FALSE, tabular.environment="array", comment=FALSE,
          print.results=FALSE,
          sanitize.colnames.function = function(x) x,
          sanitize.rownames.function = function(x) x

        )



      tagList(
        shiny::withMathJax(),
        HTML(paste0("$$", tab2, "$$"))
      )

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
          title = "R* (Estimated Relative Risk, Vaccinated vs. Not)",
          range = c(0, max(1, max(results()[["RR*"]]) * 1.1))
        ),
        yaxis = list(
          title = "Pr(RR in binned range)"
        )
      )

    # breaks = 100,
    # col = 'darkgray',
    # border = 'white',


  })

}
