#' Title
#'
#' @param ...
#'
#' @return
#' @export
#'
#' @importFrom plotly plotlyOutput
#' @importFrom shinyWidgets autonumericInput
#' @importFrom shiny shinyUI fluidPage withMathJax titlePanel sidebarLayout sidebarPanel numericInput mainPanel shinyApp
vaxEstApp = function(...)
{

  # Define UI for application that draws a histogram
  ui = shiny::shinyUI(
    shiny::fluidPage(
      tabsetPanel(
        tabPanel(
          "Simulation",
          shiny::withMathJax(),

          # Application title
          shiny::titlePanel("Vaccine efficacy estimates from cross-referenced case and vaccine registries"),

          # Sidebar with a slider input for number of bins
          shiny::sidebarLayout(
            shiny::sidebarPanel(

              shinyWidgets::autonumericInput("nsims",
                                             step = 1,
                                             "# simulation replications",
                                             min = 1,
                                             decimalPlaces = 0,
                                             value = 1000),

              shinyWidgets::autonumericInput("N",
                                             step = 1,
                                             "True population size \\((N)\\)",
                                             min = 1,
                                             decimalPlaces = 0,
                                             digitGroupSeparator = ",",

                                             value = 11000000),

              shinyWidgets::autonumericInput("N*",
                                             step = 1,
                                             min = 1,
                                             decimalPlaces = 0,
                                             "Pop. size used in analysis \\((N^*)\\)",
                                             digitGroupSeparator = ",",

                                             value = 11000000),

              shinyWidgets::autonumericInput("pV",
                                             step = .001,
                                             decimalPlaces = 3,
                                             "Actual vaccination rate (\\(P_V\\))",
                                             min = 0,
                                             max = 1,
                                             value = .4),
              shinyWidgets::autonumericInput("rV",
                                             step = .001,
                                             decimalPlaces = 3,
                                             "Pr(record | vaccinated) (\\(r_V\\))",
                                             min = 0,
                                             max = 1,
                                             value = .75),
              shinyWidgets::autonumericInput("pE",
                                             step = .00001,
                                             decimalPlaces = 5,
                                             "Pr(event | not vaccinated) (\\(P_D\\))",
                                             min = 0,
                                             max = 1,
                                             value = .0014),
              shinyWidgets::autonumericInput("RR",
                                             step = .001,
                                             decimalPlaces = 3,
                                             "Actual relative risk (Vaccinated/Not, \\(R_{true}\\))",
                                             min = 0,
                                             # max = 1,
                                             value = .2),
              shinyWidgets::autonumericInput("rE",
                                             step = .001,
                                             decimalPlaces = 3,
                                             ("Pr(record | event) (\\(r_D\\))"),
                                             min = 0,
                                             max = 1,
                                             value = .75),
              shinyWidgets::autonumericInput("pL",
                                             step = .001,
                                             decimalPlaces = 3,
                                             "Pr(linked | both recorded) (\\(P_L\\))",
                                             min = 0,
                                             max = 1,
                                             value = .75)
            ),

            # Show a plot of the generated distribution
            shiny::mainPanel(
              shiny::fluidRow(
                plotly::plotlyOutput("distPlot")),
              h3("Summary statistics:"),
              shiny::fluidRow(shiny::uiOutput("summary_stats")),
              h3(),
              shiny::fluidRow(
                plotly::plotlyOutput("scatter")),

            )
          )
        ),
        tabPanel(
          "Theoretical Results",
          shiny::withMathJax(),

          # Application title
          shiny::titlePanel("\\(\\text{Theoretical relationship between }p_L,\\ r_V,\\text{ and }R\\)"),

          # Sidebar with a slider input for number of bins
          shiny::sidebarLayout(
            shiny::sidebarPanel(
              # chooseSliderSkin("Modern"),
              shiny::sliderInput("eqpV",
                                 step = .01,
                                 "Actual vaccination rate (\\(P_V\\))",
                                 min = 0,
                                 max = 1,
                                 value = .75),

              shiny::sliderInput("eqR",
                                 step = .01,
                                 "Actual relative risk (Vaccinated/Not, \\(R_{true}\\))",
                                 min = 0,
                                 max = 2,
                                 value = .2),

              shiny::sliderInput("eqN*/N",
                                 step = .01,
                                 "\\(N^*/N\\)",
                                 min = 1/2,
                                 max = 2,
                                 value = 1),
            ),

            shiny::mainPanel(
              shiny::plotOutput("theorygraph")
            )
          )
        )
      )

    )
  )

  shiny::shinyApp(ui, server, ...)

}
