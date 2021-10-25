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

          shinyWidgets::autonumericInput("bias(N*)",
                                         step = 1,
                                         decimalPlaces = 0,
                                         "Bias of est. pop. size \\((E[N^*]-N)\\)",
                                         digitGroupSeparator = ",",

                                         value = 0),

          shinyWidgets::autonumericInput("sd(N*)",
                              "Std. dev. of est. pop. size \\((SD(N^*))\\)",
                              value = 0),


          shinyWidgets::autonumericInput("pV",
                              step = .01,
                              "Actual vaccination rate (\\(P_V\\))",
                              min = 0,
                              max = 1,
                              value = .4),
          shinyWidgets::autonumericInput("rV",
                              step = .01,
                              "Pr(record | vaccinated) (\\(r_V\\))",
                              min = 0,
                              max = 1,
                              value = .75),
          shinyWidgets::autonumericInput("pE",
                              step = .00001,
                              "Pr(event | not vaccinated) (\\(P_D\\))",
                              min = 0,
                              max = 1,
                              decimalPlaces = 5,
                              value = .0014),
          shinyWidgets::autonumericInput("RR",
                              step = .01,
                              "Actual relative risk (Vaccinated/Not, \\(R\\))",
                              min = 0,
                              max = 1,
                              value = .25),
          shinyWidgets::autonumericInput("rE",
                              step = .01,
                              ("Pr(record | event) (\\(r_D\\))"),
                              min = 0,
                              max = 1,
                              value = .75),
          shinyWidgets::autonumericInput("pL",
                              step = .01,
                              "Pr(linked | both recorded) (\\(P_L\\))",
                              min = 0,
                              max = 1,
                              value = .75)
        ),

        # Show a plot of the generated distribution
        shiny::mainPanel(
          shiny::fluidRow(plotly::plotlyOutput("distPlot")),
          h3("Summary statistics:"),
          shiny::fluidRow(shiny::uiOutput("summary_stats"))
        )
      )
    )
  )

  shiny::shinyApp(ui, server, ...)

}
