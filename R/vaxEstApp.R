
library(shiny)
library(plotly)

vax.est.app = function(...)
{

  # Define UI for application that draws a histogram
  ui = shinyUI(fluidPage(
    withMathJax(),

    # Application title
    titlePanel("Vaccine efficacy estimates from cross-referenced case and vaccine registries"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
      sidebarPanel(

        numericInput("nsims",
                     step = 1,
                     "# simulation replications",
                     min = 1,
                     value = 1000),
        numericInput("N",
                     step = 1,
                     "Population size (\\(N\\))",
                     min = 1,
                     value = 11000000),

        numericInput("pV",
                     step = .01,
                     "Actual vaccination rate (\\(P_V\\))",
                     min = 0,
                     max = 1,
                     value = .4),
        numericInput("rV",
                     step = .01,
                     "Pr(record | vaccinated) (\\(r_V\\))",
                     min = 0,
                     max = 1,
                     value = .75),
        numericInput("pE",
                     step = .00001,
                     "Pr(event | not vaccinated) (\\(P_D\\))",
                     min = 0,
                     max = 1,
                     value = .0014),
        numericInput("RR",
                     step = .01,
                     "Actual relative risk (Vaccinated/Not, \\(R\\))",
                     min = 0,
                     max = 1,
                     value = .25),
        numericInput("rE",
                     step = .01,
                     ("Pr(record | event) (\\(r_V\\))"),
                     min = 0,
                     max = 1,
                     value = .75),
        numericInput("pL",
                     step = .01,
                     "Pr(linked | both recorded) (\\(P_L\\))",
                     min = 0,
                     max = 1,
                     value = .75)
      ),

      # Show a plot of the generated distribution
      mainPanel(
        plotlyOutput("distPlot")
      )
    )
  ))

  shinyApp(ui, server, ...)

}
