#' fig1 UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_fig1_ui <- function(id){
  ns <- NS(id)
  tagList(
    shiny::withMathJax(),

    # Application title
    # shiny::titlePanel("\\(\\text{Theoretical relationship between }p_L,\\ r_V,\\text{ and }R\\)"),

    # Sidebar with a slider input for number of bins
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        # chooseSliderSkin("Modern"),
        shiny::sliderInput("eqpV" |> ns(),
          step = .01,
          "Actual vaccination rate (\\(p_V\\))",
          min = 0,
          max = 1,
          value = .75),

        shiny::sliderInput("eqR" |> ns(),
          step = .01,
          "Actual relative risk (Vaccinated/Not, \\(R_{true}\\))",
          min = 0,
          max = 2,
          value = .2),

        shiny::sliderInput("eqf" |> ns(),
          step = 1,
          "\\(f = (N-N_{true})/N_{true}\\)",
          min = -50,
          max = 200,
          post = "%",
          value = 0),
      ),

      shiny::mainPanel(
        shiny::plotOutput(width = "600px", height = "600px", "theorygraph" |> ns())
      )
    )
  )
}

#' fig1 Server Functions
#'
#' @noRd
mod_fig1_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

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
  })
}

## To be copied in the UI
# mod_fig1_ui("fig1_ui_1")

## To be copied in the server
# mod_fig1_server("fig1_ui_1")
