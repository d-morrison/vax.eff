#' eq3 UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_eq3_ui <- function(id){
  ns <- NS(id)
  tagList(
    shiny::withMathJax(),

    # Application title
    # shiny::titlePanel(""),

    # Sidebar with a slider input for number of bins
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        h4("Inputs:"),

        shinyWidgets::autonumericInput("est_R_unadjusted" |> ns(),
          allowDecimalPadding = FALSE,
          decimalPlaces = 6,
          "Unadjusted relative risk estimate \\((\\hat{R})\\)",
          min = 0,
          # max = Inf,
          value = .2),

        shinyWidgets::autonumericInput("calc_nV" |> ns(),
          step = 1,
          decimalPlaces = 0,
          min = 1,
          "Number of vaccinated individuals recorded \\((N_V)\\)",
          digitGroupSeparator = ",",
          value = 5000000),

        shinyWidgets::autonumericInput("calc_N" |> ns(),
          step = 1,
          "Population size used in unadjusted estimate \\((N)\\)",
          min = 1,
          decimalPlaces = 0,
          digitGroupSeparator = ",",
          value = 11000000
        ),

        shinyWidgets::autonumericInput("pL_guess1" |> ns(),
          allowDecimalPadding = FALSE,
          decimalPlaces = 6,
          "Linkage probability \\((p_L)\\)",
          min = 0,
          max = 1,
          value = .75),

        shinyWidgets::autonumericInput("rV_guess1" |> ns(),
          allowDecimalPadding = FALSE,
          decimalPlaces = 6,
          "Vaccination reporting probability \\((r_V)\\)",
          min = 0,
          max = 1,
          value = .75),

        shinyWidgets::autonumericInput("fguess1" |> ns(),
          allowDecimalPadding = FALSE,
          decimalPlaces = 6,
          "Population size relative bias \\((f)\\)",
          min = -1,
          value = 0),

        h4("Output:"),
        fluidRow(
          tags$style("#calculator_out {font-size:40px;}"),
          uiOutput("calculator_out" |> ns())),

      ),

      shiny::mainPanel(

        fluidRow(
          shiny::plotOutput(width = "600px", height = "600px", "adjustmentgraph" |> ns()))

      )

    )
  )
}

#' eq3 Server Functions
#'
#' @noRd
mod_eq3_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

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

  })
}

## To be copied in the UI
# mod_eq3_ui("eq3_ui_1")

## To be copied in the server
# mod_eq3_server("eq3_ui_1")
