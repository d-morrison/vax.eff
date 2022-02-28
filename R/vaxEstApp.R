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
      tabsetPanel(id = "tabset",
                  tabPanel("Summary",
                    mod_frontpage_ui("frontpage")
                  ),

                  tabPanel(
                    "Theoretical Results (Fig. 1)",
                    mod_fig1_ui("fig1")
                  ),

                  tabPanel(
                    "Simulation Results (Table 2)",
                    mod_table2_ui("table2")
                  ),
                  tabPanel(
                    "Estimate Adjustment Calculator (Eq. 3)",
                    mod_eq3_ui("eq3")
                  )
      )

    )
  )

  shiny::shinyApp(ui, server, ...)

}
