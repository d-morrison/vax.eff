#' table2 UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_table2_ui <- function(id){
  ns <- NS(id)
  tagList(
    shiny::withMathJax(),

    # Application title
    shiny::titlePanel("A simulation of vaccine effectiveness estimates from linked case and vaccine registries"),

    # Sidebar with a slider input for number of bins
    shiny::sidebarLayout(
      shiny::sidebarPanel(

        shinyWidgets::autonumericInput("nsims" |> ns(),
          step = 1,
          "# simulation replications",
          min = 1,
          decimalPlaces = 0,
          value = 1000),

        shinyWidgets::autonumericInput("N" |> ns(),
          step = 1,
          "True population size \\((N_{true})\\)",
          min = 1,
          decimalPlaces = 0,
          digitGroupSeparator = ",",

          value = 11000000),

        shinyWidgets::autonumericInput("N*" |> ns(),
          step = 1,
          min = 1,
          decimalPlaces = 0,
          "Pop. size used in analysis \\((N)\\)",
          digitGroupSeparator = ",",

          value = 11000000),

        shinyWidgets::autonumericInput("pV" |> ns(),
          step = .001,
          decimalPlaces = 3,
          "Actual vaccination rate (\\(p_V\\))",
          min = 0,
          max = 1,
          value = .4),
        shinyWidgets::autonumericInput("rV" |> ns(),
          step = .001,
          decimalPlaces = 3,
          "Pr(vaccination record | vaccinated) (\\(r_V\\))",
          min = 0,
          max = 1,
          value = .75),
        shinyWidgets::autonumericInput("pE" |> ns(),
          step = .00001,
          decimalPlaces = 5,
          "Pr(case | not vaccinated) (\\(p_C\\))",
          min = 0,
          max = 1,
          value = .0014),
        shinyWidgets::autonumericInput("RR" |> ns(),
          step = .001,
          decimalPlaces = 3,
          "Actual relative risk (Vaccinated/Not, \\(R_{true}\\))",
          min = 0,
          # max = 1,
          value = .2),
        shinyWidgets::autonumericInput("rEV" |> ns(),
          step = .001,
          decimalPlaces = 3,
          ("Pr(case record | vaccinated case) (\\(r_{C1}\\))"),
          min = 0,
          max = 1,
          value = .75),
        shinyWidgets::autonumericInput("rE!V" |> ns(),
          step = .001,
          decimalPlaces = 3,
          ("Pr(case record | unvaccinated case) (\\(r_{C0}\\))"),
          min = 0,
          max = 1,
          value = .75),
        shinyWidgets::autonumericInput("pL" |> ns(),
          step = .001,
          decimalPlaces = 3,
          "Pr(linked | both recorded) (\\(p_L\\))",
          min = 0,
          max = 1,
          value = .75)
      ),

      # Show a plot of the generated distribution
      shiny::mainPanel(
        shiny::fluidRow(
          column(width = 6,
            plotly::plotlyOutput(width = "600px", height = "600px", "distPlot" |> ns()),
            h3("Simulation summary statistics:"),
            shiny::fluidRow(shiny::uiOutput("summary_stats" |> ns())),
            # h3(),
            # shiny::fluidRow(plotly::plotlyOutput("scatter"))
          )
        )

      )
    )
  )
}

#' table2 Server Functions
#'
#' @noRd
mod_table2_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    results = shiny::reactive(
      {
        sim_data_binom(
          N = as.integer(input$N),
          `N*` = input$`N*`,
          `p(V)` = input$pV,
          `p(V*|V)` = input$rV,
          `p(E*|E,V)` = input$rEV,
          `p(E*|E,!V)` = input$`rE!V`,
          `p(L|V*,E*)` = input$pL,
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
            `p(L|V*,E*)` = input$pL,
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

  })
}

## To be copied in the UI
# mod_table2_ui("table2_ui_1")

## To be copied in the server
# mod_table2_server("table2_ui_1")
