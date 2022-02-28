#' frontpage UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_frontpage_ui <- function(id){
  ns <- NS(id)
  tagList(
    #shiny::titlePanel("Estimating Vaccine Effectiveness from Linking Population-Based Health Registries: Some Sources of Bias"),
    uiOutput("summary" |> ns())
  )
}

#' frontpage Server Functions
#'
#' @noRd
mod_frontpage_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    url <- a("https://github.com/d-morrison/vax.eff",
      href="https://github.com/d-morrison/vax.eff")

    # url2 = a("dmorrison01@ucla.edu", href = "dmorrison01@ucla.edu")


    message1 =  'This Shiny app is a supplement to the article "Estimating Vaccine Effectiveness from Linking Population-Based Health Registries: Some Sources of Bias", by Ron Brookmeyer and Doug Morrison (currently undergoing peer review).'
    message4 = "It provides interactive versions of Figure 1 (theoretical results), Table 2 (simulation results), and Equation 3 (adjustment formula) from that paper."
    message5 = "The code implementing this app and the analyses in the paper is available at "

    message2 = "Questions and feedback can be sent to rbrookmeyer at ucla.edu and dmorrison01 at ucla.edu."
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
  })
}

## To be copied in the UI
# mod_frontpage_ui("frontpage_ui_1")

## To be copied in the server
# mod_frontpage_server("frontpage_ui_1")
