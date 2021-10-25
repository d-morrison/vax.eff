# https://coderedirect.com/questions/386355/math-mode-in-shiny-table

library(shiny)
library(xtable)

ui <- fluidPage(
  titlePanel("Hello Shiny!"),
  mainPanel(
    uiOutput("table")
  )
)

server <- function(input, output) {

  output$table <- renderUI({
    x <- rnorm(2)
    y <- rnorm(2, 1)
    tab <- data.frame(x = x, y = y)
    rownames(tab) <- c("\\hat{\\alpha}",
                       "\\beta")
    colnames(tab) = c("\\hat{x}", "\\hat{y}")
    LaTeXtab <- print(xtable(tab, align=rep("c", ncol(tab)+1)),
                      floating=FALSE, tabular.environment="array", comment=FALSE,
                      print.results=FALSE,
                      sanitize.colnames.function = function(x) x,
                      sanitize.rownames.function = function(x) x)
    tagList(
      withMathJax(),
      HTML(paste0("$$", LaTeXtab, "$$"))
    )
  })

}

shinyApp(ui, server)
