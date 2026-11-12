#' Run the CIA data request app
#'
#' Placeholder UI for now: reports whether the database is reachable. The full
#' request builder arrives in build step 6 (see docs/PLAN.md).
#'
#' @param host,port Passed to [shiny::runApp()].
#' @export
run_app <- function(host = "127.0.0.1", port = 3838) {
  ui <- shiny::fluidPage(
    shiny::titlePanel("CIA data requests"),
    shiny::p("Under construction. Database status:"),
    shiny::verbatimTextOutput("db_status")
  )

  server <- function(input, output, session) {
    output$db_status <- shiny::renderText({
      tryCatch({
        con <- cia_connect()
        on.exit(DBI::dbDisconnect(con))
        n <- DBI::dbGetQuery(
          con,
          sprintf("SELECT count(*) AS n FROM %s.hauls", DBI::dbQuoteIdentifier(con, cia_schema()))
        )$n
        sprintf("Connected to %s. hauls: %s rows.", Sys.getenv("CIA_PG_HOST"), format(n, big.mark = ","))
      }, error = function(e) paste("Not connected:", conditionMessage(e)))
    })
  }

  shiny::runApp(shiny::shinyApp(ui, server), host = host, port = port, launch.browser = FALSE)
}
