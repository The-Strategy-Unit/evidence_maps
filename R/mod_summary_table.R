#' summary_table UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList

covid_data <- readxl::read_excel("inst/app/data/example_long_cvd.xlsx",
  sheet = "Understanding the condition"
) |>
  dplyr::mutate(
    id = dplyr::row_number(),
    Link = paste0("<a href='", Link, "' target = 'new'>", "Link", "</a>")
  )


mod_summary_table_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::titlePanel("Evidence Map Demo"),
    shiny::mainPanel(
      shiny::fluidRow(
        shiny::selectInput(ns("yearSelect"), label = "Select Year", choices = c("All Years", unique(covid_data$Year))),
        # shiny::checkboxInput(ns('allYears'), label = 'Return all Years')
      ),
      DT::DTOutput(ns("summary")),
      shiny::verbatimTextOutput(ns("debug"))
    )
  )
}

#' summary_table Server Functions
#'
#' @noRd
mod_summary_table_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    selectedYear <- reactive(input$yearSelect)

    output$debug <- shiny::renderPrint(selectedYear())

    summary_data <- reactive({
      shiny::req(selectedYear())
      covid_data |>
        dplyr::filter(Year == selectedYear() | selectedYear() == "All Years") |>
        dplyr::select(Theme, `Evidence Group`) |>
        dplyr::group_by(Theme, `Evidence Group`) |>
        dplyr::summarise(count = dplyr::n()) |>
        tidyr::pivot_wider(names_from = `Evidence Group`, values_from = count) |>
        dplyr::ungroup() |>
        dplyr::mutate(id = dplyr::row_number())
    })


    output$summary <- DT::renderDT(
      summary_data() |>
        dplyr::select(-id),
      options = list(
        dom = "t",
        ordering = F
      ),
      selection = list(
        mode = "single",
        target = "cell"
      ),
      rownames = F
    )


    shiny::observeEvent(input$summary_cells_selected, {
      index <- shiny::req(input$summary_cells_selected)

      row <- index[[1]]
      col <- index[[2]] + 1

      row_name <- summary_data()$Theme[row]
      col_name <- names(summary_data()[col])

      modal_table <- summary_data() |>
        dplyr::filter(Theme == row_name) |>
        dplyr::select(1, col_name)

      modal_table_tmp <- covid_data |>
        dplyr::filter(
          Theme == modal_table$Theme,
          `Evidence Group` == col_name
        ) |>
        dplyr::select(Author, Title, Year, Link)

      shiny::showModal(shiny::modalDialog(
        title = "Testing",
        "You selected:",
        DT::renderDT(modal_table,
          options = list(
            dom = "t",
            ordering = F
          ),
          rownames = F
        ),
        tags$br(),
        "Results:",
        DT::renderDT(modal_table_tmp,
          options = list(
            dom = "t",
            ordering = F
          ),
          escape = F,
          rownames = F,
          selection = "none"
        ),
        easyClose = T,
        fade = T
      ))
    })
  })
}

## To be copied in the UI
# mod_summary_table_ui("summary_table_1")

## To be copied in the server
# mod_summary_table_server("summary_table_1")
