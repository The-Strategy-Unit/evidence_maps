library(tidyverse)
library(shiny)
library(readxl)
library(DT)



covid_data <- readxl::read_excel("example_long_cvd.xlsx",
  sheet = "Understanding the condition"
) |>
  dplyr::mutate(
    id = dplyr::row_number(),
    Link = paste0("<a href='", Link, "' target = 'new'>", "Link", "</a>")
  )



summary_data <- covid_data |>
  dplyr::select(Theme, `Evidence Group`) |>
  dplyr::group_by(Theme, `Evidence Group`) |>
  dplyr::summarise(count = n()) |>
  tidyr::pivot_wider(names_from = `Evidence Group`, values_from = count) |>
  dplyr::ungroup() |>
  dplyr::mutate(id = row_number())

ui <- shiny::fluidPage(
  tags$style(
    "text/css",
    ".modal-dialog { width: fit-content !important; }"
  ),
  shiny::titlePanel("Evidence Map Demo"),
  shiny::mainPanel(
    DT::DTOutput("summary")
  )
)

server <- function(input, output) {
  output$summary <- DT::renderDT(summary_data |> select(-id),
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

    row_name <- summary_data$Theme[row]
    col_name <- names(summary_data[col])

    modal_table <- summary_data |>
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
}

shiny::shinyApp(ui = ui, server = server)
