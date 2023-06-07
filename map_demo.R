library(tidyverse)
library(shiny)
library(readxl)
library(DT)



covid_data <- read_excel("example_long_cvd.xlsx",
  sheet = "Understanding the condition"
) |>
  mutate(
    id = row_number(),
    Link = paste0("<a href='", Link, "' target = 'new'>", "Link", "</a>")
  )



summary_data <- covid_data |>
  select(Theme, `Evidence Group`) |>
  group_by(Theme, `Evidence Group`) |>
  summarise(count = n()) |>
  pivot_wider(names_from = `Evidence Group`, values_from = count) |>
  ungroup() |>
  mutate(id = row_number())

ui <- fluidPage(
  tags$style(
    "text/css",
    ".modal-dialog { width: fit-content !important; }"
  ),
  titlePanel("Evidence Map Demo"),
  mainPanel(
    DTOutput("summary")
  )
)

server <- function(input, output) {
  output$summary <- renderDT(summary_data |> select(-id),
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

  observeEvent(input$summary_cells_selected, {
    index <- req(input$summary_cells_selected)

    row <- index[[1]]
    col <- index[[2]] + 1

    row_name <- summary_data$Theme[row]
    col_name <- names(summary_data[col])

    modal_table <- summary_data |>
      filter(Theme == row_name) |>
      select(1, col_name)

    modal_table_tmp <- covid_data |>
      filter(
        Theme == modal_table$Theme,
        `Evidence Group` == col_name
      ) |>
      select(Author, Title, Year, Link)

    showModal(modalDialog(
      title = "Testing",
      "You selected:",
      renderDT(modal_table,
        options = list(
          dom = "t",
          ordering = F
        ),
        rownames = F
      ),
      tags$br(),
      "Results:",
      renderDT(modal_table_tmp,
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

shinyApp(ui = ui, server = server)
