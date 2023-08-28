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
  shiny::fluidPage(
    shiny::selectInput(ns("yearSelect"), 
                       label = "Select Year", 
                       choices = c("All Years", unique(covid_data$Year))),
      shiny::fluidRow(
        column(width = 8, DT::DTOutput(ns("summary"))),
        column(width = 4, shiny::plotOutput(ns("waffle")))
      ),
      
    shiny::verbatimTextOutput(ns("debug"))
    #)
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
    
    
    waffle_data <- reactive({
      shiny::req(selectedYear())
      covid_data |>
        dplyr::filter(Year == selectedYear() | selectedYear() == "All Years") |>
        dplyr::select(Theme, `Evidence Group`)
    })
    
    #output$waffle <- shiny::renderPlot(
      
      
      # summary_data |> #()
      #   tidyr::pivot_longer(-Theme, 
      #                names_to = 'Evidence Type', 
      #                values_to = 'Count') |>
      #   dplyr::filter(Theme == 'Causes') |> 
      #   ggplot2::ggplot(ggplot2::aes(fill = `Evidence Type`, values = Count))+
      #   waffle::geom_waffle(na.rm = T, 
      #                       color = 'white',
      #                       n_rows = 6)#+
      #   ggplot2::facet_wrap(~`Theme`)
        
        
    #)


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
      
      
      
      filtered_waffle_data <- waffle_data() |> 
        dplyr::filter(Theme == row_name) |>
        ggwaffle::waffle_iron(
          ggwaffle::aes_d(group = 'Evidence Group')) |> 
        dplyr::mutate(selected = ifelse(group == col_name, T, F))
      
      output$waffle <- shiny::renderPlot({
        
        filtered_waffle_data |> 
          ggplot2::ggplot(ggplot2::aes(x, y, fill = group))+
          ggwaffle::geom_waffle()+
          ggwaffle::geom_waffle(data = filtered_waffle_data |> 
                                  dplyr::filter(selected == T),
                                colour = 'blue',
                                show.legend = F)+
          ggplot2::coord_equal()+
          viridis::scale_fill_viridis(discrete = T)+
          ggwaffle::theme_waffle()+
          ggplot2::theme(axis.title.x = ggplot2::element_blank(),
                         axis.title.y = ggplot2::element_blank(),
                         legend.position = 'top',
                         legend.title = ggplot2::element_blank())+
          ggplot2::guides(fill = ggplot2::guide_legend(nrow=2, byrow=T))
      })
      
    })
  })
}

## To be copied in the UI
# mod_summary_table_ui("summary_table_1")

## To be copied in the server
# mod_summary_table_server("summary_table_1")
