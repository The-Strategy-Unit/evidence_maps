#' waffle UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 

waffle_data <- readxl::read_excel("inst/app/data/example_long_cvd.xlsx",
                                 sheet = "Understanding the condition") |>
  dplyr::filter(!Theme == 'Prognosis')
  # dplyr::group_by(`Evidence Group`, Theme) |> 
  # dplyr::summarise(count = dplyr::n())
 

mod_waffle_ui <- function(id){
  ns <- NS(id)
  tagList(
 
    shiny::fluidRow(
      column(width = 6, shiny::plotOutput(ns('waffle1'))),
      column(width = 6, shiny::plotOutput(ns('waffle2'))),
      column(width = 6, shiny::plotOutput(ns('waffle3'))),
      column(width = 6, shiny::plotOutput(ns('waffle4'))),
      column(width = 6, shiny::plotOutput(ns('waffle5'))),
      column(width = 6, shiny::plotOutput(ns('waffle6'))),
      #column(width = 6, shiny::plotOutput(ns('waffle7'))
    )
    
  )
}
    
#' waffle Server Functions
#'
#' @noRd 
mod_waffle_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    waffle_tab <- tibble::tibble('id' = seq(1,6), 
                                 'theme' = unique(waffle_data$Theme))
    
    plot_waffle <- function(id, theme){
    waffle <- 
      waffle_data |>
      dplyr::filter(Theme == theme) |>
      ggwaffle::waffle_iron(
        ggwaffle::aes_d(group = 'Evidence Group')) |> 
       ggplot2::ggplot(ggplot2::aes(x, y, fill = group), colour = 'red')+
         ggwaffle::geom_waffle()+
      ggplot2::coord_equal()+
      viridis::scale_fill_viridis(discrete = T)+
      
      #ggwaffle::theme_waffle()+
      ggplot2::ggtitle(stringr::str_wrap(theme, 20))
      
   output[[glue::glue('waffle{id}')]] <- shiny::renderPlot(waffle)
    }
 
    observe({
      req(waffle_tab, plot_waffle)
      #waffle_tab |> 
        purrr::walk2(waffle_tab$id, 
                     waffle_tab$theme, 
                     plot_waffle)
    })
    
  })
}
    
## To be copied in the UI
# mod_waffle_ui("waffle_1")
    
## To be copied in the server
# mod_waffle_server("waffle_1")
