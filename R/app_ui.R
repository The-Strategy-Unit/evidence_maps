#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  
  header <- bs4Dash::dashboardHeader(title = 'Evidence Maps')
  
  sidebar <- bs4Dash::dashboardSidebar(
    fixed = TRUE,
    skin = "light",
    status = "primary",
    bs4Dash::sidebarMenu(
      id = "sidebarMenu",
      bs4Dash::menuItem(
        "Home",
        tabName = "tab_home",
        icon = shiny::icon("house")
        ),
      
      bs4Dash::menuItem(
        "Summary Table",
        tabName = "tab_summary"),
      
      bs4Dash::menuItem(
        "Evidence Search",
        tabName = "tab_search"
      )
  )
  )
  
  body <- bs4Dash::dashboardBody(
    bs4Dash::tabItems(
      bs4Dash::tabItem(
        tabName = "tab_home",
        mod_home_ui("home")
        ),
      bs4Dash::tabItem(
        tabName = "tab_summary",
        mod_summary_table_ui("summary_table")
        ),
      bs4Dash::tabItem(
        tabName = "tab_search",
        mod_search_ui("search")
      )
    )
  )
  
  
  shiny::tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    shinyjs::useShinyjs(),
    bs4Dash::dashboardPage(
      header,
      sidebar,
      body
    ) 
    )
  
}


#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "evidence_maps"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
