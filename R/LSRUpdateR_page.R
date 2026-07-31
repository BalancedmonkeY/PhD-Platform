#' UI for the main LSRUpdateR page.
#'
#' @param id ID of the module.
#'
#' @return Div containing page
LSRUpdateR_page_ui <- function(id) {
  ns = NS(id)
  
  # Create selection of pages for the user to go between as they use LSRUpdateR
  div(
    # Set up each page
    bslib::navset_hidden(id = ns("LSRUpdateR_navbar"),
      nav_panel(title = "home", value = "home",
        LSRUpdateR_home_ui(id = ns("home_page"))
      ),
      nav_panel(title = "results", value = "results",
        LSRUpdateR_results_ui(id = ns("results_page"))
      ),
      nav_panel(title = "manual_update", value = "manual_update",
        LSRUpdateR_manual_update_ui(id = ns("manual_update_page"))
      ),
      nav_panel(title = "confirmation", value = "confirmation",
        LSRUpdateR_confirmation_ui(id = ns("confirmation_page")))
    )
  )
}

#' Server for the main LSRUpdateR page.
#'
#' @param id ID of the module
#' @param parent_session Session from home page (needed for navbar_select)
#' @param activity Reactive activity log
#'
#' @return 
LSRUpdateR_page_server <- function(id, parent_session, activity) {
  moduleServer(
    id,
    function(input, output, session) {
      
      ## Home page ##

      LSRUpdateR_home_server(
        id = "home_page",
        LSRUpdateR_session = session,
        parent_session = parent_session,
        activity_df = activity
      )

      ## Results page ##

      LSRUpdateR_results_server(
        id = "results_page",
        LSRUpdateR_session = session
      )
      
      ## Manual update page ##
      LSRUpdateR_manual_update_server(
        id = "manual_update_page",
        LSRUpdateR_session = session
      )

      ## Confirmation page ##

      LSRUpdateR_confirmation_server(
        id = "confirmation_page",
        LSRUpdateR_session = session
      )
      
    }
  )
}