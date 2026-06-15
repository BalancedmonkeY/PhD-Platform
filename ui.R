fluidPage(
  style = "passing: 0px;", # ensures no gap
  actionButton("reset_demo", "Reset Demo",
               style = "position: absolute; top: 5px; right: 5px; z-index:10000;"),
  page_navbar( 
    nav_panel("Home",
              home_page_ui(id = "home")), 
    nav_panel("Data",
              data_page_ui(id = "data")), 
    nav_panel("Results",
              results_page_ui(id = "results")),
    nav_panel("LSRUpdateR"),
    nav_panel("Activity"),
    nav_panel("Export"),
    title = "LivingStudios"
  )
)


