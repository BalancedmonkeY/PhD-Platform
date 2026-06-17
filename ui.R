fluidPage(
  div(
    style = "passing: 0px; position: absolute; top: 5px; right: 5px; z-index:10000; display: flex; align-items: center; gap: 12px;", # ensures no gap, puts into top right, and lines up elements
    p("Demo Date"),
    dateInput("demo_date", label = NULL, value = "2023-12-01"),
    actionButton("reset_demo", "Reset Demo")
  ),
  page_navbar( 
    nav_panel("Home",
              home_page_ui(id = "home")), 
    nav_panel("Data",
              data_page_ui(id = "data")), 
    nav_panel("Results",
              results_page_ui(id = "results")),
    nav_panel("LSRUpdateR"),
    nav_panel("Activity",
              activity_page_ui(id = "activity")),
    nav_panel("Export"),
    title = "LivingStudios"
  )
)


