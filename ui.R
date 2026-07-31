ui <- fluidPage(
  div(
    style = "passing: 0px; position: absolute; top: 5px; right: 5px; z-index:10000; display: flex; align-items: center; gap: 12px;", # ensures no gap, puts into top right, and lines up elements
    p("Demo Date"),
    dateInput("demo_date", label = NULL, value = readRDS("demo_date.rds")),
    actionButton("reset_demo", "Reset Demo")
  ),
  page_navbar(id = "app_navbar", 
    nav_panel(title = "Home", value = "home_page",
              home_page_ui(id = "home")), 
    nav_panel(title = "Data", value = "data_page",
              data_page_ui(id = "data")), 
    nav_panel(title = "Results", value = "results_page", 
              results_page_ui(id = "results")),
    nav_panel(title = "LSRUpdateR", value = "lsrupdater_page",
              LSRUpdateR_page_ui(id = "LSRUpdateR")),
    nav_panel(title = "Activity", value = "activity_page",
              activity_page_ui(id = "activity")),
    nav_panel(title = "Export", value = "export_page"),
    title = "LivingStudios"
  )
)


