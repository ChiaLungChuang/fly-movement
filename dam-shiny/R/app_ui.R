# ============================================================
# app_ui.R  —  top-level dashboard shell + tab wiring
# ============================================================

app_ui <- function() {
  dashboardPage(
    skin = "blue",

    dashboardHeader(title = "DAM Position + Sleep"),

    dashboardSidebar(
      sidebarMenu(
        id = "tabs",
        menuItem("Groups",           tabName = "groups",    icon = icon("layer-group")),
        menuItem("Data & QC",        tabName = "qc",        icon = icon("stethoscope")),
        menuItem("Position & Sleep", tabName = "position",  icon = icon("bed")),
        menuItem("Activity",         tabName = "activity",  icon = icon("chart-line")),
        menuItem("Chronogram",       tabName = "chrono",    icon = icon("clock")),
        menuItem("Downloads",        tabName = "downloads", icon = icon("download"))
      )
    ),

    dashboardBody(
      tabItems(
        tabItem("groups",    mod_group_manager_ui("groups")),
        tabItem("qc",        mod_qc_ui("qc")),
        tabItem("position",  mod_position_sleep_ui("position")),
        tabItem("activity",  mod_activity_ui("activity")),
        tabItem("chrono",    mod_chronogram_ui("chrono")),
        tabItem("downloads", mod_downloads_ui("downloads"))
      )
    )
  )
}
