#build the dashboard. Currently only has simple curve plot and function selection.

library(shiny)
library(shinydashboard)

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Tabels", tabName = "tables", icon = icon("table")),
    menuItem("Corner Point Plots", tabName = "plots", icon = icon("th"), badgeLabel = "try!", badgeColor = "red")
  )
)

body <- dashboardBody(
  # Boxes
  fluidRow(
    box(title = "Corner Point Curve",
        status = "primary", solidHeader = TRUE,
        plotOutput("plot", height = 250)
    ),
    
    box(collapsible = TRUE,
        selectInput("funcnum", "Function: ",
                    choices = c("F1" = 1, "F2" = 2, "F3" = 3, "F4" = 4, "F5" = 5, "F6" = 6, "F7" = 7, "F8" = 8, "F9" = 9, "F10" = 10,
                                "F11" = 11, "F12" = 12, "F13" = 13, "F14" = 14, "F15" = 15, "F16" = 16, "F17" = 17, "F18" = 18, "F19" = 19, "F20" = 20,
                                "F21" = 21, "F22" = 22, "F23" = 23, "F24" = 24, "F25" = 25, "F26" = 26, "F27" = 27, "F28" = 28),
                    selected = "F2")
    )
  )
)

ui <- dashboardPage(
  dashboardHeader(title = "Initialization sensitivity visualization"),
  sidebar,
  body
)

server <- function(input, output) {
  
  output$plot <- renderPlot({
    FPath <- sprintf("C:/Users/MikeSUN/Dropbox/Programmings/Initialization_Sensitivity/R_visualization/data/ForR_CEC13_A1_Tech1_Function%d_D10_NumMove10_Square.csv", as.integer(input$funcnum))
    #FPath <- input$funcnum
    PlotData <- read.csv(FPath)
    PlotData <- DataConvert(PlotData)
    p <- PlotCurve(PlotData, as.double(input$funcnum))
    print(p)
  })
  
}

shinyApp(ui, server)
