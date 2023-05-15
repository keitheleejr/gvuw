source("script.r")

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
  theme = bslib::bs_theme(bootswatch = "lumen"),
  titlePanel("Greater Valdosta United Way Needs Assessment Results"),
  fluidRow(
    column(4,
           h4("Select a county to display."),
           selectInput("county",
                       label = "",
                       choices = c("All counties",
                                   unique(sort(data$county))))),
    column(8,
           h4("Choose a variable to display."),
           selectInput("var",
                       label = "",
                       choices = c("Household Income",
                                   "Housing",
                                   "Single Parent",
                                   "Internet Access",
                                   "Food Security",
                                   "Child Care",
                                   "Child Care Needs",
                                   "Happy",
                                   "Sad",
                                   "Stressed",
                                   "Angry",
                                   "Frustrated",
                                   "Tired",
                                   "Worried",
                                   "Hopeful"),
                       selected = "Household Income")
           )
    ),
  fluidRow(
    column(4,
           mainPanel(
             plotOutput("map")
           )
    ),
    column(8,
           mainPanel(
             gt_output(outputId = "table")))
  
    )
  )
  
  
  
  
  


# Define server logic required to draw a histogram
server <- function(input, output) {
  observe(
    if (input$county == "Atkinson"){
      output$map <- renderPlot({atkinson_map}
      )
    } else if (input$county == "Bacon"){
      output$map <- renderPlot({bacon_map}
      )
    } else if (input$county == "Berrien"){
      output$map <- renderPlot({berrien_map}
      )
    } else if (input$county == "Brooks"){
      output$map <- renderPlot({brooks_map}
      )
    } else if (input$county == "Clinch"){
      output$map <- renderPlot({clinch_map}
      )
    } else if (input$county == "Coffee"){
      output$map <- renderPlot({coffee_map}
      )
    } else if (input$county == "Echols"){
      output$map <- renderPlot({echols_map}
      )
    } else if (input$county == "Jeff Davis"){
      output$map <- renderPlot({jeffdavis_map}
      )
    } else if (input$county == "Lanier"){
      output$map <- renderPlot({lanier_map}
      )
    } else if (input$county == "Lowndes"){
      output$map <- renderPlot({lowndes_map}
      )
    } else if (input$county == "Ware"){
      output$map <- renderPlot({ware_map}
      )
    } else {
      output$map <- renderPlot({all_county_map})
    }
  )
  

  observe(
    
    if (input$var == "Single Parent" &
        input$county == "All counties"){
      output$table <- render_gt(expr = sp_table)
    } else if (input$var == "Food Security" &
               input$county == "All counties"){
      output$table <- render_gt(expr = food_table)
    } else if (input$var == "Household Income" &
               input$county == "Atkinson"){
      output$table <- render_gt(expr = hi_table_atkinson)
    } else if (input$var == "Single Parent" &
               input$county == "Atkinson"){
      output$table <- render_gt(expr = sp_table_atkinson)
    } else if (input$var == "Food Security" &
               input$county == "Atkinson"){
      output$table <- render_gt(expr = food_table_atkinson)
    }else if (input$var == "Household Income" &
               input$county == "Lowndes"){
      output$table <- render_gt(expr = hi_table_lowndes)
    } else if (input$var == "Single Parent" &
               input$county == "Lowndes"){
      output$table <- render_gt(expr = sp_table_lowndes)
    } else if (input$var == "Food Security" &
               input$county == "Lowndes"){
      output$table <- render_gt(expr = food_table_lowndes)
    } else {
      output$table <- render_gt(expr = hi_table) }
  )
}
  

    

# Run the application 
shinyApp(ui = ui, server = server)
