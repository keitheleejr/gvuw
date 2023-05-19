library(shiny)

source("script.r")
source("maps.r")




# Define UI for application that draws a histogram
# ui <- fluidPage(
#   theme = bslib::bs_theme(bootswatch = "lumen"),
#   titlePanel("Greater Valdosta United Way Needs Assessment Results"),
#   fluidRow(
#     column(6,div(style = "height:20px")
#     )
#   ),
#   fluidRow(
#     column(6,div(style = "height:20px"))
#     
#   ),
#   fluidRow(
#     column(1),
#     column(4,
#            h4("Select a county to display."),
#            selectInput("county",
#                        label = "",
#                        choices = c("All Counties",
#                                    "Atkinson",
#                                    "Bacon",
#                                    "Berrien",
#                                    "Brooks",
#                                    "Clinch",
#                                    "Coffee",
#                                    "Echols",
#                                    "Jeff Davis",
#                                    "Lanier",
#                                    "Lowndes",
#                                    "Ware"))),
#     column(7,
#            h4("Choose a variable to display."),
#            selectInput("var",
#                        label = "",
#                        choices = c("Angry",
#                                    "Child Care",
#                                    "Child Care Needs",
#                                    "Food Security",
#                                    "Frustrated",
#                                    "Happy",
#                                    "Hopeful",
#                                    "Household Income",
#                                    "Housing",
#                                    "Single Parent",
#                                    "Sad",
#                                    "Stressed",
#                                    "Tired",
#                                    "Worried"),
#                        selected = "Angry")
#            )
#     ),
#   fluidRow(
#     column(1),
#     column(4,div(style = "height = 400"),
#            mainPanel(
#              plotOutput("map")
#            )
#     ),
#     column(7, align = "left",
#            mainPanel(
#              gt_output(outputId = "table")))
#   
#     ),fluidRow(
#       column(1),
#       column(4),
#       column(7,
#              h4("Select a question to display."),
#              selectInput("question",
#                          label = "",
#                          choices = c("Which services did you access?",
#                                      "Which organization(s) provided services?",
#                                      "Which services did you need but could not access?"))),
#     ),
#   fluidRow(
#     column(1),
#     column(4,
#            mainPanel(
#              img(src = "map.png")
#            )
#     ),
#     column(7, align = "left",
#            mainPanel(
#              plotOutput("bars")))
#     
#   )
#   )
#   
  

ui <- fluidPage(
  theme = bslib::bs_theme(bootswatch = "lumen"),
  titlePanel("Greater Valdosta United Way Needs Assessment Results"),
  fluidRow(
    column(12,div(style = "height:50px")
    )
  ),
  fluidRow(
    column(1),
    column(5,
           h4("Select a county to display."),
           selectInput("county",
                       label = "",
                       choices = c("All Counties",
                                   "Atkinson",
                                   "Bacon",
                                   "Berrien",
                                   "Brooks",
                                   "Clinch",
                                   "Coffee",
                                   "Echols",
                                   "Jeff Davis",
                                   "Lanier",
                                   "Lowndes",
                                   "Ware"))),
    column(6
    )
  ),
  fluidRow(
    column(1),
    column(5,div(style = "height = 400"),
           mainPanel(
             plotOutput("map")
           )
    ),
    column(1),
    column(4, 
           mainPanel(
             img(src = "map.png")
           )),
    column(1)
    
  ),  fluidRow(
    column(12,div(style = "height:50px"))
    
  ),fluidRow(
    column(1),
    column(5,
           h4("Choose a variable to display."),
           selectInput("var",
                       label = "",
                       choices = c("Angry",
                                   "Child Care",
                                   "Child Care Needs",
                                   "Food Security",
                                   "Frustrated",
                                   "Happy",
                                   "Hopeful",
                                   "Household Income",
                                   "Housing",
                                   "Single Parent",
                                   "Sad",
                                   "Stressed",
                                   "Tired",
                                   "Worried"),
                       selected = "Angry")),
    column(6,
           h4("Select a question to display."),
           selectInput("question",
                       label = "",
                       choices = c("Which services did you access?",
                                   "Which organization(s) provided services?",
                                   "Which services did you need but could not access?"))),
  ),
  fluidRow(
    column(1),
    column(5, align = "left",
           mainPanel(
             gt_output(outputId = "table"))
           ),
    column(6, align = "left",
           mainPanel(
             plotOutput("bars")
           ))
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
      output$table <- render_gt(single_parent, align = "left")
    } else if (input$var == "Household Income" &
               input$county == "All Counties"){
      output$table <- render_gt(household_income, align = "left")
    } else if (input$var == "Single Parent" &
               input$county == "All Counties"){
      output$table <- render_gt(single_parent, align = "left")
    } else if (input$var == "Housing" &
               input$county == "All Counties"){
      output$table <- render_gt(housing, align = "left")
    } else if (input$var == "Food Security" &
               input$county == "All Counties"){
      output$table <- render_gt(food)
    } else if (input$var == "Internet Access" &
               input$county == "All Counties"){
      output$table <- render_gt(internet_access, align = "left")
    } else if (input$var == "Child Care" &
               input$county == "All Counties"){
      output$table <- render_gt(child_care, align = "left")
    } else if (input$var == "Child Care Needs" &
              input$county == "All Counties"){
      output$table <- render_gt(child_care_needs, align = "left")
    } else if (input$var == "Happy" &
               input$county == "All Counties"){
      output$table <- render_gt(happy, align = "left")
    } else if (input$var == "Stressed" &
               input$county == "All Counties"){
      output$table <- render_gt(stressed, align = "left")
    } else if (input$var == "Sad" &
               input$county == "All Counties"){
      output$table <- render_gt(sad, align = "left")
    } else if (input$var == "Frustrated" &
              input$county == "All Counties"){
      output$table <- render_gt(frustrated, align = "left")
    } else if (input$var == "Tired" &
               input$county == "All Counties"){
      output$table <- render_gt(tired, align = "left")
    } else if (input$var == "Worried" &
               input$county == "All Counties"){
      output$table <- render_gt(worried, align = "left")
    } else if (input$var == "Hopeful" &
               input$county == "All Counties"){
      output$table <- render_gt(hopeful, align = "left")
    } else if (input$var == "Household Income" &
               input$county == "Atkinson"){
      output$table <- render_gt(household_income_atkinson, align = "left")
    } else if (input$var == "Single Parent" &
               input$county == "Atkinson"){
      output$table <- render_gt(single_parent_atkinson, align = "left")
    } else if (input$var == "Housing" &
               input$county == "Atkinson"){
      output$table <- render_gt(housing_atkinson, align = "left")
    } else if (input$var == "Food Security" &
               input$county == "Atkinson"){
      output$table <- render_gt(food_atkinson, align = "left")
    } else if (input$var == "Internet Access" &
               input$county == "Atkinson"){
      output$table <- render_gt(internet_access_atkinson, align = "left")
    } else if (input$var == "Child Care" &
               input$county == "Atkinson"){
      output$table <- render_gt(child_care_atkinson, align = "left")
    } else if (input$var == "Child Care Needs" &
              input$county == "Atkinson"){
      output$table <- render_gt(child_care_needs_atkinson, align = "left")
    } else if (input$var == "Happy" &
               input$county == "Atkinson"){
      output$table <- render_gt(happy_atkinson, align = "left")
    } else if (input$var == "Stressed" &
               input$county == "Atkinson"){
      output$table <- render_gt(stressed_atkinson, align = "left")
    } else if (input$var == "Sad" &
               input$county == "Atkinson"){
      output$table <- render_gt(sad_atkinson, align = "left")
    } else if (input$var == "Angry" &
               input$county == "Atkinson"){
      output$table <- render_gt(angry_atkinson, align = "left")
    } else if (input$var == "Frustrated" &
              input$county == "Atkinson"){
      output$table <- render_gt(frustrated_atkinson, align = "left")
    } else if (input$var == "Tired" &
               input$county == "Atkinson"){
      output$table <- render_gt(tired_atkinson, align = "left")
    } else if (input$var == "Worried" &
               input$county == "Atkinson"){
      output$table <- render_gt(worried_atkinson, align = "left")
    } else if (input$var == "Hopeful" &
               input$county == "Atkinson"){
      output$table <- render_gt(hopeful_atkinson, align = "left")
    } else if (input$var == "Household Income" &
                input$county == "Bacon"){
      output$table <- render_gt(household_income_bacon, align = "left")
    } else if (input$var == "Single Parent" &
               input$county == "Bacon"){
      output$table <- render_gt(single_parent_bacon, align = "left")
    } else if (input$var == "Housing" &
               input$county == "Bacon"){
      output$table <- render_gt(housing_bacon, align = "left")
    } else if (input$var == "Food Security" &
               input$county == "Bacon"){
      output$table <- render_gt(food_bacon, align = "left")
    } else if (input$var == "Internet Access" &
               input$county == "Bacon"){
      output$table <- render_gt(internet_access_bacon, align = "left")
    } else if (input$var == "Child Care" &
               input$county == "Bacon"){
      output$table <- render_gt(child_care_bacon, align = "left")
    } else if (input$var == "Child Care Needs" &
              input$county == "Bacon"){
      output$table <- render_gt(child_care_needs_bacon, align = "left")
    } else if (input$var == "Happy" &
               input$county == "Bacon"){
      output$table <- render_gt(happy_bacon, align = "left")
    } else if (input$var == "Stressed" &
               input$county == "Bacon"){
      output$table <- render_gt(stressed_bacon, align = "left")
    } else if (input$var == "Sad" &
               input$county == "Bacon"){
      output$table <- render_gt(sad_bacon, align = "left")
    } else if (input$var == "Angry" &
               input$county == "Bacon"){
      output$table <- render_gt(angry_bacon, align = "left")
    } else if (input$var == "Frustrated" &
              input$county == "Bacon"){
      output$table <- render_gt(frustrated_bacon, align = "left")
    } else if (input$var == "Tired" &
               input$county == "Bacon"){
      output$table <- render_gt(tired_bacon, align = "left")
    } else if (input$var == "Worried" &
               input$county == "Bacon"){
      output$table <- render_gt(worried_bacon, align = "left")
    } else if (input$var == "Hopeful" &
               input$county == "Bacon"){
      output$table <- render_gt(hopeful_bacon, align = "left")
    } else if (input$var == "Household Income" &
               input$county == "Berrien"){
      output$table <- render_gt(household_income_berrien, align = "left")
    } else if (input$var == "Single Parent" &
               input$county == "Berrien"){
      output$table <- render_gt(single_parent_berrien, align = "left")
    } else if (input$var == "Housing" &
               input$county == "Berrien"){
      output$table <- render_gt(housing_berrien, align = "left")
    } else if (input$var == "Food Security" &
               input$county == "Berrien"){
      output$table <- render_gt(food_berrien, align = "left")
    } else if (input$var == "Internet Access" &
               input$county == "Berrien"){
      output$table <- render_gt(internet_access_berrien, align = "left")
    } else if (input$var == "Child Care" &
               input$county == "Berrien"){
      output$table <- render_gt(child_care_berrien, align = "left")
    } else if (input$var == "Child Care Needs" &
              input$county == "Berrien"){
      output$table <- render_gt(child_care_needs_berrien, align = "left")
    } else if (input$var == "Happy" &
               input$county == "Berrien"){
      output$table <- render_gt(happy_berrien, align = "left")
    } else if (input$var == "Stressed" &
               input$county == "Berrien"){
      output$table <- render_gt(stressed_berrien, align = "left")
    } else if (input$var == "Sad" &
               input$county == "Berrien"){
      output$table <- render_gt(sad_berrien, align = "left")
    } else if (input$var == "Angry" &
               input$county == "Berrien"){
      output$table <- render_gt(angry_berrien, align = "left")
    } else if (input$var == "Frustrated" &
              input$county == "Berrien"){
      output$table <- render_gt(frustrated_berrien, align = "left")
    } else if (input$var == "Tired" &
               input$county == "Berrien"){
      output$table <- render_gt(tired_berrien, align = "left")
    } else if (input$var == "Worried" &
               input$county == "Berrien"){
      output$table <- render_gt(worried_berrien, align = "left")
    } else if (input$var == "Hopeful" &
               input$county == "Berrien"){
      output$table <- render_gt(hopeful_berrien, align = "left")
    } else if (input$var == "Household Income" &
               input$county == "Brooks"){
      output$table <- render_gt(household_income_brooks, align = "left")
    } else if (input$var == "Single Parent" &
               input$county == "Brooks"){
      output$table <- render_gt(single_parent_brooks, align = "left")
    } else if (input$var == "Housing" &
               input$county == "Brooks"){
      output$table <- render_gt(housing_brooks, align = "left")
    } else if (input$var == "Food Security" &
               input$county == "Brooks"){
      output$table <- render_gt(food_brooks, align = "left")
    } else if (input$var == "Internet Access" &
               input$county == "Brooks"){
      output$table <- render_gt(internet_access_brooks, align = "left")
    } else if (input$var == "Child Care" &
               input$county == "Brooks"){
      output$table <- render_gt(child_care_brooks, align = "left")
    } else if (input$var == "Child Care Needs" &
              input$county == "Brooks"){
      output$table <- render_gt(child_care_needs_brooks, align = "left")
    } else if (input$var == "Happy" &
               input$county == "Brooks"){
      output$table <- render_gt(happy_brooks, align = "left")
    } else if (input$var == "Stressed" &
               input$county == "Brooks"){
      output$table <- render_gt(stressed_brooks, align = "left")
    } else if (input$var == "Sad" &
               input$county == "Brooks"){
      output$table <- render_gt(sad_brooks, align = "left")
    } else if (input$var == "Angry" &
               input$county == "Brooks"){
      output$table <- render_gt(angry_brooks, align = "left")
    } else if (input$var == "Frustrated" &
              input$county == "Brooks"){
      output$table <- render_gt(frustrated_brooks, align = "left")
    } else if (input$var == "Tired" &
               input$county == "Brooks"){
      output$table <- render_gt(tired_brooks, align = "left")
    } else if (input$var == "Worried" &
               input$county == "Brooks"){
      output$table <- render_gt(worried_brooks, align = "left")
    } else if (input$var == "Hopeful" &
               input$county == "Brooks"){
      output$table <- render_gt(hopeful_brooks, align = "left")
    } else if (input$var == "Household Income" &
               input$county == "Clinch"){
      output$table <- render_gt(household_income_clinch, align = "left")
    } else if (input$var == "Single Parent" &
               input$county == "Clinch"){
      output$table <- render_gt(single_parent_clinch, align = "left")
    } else if (input$var == "Housing" &
               input$county == "Clinch"){
      output$table <- render_gt(housing_clinch, align = "left")
    } else if (input$var == "Food Security" &
               input$county == "Clinch"){
      output$table <- render_gt(food_clinch, align = "left")
    } else if (input$var == "Internet Access" &
               input$county == "Clinch"){
      output$table <- render_gt(internet_access_clinch, align = "left")
    } else if (input$var == "Child Care" &
               input$county == "Clinch"){
      output$table <- render_gt(child_care_clinch, align = "left")
    } else if (input$var == "Child Care Needs" &
              input$county == "Clinch"){
      output$table <- render_gt(child_care_needs_clinch, align = "left")
    } else if (input$var == "Happy" &
               input$county == "Clinch"){
      output$table <- render_gt(happy_clinch, align = "left")
    } else if (input$var == "Stressed" &
               input$county == "Clinch"){
      output$table <- render_gt(stressed_clinch, align = "left")
    } else if (input$var == "Sad" &
               input$county == "Clinch"){
      output$table <- render_gt(sad_clinch, align = "left")
    } else if (input$var == "Angry" &
               input$county == "Clinch"){
      output$table <- render_gt(angry_clinch, align = "left")
    } else if (input$var == "Frustrated" &
              input$county == "Clinch"){
      output$table <- render_gt(frustrated_clinch, align = "left")
    } else if (input$var == "Tired" &
               input$county == "Clinch"){
      output$table <- render_gt(tired_clinch, align = "left")
    } else if (input$var == "Worried" &
               input$county == "Clinch"){
      output$table <- render_gt(worried_clinch, align = "left")
    } else if (input$var == "Hopeful" &
               input$county == "Clinch"){
      output$table <- render_gt(hopeful_clinch, align = "left")
    } else if (input$var == "Household Income" &
               input$county == "Coffee"){
      output$table <- render_gt(household_income_coffee, align = "left")
    } else if (input$var == "Single Parent" &
               input$county == "Coffee"){
      output$table <- render_gt(single_parent_coffee, align = "left")
    } else if (input$var == "Housing" &
               input$county == "Coffee"){
      output$table <- render_gt(housing_coffee, align = "left")
    } else if (input$var == "Food Security" &
               input$county == "Coffee"){
      output$table <- render_gt(food_coffee, align = "left")
    } else if (input$var == "Internet Access" &
               input$county == "Coffee"){
      output$table <- render_gt(internet_access_coffee, align = "left")
    } else if (input$var == "Child Care" &
               input$county == "Coffee"){
      output$table <- render_gt(child_care_coffee, align = "left")
    } else if (input$var == "Child Care Needs" &
              input$county == "Coffee"){
      output$table <- render_gt(child_care_needs_coffee, align = "left")
    } else if (input$var == "Happy" &
               input$county == "Coffee"){
      output$table <- render_gt(happy_coffee, align = "left")
    } else if (input$var == "Stressed" &
               input$county == "Coffee"){
      output$table <- render_gt(stressed_coffee, align = "left")
    } else if (input$var == "Sad" &
               input$county == "Coffee"){
      output$table <- render_gt(sad_coffee, align = "left")
    } else if (input$var == "Angry" &
               input$county == "Coffee"){
      output$table <- render_gt(angry_coffee, align = "left")
    } else if (input$var == "Frustrated" &
              input$county == "Coffee"){
      output$table <- render_gt(frustrated_coffee, align = "left")
    } else if (input$var == "Tired" &
               input$county == "Coffee"){
      output$table <- render_gt(tired_coffee, align = "left")
    } else if (input$var == "Worried" &
               input$county == "Coffee"){
      output$table <- render_gt(worried_coffee, align = "left")
    } else if (input$var == "Hopeful" &
               input$county == "Coffee"){
      output$table <- render_gt(hopeful_coffee, align = "left")
    } else if (input$var == "Household Income" &
               input$county == "Echols"){
      output$table <- render_gt(household_income_echols, align = "left")
    } else if (input$var == "Single Parent" &
               input$county == "Echols"){
      output$table <- render_gt(single_parent_echols, align = "left")
    } else if (input$var == "Housing" &
               input$county == "Echols"){
      output$table <- render_gt(housing_echols, align = "left")
    } else if (input$var == "Food Security" &
               input$county == "Echols"){
      output$table <- render_gt(food_echols, align = "left")
    } else if (input$var == "Internet Access" &
               input$county == "Echols"){
      output$table <- render_gt(internet_access_echols, align = "left")
    } else if (input$var == "Child Care" &
               input$county == "Echols"){
      output$table <- render_gt(child_care_echols, align = "left")
    } else if (input$var == "Child Care Needs" &
              input$county == "Echols"){
      output$table <- render_gt(child_care_needs_echols, align = "left")
    } else if (input$var == "Happy" &
               input$county == "Echols"){
      output$table <- render_gt(happy_echols, align = "left")
    } else if (input$var == "Stressed" &
               input$county == "Echols"){
      output$table <- render_gt(stressed_echols, align = "left")
    } else if (input$var == "Sad" &
               input$county == "Echols"){
      output$table <- render_gt(sad_echols, align = "left")
    } else if (input$var == "Angry" &
               input$county == "Echols"){
      output$table <- render_gt(angry_echols, align = "left")
    } else if (input$var == "Frustrated" &
              input$county == "Echols"){
      output$table <- render_gt(frustrated_echols, align = "left")
    } else if (input$var == "Tired" &
               input$county == "Echols"){
      output$table <- render_gt(tired_echols, align = "left")
    } else if (input$var == "Worried" &
               input$county == "Echols"){
      output$table <- render_gt(worried_echols, align = "left")
    } else if (input$var == "Hopeful" &
               input$county == "Echols"){
      output$table <- render_gt(hopeful_echols, align = "left")
    } else if (input$var == "Household Income" &
               input$county == "Jeff Davis"){
      output$table <- render_gt(household_income_jeffdavis, align = "left")
    } else if (input$var == "Single Parent" &
               input$county == "Jeff Davis"){
      output$table <- render_gt(single_parent_jeffdavis, align = "left")
    } else if (input$var == "Housing" &
               input$county == "Jeff Davis"){
      output$table <- render_gt(housing_jeffdavis, align = "left")
    } else if (input$var == "Food Security" &
               input$county == "Jeff Davis"){
      output$table <- render_gt(food_jeffdavis, align = "left")
    } else if (input$var == "Internet Access" &
               input$county == "Jeff Davis"){
      output$table <- render_gt(internet_access_jeffdavis, align = "left")
    } else if (input$var == "Child Care" &
               input$county == "Jeff Davis"){
      output$table <- render_gt(child_care_jeffdavis, align = "left")
    } else if (input$var == "Child Care Needs" &
              input$county == "Jeff Davis"){
      output$table <- render_gt(child_care_needs_jeffdavis, align = "left")
    } else if (input$var == "Happy" &
               input$county == "Jeff Davis"){
      output$table <- render_gt(happy_jeffdavis, align = "left")
    } else if (input$var == "Stressed" &
               input$county == "Jeff Davis"){
      output$table <- render_gt(stressed_jeffdavis, align = "left")
    } else if (input$var == "Sad" &
               input$county == "Jeff Davis"){
      output$table <- render_gt(sad_jeffdavis, align = "left")
    } else if (input$var == "Angry" &
               input$county == "Jeff Davis"){
      output$table <- render_gt(angry_jeffdavis, align = "left")
    } else if (input$var == "Frustrated" &
              input$county == "Jeff Davis"){
      output$table <- render_gt(frustrated_jeffdavis, align = "left")
    } else if (input$var == "Tired" &
               input$county == "Jeff Davis"){
      output$table <- render_gt(tired_jeffdavis, align = "left")
    } else if (input$var == "Worried" &
               input$county == "Jeff Davis"){
      output$table <- render_gt(worried_jeffdavis, align = "left")
    } else if (input$var == "Hopeful" &
               input$county == "Jeff Davis"){
      output$table <- render_gt(hopeful_jeffdavis, align = "left")
    } else if (input$var == "Household Income" &
               input$county == "Lanier"){
      output$table <- render_gt(household_income_lanier, align = "left")
    } else if (input$var == "Single Parent" &
               input$county == "Lanier"){
      output$table <- render_gt(single_parent_lanier, align = "left")
    } else if (input$var == "Housing" &
               input$county == "Lanier"){
      output$table <- render_gt(housing_lanier, align = "left")
    } else if (input$var == "Food Security" &
               input$county == "Lanier"){
      output$table <- render_gt(food_lanier, align = "left")
    } else if (input$var == "Internet Access" &
               input$county == "Lanier"){
      output$table <- render_gt(internet_access_lanier, align = "left")
    } else if (input$var == "Child Care" &
               input$county == "Lanier"){
      output$table <- render_gt(child_care_lanier, align = "left")
    } else if (input$var == "Child Care Needs" &
              input$county == "Lanier"){
      output$table <- render_gt(child_care_needs_lanier, align = "left")
    } else if (input$var == "Happy" &
               input$county == "Lanier"){
      output$table <- render_gt(happy_lanier, align = "left")
    } else if (input$var == "Stressed" &
               input$county == "Lanier"){
      output$table <- render_gt(stressed_lanier, align = "left")
    } else if (input$var == "Sad" &
               input$county == "Lanier"){
      output$table <- render_gt(sad_lanier, align = "left")
    } else if (input$var == "Angry" &
               input$county == "Lanier"){
      output$table <- render_gt(angry_lanier, align = "left")
    } else if (input$var == "Frustrated" &
              input$county == "Lanier"){
      output$table <- render_gt(frustrated_lanier, align = "left")
    } else if (input$var == "Tired" &
               input$county == "Lanier"){
      output$table <- render_gt(tired_lanier, align = "left")
    } else if (input$var == "Worried" &
               input$county == "Lanier"){
      output$table <- render_gt(worried_lanier, align = "left")
    } else if (input$var == "Hopeful" &
               input$county == "Lanier"){
      output$table <- render_gt(hopeful_lanier, align = "left")
    } else if (input$var == "Household Income" &
               input$county == "Lowndes"){
      output$table <- render_gt(household_income_lowndes, align = "left")
    } else if (input$var == "Single Parent" &
               input$county == "Lowndes"){
      output$table <- render_gt(single_parent_lowndes, align = "left")
    } else if (input$var == "Housing" &
               input$county == "Lowndes"){
      output$table <- render_gt(housing_lowndes, align = "left")
    } else if (input$var == "Food Security" &
               input$county == "Lowndes"){
      output$table <- render_gt(food_lowndes, align = "left")
    } else if (input$var == "Internet Access" &
               input$county == "Lowndes"){
      output$table <- render_gt(internet_access_lowndes, align = "left")
    } else if (input$var == "Child Care" &
               input$county == "Lowndes"){
      output$table <- render_gt(child_care_lowndes, align = "left")
    } else if (input$var == "Child Care Needs" &
              input$county == "Lowndes"){
      output$table <- render_gt(child_care_needs_lowndes, align = "left")
    } else if (input$var == "Happy" &
               input$county == "Lowndes"){
      output$table <- render_gt(happy_lowndes, align = "left")
    } else if (input$var == "Stressed" &
               input$county == "Lowndes"){
      output$table <- render_gt(stressed_lowndes, align = "left")
    } else if (input$var == "Sad" &
               input$county == "Lowndes"){
      output$table <- render_gt(sad_lowndes, align = "left")
    } else if (input$var == "Angry" &
               input$county == "Lowndes"){
      output$table <- render_gt(angry_lowndes, align = "left")
    } else if (input$var == "Frustrated" &
              input$county == "Lowndes"){
      output$table <- render_gt(frustrated_lowndes, align = "left")
    } else if (input$var == "Tired" &
               input$county == "Lowndes"){
      output$table <- render_gt(tired_lowndes, align = "left")
    } else if (input$var == "Worried" &
               input$county == "Lowndes"){
      output$table <- render_gt(worried_lowndes, align = "left")
    } else if (input$var == "Hopeful" &
               input$county == "Lowndes"){
      output$table <- render_gt(hopeful_lowndes, align = "left")
    } else if (input$var == "Household Income" &
                 input$county == "Ware"){
      output$table <- render_gt(household_income_ware, align = "left")
    } else if (input$var == "Single Parent" &
               input$county == "Ware"){
      output$table <- render_gt(single_parent_ware, align = "left")
    } else if (input$var == "Housing" &
               input$county == "Ware"){
      output$table <- render_gt(housing_ware, align = "left")
    } else if (input$var == "Food Security" &
               input$county == "Ware"){
      output$table <- render_gt(food_ware, align = "left")
    } else if (input$var == "Internet Access" &
               input$county == "Ware"){
      output$table <- render_gt(internet_access_ware, align = "left")
    } else if (input$var == "Child Care" &
               input$county == "Ware"){
      output$table <- render_gt(child_care_ware, align = "left")
    } else if (input$var == "Child Care Needs" &
              input$county == "Ware"){
      output$table <- render_gt(child_care_needs_ware, align = "left")
    } else if (input$var == "Happy" &
               input$county == "Ware"){
      output$table <- render_gt(happy_ware, align = "left")
    } else if (input$var == "Stressed" &
               input$county == "Ware"){
      output$table <- render_gt(stressed_ware, align = "left")
    } else if (input$var == "Sad" &
               input$county == "Ware"){
      output$table <- render_gt(sad_ware, align = "left")
    } else if (input$var == "Angry" &
               input$county == "Ware"){
      output$table <- render_gt(angry_ware, align = "left")
    } else if (input$var == "Frustrated" &
              input$county == "Ware"){
      output$table <- render_gt(frustrated_ware, align = "left")
    } else if (input$var == "Tired" &
               input$county == "Ware"){
      output$table <- render_gt(tired_ware, align = "left")
    } else if (input$var == "Worried" &
               input$county == "Ware"){
      output$table <- render_gt(worried_ware, align = "left")
    } else if (input$var == "Hopeful" &
               input$county == "Ware"){
      output$table <- render_gt(hopeful_ware, align = "left")
    } else {
      output$table <- render_gt(angry, align = "left")
      }
  )
  
  observe(
    if (input$county == "All Counties"){
      output$bars <- renderPlot({services_bars})
    } else if (input$county == "All Counties"){
      output$bars <- renderPlot({org_bars})
    } else {
      output$bars <- renderPlot({rna_bars})
    }
  )
  
  
}
  

    

# Run the application 
shinyApp(ui = ui, server = server)
