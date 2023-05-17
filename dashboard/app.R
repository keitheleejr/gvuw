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
    column(8,
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
                       selected = "Angry")
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
      output$table <- render_gt(single_parent)
    } else if (input$var == "Household Income" &
               input$county == "All Counties"){
      output$table <- render_gt(household_income)
    } else if (input$var == "Single Parent" &
               input$county == "All Counties"){
      output$table <- render_gt(single_parent)
    } else if (input$var == "Housing" &
               input$county == "All Counties"){
      output$table <- render_gt(housing)
    } else if (input$var == "Food Security" &
               input$county == "All Counties"){
      output$table <- render_gt(food)
    } else if (input$var == "Internet Access" &
               input$county == "All Counties"){
      output$table <- render_gt(internet_access)
    } else if (input$var == "Child Care" &
               input$county == "All Counties"){
      output$table <- render_gt(child_care)
    } else if (input$var == "Child Care Needs" &
              input$county == "All Counties"){
      output$table <- render_gt(child_care_needs)
    } else if (input$var == "Happy" &
               input$county == "All Counties"){
      output$table <- render_gt(happy)
    } else if (input$var == "Stressed" &
               input$county == "All Counties"){
      output$table <- render_gt(stressed)
    } else if (input$var == "Sad" &
               input$county == "All Counties"){
      output$table <- render_gt(sad)
    } else if (input$var == "Frustrated" &
              input$county == "All Counties"){
      output$table <- render_gt(frustrated)
    } else if (input$var == "Tired" &
               input$county == "All Counties"){
      output$table <- render_gt(tired)
    } else if (input$var == "Worried" &
               input$county == "All Counties"){
      output$table <- render_gt(worried)
    } else if (input$var == "Hopeful" &
               input$county == "All Counties"){
      output$table <- render_gt(hopeful)
    } else if (input$var == "Household Income" &
               input$county == "Atkinson"){
      output$table <- render_gt(household_income_atkinson)
    } else if (input$var == "Single Parent" &
               input$county == "Atkinson"){
      output$table <- render_gt(single_parent_atkinson)
    } else if (input$var == "Housing" &
               input$county == "Atkinson"){
      output$table <- render_gt(housing_atkinson)
    } else if (input$var == "Food Security" &
               input$county == "Atkinson"){
      output$table <- render_gt(food_atkinson)
    } else if (input$var == "Internet Access" &
               input$county == "Atkinson"){
      output$table <- render_gt(internet_access_atkinson)
    } else if (input$var == "Child Care" &
               input$county == "Atkinson"){
      output$table <- render_gt(child_care_atkinson)
    } else if (input$var == "Child Care Needs" &
              input$county == "Atkinson"){
      output$table <- render_gt(child_care_needs_atkinson)
    } else if (input$var == "Happy" &
               input$county == "Atkinson"){
      output$table <- render_gt(happy_atkinson)
    } else if (input$var == "Stressed" &
               input$county == "Atkinson"){
      output$table <- render_gt(stressed_atkinson)
    } else if (input$var == "Sad" &
               input$county == "Atkinson"){
      output$table <- render_gt(sad_atkinson)
    } else if (input$var == "Angry" &
               input$county == "Atkinson"){
      output$table <- render_gt(angry_atkinson)
    } else if (input$var == "Frustrated" &
              input$county == "Atkinson"){
      output$table <- render_gt(frustrated_atkinson)
    } else if (input$var == "Tired" &
               input$county == "Atkinson"){
      output$table <- render_gt(tired_atkinson)
    } else if (input$var == "Worried" &
               input$county == "Atkinson"){
      output$table <- render_gt(worried_atkinson)
    } else if (input$var == "Hopeful" &
               input$county == "Atkinson"){
      output$table <- render_gt(hopeful_atkinson)
    } else if (input$var == "Household Income" &
                input$county == "Bacon"){
      output$table <- render_gt(household_income_bacon)
    } else if (input$var == "Single Parent" &
               input$county == "Bacon"){
      output$table <- render_gt(single_parent_bacon)
    } else if (input$var == "Housing" &
               input$county == "Bacon"){
      output$table <- render_gt(housing_bacon)
    } else if (input$var == "Food Security" &
               input$county == "Bacon"){
      output$table <- render_gt(food_bacon)
    } else if (input$var == "Internet Access" &
               input$county == "Bacon"){
      output$table <- render_gt(internet_access_bacon)
    } else if (input$var == "Child Care" &
               input$county == "Bacon"){
      output$table <- render_gt(child_care_bacon)
    } else if (input$var == "Child Care Needs" &
              input$county == "Bacon"){
      output$table <- render_gt(child_care_needs_bacon)
    } else if (input$var == "Happy" &
               input$county == "Bacon"){
      output$table <- render_gt(happy_bacon)
    } else if (input$var == "Stressed" &
               input$county == "Bacon"){
      output$table <- render_gt(stressed_bacon)
    } else if (input$var == "Sad" &
               input$county == "Bacon"){
      output$table <- render_gt(sad_bacon)
    } else if (input$var == "Angry" &
               input$county == "Bacon"){
      output$table <- render_gt(angry_bacon)
    } else if (input$var == "Frustrated" &
              input$county == "Bacon"){
      output$table <- render_gt(frustrated_bacon)
    } else if (input$var == "Tired" &
               input$county == "Bacon"){
      output$table <- render_gt(tired_bacon)
    } else if (input$var == "Worried" &
               input$county == "Bacon"){
      output$table <- render_gt(worried_bacon)
    } else if (input$var == "Hopeful" &
               input$county == "Bacon"){
      output$table <- render_gt(hopeful_bacon)
    } else if (input$var == "Household Income" &
               input$county == "Berrien"){
      output$table <- render_gt(household_income_berrien)
    } else if (input$var == "Single Parent" &
               input$county == "Berrien"){
      output$table <- render_gt(single_parent_berrien)
    } else if (input$var == "Housing" &
               input$county == "Berrien"){
      output$table <- render_gt(housing_berrien)
    } else if (input$var == "Food Security" &
               input$county == "Berrien"){
      output$table <- render_gt(food_berrien)
    } else if (input$var == "Internet Access" &
               input$county == "Berrien"){
      output$table <- render_gt(internet_access_berrien)
    } else if (input$var == "Child Care" &
               input$county == "Berrien"){
      output$table <- render_gt(child_care_berrien)
    } else if (input$var == "Child Care Needs" &
              input$county == "Berrien"){
      output$table <- render_gt(child_care_needs_berrien)
    } else if (input$var == "Happy" &
               input$county == "Berrien"){
      output$table <- render_gt(happy_berrien)
    } else if (input$var == "Stressed" &
               input$county == "Berrien"){
      output$table <- render_gt(stressed_berrien)
    } else if (input$var == "Sad" &
               input$county == "Berrien"){
      output$table <- render_gt(sad_berrien)
    } else if (input$var == "Angry" &
               input$county == "Berrien"){
      output$table <- render_gt(angry_berrien)
    } else if (input$var == "Frustrated" &
              input$county == "Berrien"){
      output$table <- render_gt(frustrated_berrien)
    } else if (input$var == "Tired" &
               input$county == "Berrien"){
      output$table <- render_gt(tired_berrien)
    } else if (input$var == "Worried" &
               input$county == "Berrien"){
      output$table <- render_gt(worried_berrien)
    } else if (input$var == "Hopeful" &
               input$county == "Berrien"){
      output$table <- render_gt(hopeful_berrien)
    } else if (input$var == "Household Income" &
               input$county == "Brooks"){
      output$table <- render_gt(household_income_brooks)
    } else if (input$var == "Single Parent" &
               input$county == "Brooks"){
      output$table <- render_gt(single_parent_brooks)
    } else if (input$var == "Housing" &
               input$county == "Brooks"){
      output$table <- render_gt(housing_brooks)
    } else if (input$var == "Food Security" &
               input$county == "Brooks"){
      output$table <- render_gt(food_brooks)
    } else if (input$var == "Internet Access" &
               input$county == "Brooks"){
      output$table <- render_gt(internet_access_brooks)
    } else if (input$var == "Child Care" &
               input$county == "Brooks"){
      output$table <- render_gt(child_care_brooks)
    } else if (input$var == "Child Care Needs" &
              input$county == "Brooks"){
      output$table <- render_gt(child_care_needs_brooks)
    } else if (input$var == "Happy" &
               input$county == "Brooks"){
      output$table <- render_gt(happy_brooks)
    } else if (input$var == "Stressed" &
               input$county == "Brooks"){
      output$table <- render_gt(stressed_brooks)
    } else if (input$var == "Sad" &
               input$county == "Brooks"){
      output$table <- render_gt(sad_brooks)
    } else if (input$var == "Angry" &
               input$county == "Brooks"){
      output$table <- render_gt(angry_brooks)
    } else if (input$var == "Frustrated" &
              input$county == "Brooks"){
      output$table <- render_gt(frustrated_brooks)
    } else if (input$var == "Tired" &
               input$county == "Brooks"){
      output$table <- render_gt(tired_brooks)
    } else if (input$var == "Worried" &
               input$county == "Brooks"){
      output$table <- render_gt(worried_brooks)
    } else if (input$var == "Hopeful" &
               input$county == "Brooks"){
      output$table <- render_gt(hopeful_brooks)
    } else if (input$var == "Household Income" &
               input$county == "Clinch"){
      output$table <- render_gt(household_income_clinch)
    } else if (input$var == "Single Parent" &
               input$county == "Clinch"){
      output$table <- render_gt(single_parent_clinch)
    } else if (input$var == "Housing" &
               input$county == "Clinch"){
      output$table <- render_gt(housing_clinch)
    } else if (input$var == "Food Security" &
               input$county == "Clinch"){
      output$table <- render_gt(food_clinch)
    } else if (input$var == "Internet Access" &
               input$county == "Clinch"){
      output$table <- render_gt(internet_access_clinch)
    } else if (input$var == "Child Care" &
               input$county == "Clinch"){
      output$table <- render_gt(child_care_clinch)
    } else if (input$var == "Child Care Needs" &
              input$county == "Clinch"){
      output$table <- render_gt(child_care_needs_clinch)
    } else if (input$var == "Happy" &
               input$county == "Clinch"){
      output$table <- render_gt(happy_clinch)
    } else if (input$var == "Stressed" &
               input$county == "Clinch"){
      output$table <- render_gt(stressed_clinch)
    } else if (input$var == "Sad" &
               input$county == "Clinch"){
      output$table <- render_gt(sad_clinch)
    } else if (input$var == "Angry" &
               input$county == "Clinch"){
      output$table <- render_gt(angry_clinch)
    } else if (input$var == "Frustrated" &
              input$county == "Clinch"){
      output$table <- render_gt(frustrated_clinch)
    } else if (input$var == "Tired" &
               input$county == "Clinch"){
      output$table <- render_gt(tired_clinch)
    } else if (input$var == "Worried" &
               input$county == "Clinch"){
      output$table <- render_gt(worried_clinch)
    } else if (input$var == "Hopeful" &
               input$county == "Clinch"){
      output$table <- render_gt(hopeful_clinch)
    } else if (input$var == "Household Income" &
               input$county == "Coffee"){
      output$table <- render_gt(household_income_coffee)
    } else if (input$var == "Single Parent" &
               input$county == "Coffee"){
      output$table <- render_gt(single_parent_coffee)
    } else if (input$var == "Housing" &
               input$county == "Coffee"){
      output$table <- render_gt(housing_coffee)
    } else if (input$var == "Food Security" &
               input$county == "Coffee"){
      output$table <- render_gt(food_coffee)
    } else if (input$var == "Internet Access" &
               input$county == "Coffee"){
      output$table <- render_gt(internet_access_coffee)
    } else if (input$var == "Child Care" &
               input$county == "Coffee"){
      output$table <- render_gt(child_care_coffee)
    } else if (input$var == "Child Care Needs" &
              input$county == "Coffee"){
      output$table <- render_gt(child_care_needs_coffee)
    } else if (input$var == "Happy" &
               input$county == "Coffee"){
      output$table <- render_gt(happy_coffee)
    } else if (input$var == "Stressed" &
               input$county == "Coffee"){
      output$table <- render_gt(stressed_coffee)
    } else if (input$var == "Sad" &
               input$county == "Coffee"){
      output$table <- render_gt(sad_coffee)
    } else if (input$var == "Angry" &
               input$county == "Coffee"){
      output$table <- render_gt(angry_coffee)
    } else if (input$var == "Frustrated" &
              input$county == "Coffee"){
      output$table <- render_gt(frustrated_coffee)
    } else if (input$var == "Tired" &
               input$county == "Coffee"){
      output$table <- render_gt(tired_coffee)
    } else if (input$var == "Worried" &
               input$county == "Coffee"){
      output$table <- render_gt(worried_coffee)
    } else if (input$var == "Hopeful" &
               input$county == "Coffee"){
      output$table <- render_gt(hopeful_coffee)
    } else if (input$var == "Household Income" &
               input$county == "Echols"){
      output$table <- render_gt(household_income_echols)
    } else if (input$var == "Single Parent" &
               input$county == "Echols"){
      output$table <- render_gt(single_parent_echols)
    } else if (input$var == "Housing" &
               input$county == "Echols"){
      output$table <- render_gt(housing_echols)
    } else if (input$var == "Food Security" &
               input$county == "Echols"){
      output$table <- render_gt(food_echols)
    } else if (input$var == "Internet Access" &
               input$county == "Echols"){
      output$table <- render_gt(internet_access_echols)
    } else if (input$var == "Child Care" &
               input$county == "Echols"){
      output$table <- render_gt(child_care_echols)
    } else if (input$var == "Child Care Needs" &
              input$county == "Echols"){
      output$table <- render_gt(child_care_needs_echols)
    } else if (input$var == "Happy" &
               input$county == "Echols"){
      output$table <- render_gt(happy_echols)
    } else if (input$var == "Stressed" &
               input$county == "Echols"){
      output$table <- render_gt(stressed_echols)
    } else if (input$var == "Sad" &
               input$county == "Echols"){
      output$table <- render_gt(sad_echols)
    } else if (input$var == "Angry" &
               input$county == "Echols"){
      output$table <- render_gt(angry_echols)
    } else if (input$var == "Frustrated" &
              input$county == "Echols"){
      output$table <- render_gt(frustrated_echols)
    } else if (input$var == "Tired" &
               input$county == "Echols"){
      output$table <- render_gt(tired_echols)
    } else if (input$var == "Worried" &
               input$county == "Echols"){
      output$table <- render_gt(worried_echols)
    } else if (input$var == "Hopeful" &
               input$county == "Echols"){
      output$table <- render_gt(hopeful_echols)
    } else if (input$var == "Household Income" &
               input$county == "Jeff Davis"){
      output$table <- render_gt(household_income_jeffdavis)
    } else if (input$var == "Single Parent" &
               input$county == "Jeff Davis"){
      output$table <- render_gt(single_parent_jeffdavis)
    } else if (input$var == "Housing" &
               input$county == "Jeff Davis"){
      output$table <- render_gt(housing_jeffdavis)
    } else if (input$var == "Food Security" &
               input$county == "Jeff Davis"){
      output$table <- render_gt(food_jeffdavis)
    } else if (input$var == "Internet Access" &
               input$county == "Jeff Davis"){
      output$table <- render_gt(internet_access_jeffdavis)
    } else if (input$var == "Child Care" &
               input$county == "Jeff Davis"){
      output$table <- render_gt(child_care_jeffdavis)
    } else if (input$var == "Child Care Needs" &
              input$county == "Jeff Davis"){
      output$table <- render_gt(child_care_needs_jeffdavis)
    } else if (input$var == "Happy" &
               input$county == "Jeff Davis"){
      output$table <- render_gt(happy_jeffdavis)
    } else if (input$var == "Stressed" &
               input$county == "Jeff Davis"){
      output$table <- render_gt(stressed_jeffdavis)
    } else if (input$var == "Sad" &
               input$county == "Jeff Davis"){
      output$table <- render_gt(sad_jeffdavis)
    } else if (input$var == "Angry" &
               input$county == "Jeff Davis"){
      output$table <- render_gt(angry_jeffdavis)
    } else if (input$var == "Frustrated" &
              input$county == "Jeff Davis"){
      output$table <- render_gt(frustrated_jeffdavis)
    } else if (input$var == "Tired" &
               input$county == "Jeff Davis"){
      output$table <- render_gt(tired_jeffdavis)
    } else if (input$var == "Worried" &
               input$county == "Jeff Davis"){
      output$table <- render_gt(worried_jeffdavis)
    } else if (input$var == "Hopeful" &
               input$county == "Jeff Davis"){
      output$table <- render_gt(hopeful_jeffdavis)
    } else if (input$var == "Household Income" &
               input$county == "Lanier"){
      output$table <- render_gt(household_income_lanier)
    } else if (input$var == "Single Parent" &
               input$county == "Lanier"){
      output$table <- render_gt(single_parent_lanier)
    } else if (input$var == "Housing" &
               input$county == "Lanier"){
      output$table <- render_gt(housing_lanier)
    } else if (input$var == "Food Security" &
               input$county == "Lanier"){
      output$table <- render_gt(food_lanier)
    } else if (input$var == "Internet Access" &
               input$county == "Lanier"){
      output$table <- render_gt(internet_access_lanier)
    } else if (input$var == "Child Care" &
               input$county == "Lanier"){
      output$table <- render_gt(child_care_lanier)
    } else if (input$var == "Child Care Needs" &
              input$county == "Lanier"){
      output$table <- render_gt(child_care_needs_lanier)
    } else if (input$var == "Happy" &
               input$county == "Lanier"){
      output$table <- render_gt(happy_lanier)
    } else if (input$var == "Stressed" &
               input$county == "Lanier"){
      output$table <- render_gt(stressed_lanier)
    } else if (input$var == "Sad" &
               input$county == "Lanier"){
      output$table <- render_gt(sad_lanier)
    } else if (input$var == "Angry" &
               input$county == "Lanier"){
      output$table <- render_gt(angry_lanier)
    } else if (input$var == "Frustrated" &
              input$county == "Lanier"){
      output$table <- render_gt(frustrated_lanier)
    } else if (input$var == "Tired" &
               input$county == "Lanier"){
      output$table <- render_gt(tired_lanier)
    } else if (input$var == "Worried" &
               input$county == "Lanier"){
      output$table <- render_gt(worried_lanier)
    } else if (input$var == "Hopeful" &
               input$county == "Lanier"){
      output$table <- render_gt(hopeful_lanier)
    } else if (input$var == "Household Income" &
               input$county == "Lowndes"){
      output$table <- render_gt(household_income_lowndes)
    } else if (input$var == "Single Parent" &
               input$county == "Lowndes"){
      output$table <- render_gt(single_parent_lowndes)
    } else if (input$var == "Housing" &
               input$county == "Lowndes"){
      output$table <- render_gt(housing_lowndes)
    } else if (input$var == "Food Security" &
               input$county == "Lowndes"){
      output$table <- render_gt(food_lowndes)
    } else if (input$var == "Internet Access" &
               input$county == "Lowndes"){
      output$table <- render_gt(internet_access_lowndes)
    } else if (input$var == "Child Care" &
               input$county == "Lowndes"){
      output$table <- render_gt(child_care_lowndes)
    } else if (input$var == "Child Care Needs" &
              input$county == "Lowndes"){
      output$table <- render_gt(child_care_needs_lowndes)
    } else if (input$var == "Happy" &
               input$county == "Lowndes"){
      output$table <- render_gt(happy_lowndes)
    } else if (input$var == "Stressed" &
               input$county == "Lowndes"){
      output$table <- render_gt(stressed_lowndes)
    } else if (input$var == "Sad" &
               input$county == "Lowndes"){
      output$table <- render_gt(sad_lowndes)
    } else if (input$var == "Angry" &
               input$county == "Lowndes"){
      output$table <- render_gt(angry_lowndes)
    } else if (input$var == "Frustrated" &
              input$county == "Lowndes"){
      output$table <- render_gt(frustrated_lowndes)
    } else if (input$var == "Tired" &
               input$county == "Lowndes"){
      output$table <- render_gt(tired_lowndes)
    } else if (input$var == "Worried" &
               input$county == "Lowndes"){
      output$table <- render_gt(worried_lowndes)
    } else if (input$var == "Hopeful" &
               input$county == "Lowndes"){
      output$table <- render_gt(hopeful_lowndes)
    } else if (input$var == "Household Income" &
                 input$county == "Ware"){
      output$table <- render_gt(household_income_ware)
    } else if (input$var == "Single Parent" &
               input$county == "Ware"){
      output$table <- render_gt(single_parent_ware)
    } else if (input$var == "Housing" &
               input$county == "Ware"){
      output$table <- render_gt(housing_ware)
    } else if (input$var == "Food Security" &
               input$county == "Ware"){
      output$table <- render_gt(food_ware)
    } else if (input$var == "Internet Access" &
               input$county == "Ware"){
      output$table <- render_gt(internet_access_ware)
    } else if (input$var == "Child Care" &
               input$county == "Ware"){
      output$table <- render_gt(child_care_ware)
    } else if (input$var == "Child Care Needs" &
              input$county == "Ware"){
      output$table <- render_gt(child_care_needs_ware)
    } else if (input$var == "Happy" &
               input$county == "Ware"){
      output$table <- render_gt(happy_ware)
    } else if (input$var == "Stressed" &
               input$county == "Ware"){
      output$table <- render_gt(stressed_ware)
    } else if (input$var == "Sad" &
               input$county == "Ware"){
      output$table <- render_gt(sad_ware)
    } else if (input$var == "Angry" &
               input$county == "Ware"){
      output$table <- render_gt(angry_ware)
    } else if (input$var == "Frustrated" &
              input$county == "Ware"){
      output$table <- render_gt(frustrated_ware)
    } else if (input$var == "Tired" &
               input$county == "Ware"){
      output$table <- render_gt(tired_ware)
    } else if (input$var == "Worried" &
               input$county == "Ware"){
      output$table <- render_gt(worried_ware)
    } else if (input$var == "Hopeful" &
               input$county == "Ware"){
      output$table <- render_gt(hopeful_ware)
    } else {
      output$table <- render_gt(angry)
      }
  )
}
  

    

# Run the application 
shinyApp(ui = ui, server = server)
