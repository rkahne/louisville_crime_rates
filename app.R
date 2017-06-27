library(shiny)
library(shinysense)
library(shinydashboard)
library(tidyverse)
library(markdown)


crime <- read_csv('shiny_crime.csv')
pop <- tibble(
  population = c(556160, 557789, 566869, 566492, 597269, 600425, 604609, 609863, 612367, 614748, 616261),
  year = c(2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016)
)

crime_type <- crime$CRIME_TYPE %>% unique()

ui <- dashboardPage(
  dashboardHeader(disable = T),
  dashboardSidebar(disable = T),
  dashboardBody(
    fluidRow(box(includeMarkdown('about.md'), width = 12)),
    fluidRow(
      box(title = 'All Crime',shinydrawrUI("all_crime"), width = 12)
    ),
    fluidRow(
      box(title = 'Drugs and Alcohol Violations',shinydrawrUI("drug_alcohol")),
      box(title = 'Theft and Larceny',shinydrawrUI("theft_larceny"))
    ),
    fluidRow(
      box(title = 'Fraud',shinydrawrUI("fraud")),
      box(title = 'Weapons',shinydrawrUI("weapons"))
    ),
    fluidRow(
      box(title = 'Vehicle Break-In/Theft',shinydrawrUI("vehicles")),
      box(title = 'Assault',shinydrawrUI("assault"))
    ),
    fluidRow(
      box(title = 'Burglary',shinydrawrUI("burglary")),
      box(title = 'Vandalism',shinydrawrUI("vandalism"))
    ),
    fluidRow(
      box(title = 'Motor Vehicle Theft',shinydrawrUI("motor_theft")),
      box(title = 'Robbery',shinydrawrUI("robbery"))
    ),
    fluidRow(
      box(title = 'Sex Crimes',shinydrawrUI("sex_crimes")),
      box(title = 'Disturbing the Peace',shinydrawrUI("disturbing_peace"))
    ),
    fluidRow(
      box(title = 'Homicide',shinydrawrUI("homicide")),
      box(title = 'DUI',shinydrawrUI("dui"))
    ),
    fluidRow(
      box(title = 'Arson',shinydrawrUI("arson"))
    )
  )
)
  


server <- function(input, output) {
  crime_rate <- function(crime_descrip){
    crime %>% 
      filter(CRIME_TYPE == crime_descrip) %>% 
      group_by(year) %>% 
      summarize(n = n()) %>% 
      ungroup() %>% 
      filter(year >= 2006) %>% 
      left_join(pop) %>% 
      mutate(rate = n / population * 1000) %>% 
      select(year, rate)
  }
  all_crime <- crime %>%  
    group_by(year) %>% 
    summarize(n = n()) %>% 
    ungroup() %>% 
    filter(year >= 2006) %>% 
    left_join(pop) %>% 
    mutate(rate = n / population * 1000) %>% 
    select(year, rate)
  
  #server side call of the drawr module
  drawChart_ac <- callModule(shinydrawr,"all_crime",all_crime,draw_start = 2012,x_key = "year",y_key = "rate",y_min = 0,y_max = max(all_crime$rate)*1.1)
  drawChart_drug <- callModule(shinydrawr,"drug_alcohol",crime_rate(crime_type[1]),draw_start = 2012,x_key = "year",y_key = "rate",y_min = 0, y_max = max(crime_rate(crime_type[1])$rate)*1.1)
  drawChart_theft <- callModule(shinydrawr,"theft_larceny",crime_rate(crime_type[2]),draw_start = 2012,x_key = "year",y_key = "rate",y_min = 0,y_max = max(crime_rate(crime_type[2])$rate)*1.1)
  drawChart_fraud <- callModule(shinydrawr,"fraud",crime_rate(crime_type[3]),draw_start = 2012,x_key = "year",y_key = "rate",y_min = 0,y_max = max(crime_rate(crime_type[3])$rate)*1.1)
  drawChart_weapons <- callModule(shinydrawr,"weapons",crime_rate(crime_type[4]),draw_start = 2012,x_key = "year",y_key = "rate",y_min = 0,y_max = max(crime_rate(crime_type[4])$rate)*1.1)
  drawChart_vehicles <- callModule(shinydrawr,"vehicles",crime_rate(crime_type[5]),draw_start = 2012,x_key = "year",y_key = "rate",y_min = 0,y_max = max(crime_rate(crime_type[5])$rate)*1.1)
  drawChart_assault <- callModule(shinydrawr,"assault",crime_rate(crime_type[6]),draw_start = 2012,x_key = "year",y_key = "rate",y_min = 0,y_max = max(crime_rate(crime_type[6])$rate)*1.1)
  drawChart_burglary <- callModule(shinydrawr,"burglary",crime_rate(crime_type[7]),draw_start = 2012,x_key = "year",y_key = "rate",y_min = 0,y_max = max(crime_rate(crime_type[7])$rate)*1.1)
  drawChart_vandalism <- callModule(shinydrawr,"vandalism",crime_rate(crime_type[8]),draw_start = 2012,x_key = "year",y_key = "rate",y_min = 0,y_max = max(crime_rate(crime_type[8])$rate)*1.1)
  drawChart_larceny <- callModule(shinydrawr,"theft_larceny",crime_rate(crime_type[9]),draw_start = 2012,x_key = "year",y_key = "rate",y_min = 0,y_max = max(crime_rate(crime_type[9])$rate)*1.1)
  drawChart_motortheft <- callModule(shinydrawr,"motor_theft",crime_rate(crime_type[10]),draw_start = 2012,x_key = "year",y_key = "rate",y_min = 0,y_max = max(crime_rate(crime_type[10])$rate)*1.1)
  drawChart_robbery <- callModule(shinydrawr,"robbery",crime_rate(crime_type[11]),draw_start = 2012,x_key = "year",y_key = "rate",y_min = 0,y_max = max(crime_rate(crime_type[11])$rate)*1.1)
  drawChart_sex <- callModule(shinydrawr,"sex_crimes",crime_rate(crime_type[12]),draw_start = 2012,x_key = "year",y_key = "rate",y_min = 0,y_max = max(crime_rate(crime_type[12])$rate)*1.1)
  drawChart_disturbing <- callModule(shinydrawr,"disturbing_peace",crime_rate(crime_type[13]),draw_start = 2012,x_key = "year",y_key = "rate",y_min = 0,y_max = max(crime_rate(crime_type[13])$rate)*1.1)
  drawChart_homicide <- callModule(shinydrawr,"homicide",crime_rate(crime_type[14]),draw_start = 2012,x_key = "year",y_key = "rate",y_min = 0,y_max = max(crime_rate(crime_type[14])$rate)*1.1)
  drawChart_dui <- callModule(shinydrawr,"dui",crime_rate(crime_type[15]),draw_start = 2012,x_key = "year",y_key = "rate",y_min = 0,y_max = max(crime_rate(crime_type[15])$rate)*1.1)
  drawChart_arson <- callModule(shinydrawr,"arson",crime_rate(crime_type[16]),draw_start = 2012,x_key = "year",y_key = "rate",y_min = 0,y_max = max(crime_rate(crime_type[16])$rate)*1.1)
  
  # #logic for what happens after a user has drawn their values. Note this will fire on editing again too.
  # observeEvent(drawChart_ac(), {
  #   drawnValues = drawChart_ac()
  #   
  #   drawn_data <- pass %>%
  #     filter(year >= 2012) %>%
  #     mutate(drawn = drawnValues)
  #   
  #   output$displayDrawn <- renderTable(drawn_data)
  # })

  
}

# Run the application
shinyApp(ui = ui, server = server)