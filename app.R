#
# This is a Shiny web application to visualise office hours of staff members
# Department of Criminology, The University of Manchester
#

#library(shiny)
#library(shinydashboard)
library(tidyverse)
library(rsconnect)
library(googlesheets4)
library(DT)

# Load data from Drive ----
options(
  # whenever there is one account token found, use the cached token
  gargle_oauth_email = TRUE,
  # specify auth tokens should be stored in a hidden directory ".secrets"
  gargle_oauth_cache = "secrets"
)

df <- read_sheet('https://docs.google.com/spreadsheets/d/1ajwPY1MzZLiinQjTnjOs8Q5dXm0ZhKVUKPNhqmNPIFc/edit?usp=sharing',
                 sheet = 2)
#1

#df <- read.csv("www/office_hours.csv")
#df <- df %>%
#  rename(`Staff name` = Staff.name,
#         `Helper Column 1` = Helper.Column.1,
#         `Helper Column Staff name` = Helper.Column.Staff.name,
#         `Helper Column 2` = Helper.Column.2)

#options(gargle_oauth_email = "davidbuilgil@gmail.com")

df <- df %>%
  drop_na(`Staff name`) %>%
  rename(Staff_name = `Staff name`) %>%
  select(-`Helper Column 1`,
         -`Helper Column Staff name`,
         -`Helper Column 2`)

# Unique dataset of names of staff and units
staff <- unique(df$Staff_name)
unit <- unique(df$Unit)
unit <- unit[!is.na(unit)]

# Define Criminology UoM app ----

ui <- fluidPage(
  #titlePanel(title = "Criminology@UoM office hours"),
   
  #titlePanel( div(column(width = 6, h2("Criminology@UoM office hours")), 
  #                column(width = 1, tags$img(src = "logo.JPG"))),
  #            windowTitle="MyPage"
  #),
  
  titlePanel(title = span(img(src = "logo.JPG", height = 35), "Department of Criminology - Office hours"),
             windowTitle = "Crim@UoM office hours"),
  
  helpText("We have created this site to help you find the allocated office hours of each member of the staff.\nSelect the member of the staff and/or module."),
  
  # Create a new Row in the UI for selectInputs
  fluidRow(
    column(4,
           selectInput("Staff_name",
                       "Staff name:",
                       c("All",
                         staff))
    ),
    column(4,
           selectInput("Unit",
                       "Unit:",
                       c("All",
                         unit))
    )
  ),
  # Create a new row for the table.
  DT::dataTableOutput("table"),
  verbatimTextOutput("text")
)

server <- function(input, output) {
  
  # Filter data based on selections
  filtered_rows <- reactive({
    data <- df
    if (input$Staff_name != "All") {
      data <- data[data$Staff_name == input$Staff_name,]
    }
    if (input$Unit != "All") {
      data <- data[data$Unit == input$Unit,]
    }
    if (!is.na(input$Unit)) {
      data <- data %>% 
        filter(data$Unit == input$Unit)
    }
    data
  })
  
  # Show filtered data in the datatable
  output$table <- DT::renderDataTable(DT::datatable({ filtered_rows() }))
  
  # Show selected text
  output$text <- renderText({ toString(filtered_rows()[input$table_rows_selected, c("Staff_name", "Unit")]) })
  
}


shinyApp(ui, server)
