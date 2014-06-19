# Developed by Daniel Adornes as partial requirement 
# for the Developing Data Products course, 
# by Johns Hopkins University through Coursera

library(shiny)

# Define UI for the application 
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Analysis of Violent Crime Rates by US State"),
  
  h5("Developed by Daniel Adornes as partial requirement for the Developing Data Products course, by Johns Hopkins University through Coursera"),
  
  br(),
  
  # Sidebar with controls to select the crime type and the ordering preference
  sidebarLayout(
    sidebarPanel(
      selectInput("crime", "Select a Crime type below:",
                   c("Murder" = "Murder",
                     "Assault" = "Assault",
                     "UrbanPop" = "UrbanPop",
                     "Rape" = "Rape")),
      br(),
      radioButtons("ascDesc", "Select the desired order:",
                  c("Ascending" = "A",
                    "Descending" = "D")),
      br(),br(),
      h6("The data of this application is provided by the USArrests dataset, included in the 'datasets' R package ")
    ),
    
    # Show a tabset that includes a plot, summary, and table view
    # of the data
    mainPanel(
      tabsetPanel(type = "tabs", 
                  tabPanel("Plot", plotOutput("plot", height="1000px")), 
                  tabPanel("Summary", verbatimTextOutput("summary")), 
                  tabPanel("Table", tableOutput("table"))
      ),
      br()
    )
  )
))
