

library(shiny)
library(shinythemes)
library(shinyWidgets)

source('./dashexample_plots.R')


######

ui <- fluidPage(
  theme = shinytheme("paper"),
  setBackgroundColor(color = "snow"),
  titlePanel("Hospital Department Snapshots, 2016-2018"),
  h4("2016 n = 2,497, 2017 n = 2,721, 2018 n = 2,490"),
  h5("Data is de-identified and artificial for example purposes"),
  
  br(),
  br(),
  
  tags$style(type='text/css', ".selectize-input{font-size: 16px; line-height: 32px;} label{font-size: 24px;} 
             .selectize-dropdown{font-size: 16px; line-height: 28px;}"),
  
  fluidRow(
    column(5,
           selectInput("risk", "Health Profile", c("Risk Profile", "Cantril Scale", "Little Interest/Pleasure in Things",
                                                   "Feeling Down Depressed")),
           plotOutput("risk")
    ),
    column(2,
           selectInput("yr", "Year", c("2016", "2017", "2018"),
                       multiple = TRUE, selected = c("2016", "2017", "2018")),
           p(strong("Please select one or more years.")),

           div(tableOutput("agestats"), style = "font-size:115%"),
           div(tableOutput("salarystats"), style = "font-size:115%")
           
    ),
    column(5,
           selectInput("dem", "Demographics", c("Age", "Sex", "Salary"),
                       multiple = FALSE),
           plotOutput("demographic")
    )
  ),
    
  br(),
  br(),
  br(),
  
  fluidRow(
    column(4,
           selectInput("cult", "Healthy Culture", c("Supervisor Support", "Physical Environment"), 
                       multiple = FALSE),
           plotOutput("culture")
    ),
    column(4,
           selectInput("prod", "Productivity Barriers", c("Health", "Caregiving", "Lack Resources",
                                                          "Lack Time", "Financial", "Lack Training"),
                       multiple = FALSE),
           plotOutput("productivity")
    ),
    column(4,
           selectInput("hlth", "Health Behaviors", c("Low Fruit Veg", "Lack Exercise", "Smoking", 
                                                     "Lack Sleep", "High BMI", "Unmanaged Stress"),
                       multiple = FALSE),
           plotOutput("health"))
  ),
  
  br(),
  br()
)



server <- function(input, output) {
  
  output$risk <- renderPlot({
    barrisk(input$risk, year = input$yr)
  })
  
  output$demographic <- renderPlot({
    bardem(input$dem, year = input$yr)
  })
  
  output$culture <- renderPlot({
    barcult(input$cult, year = input$yr)
  })
  
  output$health <- renderPlot({
    barhealth(input$hlth, year = input$yr)
  })
  
  output$productivity <- renderPlot({
    barprod(input$prod, year = input$yr)
  })
  output$agestats <- renderTable({
    agetable(year = input$yr)
  })
  output$salarystats <- renderTable({
    salarytable(year = input$yr)
  })
}

shinyApp(ui = ui, server = server)