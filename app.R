library(shiny)
library(ggplot2)
library(dplyr)

nba_data <- read.csv("nba_data_processed.csv")  

ui <- fluidPage(
  titlePanel("NBA Player Stats Analysis"),
  
  sidebarLayout(
    sidebarPanel(
      h3("Modeling Methodology"),
      p("This application uses linear regression to analyze the relationship between different NBA player statistics. Linear regression is a simple yet powerful statistical method used to model the relationship between a dependent variable (response) and one or more independent variables (predictors). By selecting a predictor and a response variable, you can examine how changes in the predictor are associated with changes in the response."),
      
      h3("Dataset Description"),
      p("The dataset used in this application contains NBA player statistics from the 2023-24 season. It includes various performance metrics such as points per game, assists, rebounds, steals, and more. This dataset allows for an in-depth analysis of player performance and can help identify key factors that contribute to a player's success."),
      
      selectInput("predictor", "Select Predictor:", 
                  choices = colnames(nba_data)[3:length(colnames(nba_data))]),
      selectInput("response", "Select Response:", 
                  choices = colnames(nba_data)[3:length(colnames(nba_data))]),
      actionButton("run_model", "Run Model")
    ),
    
    mainPanel(
      plotOutput("scatterPlot"),
      verbatimTextOutput("modelSummary")
    )
  )
)

server <- function(input, output) {
  # Server logic will go here
}

shinyApp(ui = ui, server = server)



server <- function(input, output) {
  observeEvent(input$run_model, {
    predictor <- input$predictor
    response <- input$response
    
    formula <- as.formula(paste(response, "~", predictor))
    model <- lm(formula, data = nba_data)
    
    output$scatterPlot <- renderPlot({
      ggplot(nba_data, aes_string(x = predictor, y = response)) +
        geom_point() +
        geom_smooth(method = "lm") +
        ggtitle(paste("Linear Regression of", response, "on", predictor))
    })
    
    output$modelSummary <- renderPrint({
      summary(model)
    })
  })
}

shinyApp(ui = ui, server = server)

