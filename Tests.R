library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)
library(stats)

# UI
ui <- fluidPage(
  titlePanel("Tests statistiques"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Importer un fichier CSV"),
      selectInput("test", "Test statistique", choices = c("Test de Student", "Test du khi-deux", "ANOVA", "MANOVA", "ANCOVA")),
      selectInput("var", "Variable dépendante", choices = NULL),
      selectInput("group", "Variable de groupe", choices = NULL),
      selectInput("covariate", "Variable de covariable (ANCOVA uniquement)", choices = NULL),
      actionButton("run", "Exécuter")
    ),
    mainPanel(
      plotOutput("plot"),
      verbatimTextOutput("summary")
    )
  )
)

# Server
server <- function(input, output) {

  # Charger les données
  data <- reactive({
    req(input$file)
    read.csv(input$file$datapath)
  })

  # Variables
  observe({
    req(data())
    updateSelectInput(session, "var", "Variable dépendante", choices = names(data()))
    updateSelectInput(session, "group", "Variable de groupe", choices = names(data()))
    updateSelectInput(session, "covariate", "Variable de covariable (ANCOVA uniquement)", choices = names(data()))
  })

  # Test de Student
  output$student_test <- reactive({
    req(input$var, input$group)
    result <- t.test(data()[[input$var]] ~ data()[[input$group]])
    return(result)
  })

  # Test du khi-deux
  output$chi_square_test <- reactive({
    req(input$var, input$group)
    result <- chisq.test(data()[[input$var]], data()[[input$group]])
    return(result)
  })

  # ANOVA
  output$anova_test <- reactive({
    req(input$var, input$group)
    result <- aov(data()[[input$var]] ~ data()[[input$group]])
    return(result)
  })

  # MANOVA
  output$manova_test <- reactive({
    req(input$var, input$group)
    result <- manova(data()[[input$var]] ~ data()[[input$group]])
    return(result)
  })

  # ANCOVA
  output$ancova_test <- reactive({
    req(input$var, input$group, input$covariate)
    result <- lm(data()[[input$var]] ~ data()[[input$group]] + data()[[input$covariate]])
    return(result)
  })

  # Résultats
  output$summary <- renderPrint({
    req(input$run)
    if (input$test == "Test de Student") {
      summary(output$student_test())
    } else if (input$test == "Test du khi-deux") {
      summary(output$chi_square_test())
    } else if (input$test == "ANOVA") {
      summary(output$anova_test())
    } else if (input$test == "MANOVA") {
      summary(output$manova_test())
    } else if (input$test == "ANCOVA") {
      summary(output$ancova_test())
    }
  })

  # Graphique
  output$plot <- renderPlot({
    req(input$run)
    if (input$test == "Test de Student") {
      plot(output$student_test())
    } else if (input$test == "Test du khi-deux") {
      plot(output$chi_square_test())
    } else if (input$test == "ANOVA") {
      plot(output$anova_test())
    } else if (input$test == "MANOVA") {
      plot(output$manova_test())
    } else if (input$test == "ANCOVA") {
      plot(output$ancova_test())
    }
  })
}

# Application
shinyApp(ui = ui, server = server)