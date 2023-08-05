library(shiny)
library(glmnet)
library(MASS)

# UI
ui <- fluidPage(
    titlePanel("Régressions avec régularisation"),
    sidebarLayout(
        sidebarPanel(
            fileInput("file", "Importer un fichier CSV"),
            selectInput("y_col", "Variable cible", choices = NULL),
            selectInput("model", "Modèle", choices = c("Régression linéaire", "Régression non linéaire", "Régression logistique", "Régression polynomiale", "Régression ordinale")),
            sliderInput("lambda", "Valeur de lambda", min = 0, max = 1, value = 0.5, step = 0.01),
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

    # Variables cibles
    observe({
        req(data())
        updateSelectInput(session, "y_col", "Variable cible", choices = names(data()))
    })

    # Régression linéaire
    output$linear_regression <- reactive({
        req(input$y_col)
        formula <- as.formula(paste(input$y_col, paste(setdiff(names(data()), input$y_col), collapse = "+"), sep = "~"))
        model <- glmnet(data = data(), formula = formula, family = "gaussian", alpha = 1, lambda = input$lambda)
        return(model)
    })

    # Régression non linéaire
    output$nonlinear_regression <- reactive({
        req(input$y_col)
        formula <- as.formula(paste(input$y_col, paste(setdiff(names(data()), input$y_col), collapse = "+"), sep = "~"))
        model <- glmnet(data = data(), formula = formula, family = "gaussian", alpha = 0, lambda = input$lambda)
        return(model)
    })

    # Régression logistique
    output$logistic_regression <- reactive({
        req(input$y_col)
        formula <- as.formula(paste(input$y_col, paste(setdiff(names(data()), input$y_col), collapse = "+"), sep = "~"))
        model <- glmnet(data = data(), formula = formula, family = "binomial", alpha = 0, lambda = input$lambda)
        return(model)
    })

    # Régression polynomiale
    output$polynomial_regression <- reactive({
        req(input$y_col)
        formula <- as.formula(paste(input$y_col, paste(setdiff(names(data()), input$y_col), collapse = "+"), sep = "~"))
        poly_data <- poly(data()[, input$y_col], degree = 2, raw = TRUE)
        model <- glmnet(data = poly_data, formula = formula, family = "gaussian", alpha = 1, lambda = input$lambda)
        return(model)
    })

    # Régression ordinale
    output$ordinal_regression <- reactive({
        req(input$y_col)
        formula <- as.formula(paste(input$y_col, paste(setdiff(names(data()), input$y_col), collapse = "+"), sep = "~"))
        model <- polr(formula, data = data(), Hess = TRUE)
        return(model)
    })

    # Résultats
    output$summary <- renderPrint({
        req(input$run)
        if (input$model == "Régression linéaire") {
            coef(output$linear_regression())
        } else if (input$model == "Régression non linéaire") {
            coef(output$nonlinear_regression())
        } else if (input$model == "Régression logistique") {
            coef(output$logistic_regression())
        } else if (input$model == "Régression polynomiale") {
            coef(output$polynomial_regression())
        } else if (input$model == "Régression ordinale") {
            summary(output$ordinal_regression())
        }
    })

    # Graphique
    output$plot <- renderPlot({
        req(input$run)
        if (input$model == "Régression linéaire") {
            plot(output$linear_regression())
        } else if (input$model == "Régression non linéaire") {
            plot(output$nonlinear_regression())
        } else if (input$model == "Régression logistique") {
            plot(output$logistic_regression())
        } else if (input$model == "Régression polynomiale") {
            plot(output$polynomial_regression())
        } else if (input$model == "Régression ordinale") {
            plot(output$ordinal_regression())
        }
    })
}

# Application
shinyApp(ui = ui, server = server)