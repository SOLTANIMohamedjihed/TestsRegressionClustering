library(shiny)
library(cluster)

# UI
ui <- fluidPage(
  titlePanel("Clustering avec différentes mesures de distance"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Importer un fichier CSV"),
      selectInput("clustering_type", "Type de clustering", choices = c("k-means", "k-medoids", "hiérarchique")),
      selectInput("distance", "Mesure de distance", choices = c("Euclidienne", "Manhattan", "Corrélation")),
      actionButton("run", "Exécuter")
    ),
    mainPanel(
      plotOutput("plot"),
      verbatimTextOutput("clusters")
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

  # Clustering
  output$clusters <- renderPrint({
    req(input$run)
    distance_matrix <- dist(data(), method = input$distance)
    if (input$clustering_type == "k-means") {
      clusters <- kmeans(data(), centers = 3)
    } else if (input$clustering_type == "k-medoids") {
      clusters <- pam(distance_matrix, k = 3)
    } else if (input$clustering_type == "hiérarchique") {
      clusters <- hclust(distance_matrix)
    }
    clusters$cluster
  })

  # Graphique
  output$plot <- renderPlot({
    req(input$run)
    distance_matrix <- dist(data(), method = input$distance)
    if (input$clustering_type == "k-means") {
      clusters <- kmeans(data(), centers = 3)
      plot(data(), col = clusters$cluster)
    } else if (input$clustering_type == "k-medoids") {
      clusters <- pam(distance_matrix, k = 3)
      plot(data(), col = clusters$clustering)
    } else if (input$clustering_type == "hiérarchique") {
      clusters <- hclust(distance_matrix)
      plot(clusters)
    }
  })
}

# Application
shinyApp(ui = ui, server = server)