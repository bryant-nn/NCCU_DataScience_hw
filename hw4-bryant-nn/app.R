#install.packages("devtools")
#install_github("vqv/ggbiplot")
#install.packages("FactoMineR")
#install.packages("ca")

library(devtools)
library(ggbiplot)
library(shiny)
#library(FactoMineR)
library(ca)
library(usethis)
library(ggplot2)
library(plyr)
library(scales)
library(grid)


# Define UI for app that draws a histogram ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("HW4"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      selectInput(inputId = "x",label = 'PCA X',choices = c("pc1" = 1, "pc2" = 2, "pc3" = 3, "pc4" = 4),
                  selected = T),
      selectInput(inputId = "y",label = 'PCA Y',choices = c("pc1" = 1, "pc2" = 2, "pc3" = 3, "pc4" = 4),
                  selected = T),
      selectInput(inputId = "dimX",label = 'CA dimesionX',choices = c( "dim1" = 1, "dim2" = 2, "dim3" = 3),
                selected = T),
      selectInput(inputId = "dimY",label = 'CA dimesionY',choices = c( "dim1" = 1, "dim2" = 2, "dim3" = 3),
                  selected = T),
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Histogram ----
      textOutput("te1"),
      tableOutput("t1"),
      plotOutput(outputId = "distPlot"),
      verbatimTextOutput("t2"),
      plotOutput(outputId = "caPlot"),
      textOutput("te2"),
      verbatimTextOutput("t3")
    )
  )
)

# Define server logic required to draw a histogram ----
server <- function(input, output) {
  output$te1 <- renderText({
    "\nFirst six data of iris:"
  })
  
  output$t1 <- renderTable({
    head(iris)
  })

  output$distPlot <- renderPlot({
    
    x1 <- as.numeric(input$x)
    y1 <- as.numeric(input$y)
    
    data(iris)
    # log transform 
    log.ir <- log(iris[, 1:4])
    ir <- iris[,1:4]
    ir.species <- iris[, 5]
    # apply PCA - scale. = TRUE is highly advisable, but default is FALSE. 
    ir.pca <- prcomp(log.ir,center = TRUE, scale. = TRUE)
    
    g <- ggbiplot(ir.pca, choices = c(x1,y1) , obs.scale = 1, var.scale = 1, groups = ir.species)
    g <- g + scale_color_discrete(name = '')
    g <- g + theme(legend.direction = 'horizontal', legend.position = 'top')
    
    plot(g)
    
  })
  output$t2 <- renderPrint({
    data(iris)
    # log transform 
    log.ir <- log(iris[, 1:4])
    # apply PCA - scale. = TRUE is highly advisable, but default is FALSE. 
    ir.pca <- prcomp(log.ir,center = TRUE, scale. = TRUE)
    
    summary(ir.pca)  
  })
  
  output$caPlot <- renderPlot({
    dx <- as.numeric(input$dimX)
    dy <- as.numeric(input$dimY)
    
    data(iris)
    ir <- iris[,1:4]
#    ir.ca <- CA(ir, ncp = 5, graph = F, axes = c(dx,dy) )

    plot(ca(ir), dim = c(dx,dy))
#    plot(iris.ca)
  })
  output$te2 <- renderText({
    "Singular Values :"
  })
  
  output$t3 <- renderPrint({
    dx <- as.numeric(input$dimX)
    dy <- as.numeric(input$dimY)
    
    data(iris)
    ir <- iris[,1:4]
    ir.ca <- ca(ir)
    ir.ca$sv 
  })
}

# Create Shiny app ----
shinyApp(ui = ui, server = server)

