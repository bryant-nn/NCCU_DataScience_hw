library(shiny)
library(devtools)
library(ggbiplot)
library(FactoMineR)
library(ca)

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
      verbatimTextOutput("t3")
    )
  )
)

