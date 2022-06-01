library(shiny)
library(devtools)
library(ggbiplot)
library(FactoMineR)
library(ca)

server <- function(input, output) {
  output$te1 <- renderText({
    "\nFirst six data of iris"
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
    ir.ca <- CA(ir, ncp = 5, graph = F, axes = c(dx,dy) )
    
    plot(ca(ir), dim = c(dx,dy))
    #    plot(iris.ca)
  })
  
  output$t3 <- renderPrint({
    data(iris)
    ir <- iris[,1:4]
    ir.ca <- CA(ir, ncp = 5, graph = F, axes = c(dx,dy) )
    summary(ir.ca)  
  })
}

