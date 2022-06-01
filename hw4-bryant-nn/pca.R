install.packages("devtools")
library(devtools)
install_github("vqv/ggbiplot")
library(ggbiplot)
library(shiny)
library(ggvis)
data(iris)
# log transform 
log.ir <- log(iris[, 1:4])
ir.species <- iris[, 5]
# apply PCA - scale. = TRUE is highly advisable, but default is FALSE. 
ir.pca <- prcomp(log.ir,center = TRUE, scale. = TRUE)
summary(ir.pca)

g <- ggbiplot(ir.pca, choices = c(1,2) , obs.scale = 1, var.scale = 1, groups = ir.species)
g <- g + scale_color_discrete(name = '')
g <- g + theme(legend.direction = 'horizontal', legend.position = 'top')
print(g)

###############
install.packages("FactoMineR")

library("FactoMineR")
ir <- iris[,1:4]
tes <- ir[,1]+ir[,2]+ir[,3]+ir[,4]
tes
r <- ir / tes

s <- data.frame(
  Sepal.Length = sum(r$Sepal.Length) / dim(r)[1],
  Sepal.Width = sum(r$Sepal.Width) / dim(r)[1],
  Petal.Length = sum(r$Petal.Length) / dim(r)[1],
  Petal.Width = sum(r$Petal.Width) / dim(r)[1]
)
s1 <- s
for(i in 1:149){
s <- rbind(s1,s)
}
s
y <- r - s
y
iris[,5]
row.names(ir) <- iris[,5]
output = as.character(levels(ir.species)[as.integer(ir.species)])

iris.ca <- CA(ir, ncp = 5, graph = T , axes = c(1,2) )
print(iris.ca)
summary(iris.ca)
plot.CA(iris.ca)
plot(iris.ca)
print(iris.ca)
?CA()
#################

?plot
library(ca)
ir <- iris[,1:4]
i.ca <- ca(ir)
plot.ca(i.ca , dim= c(1,2) )
i.ca$nd
?ca()
head(iris)
