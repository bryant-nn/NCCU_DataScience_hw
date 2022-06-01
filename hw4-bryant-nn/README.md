# hw4

### Name: [your name in Chinese or English]
### Student ID: [108703030]
### ShinyApps link: [<https://bryant0720.shinyapps.io/hw4-bryant-nn/>]

## Description
packages which are needed to install 
install.packages("devtools")
install_github("vqv/ggbiplot")
install.packages("FactoMineR")
install.packages("ca")
* **Please use R version 3.6.3**
* Make the interactive web service of PCA and CA analysis by Shinyapp
* Scoring
  * 85: Make a shiny interactive web site to show PCA analysis (as the following picture) for the iris data such that users can specify which component to show (i.e., PC1, PC2, PC3 ...)
  * 90: Make a shiny interactive web site to show correspondence analysis (CA) analysis for the iris data
  * Plus points: Extra visualizations or tables to show more information, each for 2 points
    * input data
    * PCA result (i.e., amount of variances ... )
    * ...

![pcaExample](/images/img2.png)

* You might start by integrating the following example (pcr.R) into shiny app. Of course, feel free to find other appropriate packages for PCA.

### pca.R

```R
data(iris)
# log transform 
log.ir <- log(iris[, 1:4])
ir.species <- iris[, 5]

# apply PCA - scale. = TRUE is highly advisable, but default is FALSE. 
ir.pca <- prcomp(log.ir,center = TRUE, scale. = TRUE)

library(ggbiplot)
g <- ggbiplot(ir.pca, obs.scale = 1, var.scale = 1, groups = ir.species)
g <- g + scale_color_discrete(name = '')
g <- g + theme(legend.direction = 'horizontal', legend.position = 'top')
print(g)
```
## Example

https://changlab.shinyapps.io/ggvisExample/

![ggvisExample](/images/img1.png)
