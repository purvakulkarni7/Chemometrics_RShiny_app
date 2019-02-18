##################################################
## Title: SamplePCA.R
## Description: 
## Date: Tue Feb 5 08:04:00 2019
## Author: Purva Kulkarni
##################################################

library(devtools)
install_github("vqv/ggbiplot", force = TRUE)

library(ggbiplot)


SamplePCA <- function(filePath)
{
  
  data_table <- read.table(filePath, header = TRUE)
  
  #Transpose
  data_table_t <- as.data.frame(t(data_table))
  data_table_t_sub <- data_table_t[4:nrow(data_table_t),]
  SampleType <- row.names(data_table_t_sub)
  SampleType <- substr(SampleType, 1, 2)
  data_table_t_sub <- cbind(data_table_t_sub, SampleType)
  
  #log.data <- log(data_table_sub_t_sub_ST[,1:10000])
  data<- data_table_t_sub[,(1:(ncol(data_table_t_sub)-1))]
  
  # The following step is to resolve the error
  # cannot rescale a constant/zero column to unit variance (Ref: https://stackoverflow.com/questions/40315227/how-to-solve-prcomp-default-cannot-rescale-a-constant-zero-column-to-unit-var)
  data<- data[ , apply(data, 2, var) != 0] 
  SampleType <- data_table_t_sub[,ncol(data_table_t_sub)]
  data.pca <- prcomp(data, center = TRUE, scale. = TRUE)
  
  # Generate scree plot
  plot(data.pca, type = "l", main = "Scree plot")
  
  # Generate biplot
 #  biplot(data.pca)
  
  # Generate ggbiplot
  g <- ggbiplot(data.pca, obs.scale = 1, var.scale = 1,groups = SampleType, ellipse = TRUE,circle = TRUE, labels.size = 5, var.axes = FALSE)
  g <- g + scale_color_discrete(name = '')
  g <- g + theme(legend.direction = 'horizontal',legend.position = 'top')
  print(g)
  
  
  plot(data.pca$x[,1:2], col = SampleType, cex=1.5, pch=16, legend  =TRUE)
  
  # Check which PC's display maximum variance
  par(mfrow=c(2,2))
  plot(data.pca$x[,1], col=SampleType, pch=16, cex = 1)
  plot(data.pca$x[,2], col=SampleType, pch=16, cex = 1)
  plot(data.pca$x[,3], col=SampleType, pch=16, cex = 1)
  plot(data.pca$x[,4], col=SampleType, pch=16, cex = 1)
  
}
