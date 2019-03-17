library(xlsx)

# Function to plot PCA for data from Anna

plotPCA <- function(filePath)
{
  data_table <- read.xlsx(filePath, 1)
  
  # Prepare data table based on columns of interest
  start = as.numeric(which(colnames(data_table) == "LFQ.intensity.A1_Tray02-D6_01_4536"))
  end = as.numeric(which(colnames(data_table) == "LFQ.intensity.J4_Tray02-D12_01_4542"))
  
  data_table_sub <- data_table[,start:end]
  
  data.pca <- prcomp(na.omit(data_table_sub), center = TRUE, scale. = TRUE)
  screeplot(data.pca, type = "l", main = "Scree plot")
  
  g <-
    ggbiplot(
      data.pca,
      obs.scale = 1,
      var.scale = 1,
      ellipse = TRUE,
      circle = TRUE,
      labels.size = 5,
      var.axes = FALSE
    )
  
}