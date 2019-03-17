library(ggplot2)
library(grid)
library(gridExtra)

# http://www.rpubs.com/bpiccolo/pcaplots
# http://huboqiang.cn/2016/03/03/RscatterPlotPCA
# https://stackoverflow.com/questions/45774242/plotting-princomp-loadings-with-ggplot

SamplePCA_Temp <- function()
{
  # Enter the file path (for example: Sample_data/170328_ControlsvsPatients_2018-10-08_ESIpos_HEADERREMOVED.tsv)
  filePath <- readline(prompt = "Enter file path: ")
  data_table <- read.table(filePath, header = TRUE)
  
  #Transpose
  data_table_t <- as.data.frame(t(data_table))
  data_table_t_sub <- data_table_t[4:nrow(data_table_t), ]
  SampleType <- row.names(data_table_t_sub)
  SampleType <- substr(SampleType, 1, 2)
  data_table_t_sub <- cbind(data_table_t_sub, SampleType)
  
  data<- data_table_t_sub[,(1:(ncol(data_table_t_sub)-1))]
  
  data<- data[ , apply(data, 2, var) != 0] 
  SampleType <- data_table_t_sub[,ncol(data_table_t_sub)]
  data.pca <- prcomp(data, center = TRUE, scale. = TRUE)
  
  df_out <- as.data.frame(data.pca$x)
  df_out$group <- sapply( strsplit(as.character(row.names(data)), "_"), "[[", 1 )
  
  theme<-theme(panel.background = element_blank(),panel.border=element_rect(fill=NA),panel.grid.major = element_blank(),panel.grid.minor = element_blank(),strip.background=element_blank(),axis.text.x=element_text(colour="black"),axis.text.y=element_text(colour="black"),axis.ticks=element_line(colour="black"),plot.margin=unit(c(1,1,1,1),"line"))
  
  # Calculate PC percentage
  percentage <- round(data.pca$sdev / sum(data.pca$sdev) * 100, 2)
  percentage <- paste(colnames(df_out), "(", paste( as.character(percentage), "%", ")", sep="") )
  
  # Scores plot without labels
  pdf("PCA_Score_without_labels.pdf", width = 10,height = 10)
  ggplot(df_out,aes(x=PC1,y=PC2,color=group)) + geom_point()+theme + xlab(percentage[1]) + ylab(percentage[2])
  dev.off()

  
  # Scores plot with labels
  pdf("PCA_Score_with_labels.pdf", width = 10,height = 10)
  ggplot(df_out,aes(x=PC1,y=PC2,color=group, label=row.names(data))) + geom_point()+ geom_text(size=2) + theme + xlab(percentage[1]) + ylab(percentage[2])
  dev.off()
  
  # Plot features that contribute to the classification (loadings)
  df_out_r <- as.data.frame(data.pca$rotation)
  df_out_r$feature <- row.names(df_out_r)
  
  # Loadings plot 
  pdf("PCA_Loadings.pdf", width = 10,height = 10)
  ggplot(df_out_r,aes(x=PC1,y=PC2,label=feature,color=feature )) + geom_point()+theme + geom_text(size=2)
  dev.off()

}