##################################################
## Title: BarPlot.R
## Description: The script generates bar plot for a mass feature present in multiple samples
## Date: Thu Feb 14 11:33:27 2019
## Author: Purva Kulkarni
##################################################


library(devtools)
install_github("vqv/ggbiplot", force = TRUE)

library(ggbiplot)


BarPlot <- function()
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
  
  #log.data <- log(data_table_sub_t_sub_ST[,1:10000])
  data <- data_table_t_sub[, (1:(ncol(data_table_t_sub) - 1))]
  
  # The following step is to resolve the error
  # cannot rescale a constant/zero column to unit variance (Ref: https://stackoverflow.com/questions/40315227/how-to-solve-prcomp-default-cannot-rescale-a-constant-zero-column-to-unit-var)
  data <- data[, apply(data, 2, var) != 0]
  SampleType <- data_table_t_sub[, ncol(data_table_t_sub)]
  
  repeat {
    featureID <- readline(prompt = "Enter feature ID: ")
    featureID <- as.numeric(featureID)
    
    # http://www.sthda.com/english/wiki/ggplot2-barplots-quick-start-guide-r-software-and-data-visualization
    library(ggplot2)
    df <-
      data.frame(Sample = c(1:nrow(data)), Intensity = data[featureID][[1]])
    p <-
      ggplot(data = df, aes(x = Sample, y = Intensity, fill = SampleType)) +
      geom_bar(stat = "identity") + theme(
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black")
      )
    print(p + scale_x_continuous(breaks = seq(1, nrow(data), 1)) + scale_y_continuous(expand = c(0, 0)) + ggtitle(paste("Bar plot feature ID #",featureID)))
    
    if (Continue <-
        readline(prompt = "Do you want to continue, type y or n: ") == "n")
    {
      print("The script will exit now.")
      break
    }
  }
}
