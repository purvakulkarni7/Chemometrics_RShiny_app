##################################################
## Title: BarPlot.R
## Description: The script generates bar plot for a mass feature present in multiple samples
## Date: Thu Feb 14 11:33:27 2019
## Author: Purva Kulkarni
##################################################


library(devtools)
install_github("vqv/ggbiplot", force = TRUE)

library(ggbiplot)
library(RColorBrewer)
library(ggplot2)
library(stringr)

# https://www.stat.ubc.ca/~jenny/STAT545A/block17_colorsGgplot2Qualitative.html
# https://stackoverflow.com/questions/6919025/how-to-assign-colors-to-categorical-variables-in-ggplot2-that-have-stable-mappin\
# https://stackoverflow.com/questions/36609476/ggplot2-draw-individual-ellipses-but-color-by-group

BarPlot <- function()
{
  # Enter the file path (for example: Sample_data/170328_ControlsvsPatients_2018-10-08_ESIpos_HEADERREMOVED.tsv, Sample_data/190125_ControlsvsPatients_2019-02-05_ESIpos_HEADERREMOVED.tsv)
  filePath <- readline(prompt = "Enter file path: ")
  data_table <-
    read.table(filePath,
               header = TRUE,
               skip = 2,
               fill = TRUE)
  
  #Transpose
  data_table_t <- as.data.frame(t(data_table))
  FeatureIdColumn <- data_table[1]
  MassColumn <- data_table[2]
  RTColumn <- data_table[3]
  data_table_t_sub <- data_table_t[4:nrow(data_table_t),]
  SampleType <- row.names(data_table_t_sub)
  SampleType <- word(SampleType, sep = "_")
  data_table_t_sub <- cbind(data_table_t_sub, SampleType)
  
  #log.data <- log(data_table_sub_t_sub_ST[,1:10000])
  data <- data_table_t_sub[, (1:(ncol(data_table_t_sub) - 1))]
  
  # The following step is to resolve the error
  # cannot rescale a constant/zero column to unit variance (Ref: https://stackoverflow.com/questions/40315227/how-to-solve-prcomp-default-cannot-rescale-a-constant-zero-column-to-unit-var)
  data <- data[, apply(data, 2, var) != 0]
  SampleType <- data_table_t_sub[, ncol(data_table_t_sub)]
  
  pal <- c(
    "Controle" = "#000000",
    "Patient" = "#9e9e9e",
    "QC" = "#0000ff",
    "Validation" = "#008700"
  )
  
  columnNames <- colnames(data_table)
  columnNames <- columnNames[4:length(columnNames)]
  rownames(data) = make.names(columnNames, unique = TRUE)
  colnames(data) = FeatureIdColumn$F
  
  
  repeat {
    featureID <- readline(prompt = "Enter feature ID: ")
    featureID <- as.numeric(featureID)
    
    # http://www.sthda.com/english/wiki/ggplot2-barplots-quick-start-guide-r-software-and-data-visualization
    
    temp = data[as.character(featureID)]
    
    df <-
      data.frame(Sample = c(1:nrow(data)), Intensity = c(temp[, 1]))
    rownames(df) <- rownames(data)
    
    p <-
      ggplot(data = df, aes(x = Sample, y = Intensity, fill = SampleType)) +
      geom_bar(stat = "identity") + theme(
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black")
      ) +
      theme(axis.ticks.x = element_blank()) +
      scale_x_continuous(breaks = seq(1, nrow(data)),
                         labels = c(rownames(df))) +
      theme(
        axis.text.x = element_text(angle = 90, hjust = 1),
        axis.text.x.bottom = element_text(vjust = 0.5)
      ) +
      scale_y_continuous(expand = c(0, 0)) +
      ggtitle(
        paste(
          "Feature ID #",
          featureID,
          ", m/z = ",
          MassColumn$Mass[which(colnames(data) == featureID)],
          ", RT = ",
          RTColumn$R[which(colnames(data) == featureID)],
          "min"
        )
      ) +
      theme(plot.title = element_text(size = 10, face = "bold")) +
      scale_fill_manual(values = pal, limits = names(pal))
    
    print(p)
    
    if (Continue <-
        readline(prompt = "Do you want to continue, type y or n: ") == "n")
    {
      print("The script will exit now.")
      break
    }
  }
}


# temp <- str_split(rownames(df), "_")
# paste(temp[[1]][1], temp[[1]][length(temp[[1]])], sep="_")