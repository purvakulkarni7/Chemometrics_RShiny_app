source("makeDir.R")
source("check.packages.R")
# require(ggplot2)
# require(RColorBrewer)
# require(stringr)
# require(tidyverse)
# require(gridExtra)
# require(scales)

packages<-c("ggplot2", "RColorBrewer", "stringr", "tidyverse", "gridExtra", "scales")
check.packages(packages)

PerturbedFeatureBarPlots <- function()
{
  # Enter the file path (for example: Sample_data/190125_Patient_plasma_BB18-01585_2019-02-05_ESIpos.tsv)
  patientFilePath <-
    readline(prompt = "Enter interested patient file path: ")
  
  # Enter the file path (for example: Sample_data/190125_ControlsvsPatients_2019-02-05_ESIpos.tsv)
  CoVsPaFilePath <-
    readline(prompt = "Enter control vs Patient file path: ")
  data_table <-
    read.table(CoVsPaFilePath,
               header = TRUE,
               skip = 2,
               fill = TRUE)
  
  # Feature filter options
  print("Enter the different filter options below:")
  mp <-
    readline(prompt = "Filter based on metabolic panel (YES or NO) [Press ENTER to use default = YES]: ")
  if (mp == "")
  {
    mp <- "YES"
    print(mp)
  }
  
  mass_delta <-
    as.numeric(readline(prompt = "Enter value to filter mass (ppm) [Press ENTER to use default = 5]: "))
  if (is.na(mass_delta))
  {
    mass_delta <- as.numeric(5)
    print(mass_delta)
  }
  
  RT_delta_min <-
    as.numeric(readline(prompt = "Enter minimum value to filter RT [press ENTER to use default = -1.0]: "))
  if (is.na(RT_delta_min))
  {
    RT_delta_min <- as.numeric(-1)
    print(RT_delta_min)
  }
  
  RT_delta_max <-
    as.numeric(readline(prompt = "Enter maximum value to filter RT [press ENTER to use default = 10]: "))
  if (is.na(RT_delta_max))
  {
    RT_delta_max <- as.numeric(10)
    print(RT_delta_max)
  }
  
  BFH_Pval <-
    as.numeric(
      readline(prompt = "Enter value to filter Bonferroni Holm P values [press ENTER to use default = 0.05]: ")
    )
  if (is.na(BFH_Pval))
  {
    BFH_Pval <- as.numeric(0.05)
    print(BFH_Pval)
  }
  
  patientFile <-
    read.table(
      patientFilePath,
      header = TRUE,
      sep = "\t",
      fill = TRUE,
      quote = "",
      stringsAsFactors = FALSE
    )
  patientFile = patientFile[1:(which(patientFile$Feature_ID == "Worklist") -
                                 1),]
  
  patientFileName <- basename(patientFilePath)
  temp <- str_split(patientFileName, "[_,-]+")
  patientID <-
    paste(temp[[1]][2], temp[[1]][3], temp[[1]][4], sep = "_")
  patientID <- paste(patientID, temp[[1]][5], sep = ".")
  
  if (temp[[1]][length(temp[[1]])] == "ESIpos.tsv")
    mode = "ESIpos"
  else if (temp[[1]][length(temp[[1]])] == "ESIneg.tsv")
    mode = "ESIneg"
  
  y = 1
  
  perturbedFeatureID = list()
  
  for (i in 1:nrow(patientFile))
  {
    if ((patientFile$MP[i] == mp) &&
        (patientFile$Mass_Delta_.ppm.[i] <= mass_delta) &&
        (
          patientFile$RT_Delta_...[i] >= RT_delta_min &
          patientFile$RT_Delta_...[i] <= RT_delta_max
        ) &&
        (patientFile$Bonferroni.Holm_P.Value_Final[i] <= BFH_Pval))
    {
      perturbedFeatureID[[y]] = as.numeric(patientFile$Feature_ID[i])
      y = y + 1
    }
  }
  
  # Remove duplicates in the perturbed feature ID list
  perturbedFeatureID <- unique(perturbedFeatureID)
  
  print(paste(length(perturbedFeatureID), "statistically significant features found based on filtering."))
  
  data_table_t <- as.data.frame(t(data_table))
  FeatureIdColumn <- data_table[1]
  MassColumn <- data_table[2]
  RTColumn <- data_table[3]
  data_table_t_sub <- data_table_t[4:nrow(data_table_t), ]
  
  patientIDColumnNumber = which(rownames(data_table_t_sub) == patientID)
  
  SampleType <- row.names(data_table_t_sub)
  
  for (i in 1:length(SampleType))
  {
    if (i == patientIDColumnNumber)
      SampleType[i] <- SampleType[i]
    else if (i == patientIDColumnNumber + 1)
      SampleType[i] <- SampleType[i - 1]
    else
      SampleType[i] <- word(SampleType[i], sep = "_")
  }
  
  data_table_t_sub <- cbind(data_table_t_sub, SampleType)
  
  #log.data <- log(data_table_sub_t_sub_ST[,1:10000])
  data <- data_table_t_sub[, (1:(ncol(data_table_t_sub) - 1))]
  
  # The following step is to resolve the error
  # cannot rescale a constant/zero column to unit variance (Ref: https://stackoverflow.com/questions/40315227/how-to-solve-prcomp-default-cannot-rescale-a-constant-zero-column-to-unit-var)
  data <- data[, apply(data, 2, var) != 0]
  SampleType <- data_table_t_sub[, ncol(data_table_t_sub)]
  
  # pal <- c(
  #   "Controle" = "#000000",
  #   "Patient" = "#9e9e9e",
  #   "Patient_plasma_BB18.01585" = "#FF0000",
  #   "QC" = "#0000ff",
  #   "Validation" = "#008700"
  # )
  
  pal <-
    setNames(c("#000000", "#9e9e9e", "#FF0000", "#0000ff", "#008700"),
             levels(SampleType))
  
  columnNames <- colnames(data_table)
  columnNames <- columnNames[4:length(columnNames)]
  rownames(data) = make.names(columnNames, unique = TRUE)
  colnames(data) = FeatureIdColumn$F
  
  newDir = paste(patientID, "OutputPlots", mode, sep = "_")
  makeDir(newDir)
  outputPath = paste(".", "/", newDir, sep = "")
  
  plotList = list()
  
  for (x in 1:length(perturbedFeatureID))
  {
    featureID <- as.numeric(perturbedFeatureID[[x]])
    
    # http://www.sthda.com/english/wiki/ggplot2-barplots-quick-start-guide-r-software-and-data-visualization
    
    temp = data[as.character(featureID)]
    
    df <-
      data.frame(Sample = c(1:nrow(data)), Intensity = c(temp[, 1]))
    rownames(df) <- rownames(data)
    plotName <- paste("Plot", featureID, sep = "_")
    
    p <-
      ggplot(data = df, aes(x = Sample, y = Intensity, fill = SampleType)) +
      geom_bar(stat = "identity") + theme(
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "#707070", size = 0.25),
        axis.ticks = element_line(colour = "#707070", size = 0.25)
      ) +
      theme(axis.ticks.x = element_blank()) +
      theme(aspect.ratio = 0.5 / 1) +
      scale_y_continuous(expand = c(0, 0)) +
      ggtitle(
        paste(
          "Feature ID #",
          featureID,
          ", m/z = ",
          MassColumn$Mass[which(colnames(data) == featureID)],
          "\n",
          "RT = ",
          RTColumn$R[which(colnames(data) == featureID)],
          "min"
        )
      ) +
      theme(plot.title = element_text(size = 5, face = "bold")) +
      scale_fill_manual(values = pal, limits = names(pal)) +
      theme(legend.position = "none") +
      theme(axis.text = element_text(size = 4),
            axis.title = element_text(size = 5, face = "bold"))
    
    
    plotList[[x]] = p
    
    p <- p + theme(
      legend.position = "bottom",
      legend.text = element_text(size = 7),
      legend.title = element_blank()
    ) +
      theme(
        axis.text.x = element_text(angle = 90, hjust = 1),
        axis.text.x.bottom = element_text(vjust = 0.5)
      ) +  scale_x_continuous(breaks = seq(1, nrow(data)),
                              labels = c(rownames(df))) + theme(plot.title = element_text(size = 8, face = "bold"))  +
      scale_y_continuous(expand = c(0, 0), labels = comma) +
      theme(axis.text = element_text(size = 6),
            axis.title = element_text(size = 7, face = "bold"))
    
    plotFileName <-
      paste(featureID, patientID, mode, ".png", sep = "_")
    ggsave(
      plotFileName,
      device = "png",
      scale = 1,
      dpi = 600,
      width = 10,
      height = 7,
      path = outputPath
    )
  }
  
  if (length(plotList) > 12)
  {
    plotNumber <- length(plotList)
    FullPlotSheetNumber = plotNumber %/% 12
    
    RemainderPlotSheet = plotNumber %% 12
    
    for (i in 1:FullPlotSheetNumber)
    {
      temp <- n2mfrow(12)
      val <-
        grid.arrange(grobs = plotList[((12 * (i - 1)) + 1):(i * 12)],  ncol = temp[2])
      fileName <-
        paste(patientID, mode, "CombinedPlots", i, sep = "_")
      ggsave(paste(fileName, ".png", sep = ""), val, dpi = 600)
    }
    
    if (RemainderPlotSheet > 0)
    {
      temp <- n2mfrow(RemainderPlotSheet)
      val <-
        grid.arrange(grobs = plotList[((FullPlotSheetNumber * 12) + 1):plotNumber],  ncol = temp[2])
      ggsave(paste(patientID, mode, "CombinedPlots_last.png", sep = "_"),
             val, dpi = 600)
    }
    
  }
  else
  {
    temp <- n2mfrow(length(plotList))
    val <- grid.arrange(grobs = plotList,  ncol = temp[2], legend)
    ggsave(paste(patientID, mode, "CombinedPlots.png", sep = "_"),
           val,
           dpi = 600)
  }
}