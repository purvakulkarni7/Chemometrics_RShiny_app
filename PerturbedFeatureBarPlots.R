PerturbedFeatureBarPlots <- function()
{
  # Enter the file path (for example: Sample_data/190125_Patient_plasma_BB18-01585_2019-02-05_ESIpos.tsv)
  patientFilePath <-
    readline(prompt = "Enter interested patient file path: ")
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
                                 1), ]
  
  patientFileName <- basename(patientFilePath)
  temp <- str_split(patientFileName, "[_,-]+")
  patientID <-
    paste(temp[[1]][2], temp[[1]][3], temp[[1]][4], sep = "_")
  patientID <- paste(patientID, temp[[1]][5], sep = ".")
  
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
    readline(prompt = "Filter based on metabolic panel (YES or NO): ")
  mass_delta <-
    as.numeric(readline(prompt = "Enter value to filter mass (ppm): "))
  RT_delta_min <-
    as.numeric(readline(prompt = "Enter minimum value to filter RT: "))
  RT_delta_max <-
    as.numeric(readline(prompt = "Enter maximum value to filter RT: "))
  BFH_Pval <-
    as.numeric(readline(prompt = "Enter value to filter Bonferroni Holm P values: "))
  
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
  
  data_table_t <- as.data.frame(t(data_table))
  FeatureIdColumn <- data_table[1]
  MassColumn <- data_table[2]
  RTColumn <- data_table[3]
  data_table_t_sub <- data_table_t[4:nrow(data_table_t),]
  
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
  
  dir.create(paste(patientID, "OutputBarPlots", sep = "_"))
  outputPath = paste(".", "/", paste(patientID, "OutputBarPlots", sep = "_"), sep = "")
  
  
  for (x in 1:length(perturbedFeatureID))
  {
    featureID <- as.numeric(perturbedFeatureID[[x]])
    
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
      scale_fill_manual(values = pal, limits = names(pal)) +
      theme(
        legend.position = "bottom",
        legend.text = element_text(size = 7),
        legend.title = element_text(size = 7, face = "bold")
      )
    
    plotName <- paste(featureID, patientID, ".png", sep = "_")
    ggsave(
      plotName,
      device = "png",
      scale = 1,
      dpi = 300,
      width = 10,
      height = 7,
      path = outputPath
    )
  }
}