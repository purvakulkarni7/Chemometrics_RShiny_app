##################################################
## Title: PerturbedFeatureBarPlots.R
## Description: This function generates patient vs control bar plots where a selected patient of interest is highlighted in red and the other patients in grey.
## Input:
## The function asks for the following user input
## 1. File path to interested patient data (exported by the UDA pipeline)
## 2. File path to to Control vs Patient data (exported by the UDA pipeline)
## 3. Filtering parameters (if the user wants to use default parameters , press ENTER key on every parameter prompt)
## Output:
## The function prints multiple individual bar plots equivalent to the number of significant features found based on the filtering parameters provided. The function also exports all bar plots placed together in a grid manner.
## Date: Thu Feb 25 10:06:37 2019
## Author: Purva Kulkarni
## Email: purva.kulkarni@radboudumc.nl
##################################################

# Call the required functions
source("Support_scripts/makeDir.R")
source("Support_scripts/check.packages.R")

# List of required packages and load them
packages <-
  c("ggplot2",
    "RColorBrewer",
    "stringr",
    "tidyverse",
    "gridExtra",
    "scales")
check.packages(packages)

# Start of function
PerturbedFeatureBarPlots <- function()
{
  # Enter the file path (for example: Sample_data\190125_Patient_plasma_BB18-01585_2019-02-05_ESIpos.tsv)
  patientFilePath <-
    readline(prompt = "Enter interested patient file path: ")
  
  # Enter the file path (for example: Sample_data\190125_ControlsvsPatients_2019-02-05_ESIpos.tsv)
  CoVsPaFilePath <-
    readline(prompt = "Enter control vs Patient file path: ")
  
  # Feature filter options
  cat("Enter the different filter options below:")
  mp <-
    readline(prompt = "Filter based on metabolic panel (YES or NO) [Press ENTER to use default = YES]: ")
  if (mp == "")
  {
    mp <- "YES"
    cat(mp)
  }
  
  mass_delta <-
    as.numeric(readline(prompt = "Enter value to filter mass (ppm) [Press ENTER to use default = 5]: "))
  if (is.na(mass_delta))
  {
    mass_delta <- as.numeric(5)
    cat(mass_delta)
  }
  
  RT_delta_min <-
    as.numeric(readline(prompt = "Enter minimum value to filter RT [press ENTER to use default = -1.0]: "))
  if (is.na(RT_delta_min))
  {
    RT_delta_min <- as.numeric(-1)
    cat(RT_delta_min)
  }
  
  RT_delta_max <-
    as.numeric(readline(prompt = "Enter maximum value to filter RT [press ENTER to use default = 10]: "))
  if (is.na(RT_delta_max))
  {
    RT_delta_max <- as.numeric(10)
    cat(RT_delta_max)
  }
  
  BFH_Pval <-
    as.numeric(
      readline(prompt = "Enter value to filter Bonferroni Holm P values [press ENTER to use default = 0.05]: ")
    )
  if (is.na(BFH_Pval))
  {
    BFH_Pval <- as.numeric(0.05)
    cat(paste(BFH_Pval, "\n\n"))
  }
  
  # Read patient data file
  patientFile <-
    read.table(
      patientFilePath,
      header = TRUE,
      sep = "\t",
      fill = TRUE,
      quote = "",
      stringsAsFactors = FALSE
    )
  
  # Extract the patient identifier from the patient file 
  rawDataIdentifier = patientFile[which(patientFile$Feature_ID == "Raw Data Identifiers"), 2]
  tempString = str_split(rawDataIdentifier, ";")
  rawDataIdentifier = tempString[[1]][1]
  
  data_table <- read.table(CoVsPaFilePath, fill = TRUE)
  patientID <-
    as.character(data_table[2, ((which(data_table[1, 1:ncol(data_table)] == rawDataIdentifier)) + 3)])
  
  data_table <- data_table[(2:nrow(data_table)), ]
  colnames(data_table) <- as.character(unlist(data_table[1, ]))
  data_table <- data_table[(2:nrow(data_table)), ]
  
  patientFile = patientFile[1:(which(patientFile$Feature_ID == "Worklist") -
                                 1),]
  
  # Extract acquisition mode information
  patientFileName <- basename(patientFilePath)
  temp <- str_split(patientFileName, "[_,-]+")
  if (temp[[1]][length(temp[[1]])] == "ESIpos.tsv")
    mode = "ESIpos"
  else if (temp[[1]][length(temp[[1]])] == "ESIneg.tsv")
    mode = "ESIneg"
  
  y = 1
  
  # Define a empty list of perturbed feature IDs
  perturbedFeatureID = list()
  
  # Perform filtering in the patient data based on specified filtering parameters
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
  
  # Print the number of perturbed features found after filtering
  cat(
    paste(
      length(perturbedFeatureID),
      "statistically significant features found based on filtering. Now printing bar plots..."
    )
  )
  
  # Handle duplicate rownames 
  tempList <- colnames(data_table)
  
  j = 1
  k = 1
  
  for (i in 5:length(tempList))
  {
    if (startsWith(tempList[i], "C") || startsWith(tempList[i], "P"))
    {
      if ((i %% 2) != 0)
        tempList[i] = paste(tempList[i], ".1", sep = "")
    }
    else if (startsWith(tempList[i], "Q"))
    {
      tempList[i] = paste(tempList[i], ".", j, sep = "")
      j = j + 1
    }
    else if (startsWith(tempList[i], "V"))
    {
      tempList[i] = paste(tempList[i], ".", k, sep = "")
      k = k + 1
    }
  }
  
  # Extract requires column information from the patient vs coltrol data
  data_table_t <- as.data.frame(t(data_table))
  FeatureIdColumn <- data_table[1]
  MassColumn <- data_table[2]
  RTColumn <- data_table[3]
  data_table_t_sub <- data_table_t[4:nrow(data_table_t), ]
  row.names(data_table_t_sub) <- tempList[4:length(tempList)]
  
  patientIDColumnNumber = which(rownames(data_table_t_sub) == patientID)
  SampleType <- row.names(data_table_t_sub)
  
  # Find the Sample type based on column names in patient vs coltrol data
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
  
  # Generate a color pallete to color fill bars from different sample type
  pal <-
    setNames(c("#000000", "#9e9e9e", "#FF0000", "#0000ff", "#008700"),
             levels(SampleType))
  
  # set row and column names for the transposed data frame
  columnNames <- colnames(data_table)
  columnNames <- columnNames[4:length(columnNames)]
  rownames(data) = make.names(columnNames, unique = TRUE)
  colnames(data) = FeatureIdColumn$F
  
  # Generate a new directory to export the bar plots
  newDir = paste(patientID, "OutputPlots", mode, sep = "_")
  makeDir(newDir)
  outputPath = paste(".", "/", newDir, sep = "")
  
  # Create an empty plotlist to save ggplot objects
  plotList = list()
  
  # For loop to generate a barplot for each pertubed feature
  for (x in 1:length(perturbedFeatureID))
  {
    # Extract single feature ID and related information
    featureID <- as.numeric(perturbedFeatureID[[x]])
    temp = data[as.character(featureID)]
    
    df <-
      data.frame(Sample = c(1:nrow(data)), Intensity = c(as.numeric(as.character(temp[,1]))))
    
    rownames(df) <- rownames(data)
    
    # Generate plot name
    plotName <- paste("Plot", featureID, sep = "_")
    
    # Create the ggplot (themes used here are compatible with combined bar plots displayed in a grid)
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
      theme(plot.title = element_text(size = 4, face = "bold")) +
      scale_fill_manual(values = pal, limits = names(pal)) +
      theme(legend.position = "none") +
      theme(axis.text = element_text(size = 4),
            axis.title = element_text(size = 4, face = "bold"))
    
    # Save the ggPlot object in the plotList
    plotList[[x]] = p
    
    # Add additional themes for exporting individual bar plots
    suppressMessages(
      p <- p + theme(
        legend.position = "bottom",
        legend.text = element_text(size = 7),
        legend.title = element_blank()
      ) +
        theme(
          axis.text.x = element_text(angle = 90, hjust = 1),
          axis.text.x.bottom = element_text(vjust = 0.5)
        ) +  scale_x_continuous(
          breaks = seq(1, nrow(data)),
          labels = c(rownames(df))
        ) + theme(plot.title = element_text(size = 8, face = "bold"))  +
        scale_y_continuous(expand = c(0, 0), labels = comma) +
        theme(
          axis.text = element_text(size = 6),
          axis.title = element_text(size = 7, face = "bold")
        )
    )
    
    # Generate name for bar plot image file
    plotFileName <-
      paste(featureID, patientID, mode, ".png", sep = "_")
    
    # Save the bar plot
    suppressMessages(
      ggsave(
        plotFileName,
        device = "png",
        scale = 1,
        dpi = 600,
        width = 10,
        height = 7,
        path = outputPath
      )
    )
  }
  
  # Generate combined bar plot and save in a grid manner (16 plots per sheet)
  if (length(plotList) > 16)
  {
    plotNumber <- length(plotList)
    FullPlotSheetNumber = plotNumber %/% 16
    
    RemainderPlotSheet = plotNumber %% 16
    
    for (i in 1:FullPlotSheetNumber)
    {
      temp <- n2mfrow(16)
      val <-
        grid.arrange(grobs = plotList[((16 * (i - 1)) + 1):(i * 16)],  ncol = temp[2])
      fileName <-
        paste(patientID, mode, "CombinedPlots", i, sep = "_")
      ggsave(
        paste(fileName, ".png", sep = ""),
        val,
        dpi = 600,
        width = 9.43,
        height = 4.8
      )
    }
    
    if (RemainderPlotSheet > 0)
    {
      temp <- n2mfrow(RemainderPlotSheet)
      val <-
        grid.arrange(grobs = plotList[((FullPlotSheetNumber * 16) + 1):plotNumber],  ncol = temp[2])
      ggsave(
        paste(patientID, mode, "CombinedPlots_last.png", sep = "_"),
        val,
        dpi = 600,
        width = 9.43,
        height = 4.8
      )
    }
    
  }
  else
  {
    temp <- n2mfrow(length(plotList))
    val <- grid.arrange(grobs = plotList,  ncol = temp[2], legend)
    ggsave(
      paste(patientID, mode, "CombinedPlots.png", sep = "_"),
      val,
      dpi = 600,
      width = 9.43,
      height = 4.8
    )
  }
  
  cat("\nAll barplots successfully generated!\n")
  cat("==================================================\n")
  # End of function
}
