##################################################
## Title: makeDir.R
## Description: Checks in the working directory if a new folder that is to be created is already present. If yes, overwirtes this folder. If not, create a new folder with the specified name.
## Input: Name of the folder (directory) to be created
## Output: The function does not retun an output. It creates a new folder (directory) in the working directory using the specified name
## Date: Thu Mar 07 10:19:22 2019
## Author: Purva Kulkarni
## Email: purva.kulkarni@radboudumc.nl
##################################################

makeDir <- function(fp) {
  if (!file.exists(fp)) {
    # If the folder does not exist, create a new one
    # make.dir(dirname(fp))
    dir.create(fp)
  } else {
    # If it existed, delete and replace with a new one
    unlink(fp, recursive = TRUE)
    dir.create(fp)
  }
} 