##################################################
## Title: check.packages.R
## Description: This function checks if a specific package has been installed. If yes, then it loads this package. If not, then the function installs the package first and then loads it in the session. 
## Input: pkg - Single package name or a list of packages 
## Output: No output returned. Only installs and loads provided packages
## Date: Thu Mar 07 10:16:10 2019
## Author: Purva Kulkarni
## Email: purva.kulkarni@radboudumc.nl
##################################################


check.packages <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}
