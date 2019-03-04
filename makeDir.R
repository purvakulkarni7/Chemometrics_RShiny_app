makeDir <- function(fp) {
  if(!file.exists(fp)) {  # If the folder does not exist, create a new one
   # make.dir(dirname(fp))
    dir.create(fp)
  } else {   # If it existed, delete and replace with a new one  
    unlink(fp, recursive = TRUE)
    dir.create(fp)
  }
} 