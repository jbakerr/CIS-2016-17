#Run Script to Call All Scripts

macwd <- "~/Code/CIS-2016-17"
windowswd <- "C:/Users/USER/Code/CIS-2016-17"
if(file.exists(macwd)){
  setwd(file.path(macwd))
} else { 
  if(file.exists(windowswd)){
    setwd(file.path(windowswd))
  }
}


source("Cleaning Metrics.R")

if(file.exists(macwd)){
  setwd(file.path(macwd))
} else { 
  if(file.exists(windowswd)){
    setwd(file.path(windowswd))
  }
}

source("Cleaning Services.R")


if(file.exists(macwd)){
  setwd(file.path(macwd))
} else { 
  if(file.exists(windowswd)){
    setwd(file.path(windowswd))
  }
}

source("Student Metrics.R")

