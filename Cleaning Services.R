#install.packages("dplyr") # These packages need to be installed once, after that they can be called up using library. This package is for restructuring data
#install.packages("XLConnect") # This package is for loading / writing excel spreadsheets
#install.packages("tidyr")
#install.packages("plyr")
library(plyr) # ****IMPORTANT**** Load plyr before dplyr- they have some of the same named functions, and if you load in a different order it will cause problems
library(dplyr)
library(tidyr)
library(XLConnect)
###################################      SERVICES DATA CHECK      ##########################################
# Set the working directory to the local folder containing dataset. Can be done manually
# by going to Session -> Set Working Directory -> Choose Directory 
macdatawd <- "~/Google Drive/Data Files"
windowsdatawd <- "C:/Users/USER/Google Drive/Data Files"
if(file.exists(macdatawd)){
  setwd(file.path(macdatawd))
} else { 
  if(file.exists(windowsdatawd)){
    setwd(file.path(windowsdatawd))
  }
}


#Setting Vectors that will be used throughout program
metrics <- c("Math","Science","ELA", "Suspensions", "Attendance Rate")

elem <- c("Glenn Elementary School", "Eno Valley Elementary", "EK Powe Elementary School", "YE Smith Elementary")
high <- c("Neal Middle School", "Durham Performance Learning Center", "Hillside High School", "Southern High School", "Northern")

# This is the name I have been using for the detailed service dataset, however we may want to start using a different naming convention
# This is the "Tier II and III Support Detail" report. 
# **** IMPORTANT ***** delete the last row that sums all hours before saving the excel file. That row will cause problems
data<-readWorksheetFromFile('ServiceD.xlsx', sheet=1, header = T, startRow = 2)
colnames(data)[1] <- "Student.ID"
data <- data[!is.na(data$Student.ID), ]# get rid of accidental blank rows
data <- data[-nrow(data),] #removes summation row


#removing Nina's test data
data <- subset(data, !data$Entered.By == "nina")
drops <- c("Provider.Type.2","Provider.Name.2", "Provider.Type.3", "Provider.Name.3", "Case.Management.Intensity.Level",  "School.Where..Support.Was.Provided" )
data <- data[, ! (names(data) %in% drops)]


#Chancing Name format to Fist Name Last Name 
data$Student <- strsplit(data$Student, ", ")

data$Student <- unlist(lapply(names, 
                              function(x) paste(x[1:length(x) %% 2 == 0], 
                                                x[1:length(x) %% 2 != 0])))
data$Student <- trimws(data$Student)

# 
# 
# d <- data %>% group_by(Home.School, Entry.Date, Support.Date, Provider.Type.1, Provider.Name.1, Individual.or.Group, Student.Support.Category, 
#                        Hours, Tier, Notes) %>% summarize(groupsize = n())
# d$groupsize[(!is.na(d$Individual.or.Group)) & d$Individual.or.Group == "Individual"] <- 1
# 
# data_test <- merge(data, d, by = c("Home.School", "Entry.Date", "Support.Date", "Provider.Type.1", "Provider.Name.1", "Individual.or.Group", "Student.Support.Category", 
#                               "Hours", "Tier"))
# data$hoursspent <- data$Hours/data$groupsize
# 




#Saving Data

write.csv(data, "Services.csv")

